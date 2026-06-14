# ------------------------------------------------------------------------
# 1. Load and Clean the Data
# ------------------------------------------------------------------------
data_clean_path <- here("data", "clean.rds")
if (!file.exists(data_clean_path)) {
  stop("Clean data file not found! Please run 'scripts/main.R' or 'scripts/001_import.R' first.")
}
df <- read_rds(data_clean_path)

mean_x <- mean(df$X, na.rm = TRUE)
mean_y <- mean(df$Y, na.rm = TRUE)
median_x <- median(df$X, na.rm = TRUE)
median_y <- median(df$Y, na.rm = TRUE)

df_tested <- df |>
  mutate(
    dist_mean_center = sqrt((X - mean_x)^2 + (Y - mean_y)^2) / 1000,
    dist_median_center = sqrt((X - median_x)^2 + (Y - median_y)^2) / 1000,
    severity = factor(X
      HUMRAT_TEUNA,
      levels = c(3, 2, 1),
      labels = c("Minor", "Severe", "Fatal"),
      ordered = TRUE
    ),
    is_severe_or_fatal = HUMRAT_TEUNA %in% c(1, 2),
    road_type = factor(
      ifelse(SUG_DEREH %in% 1:4, SUG_DEREH, NA),
      levels = 1:4,
      labels = c("Urban Inter.", "Urban Non-Inter.", "Non-Urban Inter.", "Non-Urban Non-Inter.")
    ) |> droplevels(),
    is_urban = SUG_DEREH %in% c(1, 2),
    day_night = factor(
      ifelse(YOM_LAYLA == 1, "Day", ifelse(YOM_LAYLA == 5, "Night", NA)),
      levels = c("Day", "Night")
    ) |> droplevels()
  )

# Remove any rows with missing key covariates for modeling
df_model_clean <- df_tested |>
  filter(!is.na(road_type), !is.na(day_night), !is.na(dist_mean_center))

cat("========================================================================\n")
cat("          ADVANCED STATISTICAL ANALYSIS AND HYPOTHESIS TESTING\n")
cat("========================================================================\n\n")

cat(sprintf("Total Observations: %d\n", nrow(df_tested)))
cat(sprintf("Observations for regression/modeling (complete cases): %d\n\n", nrow(df_model_clean)))

# ------------------------------------------------------------------------
# 2. Multiple Testing Correction (Post-hoc Pairwise Comparisons)
# ------------------------------------------------------------------------
cat("--- SECTION 1: Multiple Testing & Post-hoc Pairwise Comparisons ---\n")
pairwise_res <- pairwise.wilcox.test(df_tested$dist_mean_center, df_tested$severity, p.adjust.method = "none")
p_vals <- c(
  "Minor vs Severe" = pairwise_res$p.value["Severe", "Minor"],
  "Minor vs Fatal"  = pairwise_res$p.value["Fatal", "Minor"],
  "Severe vs Fatal" = pairwise_res$p.value["Fatal", "Severe"]
)
p_bonf <- p.adjust(p_vals, method = "bonferroni")
p_fdr  <- p.adjust(p_vals, method = "fdr")

comp_summary <- data.frame(
  "Raw_p_value" = p_vals,
  "Bonferroni_p" = p_bonf,
  "FDR_p" = p_fdr
)
print(comp_summary)
cat("\n")

# ------------------------------------------------------------------------
# 3. Bootstrap Confidence Intervals
# ------------------------------------------------------------------------
cat("--- SECTION 2: Bootstrap Confidence Intervals (B = 500) ---\n")
set.seed(42)
B <- 500

# Bootstrap difference in median distances (Fatal - Minor)
boot_med_diffs <- replicate(B, {
  indices <- sample(1:nrow(df_tested), replace = TRUE)
  sub_df <- df_tested[indices, ]
  median(sub_df$dist_mean_center[sub_df$severity == "Fatal"], na.rm = TRUE) - 
    median(sub_df$dist_mean_center[sub_df$severity == "Minor"], na.rm = TRUE)
})
ci_med_diff <- quantile(boot_med_diffs, probs = c(0.025, 0.975))

# Bootstrap Weibull MLE Parameters
boot_weibull <- replicate(B, {
  indices <- sample(1:nrow(df_tested), replace = TRUE)
  d_boot <- df_tested$dist_mean_center[indices]
  fit <- tryCatch(
    fitdistr(d_boot, "weibull"), 
    error = function(e) list(estimate = c(shape = NA, scale = NA))
  )
  fit$estimate
})
ci_shape <- quantile(boot_weibull["shape", ], probs = c(0.025, 0.975), na.rm = TRUE)
ci_scale <- quantile(boot_weibull["scale", ], probs = c(0.025, 0.975), na.rm = TRUE)

cat(sprintf("95%% Bootstrap CI for Median Distance Diff (Fatal - Minor): [%.3f km, %.3f km]\n", ci_med_diff[1], ci_med_diff[2]))
cat(sprintf("95%% Bootstrap CI for Weibull Shape parameter (alpha):    [%.4f, %.4f]\n", ci_shape[1], ci_shape[2]))
cat(sprintf("95%% Bootstrap CI for Weibull Scale parameter (beta):     [%.3f km, %.3f km]\n\n", ci_scale[1], ci_scale[2]))

# Get point estimates for Weibull fit to pass to plotting
fit_weib <- fitdistr(df_tested$dist_mean_center, "weibull")
alpha_hat <- fit_weib$estimate["shape"]
beta_hat <- fit_weib$estimate["scale"]

# ------------------------------------------------------------------------
# 4. Generalized Likelihood Ratio Test (GLRT) for Interaction
# ------------------------------------------------------------------------
cat("--- SECTION 3: Generalized Likelihood Ratio Test (GLRT) ---\n")
model_reduced <- glm(is_severe_or_fatal ~ dist_mean_center + road_type, data = df_model_clean, family = binomial)
model_full <- glm(is_severe_or_fatal ~ dist_mean_center * road_type, data = df_model_clean, family = binomial)

glrt_res <- anova(model_reduced, model_full, test = "LRT")
print(glrt_res)
cat("\n")

# Generate predicted values grid for plotting interaction
grid_data <- expand.grid(
  dist_mean_center = seq(0, 150, length.out = 200),
  road_type = levels(df_model_clean$road_type)
)
preds <- predict(model_full, newdata = grid_data, type = "link", se.fit = TRUE)
grid_data$pred_prob <- model_full$family$linkinv(preds$fit)
grid_data$se_upper <- model_full$family$linkinv(preds$fit + 1.96 * preds$se.fit)
grid_data$se_lower <- model_full$family$linkinv(preds$fit - 1.96 * preds$se.fit)

# ------------------------------------------------------------------------
# 5. Wald's Sequential Probability Ratio Test (SPRT) & Stopping Times
# ------------------------------------------------------------------------
cat("--- SECTION 4: Wald's Sequential Probability Ratio Test (SPRT) ---\n")
p0 <- 0.32   # Baseline rate
p1 <- 0.36   # Elevated/Critical rate
alpha <- 0.05
beta <- 0.10

A <- log(beta / (1 - alpha))
B <- log((1 - beta) / alpha)

y_seq <- as.numeric(df_model_clean$is_severe_or_fatal)
log_lr <- y_seq * log(p1 / p0) + (1 - y_seq) * log((1 - p1) / (1 - p0))
cum_log_lr <- cumsum(log_lr)

stopping_idx <- which(cum_log_lr <= A | cum_log_lr >= B)[1]
stopping_time <- ifelse(is.na(stopping_idx), length(cum_log_lr), stopping_idx)
decision <- if (is.na(stopping_idx)) {
  "No decision reached before sequence ended"
} else if (cum_log_lr[stopping_idx] >= B) {
  "Reject H0 (Accept H1: Severity proportion is significantly elevated)"
} else {
  "Accept H0 (Severity proportion remains at baseline)"
}

cat(sprintf("Baseline rate (p0): %.2f, Elevated rate (p1): %.2f\n", p0, p1))
cat(sprintf("Lower Boundary A:   %.4f, Upper Boundary B:  %.4f\n", A, B))
cat(sprintf("Stopping Time (N):  %d accidents\n", stopping_time))
cat(sprintf("Decision Reached:   %s\n\n", decision))

# ------------------------------------------------------------------------
# 6. Statistical Power Simulation
# ------------------------------------------------------------------------
cat("--- SECTION 5: Simulation-Based Power Analysis ---\n")
sim_power <- function(n_sample, effect_size_km, alpha = 0.05, n_sims = 150) {
  sigma <- sd(df_tested$dist_mean_center, na.rm = TRUE)
  replicate(n_sims, {
    minor_sim <- rnorm(n_sample, mean = 46.5, sd = sigma)
    severe_sim <- rnorm(n_sample, mean = 46.5 + effect_size_km, sd = sigma)
    res <- wilcox.test(severe_sim, minor_sim, alternative = "greater")
    res$p.value < alpha
  }) |> mean()
}

sample_sizes <- c(100, 250, 500, 1000, 2000, 3000)
powers_2km <- sapply(sample_sizes, sim_power, effect_size_km = 2)
powers_4km <- sapply(sample_sizes, sim_power, effect_size_km = 4)

cat("Estimated Power for 2 km difference in medians:\n")
print(data.frame(N = sample_sizes, Power = powers_2km))
cat("\n")

# ------------------------------------------------------------------------
# 7. Classification and Evaluation Metrics
# ------------------------------------------------------------------------
cat("--- SECTION 6: Classification & Model Evaluation (Train/Test Split) ---\n")
set.seed(42)
train_idx <- sample(1:nrow(df_model_clean), size = 0.7 * nrow(df_model_clean))
train_data <- df_model_clean[train_idx, ]
test_data  <- df_model_clean[-train_idx, ]

class_model <- glm(is_severe_or_fatal ~ dist_mean_center + road_type + day_night, data = train_data, family = binomial)

test_probs <- predict(class_model, newdata = test_data, type = "response")
threshold <- 0.342 
test_preds <- test_probs >= threshold

tp <- sum(test_preds & test_data$is_severe_or_fatal)
fp <- sum(test_preds & !test_data$is_severe_or_fatal)
fn <- sum(!test_preds & test_data$is_severe_or_fatal)
tn <- sum(!test_preds & !test_data$is_severe_or_fatal)

sensitivity <- tp / (tp + fn) 
specificity <- tn / (tn + fp)
precision   <- tp / (tp + fp)
f1_score    <- 2 * (precision * sensitivity) / (precision + sensitivity)
accuracy    <- (tp + tn) / (tp + tn + fp + fn)

cat("Confusion Matrix:\n")
cat(sprintf("                Actual Severe/Fatal  Actual Minor\n"))
cat(sprintf("Pred Severe/Fatal     %-19d %-10d\n", tp, fp))
cat(sprintf("Pred Minor            %-19d %-10d\n\n", fn, tn))

cat(sprintf("Accuracy:    %.4f\n", accuracy))
cat(sprintf("Sensitivity: %.4f (Recall)\n", sensitivity))
cat(sprintf("Specificity: %.4f\n", specificity))
cat(sprintf("Precision:   %.4f\n", precision))
cat(sprintf("F1-Score:    %.4f\n\n", f1_score))

roc_curve_data <- function(probs, actuals) {
  thresholds <- seq(0, 1, by = 0.005)
  tprs <- numeric(length(thresholds))
  fprs <- numeric(length(thresholds))
  for (i in seq_along(thresholds)) {
    preds <- probs >= thresholds[i]
    tp_s <- sum(preds & actuals)
    fp_s <- sum(preds & !actuals)
    fn_s <- sum(!preds & actuals)
    tn_s <- sum(!preds & !actuals)
    tprs[i] <- ifelse((tp_s + fn_s) > 0, tp_s / (tp_s + fn_s), 0)
    fprs[i] <- ifelse((fp_s + tn_s) > 0, fp_s / (fp_s + tn_s), 0)
  }
  return(data.frame(Threshold = thresholds, TPR = tprs, FPR = fprs))
}

roc_df <- roc_curve_data(test_probs, test_data$is_severe_or_fatal)
roc_df <- roc_df[order(roc_df$FPR), ]
auc_val <- sum(diff(roc_df$FPR) * (roc_df$TPR[-1] + roc_df$TPR[-length(roc_df$TPR)]) / 2)
cat(sprintf("Calculated Area Under Curve (AUC): %.4f\n\n", auc_val))

# ------------------------------------------------------------------------
# 8. Save Data for Plotting
# ------------------------------------------------------------------------
cat("--- SECTION 7: Saving Results for Plotting ---\n")
results_for_plotting <- list(
  df_tested = df_tested,
  df_model_clean = df_model_clean,
  weib_fit = list(shape = alpha_hat, scale = beta_hat),
  interaction_grid = grid_data,
  sprt = list(cum_log_lr = cum_log_lr, stopping_time = stopping_time, A = A, B = B),
  power = list(sample_sizes = sample_sizes, powers_2km = powers_2km, powers_4km = powers_4km),
  roc = list(roc_df = roc_df, auc = auc_val)
)

out_file <- here("data", "test_results.rds")
dir.create(dirname(out_file), showWarnings = FALSE)
write_rds(results_for_plotting, out_file)
cat("Test results successfully written to: data/test_results.rds\n")
cat("========================================================================\n")
