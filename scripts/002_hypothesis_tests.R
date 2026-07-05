data_clean_path <- here("data", "clean.rds")
if (!file.exists(data_clean_path)) {
  stop("Clean data file not found! Please run 'scripts/main.R' or 'scripts/001_import.R' first.")
}
df <- read_rds(data_clean_path)

set.seed(42)

train_index <- createDataPartition(df$is_severe_or_fatal, p = 0.8, list = FALSE)
train_data  <- df[train_index, ]
test_data   <- df[-train_index, ]

header <- strrep("=", 15)
cat(header, "Statistical Tests", header, "\n")

# Individual chisq tests
cat(header, "Individual chisq tests", header)


chisq_road_state_weather <- chisq.test(table(data_clean$road_state, data_clean$weather))
chisq_road <- chisq.test(table(data_clean$is_severe_or_fatal, data_clean$road_type))
chisq_day_night <- chisq.test(table(data_clean$is_severe_or_fatal, data_clean$day_night))
chisq_weather <- chisq.test(table(data_clean$is_severe_or_fatal, data_clean$weather))
chisq_accident_type <- chisq.test(table(data_clean$is_severe_or_fatal, data_clean$accident_type))

print(chisq_road_state_weather)
print(chisq_road)
print(chisq_day_night)
print(chisq_weather)
print(chisq_accident_type)

raw_p_values <- c(
  road_state_weather = chisq_road_state_weather$p.value,
  road               = chisq_road$p.value,
  day_night          = chisq_day_night$p.value,
  weather            = chisq_weather$p.value,
  accident_type      = chisq_accident_type$p.value
)

adjusted_p_values <- p.adjust(raw_p_values, method = "holm")

p_comparison <- data.frame(Raw_P = raw_p_values, Adjusted_P = adjusted_p_values)
print(p_comparison)

# Logistic Regression
cat(header, "Logistic Regression", header, "\n")

model <- glm(is_severe_or_fatal ~ day_night + weather + road_type + accident_type + 
               (road_type:accident_type) + (road_type:weather),
             data = train_data, family = binomial)
print(summary(model))

# Odd ratios
cat(header, "Odd Ratios", header, "\n")
odds_ratios <- exp(cbind(OR = coef(model), confint(model)))
print(odds_ratios)

# Compare to null model
cat(header, "Null comparison", header, "\n")
null_model <- glm(is_severe_or_fatal ~ 1,
                  data = train_data, family = binomial)
res_anova <- anova(null_model, model, test = "Chisq")
print(res_anova)

# Pseudo-R2
logLik_model <- logLik(model)
logLik_null <- logLik(null_model)
mcfadden_r2 <- as.numeric(1- (logLik_model / logLik_null))
cat("McFadden Pseudo-R2 (model vs null model): ", mcfadden_r2, "\n")

# ROC-AUC
cat(header, "ROC-AUC", header, "\n")
test_data$predicted_prob <- predict(model, newdata = test_data, type = "response")
roc_obj <- roc(test_data$is_severe_or_fatal, test_data$predicted_prob)
print(auc(roc_obj))
roc_df <- data.frame(
  Sensitivity = roc_obj$sensitivities,
  Specificity = roc_obj$specificities,
  FPR = 1 - roc_obj$specificities  # False Positive Rate for the X-axis
)



# Write results
results <- list(
  model = model,
  odds_ratios = odds_ratios,
  roc = roc_df,
  auc = auc(roc_obj),
  test_data = test_data
)

write_rds(results, here("data", "test_results.rds"))