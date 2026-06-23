data_clean_path <- here("data", "clean.rds")
if (!file.exists(data_clean_path)) {
  stop("Clean data file not found! Please run 'scripts/main.R' or 'scripts/001_import.R' first.")
}
df <- read_rds(data_clean_path)

header <- strrep("=", 15)
cat(header, "Statistical Tests", header, "\n")

# Individual chisq tests
cat(header, "Individual chisq tests", header)

chisq_road_state_weather <- chisq.test(table(data_clean$road_state, data_clean$weather))
chisq_road <- chisq.test(table(data_clean$severity, data_clean$road_type))
chisq_day_night <- chisq.test(table(data_clean$severity, data_clean$day_night))
chisq_weather <- chisq.test(table(data_clean$severity, data_clean$weather))
chisq_accident_type <- chisq.test(table(data_clean$severity, data_clean$accident_type))

print(chisq_road_state_weather)
print(chisq_road)
print(chisq_day_night)
print(chisq_weather)
print(chisq_accident_type)

# Logistic Regression
cat(header, "Logistic Regression", header, "\n")

model <- glm(is_severe_or_fatal ~ day_night + weather + road_type + accident_type + (road_type:accident_type) + (road_type:weather),
             data = df, family = binomial)
print(summary(model))

# Odd ratios
cat(header, "Odd Ratios", header, "\n")
odds_ratios <- exp(cbind(OR = coef(model), confint(model)))
print(odds_ratios)

# Compare to null model
cat(header, "Null comparison", header, "\n")
null_model <- glm(is_severe_or_fatal ~ 1,
                  data = df, family = binomial)
res_anova <- anova(null_model, model, test = "Chisq")
print(res_anova)

# Pseudo-R2
logLik_model <- logLik(model)
logLik_null <- logLik(null_model)
mcfadden_r2 <- as.numeric(1- (logLik_model / logLik_null))
cat("McFadden Pseudo-R2 (model vs null model): ", mcfadden_r2, "\n")

# ROC-AUC
cat(header, "ROC-AUC", header, "\n")
df$predicted_prob <- predict(model, type = "response")
roc_obj <- roc(df$is_severe_or_fatal, df$predicted_prob)
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
  predicted_prob = df$predicted_prob
)

write_rds(results, here("data", "test_results.rds"))