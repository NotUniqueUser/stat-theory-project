res_data <- read_rds(here("data", "test_results.rds"))
df_tested <- res_data$df_tested

dir.create(here("plots"), showWarnings = FALSE)

# Plot A: Proportions Bar Chart
p_bar <- ggplot(df_tested |> filter(!is.na(road_category)), aes(x = road_category, fill = severity)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = c("Minor" = "#3498db", "Severe" = "#e67e22", "Fatal" = "#e74c3c")) +
  theme_minimal() +
  labs(title = "Severity Proportions: Urban vs Non-Urban", y = "Proportion", x = "Road Category")
ggsave(here("plots", "severity_bars.png"), p_bar, width = 6, height = 5)

# Plot B: Bootstrap CI Density
p_boot <- ggplot(data.frame(Diff = res_data$boot_diffs), aes(x = Diff)) +
  geom_density(fill = "#9b59b6", alpha = 0.5) +
  geom_vline(xintercept = quantile(res_data$boot_diffs, c(0.025, 0.975)), linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Bootstrap Distribution: Diff in Severe Rates (Non-Urban - Urban)")
ggsave(here("plots", "boot_density.png"), p_boot, width = 6, height = 4)

# Plot C: Interaction Effect
p_int <- ggplot(res_data$grid_data,
                aes(x = road_category, y = pred_prob,
                    color = day_night, group = day_night)) +
  geom_point(size = 4) +
  geom_line(size = 1.2) +
  geom_errorbar(aes(ymin = se_lower, ymax = se_upper), width = 0.1) +
  theme_minimal() +
  labs(title = "Interaction: Road Category & Time of Day on Severity")
ggsave(here("plots", "interaction.png"), p_int, width = 7, height = 5)

# Plot D: SPRT
png(here("plots", "wald_sprt.png"), width = 800, height = 500)
plot(1:res_data$sprt$stop, res_data$sprt$cum[1:res_data$sprt$stop],
  type = "l", col = "blue", lwd = 2,
  main = "SPRT: Non-Urban Severity Rate",
  ylab = "Log-LR", xlab = "Accident Sequence"
)
abline(h = res_data$sprt$A, col = "red", lwd = 2)
abline(h = res_data$sprt$B, col = "green", lwd = 2)
dev.off()

# Plot E: ROC Curve
png(here("plots", "roc_curve.png"), width = 600, height = 600)
plot(res_data$roc$df$FPR, res_data$roc$df$TPR,
  type = "l", col = "purple", lwd = 2,
  main = sprintf("ROC Curve (AUC = %.3f)", res_data$roc$auc),
  xlab = "FPR", ylab = "TPR", xlim = c(0, 1), ylim = c(0, 1)
)
abline(a = 0, b = 1, lty = 2)
dev.off()
