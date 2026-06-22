res_data <- read_rds(here("data", "test_results.rds"))
plots_dir <- here("plots")

data_clean <- read_rds(here("data", "clean.rds"))
dir.create(plots_dir, showWarnings = FALSE)

# Plot bars (road types with severity)
plot <- ggplot(data_clean, aes(x = road_type, fill = severity)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(
    palette = "Set2",
  ) +
  labs(
    title = "Number of accidents by severity and road types",
    x = "Road Type",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

print(plot)

ggsave(
  filename = paste0(plots_dir, "/stacks.png"),
  plot = plot,
  width = 8,
  height = 6,
  dpi = 300
)


# Plot ROC-AUC
roc_df <- res_data$roc
auc_val <- res_data$auc

plot <- ggplot(roc_df, aes(x = FPR, y = Sensitivity)) +
  # Draw the proper diagonal baseline (0,0 to 1,1)
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  geom_line(color = "#1f77b4", size = 1.3) +
  scale_x_continuous(limits = c(0, 1), expand = 0) +
  scale_y_continuous(limits = c(0, 1), expand = 0) +
  labs(
    title = "ROC Curve for Accident Severity Predictor Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  annotate("text", x = 0.65, y = 0.25, 
           label = paste("AUC =", round(auc_val, 4)), 
           fontface = "bold", size = 5, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

print(plot)

ggsave(
  filename = paste0(plots_dir, "/auc.png"),
  plot = plot,
  width = 8,
  height = 8,
  dpi = 300
)

# Plot odds-ratio
model <- res_data$model
model_tidy <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
model_tidy <- model_tidy[model_tidy$term != "(Intercept)", ]

plot <- ggplot(model_tidy, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = '#1f77b4', size = 1, orientation = "y") +
  geom_point(color = '#1f77b4', size = 3) +
  scale_x_log10() +
  labs(
    title = "Model Odds Ratios & 95% Confidence Intervals",
    x = "Odds ratios (Log Scale)",
    y = "Predictor Variable"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(plot)

ggsave(
  filename = paste0(plots_dir, "/odds-ratios.png"),
  plot = plot,
  width = 10,
  height = 8,
  dpi = 300
)

# Calculate predicted probabilities for combinations
predicted_prob <- res_data$predicted_prob

plot_data <- aggregate(predicted_prob ~ road_type + weather, data = data_clean, FUN = mean)

plot <- ggplot(plot_data, aes(x = weather, y = predicted_prob, group = road_type, color = road_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_discrete(expand = 0) +
  labs(
    title = "Interaction Effect: How Weather Impacts Severity across Road Types",
    x = "Weather Condition",
    y = "Mean Predicted Probability of Severe/Fatal Crash",
    color = "Road Type"
  ) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom",
        aspect.ratio = 2)

print(plot)

ggsave(
  filename = paste0(plots_dir, "/interaction.png"),
  plot = plot,
  width = 8,
  height = 6,
  dpi = 300
)