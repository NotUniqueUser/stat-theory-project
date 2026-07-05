library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(scales)
library(ggspatial)
library(here)

# 1. Load the results from the hypothesis test script
results_file <- here("data", "clean.rds")
if (!file.exists(results_file)) {
  stop("Test results RDS file not found! Please run 'scripts/003_hypothesis_tests.R' first.")
}

data_plot <- read_rds(results_file)
#data_plot <- res_data$df_tested

# 2. Load geographic map data
israel_map <- ne_countries(scale = "medium", country = c("Israel", "Palestine"), returnclass = "sf")
full_map_itm <- st_transform(israel_map, crs = 2039)

# Filter for plotting point layers (Severe and Fatal)
severe_points <- data_plot |> filter(severity == "Severe")
fatal_points  <- data_plot |> filter(severity == "Fatal")

plot_file <- here("plots", "accident_heatmap.png")

cat("========================================================================\n")
cat("          GENERATING SPATIAL SEVERITY MAP (002_plot.R)\n")
cat("========================================================================\n\n")
cat("Generating plot: plots/accident_heatmap.png...\n")

plot <- ggplot() +
  # Map boundaries
  geom_sf(data = full_map_itm, fill = "#fcfcfc", color = "#d5dbdb", lwd = 0.4) +
  
  # 2D density heatmap for general concentration (all accidents)
  stat_density_2d(
    data = data_plot,
    aes(x = X, y = Y, fill = after_stat(nlevel), alpha = after_stat(nlevel)),
    geom = "polygon",
    bins = 25
  ) +
  scale_fill_gradient(
    low = "#e8f8f5", 
    high = "#1abc9c", 
    name = "Accident Density\n(Overall Concentration)"
  ) +
  scale_alpha_continuous(range = c(0.15, 0.65), guide = "none") +
  
  # Layer for Severe accidents (Orange dots)
  geom_point(
    data = severe_points,
    aes(x = X, y = Y, color = "Severe Accident", shape = road_type),
    size = 0.8,
    alpha = 0.35
  ) +
  
  # Layer for Fatal accidents (Red dots)
  geom_point(
    data = fatal_points,
    aes(x = X, y = Y, color = "Fatal Accident"),
    size = 1.3,
    alpha = 0.7
  ) +
  
  # Custom scale for the point colors
  scale_color_manual(
    name = "Accident Severity",
    values = c("Severe Accident" = "#e67e22", "Fatal Accident" = "#e74c3c")
  ) +
  
  # Guides formatting
  guides(
    fill = guide_colorbar(order = 1),
    color = guide_legend(order = 2, override.aes = list(size = c(2.0, 1.5), alpha = 1.0)),
    shape = guide_legend(order = 3)
  ) +
  
  # Scale bar layer
  annotation_scale(
    location = "br",
    width_hint = 0.2,
    unit_category = "metric",
    style = "ticks",
    text_cex = 0.6,
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm")
  ) +
  
  # North arrow layer
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm"),
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm"),
    style = north_arrow_orienteering(
      fill = c("black", "white"),
      text_size = 7
    )
  ) +
  
  # Coordinate limits tight around the data points
  coord_sf(
    xlim = c(min(data_plot$X, na.rm = TRUE) - 5000, max(data_plot$X, na.rm = TRUE) + 5000),
    ylim = c(min(data_plot$Y, na.rm = TRUE) - 5000, max(data_plot$Y, na.rm = TRUE) + 5000),
    expand = FALSE
  ) +
  
  labs(
    #title = "2024 Spatial Car Accident Intensity & Severity Map",
    #subtitle = "Overlay of Fatal/Severe events on overall accident density",
    x = "Easting (ITM Meters)",
    y = "Northing (ITM Meters)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#f2f4f4", color = NA),
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.box = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

print(plot)

dir.create(here("plots"), showWarnings = FALSE)
ggsave(
  filename = plot_file,
  plot = plot,
  width = 8,
  height = 12,
  dpi = 300
)
cat("Spatial severity map saved to: plots/accident_heatmap.png\n")
cat("========================================================================\n")