csv_file <- here("data-raw", "h20241331data.csv")
out_file <- here("data", "clean.rds")

data_clean <- read_csv(csv_file) |>
  filter(!is.na(X), !is.na(Y)) |>
  mutate(
    X_c = median(X),
    Y_c = median(Y),
    dist = sqrt((X - X_c)^2 + (Y - Y_c)^2) / 1000 # in kilometers
  )

dir.create("data", showWarnings = FALSE)
write_rds(data_clean, out_file)