csv_file <- here("data-raw", "h20241331data.csv")
out_file <- here("data", "clean.rds")

data_clean <- read_csv(csv_file) |>
  filter(!is.na(X), !is.na(Y), !is.na(SUG_DEREH), !is.na(YOM_LAYLA),
         !is.na(HUMRAT_TEUNA), MEZEG_AVIR %in% 1:4, PNE_KVISH %in% 1:6) |>
  mutate(
    severity = factor(
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
      ifelse(YOM_LAYLA == 1, "Day", "Night"),
      levels = c("Day", "Night")
    ) |> droplevels(),
    
    weather = factor(
      ifelse(MEZEG_AVIR %in% c(1,3), "Clear", "RainFog"),
      levels = c("Clear", "RainFog")
    ) |> droplevels(),
    
    road_state = factor(
      ifelse(PNE_KVISH == 1, "Dry", "WetObstructed"),
      levels = c("Dry", "WetObstructed")
    ) |> droplevels(),
    
    accident_type = factor(
      case_when(
        SUG_TEUNA == 1 ~ "Pedestrian",
        SUG_TEUNA %in% c(2:7, 17:18) ~ "CarCrash",
        SUG_TEUNA %in% c(10:14, 19:20) ~ "SelfCrash",
        TRUE ~ NA_character_
      )
    )
  ) |> filter(!is.na(accident_type))

dir.create("data", showWarnings = FALSE)
write_rds(data_clean, out_file)