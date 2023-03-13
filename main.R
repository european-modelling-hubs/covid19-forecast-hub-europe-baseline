library(dplyr)
library(purrr)
library(lubridate)
library(covidModels)
library(here)
library(readr)
library(EuroForecastHub)
source(here("R", "build_baseline.R"))

config_file <- paste0(
  "https://github.com/covid19-forecast-hub-europe/",
  "covid19-forecast-hub-europe/raw/main/project-config.json"
)

model_name <- get_hub_config("baseline", config_file)[["name"]]

model_folder <- here("data-processed", model_name)
if (!dir.exists(model_folder)) {
  dir.create(model_folder, recursive = TRUE)
}

hub_quantiles <- get_hub_config("forecast_type", config_file)[["quantiles"]]
hub_horizon <- max(get_hub_config("horizon", config_file)[["values"]])
hub_targets <- get_hub_config("target_variables", config_file)

forecast_date <- today()
wday(forecast_date) <- get_hub_config("forecast_week_day", config_file)

raw_truth <- covidHubUtils::load_truth(
  truth_source = "JHU",
  temporal_resolution = "weekly",
  truth_end_date = as.character(forecast_date - 1),
  hub = "ECDC"
) |>
  EuroForecastHub::add_status()

max_horizon <- raw_truth |>
  dplyr::group_by(location, target_variable) |>
  dplyr::summarise(
    weeks_cutoff = sum(!grepl("final", status)), .groups = "drop"
  ) |>
  dplyr::mutate(max_horizon = hub_horizon + weeks_cutoff) |>
  dplyr::select(-weeks_cutoff)

baseline_forecast <- raw_truth  |>
  dplyr::filter(grepl("final", status)) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::inner_join(max_horizon, by = c("location", "target_variable")) |>
  dplyr::group_by(location, target_variable) |>
  dplyr::group_map(
    ~ dplyr::full_join(
      .y,
      build_baseline(
        .x$value, quantiles = hub_quantiles, horizon = unique(.x$max_horizon)
      ),
      by = character()
    ) |>
      dplyr::mutate(horizon = horizon - max(horizon) + hub_horizon)
  ) |>
  dplyr::bind_rows() |>
  dplyr::filter(type %in% substr(hub_targets, 1, 3)) |>
  dplyr::mutate(type = "quantile")

format_ensemble(baseline_forecast, forecast_date) |>
  write_csv(paste0(model_folder, "/", forecast_date, "-", model_name, ".csv"))
