
box::use(
  caret,
  dplyr[filter, mutate, rename, select, across, everything],
  lubridate[hour, wday, month],
  randomForest,
  stats[sd, predict],
  zoo[rollapply]
)

box::use(
  app/logic/constants
)

load_models <- function() {
  # Load the saved models and preprocessed data
  load("./app/static/stress_algorithm/models/temperature_model.RData")  # Temperature model
  brf_model_temp <- brf_model
  scaler_temp <- scaler

  load("./app/static/stress_algorithm/models/motion_model.RData")  # Motion model
  brf_model_motion <- brf_model
  scaler_motion <- scaler

  load("./app/static/stress_algorithm/models/eda_model.RData")  # EDA model
  brf_model_eda <- brf_model
  scaler_eda <- scaler

  load("./app/static/stress_algorithm/models/pulserate_model.RData")  # Pulse rate model
  brf_model_pulse <- brf_model
  scaler_pulse <- scaler

  return(list(
    temperature = list(model = brf_model_temp, scaler = scaler_temp),
    motion = list(model = brf_model_motion, scaler = scaler_motion),
    eda = list(model = brf_model_eda, scaler = scaler_eda),
    pulse = list(model = brf_model_pulse, scaler = scaler_pulse)
  ))
}

# General function to preprocess new data
preprocess_data <- function(new_data, value_col, scaler, range_min = NULL, range_max = NULL) {
  if (!is.null(range_min) && !is.null(range_max)) {
    new_data <- new_data |> filter(.data[[value_col]] >= range_min & .data[[value_col]] <= range_max)
  }

  new_data <- new_data |>
    mutate(mean_val = rollapply(.data[[value_col]], width = 3, FUN = mean, fill = NA, align = 'right', partial = TRUE),
           std_val = rollapply(.data[[value_col]], width = 3, FUN = sd, fill = NA, align = 'right', partial = TRUE),
           min_val = rollapply(.data[[value_col]], width = 3, FUN = min, fill = NA, align = 'right', partial = TRUE),
           max_val = rollapply(.data[[value_col]], width = 3, FUN = max, fill = NA, align = 'right', partial = TRUE),
           log_val = log1p(.data[[value_col]]),
           hour = hour(DateTime),
           day_of_week = wday(DateTime),
           month = month(DateTime),
           # wday() counts from Sunday (1) to Saturday (7)
           is_weekend = ifelse(day_of_week %in% c(1, 7), 1, 0))

  # The models were trained with features named after the parameter (TEMP_value,
  # TEMP_mean, scl_mean, ...). Without this rename those columns are missing and
  # get filled with 0 below, so the model would only see the time features.
  if (value_col == "TEMP") {
    new_data <- new_data |>
      rename(TEMP_value = TEMP,
             TEMP_mean = mean_val,
             TEMP_std = std_val,
             TEMP_min = min_val,
             TEMP_max = max_val,
             TEMP_log = log_val)
  } else if (value_col == "accelerometers_std_g") {
    new_data <- new_data |>
      rename(accelerometers_std_g_mean = mean_val,
             accelerometers_std_g_std = std_val,
             accelerometers_std_g_min = min_val,
             accelerometers_std_g_max = max_val,
             accelerometers_std_g_log = log_val)
  } else if (value_col == "EDA") {
    # The EDA model calls the skin conductance "scl"
    new_data <- new_data |>
      rename(scl = EDA,
             scl_mean = mean_val,
             scl_std = std_val,
             scl_min = min_val,
             scl_max = max_val,
             scl_log = log_val)
  }

  new_data <- select(new_data, -DateTime)

  # Fill NA values
  new_data <- new_data |> mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

  # Ensure all columns are numeric
  new_data[] <- lapply(new_data, as.numeric)

  # Ensure new_data has the same columns as the training data
  missing_cols <- setdiff(names(scaler$mean), names(new_data))
  for (col in missing_cols) {
    new_data[[col]] <- 0  # Add missing columns with default value of 0
  }
  new_data <- new_data[, names(scaler$mean)]

  # Scale the new data using the scaler from the training phase
  new_data_scaled <- predict(scaler, new_data)

  return(new_data_scaled)
}

# Function to make predictions on new data using a specified model
predict_new_data <- function(new_data, value_col, range_min = NULL, range_max = NULL) {

  models <- constants$models

  # Determine the appropriate model and scaler
  if (value_col == "TEMP") {
    model <- models$temperature$model
    scaler <- models$temperature$scaler
  } else if (value_col == "accelerometers_std_g") {
    model <- models$motion$model
    scaler <- models$motion$scaler
  } else if (value_col == "EDA") {
    model <- models$eda$model
    scaler <- models$eda$scaler
  } else if (value_col == "HR") {
    model <- models$pulse$model
    scaler <- models$pulse$scaler
  } else {
    stop("Unknown value column")
  }

  new_data_scaled <- preprocess_data(new_data, value_col, scaler, range_min, range_max)
  predictions <- predict(model, new_data_scaled)

  return(predictions)
}
