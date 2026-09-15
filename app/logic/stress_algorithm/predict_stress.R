
box::use(
  dplyr[arrange, full_join]
)

box::use(
  app/logic/stress_algorithm/helpers
)

#' stress_weights
#'
#' @description
#' The weights that weighted_stress_score() uses to combine the per parameter
#' stress predictions into a single score. EDA and HR get more say in the score
#' than temperature and movement.
#'
#' @noRd
stress_weights <- c(TEMP = 1, MOVE = 1, HR = 2, EDA = 2.5)

# nowatch_folder <- "../wearalyze/app/static/example_data/nowatch_large"
# nowatch_data <- wearables::read_nowatch(folder = nowatch_folder)
# aggregate_nowatch_data <- wearables::aggregate_nowatch_data(nowatch_data)
#
# embrace_plus_folder <- "../wearalyze/app/static/example_data/embrace-plus_large"
# embrace_plus_data <- wearables::read_embrace_plus(folder = embrace_plus_folder, type = "aggregated")
# aggregate_embrace_plus_data <- wearables::aggregate_embrace_plus_data(embrace_plus_data, "5 min")

return_predictions <- function(data, types) {

  predictions <- lapply(types, function(type) {

    if (type == "MOVE") {
      df_name <- "ACCELEROMETERS-STD"
      type <- "accelerometers_std_g"
    } else {
      df_name <- type
    }

    if (df_name %in% names(data)) {
      predictions <- helpers$predict_new_data(data[[df_name]], type)
      predictions_df <- data.frame(predictions)

      # add DateTime
      predictions_df$DateTime <- data[[df_name]]$DateTime

      # rename "predictions" into type
      if (type == "accelerometers_std_g") {
        type <- "MOVE"
      }
      names(predictions_df)[1] <- type

      return(predictions_df)
    } else {
      return(NULL)
    }
  })

  names(predictions) <- types

  return(predictions)

}

#' combine_predictions
#'
#' @description
#' Combines the per parameter predictions into a single wide data frame with one
#' row per DateTime and one numeric column per parameter.
#'
#' @details
#' The predictions are factors with levels "1" to "7", hence the conversion via
#' as.character() instead of as.numeric() alone. Types that the device does not
#' provide get an NA column, so callers can always address all four parameters.
#'
#' @param predicted_data A named list of prediction data frames, as returned by
#'   return_predictions(). Elements are NULL or a data frame.
#' @param types A character vector with the parameters to combine.
#'
#' @return A data frame sorted by DateTime, with one numeric column per entry in
#'   `types`. NULL when there are no predictions for any of them.
#'
#' @noRd
combine_predictions <- function(predicted_data, types = c("TEMP", "MOVE", "EDA", "HR")) {

  available <- types[!vapply(predicted_data[types], is.null, logical(1))]

  if (length(available) == 0) {
    return(NULL)
  }

  frames <- lapply(available, function(type) {
    df <- predicted_data[[type]]
    df[[type]] <- as.numeric(as.character(df[[type]]))
    df[, c("DateTime", type)]
  })

  combined <- Reduce(function(x, y) full_join(x, y, by = "DateTime"), frames)

  for (type in setdiff(types, available)) {
    combined[[type]] <- NA_real_
  }

  arrange(combined, DateTime)

}

#' weighted_stress_score
#'
#' @description
#' Combines the per parameter stress predictions into a single stress score per
#' measurement, as a weighted average over the parameters.
#'
#' @details
#' The weights are renormalised per row over the parameters that actually have
#' a value, so a missing parameter does not pull the score down and the result
#' stays on the same 1-7 scale as the individual predictions.
#'
#' @param df_predictions A data frame with one row per measurement and one
#'   numeric column per parameter, as returned by combine_predictions().
#'   Columns that are not named in `stress_weights` are ignored.
#'
#' @return A numeric vector with one score per row of `df_predictions`, on the
#'   same 1-7 scale as the individual predictions. NA for rows where none of
#'   the weighted parameters has a value, and an all NA vector when
#'   `df_predictions` holds none of the weighted parameters at all.
#'
#' @noRd
weighted_stress_score <- function(df_predictions) {
  weights <- stress_weights
  types <- intersect(names(weights), names(df_predictions))

  if (length(types) == 0) {
    return(rep(NA_real_, nrow(df_predictions)))
  }

  values <- as.matrix(df_predictions[, types, drop = FALSE])
  w <- weights[types]

  has_value <- !is.na(values)
  values[!has_value] <- 0

  total <- as.vector(values %*% w)
  denominator <- as.vector(has_value %*% w)

  ifelse(denominator > 0, total / denominator, NA_real_)

}

# test <- return_predictions(aggregate_embrace_plus_data, types = c("TEMP", "MOVE", "EDA", "HR"))
