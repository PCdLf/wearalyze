
box::use(
  app/logic/stress_algorithm/helpers
)

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

# test <- return_predictions(aggregate_embrace_plus_data, types = c("TEMP", "MOVE", "EDA", "HR"))
