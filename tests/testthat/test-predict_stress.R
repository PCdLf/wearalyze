box::use(
  testthat[...],
)
box::use(
  app/logic/stress_algorithm/predict_stress[
    combine_predictions,
    stress_weights,
    weighted_stress_score
  ],
)

times <- as.POSIXct(c("2026-01-01 10:00:00", "2026-01-01 10:05:00", "2026-01-01 10:10:00"),
                    tz = "UTC")

# Predictions come back as factors whose levels are the stress levels.
prediction <- function(type, datetimes, levels) {
  df <- data.frame(DateTime = datetimes)
  df[[type]] <- factor(levels)
  df
}

# combine_predictions ----

test_that("combine_predictions returns NULL when there are no predictions", {
  expect_null(combine_predictions(list(TEMP = NULL, MOVE = NULL, EDA = NULL, HR = NULL)))
  expect_null(combine_predictions(list()))
})

test_that("combine_predictions converts factor levels to their values, not their codes", {
  # As codes these would be 1, 2, 3; the values are 2, 5, 7.
  predicted <- list(TEMP = prediction("TEMP", times, c("2", "5", "7")))
  result <- combine_predictions(predicted, types = "TEMP")
  expect_equal(result$TEMP, c(2, 5, 7))
  expect_type(result$TEMP, "double")
})

test_that("combine_predictions fills missing parameters with NA", {
  predicted <- list(
    TEMP = prediction("TEMP", times, c("1", "2", "3")),
    MOVE = NULL,
    EDA = prediction("EDA", times, c("4", "5", "6")),
    HR = NULL
  )
  result <- combine_predictions(predicted)

  expect_setequal(names(result), c("DateTime", "TEMP", "MOVE", "EDA", "HR"))
  expect_equal(result$TEMP, c(1, 2, 3))
  expect_equal(result$EDA, c(4, 5, 6))
  expect_true(all(is.na(result$MOVE)))
  expect_true(all(is.na(result$HR)))
  expect_type(result$MOVE, "double")
})

test_that("combine_predictions joins on DateTime and sorts the result", {
  predicted <- list(
    TEMP = prediction("TEMP", times[c(3, 1)], c("3", "1")),
    HR = prediction("HR", times[c(2, 1)], c("6", "4"))
  )
  result <- combine_predictions(predicted, types = c("TEMP", "HR"))

  expect_equal(result$DateTime, times)
  expect_equal(result$TEMP, c(1, NA, 3))
  expect_equal(result$HR, c(4, 6, NA))
})

test_that("combine_predictions keeps only DateTime and the parameter columns", {
  df <- prediction("TEMP", times, c("1", "2", "3"))
  df$extra <- "x"
  result <- combine_predictions(list(TEMP = df), types = "TEMP")
  expect_named(result, c("DateTime", "TEMP"))
})

# weighted_stress_score ----

test_that("weighted_stress_score is the weighted average over all parameters", {
  df <- data.frame(TEMP = 1, MOVE = 2, HR = 6, EDA = 7)
  w <- stress_weights
  expected <- (1 * w[["TEMP"]] + 2 * w[["MOVE"]] + 6 * w[["HR"]] + 7 * w[["EDA"]]) / sum(w)
  expect_equal(weighted_stress_score(df), expected)
})

test_that("weighted_stress_score renormalises the weights over the values present", {
  df <- data.frame(TEMP = c(2, 4), MOVE = c(NA, 4), HR = c(5, NA), EDA = c(NA, NA))
  w <- stress_weights
  expected <- c(
    (2 * w[["TEMP"]] + 5 * w[["HR"]]) / (w[["TEMP"]] + w[["HR"]]),
    4
  )
  expect_equal(weighted_stress_score(df), expected)
})

test_that("weighted_stress_score stays on the scale of the predictions", {
  df <- data.frame(TEMP = c(7, NA), MOVE = c(NA, 1), HR = c(7, NA), EDA = c(NA, 1))
  expect_equal(weighted_stress_score(df), c(7, 1))
})

test_that("weighted_stress_score gives NA for rows without any value", {
  df <- data.frame(TEMP = c(3, NA), MOVE = c(3, NA), HR = c(3, NA), EDA = c(3, NA))
  result <- weighted_stress_score(df)
  expect_equal(result[1], 3)
  expect_true(is.na(result[2]))
})

test_that("weighted_stress_score ignores columns without a weight", {
  df <- data.frame(DateTime = times[1], TEMP = 2, HR = 5, other = 100)
  w <- stress_weights
  expected <- (2 * w[["TEMP"]] + 5 * w[["HR"]]) / (w[["TEMP"]] + w[["HR"]])
  expect_equal(weighted_stress_score(df), expected)
})

test_that("weighted_stress_score returns all NA without any weighted parameter", {
  df <- data.frame(DateTime = times, other = 1:3)
  result <- weighted_stress_score(df)
  expect_length(result, 3)
  expect_true(all(is.na(result)))
})
