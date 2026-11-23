box::use(
  testthat[...],
)

# app/logic/constants reads config/*.yml relative to the working directory, so
# load the module from the project root.
withr::with_dir(test_path("..", ".."), box::use(
  app/logic/functions[filter_dates],
))

# Noon local time, so the conversion to a date never crosses a day boundary.
local_noon <- function(dates) {
  as.POSIXct(paste(dates, "12:00:00"), tz = Sys.timezone())
}

sample_data <- data.frame(
  DateTime = local_noon(c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04")),
  value = 1:4
)

test_that("filter_dates returns NULL data unchanged", {
  expect_message(result <- filter_dates(NULL, as.Date(c("2026-01-01", "2026-01-02"))),
                 "there is no data")
  expect_null(result)
})

test_that("filter_dates returns the data unfiltered without a range", {
  expect_message(result <- filter_dates(sample_data, NULL), "there is no date range")
  expect_identical(result, sample_data)
})

test_that("filter_dates returns the data unfiltered when the column is missing", {
  range <- as.Date(c("2026-01-02", "2026-01-03"))
  expect_message(result <- filter_dates(sample_data, range, column = "Timestamp"),
                 "column 'Timestamp' is missing")
  expect_identical(result, sample_data)
})

test_that("filter_dates keeps the rows inside the range, including both ends", {
  range <- as.Date(c("2026-01-02", "2026-01-03"))
  result <- filter_dates(sample_data, range)
  expect_equal(result$value, c(2, 3))
})

test_that("filter_dates drops rows without a date", {
  data <- sample_data
  data$DateTime[2] <- NA
  range <- as.Date(c("2026-01-01", "2026-01-04"))
  result <- filter_dates(data, range)
  expect_equal(result$value, c(1, 3, 4))
})

test_that("filter_dates uses the given column", {
  data <- data.frame(Start = sample_data$DateTime, value = 1:4)
  range <- as.Date(c("2026-01-03", "2026-01-04"))
  result <- filter_dates(data, range, column = "Start")
  expect_equal(result$value, c(3, 4))
})

test_that("filter_dates returns an empty data frame when no rows match", {
  range <- as.Date(c("2027-01-01", "2027-01-02"))
  result <- filter_dates(sample_data, range)
  expect_equal(nrow(result), 0)
  expect_named(result, names(sample_data))
})
