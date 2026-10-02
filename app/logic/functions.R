
box::use(
  dplyr[group_by, mutate],
  glue[glue],
  lubridate[year, month, day, hour, minute, second],
  readxl[read_excel],
  shiny[div, getDefaultReactiveDomain, tags],
  shinyjs[addCssClass, hide, show, removeCssClass],
  shinytoastr[toastr_warning],
  stringr[str_to_title],
  tibble[as_tibble, tribble],
  tools[file_ext],
  utils[read.csv2]
)

box::use(
  app/logic/constants
)

hide_tab <- function(value){
  hide(selector = glue("li > a[data-value='{value}']"))
}

show_tab <- function(value){
  show(selector = glue("li > a[data-value='{value}']"))
}

disable_link <- function(menu, name){
  addCssClass(selector = glue("a[data-value='{menu}'] + ul > li > a[data-value='{name}']",
                              menu = paste0(menu, "-menu")),
              class = "inactivelink")
}

enable_link <- function(menu, name){
  removeCssClass(selector = glue("a[data-value='{menu}'] + ul > li > a[data-value='{name}']",
                                 menu = paste0(menu, "-menu")),
                 class = "inactivelink")
}

side_by_side <- function(...){

  mc <- list(...)
  lapply(mc, function(x){

    div(style = paste("display: inline-block;",
                    "vertical-align: top;"),
        x)

  })

}

logo_image_with_link <- function(img_path, url, width = 130){
  tags$a(tags$img(src = img_path, class = "grayscale", width = 130, style = "padding: 10px"),
         href = url, target = "_blank")
}

analysis_summary_table <- function(a){

  tribble(~Parameter, ~Value,
          "Mean acceleration", mean(a$data$ACC$a, na.rm=TRUE),
          "Mean temperature", mean(a$data$TEMP$TEMP, na.rm=TRUE),
          "Mean HR", mean(a$data$HR$HR, na.rm=TRUE),
          "Nr. accepted beats", ifelse(!is.null(a$ibi), a$ibi$summary$beats$beats_accepted, NA),
          "Nr. original beats", ifelse(!is.null(a$ibi), a$ibi$summary$beats$beats_original, NA),
          "rMSSD", ifelse(!is.null(a$ibi), a$ibi$time_analysis$rMSSD, NA),
          "Mean EDA (cleaned)", mean(a$data_summary$EDA$EDA_clean_mean, na.rm = TRUE),
          "Number of peaks per minute (cleaned)", a$data_summary$peaks$peaks_clean_per_min,
          "Mean area under the curve (AUC) (cleaned)", a$data_summary$peaks$peaks_clean_mean_auc,
          "% of data with EDA artefacts", 100 * mean(a$eda_bin$label == -1)
  )
}

data_datetime_range <- function(data){

  if ("EDA" %in% names(data)){
    r <- range(data$EDA[[get_datetime_column(data$EDA)]])
  } else if ("ACT" %in% names(data)){
    r <- range(data$ACT[[get_datetime_column(data$ACT)]])
  }

  as.numeric(difftime(r[2],r[1], units = "hours"))

}

#' filter_dates
#'
#' @description
#' Keeps the rows of a data frame whose date in `column` falls within the given
#' date range.
#'
#' @details
#' The data is returned unfiltered when there is nothing to filter on: no
#' (complete) range, no data, or a `column` that is missing from `data`. The
#' reason is written to the log, and the last two are also shown in the
#' dashboard, as they point at a problem, while a missing range does not.
#' Timestamps are converted to dates in the system time zone, and rows whose
#' `column` is NA are dropped whenever filtering does take place.
#'
#' @param data A data frame to filter on date, or NULL.
#' @param range A vector of two dates, the start and the end of the range, both
#'   inclusive, or NULL.
#' @param column The name of the column holding the date or timestamp to filter
#'   on, for example "DateTime" for measurements and "Start" for calendar
#'   events.
#'
#' @return `data` with only the rows inside `range`, or `data` unchanged when no
#'   filtering is possible.
#'
#' @noRd
filter_dates <- function(data, range, column = "DateTime"){
  if (is.null(data) || is.null(range) || !column %in% names(data)) {
    reason <- if (is.null(data)) {
      "there is no data"
    } else if (is.null(range)) {
      "there is no date range"
    } else {
      glue("column '{column}' is missing from the data")
    }

    message(glue("filter_dates: {reason}, returning the data unfiltered."))

    # Not for a missing range: that is a normal state - no selection yet, or a
    # date field that is being typed in. Only inside a Shiny session, as toastr
    # needs one.
    if (!is.null(range) && !is.null(getDefaultReactiveDomain())) {
      toastr_warning(glue("Could not filter on date."))
    }

    return(data)
  }

  dates <- as.Date(data[[column]], tz = Sys.timezone())

  data[!is.na(dates) & dates >= range[1] & dates <= range[2], , drop = FALSE]
}

rectify_datetime <- function(date, time){
  ISOdatetime(year(date), month(date), day(date),
              hour(time), minute(time), second(time))
}

default_colors <- function() {
  c("darkorange",
    "darkgreen",
    "darkblue",
    "darkred",
    "darkcyan",
    "darkmagenta",
    "yellow",
    "purple",
    "pink",
    "brown",
    "grey",
    "skyblue",
    "lightgreen",
    "lightblue")
}

read_calendar <- function(fn){

  ext <- tolower(file_ext(fn))

  switch(ext,
         xls = read_excel(fn),
         xlsx = read_excel(fn),
         txt = read.csv2(fn)
  ) |>
    as_tibble() |>
    mutate(`Start Date` = as.Date(`Start Date`),  ## ????
           Start = rectify_datetime(`Start Date`, Start),
           `End Date` = as.Date(`End Date`),
           End = rectify_datetime(`End Date`, End)) |>
    group_by(Text) |>
    mutate(Color = ifelse(is.na(Color), sample(default_colors(), 1), Color))


}

validate_calendar <- function(data){

  nms <- c("Start Date" ,"Start", "End Date", "End" , "Text")

  all(nms %in% names(data))

}

#' validate_calendar_rows
#'
#' @description
#' Checks whether the given data frame contains any completely empty rows.
#'
#' @param data A data frame to be checked.
#'
#' @return TRUE if there are no empty rows, FALSE otherwise.
#'
#' @noRd
validate_calendar_rows <- function(data) {
  # Ignore the color column when checking for empty rows, as it always gets a
  # value when data is read.
  data$Color <- NULL
  result <- !any(rowSums(is.na(data)) == ncol(data))

  return(result)
}

read_problemtarget<- function(fn){

  ext <- tolower(file_ext(fn))

  switch(ext,
         xls = read_excel(fn),
         xlsx = read_excel(fn),
         txt = read.csv2(fn)
  ) |>
    as_tibble() |>
    mutate(Date = as.Date(Date))

}

validate_problemtarget <- function(data){

  nms <- c("Date" ,"Problem or Target Behavior", "Score")

  all(nms %in% names(data))

}

calendar_add_color <- function(data, app_config){

  if(!"Color" %in% names(data)){
    data <- data |>
      group_by(Text) |>
      mutate(Color = sample(default_colors(), 1))
  }

  return(data)
}

get_device_name <- function(device, title = FALSE){

  device <- gsub("-", " ", device)

  if (title){
     device <- str_to_title(device)
  }

  return(device)

}

get_device_id <- function(device) {

  device <- tolower(gsub(" ", "-", device))
  return(device)

}

get_datetime_column <- function(data){

  if ("DateTime" %in% names(data)){
    return("DateTime")
  } else {
    stop("DateTime column not found in data. Please make sure it's there!")
  }

}
