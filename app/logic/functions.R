
box::use(
  dplyr[mutate],
  glue[glue],
  lubridate[year, month, day, hour, minute, second],
  readxl[read_excel],
  shiny[div, tags],
  shinyjs[addCssClass, hide, show, removeCssClass],
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

rectify_datetime <- function(date, time){
  ISOdatetime(year(date), month(date), day(date),
              hour(time), minute(time), second(time))
}

read_calendar <- function(fn){

  ext <- tolower(file_ext(fn))

  switch(ext,
         xls = read_excel(fn),
         xlsx = read_excel(fn),
         txt = read.csv2(fn)
  ) |>
    as_tibble() |>
    mutate(Date = as.Date(Date),  ## ????
           Start = rectify_datetime(Date, Start),
           End = rectify_datetime(Date, End))

}

validate_calendar <- function(data){

  nms <- c("Date" ,"Start", "End" , "Text")

  all(nms %in% names(data))

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
    data$Color <- constants$app_config$visualisation$default_color
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
