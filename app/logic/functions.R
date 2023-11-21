
box::use(
  shiny[tags],
  shinyjs[addCssClass, hide, show, removeCssClass],
  tibble[tribble]
)

#' @export
hide_tab <- function(value){
  hide(selector = glue("li > a[data-value='{value}']")) 
}

#' @export
show_tab <- function(value){
  show(selector = glue("li > a[data-value='{value}']")) 
}

#' @export
disable_link <- function(name){
  addCssClass(selector = glue("a[data-value='{name}']"), 
              class = "inactivelink")
}

#' @export
enable_link <- function(name){
  removeCssClass(selector = glue("a[data-value='{name}']"), 
                 class = "inactivelink")
}

#' @export
side_by_side <- function(...){
  
  mc <- list(...)
  lapply(mc, function(x){
    
    tags$div(style=paste("display: inline-block;",
                         "vertical-align: top;"), 
             x)  
    
  })
  
}

#' @export
logo_image_with_link <- function(img_path, url, width = 130){
  tags$a(tags$img(src = img_path, class = "grayscale", width = 130, style = "padding: 10px"), 
         href = url, target = "_blank")
}

#' @export
analysis_summary_table <- function(a){
  
  tribble(~Parameter, ~Value,
          "Mean acceleration", mean(a$data$ACC$a, na.rm=TRUE),
          "Mean temperature", mean(a$data$TEMP$TEMP, na.rm=TRUE),
          "Mean HR", mean(a$data$HR$HR, na.rm=TRUE),
          "Nr. accepted beats", a$ibi$summary$beats$beats_accepted,
          "Nr. original beats", a$ibi$summary$beats$beats_original,
          "rMSSD", a$ibi$time_analysis$rMSSD,
          "Mean EDA (cleaned)", mean(a$data_summary$EDA$EDA_clean_mean, na.rm = TRUE),
          "Number of peaks per minute (cleaned)", a$data_summary$peaks$peaks_clean_per_min,
          "Mean area under the curve (AUC) (cleaned)", a$data_summary$peaks$peaks_clean_mean_auc,
          "% of data with EDA artefacts", 100 * mean(a$eda_bin$label == -1)
  )
}

#' @export
e4_data_datetime_range <- function(data){
  
  r <- range(data$EDA$DateTime)
  as.numeric(difftime(r[2],r[1], units = "hours"))
  
}
