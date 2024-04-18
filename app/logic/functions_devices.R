
box::use(
  dplyr[filter],
  dygraphs[dygraph, dyHighlight, dyOptions, dyUnzoom, dyLimit, 
           dyAxis, dyEvent, dyRangeSelector, dyShading],
  wearables[as_timeseries],
  zoo[index]
)

box::use(
  app/logic/constants
)

# Utility function to make 4 timeseries based on a dataset
# this timeseries is used in timeseries_plot
make_timeseries <- function(data){
  
  # remove unix_timestamp column from data
  data <- lapply(data, function(x) {
    if (is.data.frame(x)){
      x <- x[!grepl("unix_timestamp", names(x))]
    }
    return(x)
  })
  
  if ("HR" %in% names(data)){
    HR_timeseries <- as_timeseries(data$HR, name_col = "HR")
  } else {
    HR_timeseries <- as_timeseries(data.frame(DateTime = data$EDA$DateTime, HR = NA), name_col = "HR")
  }
  
  if ("ACC" %in% names(data)){
    MOVE_timeseries <- as_timeseries(data$ACC, index = which(names(data$ACC) == "a"), name_col = "Movement")
  } else if ("MOVE" %in% names(data)){
    # For embraceplus data this is accelerometer_std_g (index 3)
    MOVE_timeseries <- as_timeseries(data$MOVE, 3, name_col = "Movement")
  } else {
    MOVE_timeseries <- as_timeseries(data.frame(DateTime = data$EDA$DateTime, Movement = NA), name_col = "Movement")
  }
  
  list(
    EDA = as_timeseries(data$EDA, name_col = "EDA"),
    HR = HR_timeseries,
    TEMP = as_timeseries(data$TEMP, name_col = "Temperature"),
    MOVE = MOVE_timeseries
  )
  
}

my_dygraph <- function(ts, 
                       main_title = FALSE, 
                       ylab_title = FALSE, 
                       draw_x_axis = FALSE, 
                       color = "black",
                       y_line_type = NULL,
                       y_line_val = 0,
                       events = calendar_data,
                       events_label = FALSE,
                       tags = NULL,
                       tag_label = FALSE,
                       yaxis_range = NULL,
                       plot_tags = TRUE
){
  
  begin_time <- min(index(ts)) - 30*60
  
  out <- dygraph(ts, main = main_title, group = "plot2", 
                 ylab = ylab_title, height = 150, width = 900) |>   
    dyHighlight(highlightCircleSize = 5) |>
    dyOptions(drawPoints = FALSE, 
              drawXAxis = draw_x_axis,
              connectSeparatedPoints = TRUE,
              colors = color) |>
    dyUnzoom()
  
  if(!is.null(y_line_type)){
    
    if(y_line_type == "mean"){
      y_line_val <- base::mean(ts, na.rm = TRUE) 
      label <- "Mean"
    } else {
      label <- NULL
    }
    
    out <- out |>
      dyLimit(y_line_val, label, strokePattern = "dashed", color = color)
    
  }
  
  if(!is.null(yaxis_range)){
    out <- out |>
      dyAxis("y", valueRange = yaxis_range)
  }
  
  # Plot calendar events, with Start and End times.
  if(!is.null(events)){
    
    # 'Start' events
    for(i in seq_len(nrow(events))){
      
      txt <- ifelse(events_label, events$Text[i], "")
      
      out <- out |>
        dyEvent(events$Start[i], txt, labelLoc="bottom")
      
    }
    
    
    # Shading events
    eventsub <- filter(events, !is.na(Start) & !is.na(End))
    if(nrow(eventsub)){
      for(i in 1:nrow(eventsub)){
        
        out <- out |>
          dyShading(from = eventsub$Start[i],
                    to = eventsub$End[i],
                    color = eventsub$Color[i])
        
      }
    }
    
  }
  
  # Plot tags, if available.
  if(plot_tags && !is.null(tags)){
    for(i in seq_len(nrow(tags))){
      
      txt <- ifelse(tag_label, "Tag", "")
      
      out <- out |>
        dyEvent(tags$DateTime[i], txt, 
                labelLoc="bottom",
                color = "red", strokePattern = "solid"
        )
    }
    
  }
  
  return(out)
}

# Main function to make dynamic plot
timeseries_plot <- function(data, 
                            main_title = "",
                            calendar_data = NULL,
                            tags = NULL,
                            plot_tags = TRUE,
                            series_options = NULL){
  
  
  list(
    my_dygraph(data$EDA, 
               main_title = main_title, 
               ylab_title = "EDA", 
               events_label = TRUE,
               events = calendar_data,
               tags = tags,
               y_line_type = series_options$EDA$line_type,
               y_line_val = series_options$EDA$custom_y_val,
               yaxis_range = series_options$EDA$yaxis_range,
               color = constants$app_config$visualisation$eda$color,
               plot_tags = plot_tags),
    my_dygraph(data$HR, 
               ylab_title = "HR", 
               tags = tags,
               events = calendar_data,
               y_line_type = series_options$HR$line_type,
               y_line_val = series_options$HR$custom_y_val,
               yaxis_range = series_options$HR$yaxis_range,
               color = constants$app_config$visualisation$hr$color,
               plot_tags = plot_tags),
    my_dygraph(data$TEMP, 
               ylab_title = "Temperature", 
               tags = tags,
               events = calendar_data,
               y_line_type = series_options$TEMP$line_type,
               y_line_val = series_options$TEMP$custom_y_val,
               yaxis_range = series_options$TEMP$yaxis_range,
               color = constants$app_config$visualisation$temp$color,
               plot_tags = plot_tags),
    my_dygraph(data$MOVE, 
               draw_x_axis = TRUE, 
               tags = tags,
               events = calendar_data,
               tag_label = TRUE,
               ylab_title = "Movement", 
               y_line_type = series_options$MOVE$line_type,
               y_line_val = series_options$MOVE$custom_y_val,
               yaxis_range = series_options$MOVE$yaxis_range,
               color = constants$app_config$visualisation$move$color,
               plot_tags = plot_tags) |>
      dyRangeSelector()
  )
}


