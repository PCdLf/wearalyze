
box::use(
  bslib[card, card_header],
  lubridate[day, hour, minute, month, second, year],
  shiny[actionButton, column, bindEvent, br, dateInput, div, fluidRow, icon,
        moduleServer, numericInput, NS, observe, p, reactive, reactiveVal, 
        renderUI, req, tagList, tags, uiOutput, updateDateInput, updateNumericInput],
  shinyFiles[shinyDirButton, shinyDirChoose],
  shinyjs[disable, enable, hidden, hide, show, toggleState],
  shinytoastr[toastr_info, toastr_success],
  wearables[filter_createdir_zip]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/view/components/helpButton
)

ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Data cutter"),
      
      tags$div(style = "width: 100%; height: 30px;",
               tags$div(style = "float: right;",
                        helpButton$ui(ns("help"))
               )
      ),
      
      
      fluidRow(
        column(6, 
               
               tags$p("Cut the data into separate ZIP files, for example 5min in each file."),
               
               tags$p("Select start and end time of the desired section on the right."),
               
               numericInput(ns("num_interval_length"), "Select interval length (minutes)",
                            value = 5, min = 1, step = 1),
               
               functions$side_by_side(
                 shinyDirButton(ns("btn_select_folder_output"), 
                                label = "Select output folder",
                                title = "Select output folder",
                                icon = icon("folder-open"), 
                                class = "btn-light"),
                 uiOutput(ns("ui_folder_out"), inline = TRUE)
               ),
               
               
               uiOutput(ns("ui_can_do_cut"), style = "padding-top: 24px;"),
               actionButton(ns("btn_do_cut"), "Perform cut", class = "btn-success btn-lg", 
                            icon = icon("play"))       
               
        ),
        column(6,
               tags$h4("Select begin date / time"),
               
               functions$side_by_side(
                 dateInput(ns("date_analysis_start"), label = "Date",
                           value = NULL, min = NULL, max = NULL,
                           width = 200),
                 numericInput(ns("time_hour_start"), "Hour", value = 0, width = 100, max = 24),
                 numericInput(ns("time_minute_start"), "Minutes", value = 0, width = 100, max = 60),
                 numericInput(ns("time_second_start"), "Seconds", value = 0, width = 100, max = 60)
               ),
               tags$br(),
               
               tags$h4("Select end date / time"),
               functions$side_by_side(
                 dateInput(ns("date_analysis_end"), label = "Date",
                           value = NULL, min = NULL, max = NULL,
                           width = 200),
                 numericInput(ns("time_hour_end"), "Hour", value = 0, width = 100, max = 24),
                 numericInput(ns("time_minute_end"), "Minutes", value = 0, width = 100, max = 60),
                 numericInput(ns("time_second_end"), "Seconds", value = 0, width = 100, max = 60)
               )
               
        )
      )
      
    )
  )
  
}

server <- function(id, data = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values -------------------------------
    folder_out <- reactiveVal()
    
    interval_can_be_cut <- reactive({
      if(is.na(start_time()) || is.na(end_time())){
        return(FALSE)
      } else {
        m <- as.numeric(difftime(start_time(), end_time(), units= "mins"))
        m %% input$num_interval_length == 0  
      }
    })
    
    start_time <- reactive({
      ISOdatetime(
        year = year(input$date_analysis_start),
        month = month(input$date_analysis_start),
        day = day(input$date_analysis_start),
        hour = input$time_hour_start,
        min = input$time_minute_start,
        sec = input$time_second_start,
        tz = "UTC"
      )
    })
    
    end_time <- reactive({
      ISOdatetime(
        year = year(input$date_analysis_end),
        month = month(input$date_analysis_end),
        day = day(input$date_analysis_end),
        hour = input$time_hour_end,
        min = input$time_minute_end,
        sec = input$time_second_end,
        tz = "UTC"
      ) 
    })
    
    # Modules --------------------------------------
    helpButton$server("help", helptext = constants$help_config$cut)
    
    # Functionality ---------------------------------
    shinyDirChoose(input, 
                   "btn_select_folder_output",
                   roots = c(home = "~", 
                             wd = "."))
    
    observe({
      
      data <- data()$data
      
      req(nrow(data$EDA) > 0)
      
      tms <- range(data$EDA$DateTime)
      updateDateInput(session, "date_analysis_start",
                      value = min(as.Date(tms)),
                      min = min(as.Date(tms)),
                      max = max(as.Date(tms))
      )
      updateDateInput(session, "date_analysis_end",
                      value = max(as.Date(tms)),
                      min = min(as.Date(tms)),
                      max = max(as.Date(tms))
      )
      
      updateNumericInput(session, "time_hour_start",
                         value = hour(min(tms)), min = 0, max = 23)
      updateNumericInput(session, "time_hour_end",
                         value = hour(max(tms)), min = 0, max = 23)
      
      updateNumericInput(session, "time_minute_start",
                         value = minute(min(tms)), min = 0, max = 59)
      updateNumericInput(session, "time_minute_end",
                         value = minute(max(tms)), min = 0, max = 59)
      
      updateNumericInput(session, "time_second_start",
                         value = second(min(tms)), min = 0, max = 59)
      updateNumericInput(session, "time_second_end",
                         value = second(max(tms)), min = 0, max = 59)
      
      
    })
    
    observe({
      toggleState("btn_do_cut", condition = interval_can_be_cut() & !is.null(folder_out()))
    })
    
    output$ui_can_do_cut <- renderUI({
      
      if(isTRUE(interval_can_be_cut())){
        NULL
      } else {
        
        if(is.null(folder_out())){
          tags$p("Please select an output folder.",
                 style = "font-size: 0.9em; font-style: italic;")        
        } else {
          tags$p("Please select start and end times that can be exactly cut by the interval.",
                 style = "font-size: 0.9em; font-style: italic;")
        }
        
      }
      
    })
    
    observe({
      
      req(input$btn_select_folder_output)
      
      if(length(input$btn_select_folder_output) > 1){
        chc <- paste0(ifelse(input$btn_select_folder_output$root == "home", "~", "."), 
                      paste0(input$btn_select_folder_output$path, collapse = "/")
        )
      } else {
        chc <- NA
      }
      
      if(!is.na(chc)){
        folder_in(chc)  
      }
      
    }) 
    
    output$ui_folder_out <- renderUI({
      
      req(folder_out())
      tags$p(folder_out(), style = "font-size: 0.9em; font-style: italic; padding-top: 8px;")
      
    })
    
    observe({
      
      toastr_info("ZIP file cutting started....")
      disable("btn_do_cut")
      
      filter_createdir_zip(data = data()$data,
                                      time_start = start_time(),
                                      time_end = end_time(),
                                      interval = input$num_interval_length, 
                                      out_path = folder_out(),
                                      fn_name = data()$fn_names[1]
      )
      
      toastr_success("ZIP file cut into pieces")
      enable("btn_do_cut")
      
    }) |> bindEvent(input$btn_do_cut)
    
  })
}