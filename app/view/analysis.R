
box::use(
  bslib[card, card_body, card_header],
  lubridate[day, hour, minute, month, second, year],
  shiny[actionButton, bindEvent, dateInput, div, downloadButton, downloadHandler, NS, numericInput,
        fluidRow, tags, updateDateInput, updateNumericInput, column, icon,
        uiOutput, moduleServer, observe, p,
        reactive, reactiveVal, renderUI, req, tagList],
  shinyjs[hidden, hide, show],
  shinytoastr[toastr_info, toastr_warning],
  wearables[filter_e4data_datetime, process_e4],
  rmarkdown[render]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/view/components/helpButton
)

ui <- function(id){
  
  ns <- NS(id)
  tagList(
    card(
      card_header("Analysis"),
      card_body(
        tags$div(style = "width: 100%; height: 30px;",
                 tags$div(style = "float: right;",
                          helpButton$ui(ns("help"))
                 )
        ),
        fluidRow(
          column(6, 
                 
                 tags$p("Run the signal analysis for the time period shown on the visualisation tab, 
                                          or adjust the period with the menu on the right."),
                 tags$p("When the analysis is complete, you can download the report in a box below."),
                 
                 actionButton(ns("btn_do_analysis"), "Run analysis", class = "btn-success btn-lg", 
                              icon = icon("calculator"))       
                 
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
    ),
    uiOutput(ns("ui_download_report")) 
  )
}




server <- function(id, data = reactive(NULL), plots = reactive(NULL), calendar = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Modules --------------------------------------
    helpButton$server("help", helptext = constants$help_config$analysis)
    helpButton$server("help-report", helptext = constants$help_config$report)
    
    # Functionality --------------------------------
    observe({
      
      data <- data()$data
      
      req(nrow(data$EDA) >0)
      
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
    
    
    last_analysis <- reactiveVal()
    
    observe({
      
      toastr_warning("Analysis started - please be patient, this can take a minute or longer", 
                     timeOut = 3000)
      
      start <- ISOdatetime(
        year = year(input$date_analysis_start),
        month = month(input$date_analysis_start),
        day = day(input$date_analysis_start),
        hour = input$time_hour_start,
        min = input$time_minute_start,
        sec = input$time_second_start,
        tz = "UTC"
      )
      
      end <- ISOdatetime(
        year = year(input$date_analysis_end),
        month = month(input$date_analysis_end),
        day = day(input$date_analysis_end),
        hour = input$time_hour_end,
        min = input$time_minute_end,
        sec = input$time_second_end,
        tz = "UTC"
      )
      
      data <- data()$data
      data <- filter_e4data_datetime(data, start, end)
      
      analysis_out <- process_e4(data)
      
      last_analysis(
        analysis_out
      )
      
    }) |> bindEvent(input$btn_do_analysis)
    
    
    
    output$ui_download_report <- renderUI({
      req(last_analysis())
      
      card(
        card_header("Report"),
        card_body(
          tags$p("Download a report of the current analysis."),
          downloadButton(session$ns("btn_download_report"), "Download Report",
                         icon = icon("file-download"),
                         class = "btn btn-lg btn-success"),
          tags$div(style = "width: 100%;",
                   tags$div(style = "float: right;",
                            helpButton$ui(session$ns("help-report"))
                   )
          )
        )
      )
    })
    
    output$btn_download_report <- downloadHandler(
      
      filename = "e4_analysis.html",
      
      content = function(file){
        
        toastr_info("Download in preparation ...")
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("./app/static/report.Rmd", tempReport, overwrite = TRUE)
        
        analysis <- last_analysis()
        calendar <- calendar()
        plots <- plots()
        
        render(tempReport, output_file = file)
        
      }
      
    )
    
  })
  
}

