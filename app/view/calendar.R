
box::use(
  bslib[card, card_header],
  dplyr[mutate],
  DT[datatable, dataTableOutput, renderDataTable],
  glue[glue],
  shiny[actionButton, bindEvent, br, div, fileInput,  h4, icon, moduleServer,
        NS, observe, p, reactiveVal, renderUI, req, tagList, uiOutput],
  shinycssloaders[withSpinner],
  shinyjs[hide, hidden, show],
  shinytoastr[toastr_error]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/view/components/helpButton
)

ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    card(card_header("Calendar"),
         
         div(style = "width: 100%;",
             div(style = "float: right;",
                 helpButton$ui(ns("help"))
             )
         ),
         
         div(id = "calendar_in_block",
             p("Optionally, select an Excel spreadsheet or textfile with Calendar data."),
             p("Please consult the documentation or Help button for the format of the calendar."),
             
             functions$side_by_side(
               fileInput(ns("select_calendar_file"),
                         label = "Choose Calendar (XLS/XLSX/TXT) file", 
                         multiple = FALSE, 
                         accept = c(".xls",".xlsx", ".txt"),
                         width = 300,
                         buttonLabel = "Browse..."),
               div(style = "padding-left: 50px; padding-top: 25px;",
                   actionButton(ns("btn_use_example_data_large"), "Use large example data", 
                                icon = icon("male"), class = "btn-info"),
                   actionButton(ns("btn_use_example_data_small"), "Use small example data", 
                                icon = icon("child"), class = "btn-info")
               )
             )
         ),
         
         br(),
         hidden(
           div(id = ns("calendar_block"),
               h4("Calendar data"),
               withSpinner(
                 dataTableOutput(ns("dt_calendar"))
               )           
           )
         )
    ),
    uiOutput(ns("ui_problemtarget_block"))
  )
  
  
}



server <- function(id, device, r) {
  moduleServer(id, function(input, output, session) {
    
    calendar_out <- reactiveVal()
    calendar_file <- reactiveVal()
    problemtarget_out <- reactiveVal()
    problemtarget_file <- reactiveVal()
    
    helpButton$server("help", helptext = constants$help_config$calendar)
    
    if(!file.exists(glue("./app/static/example_data/{device}_calendar_large.xlsx"))){
      hide("btn_use_example_data_large")
    }
    if(!file.exists(glue("./app/static/example_data/{device}_calendar_small.xlsx"))){
      hide("btn_use_example_data_small")
    }
    
    observe({
      calendar_file(
        data.frame(
          name = glue("{device}_calendar_large.xlsx"),
          size = NA,
          type = NA,
          datapath = glue("./app/static/example_data/{device}_calendar_large.xlsx")
        )
      )
    }) |> bindEvent(input$btn_use_example_data_large)
    
    observe({
      calendar_file(
        data.frame(
          name = glue("{device}_calendar_small.xlsx"),
          size = NA,
          type = NA,
          datapath = glue("./app/static/example_data/{device}_calendar_small.xlsx")
        )
      )
    }) |> bindEvent(input$btn_use_example_data_small)
    
    observe({
      calendar_file(input$select_calendar_file)
    })
    
    observe({
      
      req(calendar_file())
      
      data <- functions$read_calendar(calendar_file()$datapath)
      
      if(!functions$validate_calendar(data)){
        toastr_error("Calendar data must have columns Date, Start, End, Text, (Color), click Help!")
      } else {
        
        data <- functions$calendar_add_color(data)
        
        calendar_out(
          data
        )
        
        hide("calendar_in_block")
        show("calendar_block")  
      }
      
    })
    
    output$dt_calendar <- renderDataTable({
      
      req(calendar_out())
      
      calendar_out() |>
        mutate(Date = format(Date, "%Y-%m-%d"),
               Start = format(Start, "%H:%M:%S"),
               End = format(End, "%H:%M:%S")
        ) |>
        datatable(selection = "none")
      
    })
    
    output$ui_problemtarget_block <- renderUI({
      
      ns <- session$ns
      
      if (r$more_than_24h == TRUE) {
        
        buttons <- c()
        
        if (file.exists(glue("./app/static/example_data/{device}_problemtarget_large.xlsx"))) {
          buttons <- tagList(buttons, 
                             actionButton(ns("btn_use_example_problemtarget_large"), "Use large example data", 
                                          icon = icon("male"), class = "btn-info"))
        }
        
        if (file.exists(glue("./app/static/example_data/{device}_problemtarget_small.xlsx"))) {
          buttons <- tagList(buttons, 
                             actionButton(ns("btn_use_example_problemtarget_small"), "Use small example data", 
                                          icon = icon("child"), class = "btn-info"))
        }
        
        card(
          card_header("Problem or Target Behavior"),
          div(style = "width: 100%;",
              div(style = "float: right;",
                  helpButton$ui(ns("help2"))
              )
          ),
          div(id = "problemtarget_in_block",
              p("The selected device has a recording interval of more than 24 hours."),
              p("Please upload an Excel file with the Problem or Target behavior data."),
              functions$side_by_side(
                fileInput(ns("select_problemtarget_file"),
                          label = "Choose Problem or Target Behavior (XLS/XLSX/TXT) file", 
                          multiple = FALSE, 
                          accept = c(".xls",".xlsx", ".txt"),
                          width = 450,
                          buttonLabel = "Browse..."),
                div(style = "padding-left: 50px; padding-top: 25px;",
                    buttons
                )
              ),
              br(),
              hidden(
                div(id = ns("problemtarget_block"),
                    h4("Problem or Target Behavior data"),
                    withSpinner(
                      dataTableOutput(ns("dt_problemtarget"))
                    )           
                )
              )
          )
        )
      }
    })
    
    observe({
      
      if (r$more_than_24h == TRUE) {
        helpButton$server("help2", helptext = constants$help_config$problemtarget)
      }
      
    })
    
    observe({
      problemtarget_file(
        data.frame(
          name = glue("{device}_problemtarget_large.xlsx"),
          size = NA,
          type = NA,
          datapath = glue("./app/static/example_data/{device}_problemtarget_large.xlsx")
        )
      )
    }) |> bindEvent(input$btn_use_example_problemtarget_large)
    
    observe({
      problemtarget_file(
        data.frame(
          name = glue("{device}_problemtarget_small.xlsx"),
          size = NA,
          type = NA,
          datapath = glue("./app/static/example_data/{device}_problemtarget_small.xlsx")
        )
      )
    }) |> bindEvent(input$btn_use_example_problemtarget_small)
    
    observe({
      problemtarget_file(input$select_problemtarget_file)
    })
    
    observe({
      
      req(problemtarget_file())
      
      data <- functions$read_problemtarget(problemtarget_file()$datapath)
      
      if(!functions$validate_problemtarget(data)){
        toastr_error("Problem or Target Behavior data must have columns Date, Problem or Target Behavior, Score, click Help!")
      } else {
        
        problemtarget_out(
          data
        )
        
        hide("problemtarget_in_block")
        show("problemtarget_block")  
      }
      
    })
    
    output$dt_problemtarget <- renderDataTable({
      
      req(problemtarget_out())
      
      problemtarget_out() |>
        mutate(Date = format(Date, "%Y-%m-%d")) |>
        datatable(selection = "none")
      
    })
    
    return(
      list(
        calendar = calendar_out,
        problemtarget = problemtarget_out
      )
    )
    
  })
}
