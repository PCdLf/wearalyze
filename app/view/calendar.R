
box::use(
  bslib[card, card_header],
  dplyr[mutate],
  DT[datatable, dataTableOutput, renderDataTable],
  glue[glue],
  shiny[actionButton, bindEvent, br, div, fileInput,  h4, icon, moduleServer,
        NS, observe, p, reactiveVal, req, tagList],
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
         
    )
  )
  
  
}



server <- function(id, device, r) {
  moduleServer(id, function(input, output, session) {
    
    calendar_out <- reactiveVal()
    calendar_file <- reactiveVal()
    
    helpButton$server("help", helptext = constants$help_config$calendar)
    
    # check if _small or _large files are available for device
    switch(device,
           e4 = {
             if(!file.exists("./app/static/example_data/e4_calendar_large.xlsx")){
               hide("btn_use_example_data_large")
             }
             if(!file.exists("./app/static/example_data/e4_calendar_small.xlsx")){
               hide("btn_use_example_data_small")
             }
           },
           `embrace-plus` = {
             if(!file.exists(glue("./app/static/example_data/embrace-plus_calendar_large.xlsx"))){
               hide("btn_use_example_data_large")
             }
             if(!file.exists("./app/static/example_data/embrace-plus_calendar_small.xlsx")){
               hide("btn_use_example_data_small")
             }
           },
           nowatch = {
             if(!file.exists("./app/static/example_data/nowatch_calendar_large.xlsx")){
               hide("btn_use_example_data_large")
             }
             if(!file.exists("./app/static/example_data/nowatch_calendar_small.xlsx")){
               hide("btn_use_example_data_small")
             }
           }
    )
    
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
    
    return(calendar_out)
    
  })
}
