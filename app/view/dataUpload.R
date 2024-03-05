
box::use(
  bslib[card, card_header],
  glue[glue],
  shiny[bindEvent, div, NS, fluidRow, tags, column, fileInput, actionButton, htmlOutput, icon,
        uiOutput, reactiveValues, tagList, br, moduleServer, observe, p, withProgress,
        renderUI, req, reactive, incProgress],
  shinyjs[hidden, hide, show],
  shinytoastr[toastr_success, toastr_error],
  stats[runif],
  stringr[str_to_title],
  wearables[aggregate_e4_data, aggregate_embrace_plus_data,
            rbind_e4, rbind_embrace_plus,
            read_e4, read_embrace_plus]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/view/components/helpButton
)

ui <- function(id, device) {
  
  device_name <- functions$get_device_name(device, title = TRUE)
  
  ns <- NS(id)
  
  tagList(
    
    card(
      card_header("Start"),
      fluidRow(
        column(8, 
               tags$p(glue("This Shiny application was designed to visualize and process {constants$device_config[[device]]$company} {device_name} data.")),
               tags$p(glue("The {constants$device_config[[device]]$company} {device_name} is a wearable wristband that can be used to record physiological signals such as heart rate, temperature, movement and skin conductance.")),
               tags$p("The data will not be permanently stored on the server, no trackers or cookies are used.")
        ),
        
        column(4,
               tags$a(href = constants$device_config[[device]]$website, target = "_blank",
                      tags$img(src = glue("static/devices/{device}.png"), 
                               height = "150px", 
                               width = "150px", 
                               align = "left")
               )
        )
      ),
      fluidRow(style = "padding-top: 24px;",
               
               column(4, functions$logo_image_with_link("static/logos/logo_deborg.svg", "https://www.deborg.nl/", width = "100%")),
               column(4, functions$logo_image_with_link("static/logos/mit_media_lab.png", "https://www.media.mit.edu/groups/affective-computing/overview/", width = "100%")),
               column(4, functions$logo_image_with_link("static/logos/u_twente.png","https://www.utwente.nl/nl/bms/pgt/", width = "100%"))
      ),
      fluidRow(
        column(4, functions$logo_image_with_link("static/logos/umcu.png", "https://www.umcutrecht.nl/nl/innovatie-in-de-psychiatrie", width = "100%")),
        column(4, functions$logo_image_with_link("static/logos/radboud.png","https://www.ru.nl/bsi/", width = "100%"))
        
      )
    ),
    card(
      card_header("Data input"),
      tags$div(style = "width: 100%;",
               tags$div(style = "position: absolute; right: 10px",
                        helpButton$ui(ns("help"))
               )
      ),
      
      tags$div(id = ns("div_upload_file"),
               tags$p(glue("Click Browse to select {device_name} zip files to use in the application.")),
               
               fileInput(ns("select_zip_files"),
                         label = "Choose ZIP file(s)", 
                         multiple = TRUE, 
                         accept = ".zip",
                         buttonLabel = "Browse..."),
               
               tags$p("Or, use one of the built-in example datasets.",
                      style = "font-size: 0.95em; font-style: italic;"),
               actionButton(ns("btn_use_example_data_large"), "Use large example dataset", 
                            icon = icon("male"), class = "btn-info"),
               actionButton(ns("btn_use_example_data_small"), "Use small example dataset", 
                            icon = icon("child"), class = "btn-info"),
               
               uiOutput(ns("msg_files_selected")),
               br(),
               htmlOutput(ns("msg_data_read"))
      ),
      
      hidden(
        div(id = ns("div_restart_application"),
            actionButton(ns("btn_restart_app"), "Reset and start over",
                         icon = icon("sync"), class = "btn-lg btn-success")
        )
      )
    )
    
  )
}

server <- function(id, device) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values -------------------------------
    rv <- reactiveValues(
      zip_files = NULL,
      data = NULL,
      timeseries = NULL,
      data_agg = NULL,
      newdata = NULL,
      fn_names = NULL
    )
    
    # Modules --------------------------------------
    helpButton$server("help", helptext = constants$help_config$dataupload[[device]])
    
    # Functionality ---------------------------------
    # check if _small or _large files are available for device
    switch(device,
           e4 = {
             if(!file.exists("./app/static/example_data/e4_large.zip")){
               hide("btn_use_example_data_large")
             }
             if(!file.exists("./app/static/example_data/e4_small.zip")){
               hide("btn_use_example_data_small")
             }
           },
           `embrace-plus` = {
             if(!file.exists("./app/static/example_data/embrace-plus_large.zip")){
               hide("btn_use_example_data_large")
             }
             if(!file.exists("./app/static/example_data/embrace-plus_small.zip")){
               hide("btn_use_example_data_small")
             }
           },
           nowatch = {
             if(!file.exists("./app/static/example_data/nowatch_large.zip")){
               hide("btn_use_example_data_large")
             }
             if(!file.exists("./app/static/example_data/nowatch_small.zip")){
               hide("btn_use_example_data_small")
             }
           }
    )
    
    observe({
      rv$zip_files <- data.frame(
        name = glue("{device}_large.zip"),
        size = NA,
        type = "application/x-zip-compressed",
        datapath = glue("./app/static/example_data/{device}_large.zip")
      )
    }) |> bindEvent(input$btn_use_example_data_large)
    
    observe({
      rv$zip_files <- data.frame(
        name = glue("{device}_small.zip"),
        size = NA,
        type = "application/x-zip-compressed",
        datapath = glue("./app/static/example_data/{device}_small.zip")
      )
    }) |> bindEvent(input$btn_use_example_data_small)
    
    observe({
      rv$zip_files <- input$select_zip_files
    })
    
    observe({
      
      req(rv$zip_files)
      
      # Read selected ZIP files
      fns <- rv$zip_files$datapath
      fn_names <- rv$zip_files$name
      rv$fn_names <- fn_names
      
      # Read data into a list (Each element of the list contents from 1 zip file)
      data <- list()
      n <- length(fns) + 1
      withProgress(message = "Reading data...", value = 0, {
        
        for(i in seq_along(fns)){
          
          incProgress(1/n, detail = fn_names[i])
          
          switch(device,
                 e4 = {
                   out <- read_e4(fns[i])
                 },
                 `embrace-plus` = {
                   out <- read_embrace_plus(fns[i])
                 })
          
          if(is.null(out)){
            
            toastr_error("One or more data files empty - check data!")
            break
            
          } else {
            data[[i]] <- out  
          }
          
        }
        
        if(length(data) > 0){
          
          # If more than 1 zip file selected, row-bind them using our custom function
          incProgress(1/n, detail = "Row-binding")
          
          switch(device,
                 e4 = {
                   rv$data <- rbind_e4(data)
                   rv$data_agg <- aggregate_e4_data(rv$data)
                 },
                 `embrace-plus` = {
                   rv$data <- rbind_embrace_plus(data[[1]])
                   rv$data_agg <- aggregate_embrace_plus_data(rv$data)
                 },
                 #TODO: Add nowatch
                 nowatch = {
                   rv$data <- rbind_nowatch(data)
                   rv$data_agg <- aggregate_nowatch_data(rv$data)
                 })
          
          rv$newdata <- runif(1)
          
          # Message: data read!
          toastr_success("Data read successfully.")
          
          functions$enable_link(menu = device, name = "Calendar")
          functions$enable_link(menu = device, name = "Visualization")
          functions$enable_link(menu = device, name = "Data cutter")
          
          hide("div_upload_file")
          show("div_restart_application")
          
        } else {
          
          functions$disable_link(menu = device, name = "Calendar")
          functions$disable_link(menu = device, name = "Visualization")
          functions$disable_link(menu = device, name = "Data cutter")
        }
        
      })
      
    })
    
    output$msg_data_read <- renderUI({
      
      req(rv$data)
      tagList(
        tags$p("Data was uploaded and read successfully. Go to the Calendar Tab.",
               style = "color: blue;"),
        tags$p("To read in a new dataset, upload a new Zip file.")
      )
      
    })
    
    
    output$msg_files_selected <- renderUI({
      
      req(rv$zip_files)
      n <- nrow(rv$zip_files)
      if(n > 0){
        p(glue("You have selected {n} ZIP files."))
      }
      
    })
    
    observe({
      session$reload()
    }) |> bindEvent(input$btn_restart_app)
    
    
    out <- reactive({
      
      list(
        data = rv$data,
        data_agg = rv$data_agg,
        timeseries = rv$timeseries,
        newdata = rv$newdata,
        fn_names = rv$fn_names
      )
      
    })
    
    return(out)
    
  })
}