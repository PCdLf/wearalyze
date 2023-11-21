
box::use(
  bslib[card, card_header],
  shiny[NS, fluidRow, tags, column, fileInput, actionButton, htmlOutput, icon,
        uiOutput, tagList, br],
  shinyjs[hidden]
)

box::use(
  app/logic/functions
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    card(
      card_header("Start"),
      fluidRow(
        column(6, 
               tags$p("This Shiny application was designed to visualize and process Empatica E4 data."),
               tags$p("The Empatica E4 is a wearable wristband that can be used to record physiological signals such as heart rate, temperature, movement and skin conductance."),
               tags$p("The data will not be permanently stored on the server, no trackers or cookies are used.")
        ),
        
        column(6,
               tags$img(src= "static/devices/e4_hero_device-lg-hdpi.jpg", 
                        height="150px", 
                        width="150px", 
                        align="left",href="https://www.empatica.com/research/e4/",target = "_blank")
               
        )
      ),
      fluidRow(style = "padding-top: 24px;",
               
               column(3, functions$logo_image_with_link("static/logos/logo_deborg.svg", "https://www.deborg.nl/", width = "100%")),
               column(3, functions$logo_image_with_link("static/logos/mit_media_lab.png", "https://www.media.mit.edu/groups/affective-computing/overview/", width = "100%")),
               column(3, functions$logo_image_with_link("static/logos/u_twente.png","https://www.utwente.nl/nl/bms/pgt/", width = "100%"))
      ),
      fluidRow(
        column(3, functions$logo_image_with_link("static/logos/umcu.png", "https://www.umcutrecht.nl/nl/innovatie-in-de-psychiatrie", width = "100%")),
        column(3, functions$logo_image_with_link("static/logos/radboud.png","https://www.ru.nl/bsi/", width = "100%"))
        
      )
    ),
    card(
      card_header("Data input"),
      tags$div(style = "width: 100%;",
               tags$div(style = "float: right;",
                        #helpButtonUI(ns("help"))
               )
      ),
      
      tags$div(id = ns("div_upload_file"),
               tags$p("Click Browse to select E4 zip files to use in the application."),
               
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
        tags$div(id = ns("div_restart_application"),
                 
                 actionButton(ns("btn_restart_app"), "Reset and start over",
                              icon = icon("sync"), class = "btn-lg btn-success")
                 
        )
      )
    )

  )
}

#' @export
server <- function(input, output, session) {
  serverModule(id, function(input, output, session) {
    # observeEvent(input$mybutton, {
    #   shinyjs::toggle(id = "mydiv")
    # })
  })
}