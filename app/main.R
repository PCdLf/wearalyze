box::use(
  bslib[card, layout_columns, nav_panel, nav_panel_hidden, nav_menu, page_navbar],
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon, tagList, br,
        reactiveValues, p],
  shinyjs[useShinyjs],
  shinytoastr[useToastr],
  waiter[useWaiter, waiterPreloader, spin_6]
)

box::use(
  app/logic/constants,
  app/view/dataUpload,
  app/view/calendar,
  app/view/visualization
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    header = tagList(
      useShinyjs(),
      useToastr(),
      # useWaiter(),
      # waiterPreloader(
      #   html = tagList(spin_6(),
      #                  br(),
      #                  br(),
      #                  "Loading..."
      #   )
      # )
    ),
    id = ns("navbar_id"),
    title = "Wearalyze",
    theme = constants$wearalyze_theme,
    fillable = FALSE,
    nav_menu(
      title = "E4",
      value = "e4-menu",
      icon = icon("heart-circle-bolt"),
      nav_panel("Data",
                icon = icon("file-upload"),
                dataUpload$ui(ns("e4-data"))
      ),
      nav_panel("Calendar",
                icon = icon("calendar-alt"),
                calendar$ui(ns("e4-calendar"))
      ),
      nav_panel("Visualization",
                icon = icon("chart-bar"),
                visualization$ui(ns("e4-visualization"))
                
      ),
      nav_panel("Analysis",
                icon = icon("chart-line"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Batch analysis",
                icon = icon("list-ol"),
                card(
                  p("placeholder")
                )
                
      )
    ),
    nav_menu(
      title = "Embrace Plus",
      icon = icon("heart-pulse"),
      nav_panel("Data",
                icon = icon("file-upload"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Calendar",
                icon = icon("calendar-alt"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Visualization",
                icon = icon("chart-bar"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Analysis",
                icon = icon("chart-line"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Batch analysis",
                icon = icon("list-ol"),
                card(
                  p("placeholder")
                )
                
      )
    ),
    nav_menu(
      title = "Nowatch",
      icon = icon("clock"),
      nav_panel("Data",
                icon = icon("file-upload"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Calendar",
                icon = icon("calendar-alt"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Visualization",
                icon = icon("chart-bar"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Analysis",
                icon = icon("chart-line"),
                card(
                  p("placeholder")
                )
                
      ),
      nav_panel("Batch analysis",
                icon = icon("list-ol"),
                card(
                  p("placeholder")
                )
                
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values -------------------------------
    r <- reactiveValues(placeholder = NULL)
    
    # Modules ---------------------------------------
    e4_data_in <- dataUpload$server(id = "e4-data")
    e4_calendar <- calendar$server(id = "e4-calendar")
    e4_calendar <- calendar$server(id = "e4-calendar")
    e4_visualization <- visualization$server(id = "e4-visualization",
                                             data = e4_data_in)
    
  })
}
