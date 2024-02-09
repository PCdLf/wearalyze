box::use(
  bslib[card, layout_columns, nav_panel, nav_panel_hidden, nav_menu, page_navbar],
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon, tagList, br,
        reactiveValues, p],
  shinyjs[useShinyjs],
  shinytoastr[useToastr]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/view/devicePage
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    header = tagList(
      useShinyjs(),
      useToastr(),
    ),
    id = ns("navbar_id"),
    title = "Wearalyze",
    theme = constants$wearalyze_theme,
    fillable = FALSE,
    devicePage$ui(id = ns("e4"), device = "E4"),
    devicePage$ui(id = ns("embrace-plus"), device = "Embrace Plus"),
    devicePage$ui(id = ns("nowatch"), device = "Nowatch"),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Init ------------------------------------------
    options(shiny.maxRequestSize = 100*1024^2)
    
    for (device in c("e4", "embrace-plus", "nowatch")) {
      functions$disable_link(menu = device, name = "Calendar")
      functions$disable_link(menu = device, name = "Visualization")
      functions$disable_link(menu = device, name = "Data cutter")
      functions$disable_link(menu = device, name = "Analysis") 
    }
    
    # Modules ---------------------------------------
    devicePage$server(id = "e4", device = "E4")
    devicePage$server(id = "embrace-plus", device = "Embrace Plus")
    devicePage$server(id = "nowatch", device = "Nowatch")
    
  })
}
