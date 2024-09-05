box::use(
  bslib[card, layout_columns, nav_panel, nav_panel_hidden, nav_menu, page_navbar],
  sever[reload_button, sever, useSever],
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon, tagList, br,
        reactiveValues, p, img, h1],
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
      useSever()
    ),
    id = ns("navbar_id"),
    title = "Wearalyze",
    theme = constants$wearalyze_theme,
    fillable = FALSE,
    # Note that the device here has a "friendly" name
    devicePage$ui(id = ns("embrace-plus"), device = "Embrace Plus"),
    devicePage$ui(id = ns("nowatch"), device = "Nowatch"),
    devicePage$ui(id = ns("e4"), device = "E4")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Error catching --------------------------------
    disconnected <- tagList(
      h1("Oops, lost connection!",
         style = "color: black;"),
      p("Please try to reconnect."),
      div(
        img(src = "https://github.com/PCdLf/wearalyze/blob/main/app/static/logos/wearalyz_oops.png?raw=true",
            width = "300px")
      ),
      br(),
      div(
        reload_button("Reconnect", class = "warning")
      )
    )

    sever(html = disconnected, bg_color = "#f2f2f2", color = "black")

    # Init ------------------------------------------
    options(shiny.maxRequestSize = 100*1024^2)

    for (device in c("e4", "embrace-plus", "nowatch")) {
      functions$disable_link(menu = device, name = "Calendar")
      functions$disable_link(menu = device, name = "Visualization")
      functions$disable_link(menu = device, name = "Data cutter")
      functions$disable_link(menu = device, name = "Analysis")
      if (device == "embrace-plus" | device == "nowatch") {
        functions$disable_link(menu = device, name = "Batch analysis")
      }
    }

    # Modules ---------------------------------------
    devicePage$server(id = "e4", device = "E4")
    devicePage$server(id = "embrace-plus", device = "Embrace Plus")
    devicePage$server(id = "nowatch", device = "Nowatch")

  })
}
