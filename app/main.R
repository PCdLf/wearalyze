box::use(
  bslib[card, layout_columns, nav_panel, nav_menu, page_navbar],
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, icon, tagList, br,
        reactiveValues, p],
  shinyjs[useShinyjs],
  waiter[useWaiter, waiterPreloader, spin_6]
)

box::use(
  app/logic/constants
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    header = tagList(
      useShinyjs(),
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
    nav_panel("E4",
              icon = icon("heart-circle-bolt"),
              card(
                p("placeholder")
              )
    ),
    nav_panel("Embrace Plus",
              icon = icon("heart-pulse"),
              layout_columns(
                card(
                  p("placeholder")
                ),
                card(
                  p("placeholder")
                )
              )
    ),
    nav_panel("Nowatch",
              icon = icon("clock"),
              card(
                p("placeholder")
              )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # reactive values
    r <- reactiveValues(placeholder = NULL)

  })
}
