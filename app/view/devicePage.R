
box::use(
  bslib[nav_menu, nav_panel],
  shiny[icon, moduleServer, NS, reactiveValues]
)

box::use(
  app/view/dataUpload,
  app/view/calendar,
  app/view/visualization,
  app/view/analysis,
  app/view/cutData,
  app/view/batch,
  app/logic/functions,
  app/logic/constants
)

ui <- function(id, device){

  ns <- NS(id)

  # The device can contain spaces and upper case letters,
  # this is stored in device_name and can be used in text to
  # display the device in a friendly way. The device variable
  # is overwritten and used to identify the device in the back-end.
  device_name <- device
  device <- functions$get_device_id(device)

  nav_menu(
    title = device_name,
    value = paste0(device, "-menu"),
    icon = icon("heart-circle-bolt"),
    nav_panel("Data",
              icon = icon("file-upload"),
              dataUpload$ui(ns(paste0(device, "-data")),
                            device = device)
    ),
    nav_panel("Calendar",
              icon = icon("calendar-alt"),
              calendar$ui(ns(paste0(device, "-calendar")))
    ),
    nav_panel("Visualization",
              icon = icon("chart-bar"),
              visualization$ui(ns(paste0(device, "-visualization")))
    ),
    nav_panel("Analysis",
              icon = icon("chart-line"),
              analysis$ui(ns(paste0(device, "-analysis")))
    ),
    nav_panel("Data cutter",
              icon = icon("cut"),
              cutData$ui(ns(paste0(device, "-cut")))
    ),
    nav_panel("Batch analysis",
              icon = icon("list-ol"),
              batch$ui(ns(paste0(device, "-batch")))
    )
  )

}

server <- function(id, device) {
  moduleServer(id, function(input, output, session) {

    device <- functions$get_device_id(device)

    # reactive values -------------------------------
    # top-level reactive values object, following
    # the strategie du petit r
    # this object is passed along to child modules and can
    # be accessed and updated from inside those modules
    r <- reactiveValues(
      device = NULL,
      type = ifelse(constants$device_config[[device]]$aggregated, "aggregated", "raw"),
      more_than_24h = FALSE,
      more_than_2weeks = FALSE
    )

    # Modules ---------------------------------------
    data_in <- dataUpload$server(id = paste0(device, "-data"),
                                 device = device,
                                 r = r)

    calendar <- calendar$server(id = paste0(device, "-calendar"),
                                device = device,
                                r = r)

    visualization <- visualization$server(id = paste0(device, "-visualization"),
                                          data = data_in,
                                          device = device,
                                          calendar = calendar$calendar,
                                          problemtarget = calendar$problemtarget,
                                          r = r)

    analysis$server(id = paste0(device, "-analysis"),
                    data = data_in,
                    plots = visualization,
                    calendar = calendar$calendar,
                    device = device)

    cutData$server(id = paste0(device, "-cut"),
                   data = data_in)

    batch$server(id = paste0(device, "-batch"),
                 device = device)

  })
}
