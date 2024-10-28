
box::use(
  bslib[card, card_header],
  glue[glue],
  shiny[bindEvent, checkboxInput, div, NS, fluidRow, tags, column, fileInput, actionButton, htmlOutput, icon,
        uiOutput, reactiveValues, tagList, br, moduleServer, observe, p, withProgress,
        renderUI, req, reactive, incProgress],
  shinyFiles[shinyDirButton, shinyDirChoose],
  shinyjs[disable, enable, hidden, hide, show],
  shinytoastr[toastr_info, toastr_success, toastr_error],
  stats[runif],
  stringr[str_to_title],
  wearables[aggregate_e4_data, aggregate_embrace_plus_data, aggregate_nowatch_data,
            rbind_e4, rbind_embrace_plus, rbind_nowatch,
            read_e4, read_embrace_plus, read_nowatch]
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

               # if device is embrace-plus, add checkbox with aggregated data
               if (device == "embrace-plus") {
                 checkboxInput(ns("use_aggregated"),
                               label = "Use aggregated data",
                               value = constants$device_config[[device]]$aggregated)
               },

               fileInput(ns("select_zip_files"),
                         label = "Choose ZIP file(s)",
                         multiple = TRUE,
                         accept = ".zip",
                         buttonLabel = "Browse..."),

               # if device is embrace-plus or nowatch, show button to choose dir
               # for E4, it's only possible to upload zip files
               if (device %in% c("embrace-plus", "nowatch")) {
                 tagList(
                   tags$p("Or, select a folder containing the data files."),
                   shinyDirButton(ns("select_folder"),
                                  label = "Select folder",
                                  title = "Select folder",
                                  icon = icon("folder-open"),
                                  class = "btn-light"),
                   br(),
                   br()
                 )
               },

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

server <- function(id, device, r) {
  moduleServer(id, function(input, output, session) {

    # Reactive values -------------------------------
    # Note that this object is called rv, and not r, to
    # make a distinction between the petit r object defined in
    # devicePage.R
    rv <- reactiveValues(
      zip_files = NULL,
      data = NULL,
      timeseries = NULL,
      data_agg = NULL,
      newdata = NULL,
      fn_names = NULL,
      aggregated = constants$device_config[[device]]$aggregated
    )

    # if checkbox is used, update rv$aggregated
    observe({
      if (device == "embrace-plus") {
        rv$aggregated <- input$use_aggregated

        if (input$use_aggregated) {
          r$type <- "aggregated"
        } else {
          r$type <- "raw"
        }
      } else if (device == "nowatch") {
        r$type <- "aggregated"
      }
    })

    # Modules --------------------------------------
    helpButton$server("help", helptext = constants$help_config$dataupload[[device]])

    # Functionality ---------------------------------
    shinyDirChoose(input,
                   "select_folder",
                   roots = c(home = ifelse(.Platform$OS.type == "windows", "C:", "~"),
                             wd = "."))

    observe({
      session$reload()
    }) |> bindEvent(input$btn_restart_app)

    ## Example files --------------------------------
    # check if _small or _large files are available for device,
    # and show or hide buttons accordingly
    observe({
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
               if(!file.exists("./app/static/example_data/embrace-plus_large.zip") &
                  !dir.exists("./app/static/example_data/embrace-plus_large")) {
                 hide("btn_use_example_data_large")
               }
               if(!file.exists("./app/static/example_data/embrace-plus_small.zip") &
                  !dir.exists("./app/static/example_data/embrace-plus_small")){
                 hide("btn_use_example_data_small")
               }
             },
             nowatch = {
               if(!file.exists("./app/static/example_data/nowatch_large.zip") &
                  !dir.exists("./app/static/example_data/nowatch_large")){
                 hide("btn_use_example_data_large")
               }
               if(!file.exists("./app/static/example_data/nowatch_small.zip") &
                  !dir.exists("./app/static/example_data/nowatch_small")){
                 hide("btn_use_example_data_small")
               }
             }
      )
    })

    observe({

      disable("btn_use_example_data_large")

      if (device %in% c("embrace-plus")) {
        rv$folder <- glue("./app/static/example_data/{device}_large")
      } else {
        rv$zip_files <- data.frame(
          name = glue("{device}_large.zip"),
          size = NA,
          type = "application/x-zip-compressed",
          datapath = glue("./app/static/example_data/{device}_large.zip")
        )
      }

    }) |> bindEvent(input$btn_use_example_data_large)

    observe({

      disable("btn_use_example_data_small")

      if (device %in% c("embrace-plus", "nowatch")) {
        rv$folder <- glue("./app/static/example_data/{device}_small")
      } else {
        rv$zip_files <- data.frame(
          name = glue("{device}_small.zip"),
          size = NA,
          type = "application/x-zip-compressed",
          datapath = glue("./app/static/example_data/{device}_small.zip")
        )
      }

    }) |> bindEvent(input$btn_use_example_data_small)

    ## Select folder --------------------------------
    # Two ways in which rv$folder gets set:
    # 1. When a folder is selected using the shinyDirChoose function
    # 2. When an example dataset is selected, and rv$folder is defined
    #    on the server side.
    observe({

      req(input$select_folder)
      if(length(input$select_folder) == 1) {
        req(input$select_folder != 1)
      }

      toastr_info("Processing your data... Please wait!")
      disable("select_folder")

      if(length(input$select_folder) > 1){
        # if windows, use \, otherwise use /
        if (.Platform$OS.type == "windows") {
          chc <- paste0(ifelse(input$select_folder$root == "home", ifelse(.Platform$OS.type == "windows", "C:", "~"), "."),
                        paste0(input$select_folder$path, collapse = "\\")
          )
        } else {
          chc <- paste0(ifelse(input$select_folder$root == "home", ifelse(.Platform$OS.type == "windows", "C:", "~"), "."),
                        paste0(input$select_folder$path, collapse = "/")
          )
        }
      } else {
        chc <- NA
      }

      if(!is.na(chc)){
        rv$folder <- chc
      }

    })

    observe({

      req(rv$folder)

      switch(device,
             `embrace-plus` = {
               out <- read_embrace_plus(folder = rv$folder, type = ifelse(rv$aggregated, "aggregated", "raw"))
             },
             nowatch = {
               out <- read_nowatch(folder = rv$folder)
             })

      if(is.null(out)){

        toastr_error("Something went wrong with getting the data - check folder!")

        functions$disable_link(menu = device, name = "Calendar")
        functions$disable_link(menu = device, name = "Visualization")
        functions$disable_link(menu = device, name = "Data cutter")

      } else {
        switch(device,
               `embrace-plus` = {
                 rv$data <- out
                 if (rv$aggregated) {
                   rv$data <- out
                 } else {
                   rv$data <- rbind_embrace_plus(rv$data)
                 }
               },
               nowatch = {
                 rv$data <- out
               })

        # Get min and max date, and determine whether or not there's more than 24 hours
        if (device == "nowatch") {
          min_date <- min(rv$data$HR$DateTime)
          max_date <- max(rv$data$HR$DateTime)
        } else {
          min_date <- min(rv$data$EDA$DateTime)
          max_date <- max(rv$data$EDA$DateTime)
        }
        r$more_than_24h <- difftime(max_date, min_date, units = "hours") > 24
        r$more_than_2weeks <- difftime(max_date, min_date, units = "weeks") > 2

        switch(device,
               `embrace-plus` = {
                 if (rv$aggregated) {
                   rv$data_agg <- aggregate_embrace_plus_data(rv$data, interval = ifelse(r$more_than_2weeks, "5 min", "1 min"))
                 } else {
                   rv$data_agg <- aggregate_embrace_plus_data(rv$data, interval = ifelse(r$more_than_2weeks, "5 min", "1 min"))
                 }
               },
               nowatch = {
                 rv$data_agg <- aggregate_nowatch_data(rv$data, interval = ifelse(r$more_than_2weeks, "5 min", "1 min"))
               })

        rv$newdata <- runif(1)

        # Message: data read!
        toastr_success("Data read successfully.")

        functions$enable_link(menu = device, name = "Calendar")
        functions$enable_link(menu = device, name = "Visualization")

        if (device == "e4") {
          functions$enable_link(menu = device, name = "Data cutter")
        }

        hide("div_upload_file")
        show("div_restart_application")
        enable("btn_use_example_data_small")
        enable("btn_use_example_data_large")
        enable("select_folder")
      }
    }) |> bindEvent(rv$folder)


    ## Select zip file(s) ---------------------------
    observe({
      rv$zip_files <- input$select_zip_files
    })

    observe({

      req(rv$zip_files)

      disable("select_zip_files")

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
                   out <- read_embrace_plus(zipfile = fns[i])
                 },
                 nowatch = {
                   out <- read_nowatch(fns[i])
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
                 },
                 `embrace-plus` = {
                   rv$data <- rbind_embrace_plus(data[[1]])
                 },
                 nowatch = {
                   rv$data <- rbind_nowatch(data)
                 })

          if (device == "nowatch") {
            min_date <- min(rv$data$HR$DateTime)
            max_date <- max(rv$data$HR$DateTime)
          } else {
            min_date <- min(rv$data$EDA$DateTime)
            max_date <- max(rv$data$EDA$DateTime)
          }
          r$more_than_24h <- difftime(max_date, min_date, units = "hours") > 24
          r$more_than_2weeks <- difftime(max_date, min_date, units = "weeks") > 2

          switch(device,
                 e4 = {
                   rv$data_agg <- aggregate_e4_data(rv$data)
                 },
                 `embrace-plus` = {
                   rv$data_agg <- aggregate_embrace_plus_data(rv$data, interval = ifelse(r$more_than_2weeks, "5 min", "1 min"))
                 },
                 nowatch = {
                   rv$data_agg <- aggregate_nowatch_data(rv$data, interval = ifelse(r$more_than_2weeks, "5 min", "1 min"))
                 })

          rv$newdata <- runif(1)

          # Message: data read!
          toastr_success("Data read successfully.")

          functions$enable_link(menu = device, name = "Calendar")
          functions$enable_link(menu = device, name = "Visualization")

          if (device == "e4") {
            functions$enable_link(menu = device, name = "Data cutter")
          }

          hide("div_upload_file")
          show("div_restart_application")
          enable("btn_use_example_data_small")
          enable("btn_use_example_data_large")
          enable("select_zip_files")

        } else {

          functions$disable_link(menu = device, name = "Calendar")
          functions$disable_link(menu = device, name = "Visualization")
          functions$disable_link(menu = device, name = "Data cutter")
        }

      })

    }) |> bindEvent(rv$zip_files)

    ## Messages ------------------------------------
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

    # Return values ---------------------------------
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
