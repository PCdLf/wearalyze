
box::use(
  bslib[navset_tab, nav_panel, nav_select, card, card_header],
  DT[datatable, DTOutput, renderDT],
  dplyr[filter, group_by, join_by, left_join, mutate, summarise, ungroup],
  dygraphs[dygraphOutput, renderDygraph],
  echarts4r[e_bar, e_charts, e_connect_group, e_data, e_datazoom,
            e_flip_coords, e_grid, e_group, e_heatmap,
            e_legend, e_line, e_mark_area, e_mark_line, e_visual_map,
            e_x_axis, e_y_axis, e_title, e_tooltip,
            echarts4rOutput, renderEcharts4r],
  htmlwidgets[JS, onRender],
  lubridate[ymd_hms],
  scales[rescale],
  shiny[actionButton, bindEvent, br, checkboxInput, column, fluidRow, hr,
        icon, isTruthy, moduleServer, NS, observe, radioButtons,
        reactive, reactiveVal, renderUI, req, tagList, tags, textInput, uiOutput,
        updateActionButton, p],
  shinycssloaders[withSpinner],
  shinyjs[hide, show],
  shinytoastr[toastr_info, toastr_success],
  stats[runif]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/logic/functions_devices,
  app/view/components/helpButton,
  app/view/components/visSeriesOptions
)

ui <- function(id) {

  ns <- NS(id)

  tagList(
    navset_tab(
      id = ns("tabs"),
      nav_panel(
        title = "Settings",
        icon = icon("cogs"),
        value = "settingstab",
        fluidRow(
          column(5,
                 textInput(ns("txt_plot_main_title"), "Title"),
                 tags$label(class = "control-label", "Annotations"),
                 checkboxInput(ns("check_add_calendar_annotation"),
                               label = "Calendar events",
                               value = TRUE),

                 uiOutput(ns("ui_plot_agg_data")),
                 uiOutput(ns("ui_plot_tags")),

                 tags$br(),
                 tags$hr(),
                 actionButton(ns("btn_make_plot"),
                              "Make plot",
                              icon = icon("check"),
                              class = "btn-success btn-lg"),

                 tags$br(),
                 tags$hr(),
                 helpButton$ui(ns("help"))

          ),
          column(7,
                 tags$div(id = ns("eda_options"),
                          tags$h4("EDA"),
                          visSeriesOptions$ui(ns("eda"), y_range = constants$app_config$visualisation$eda$yrange),
                          tags$hr()),

                 tags$div(id = ns("hr_options"),
                          tags$h4("HR"),
                          visSeriesOptions$ui(ns("hr"), y_range =constants$app_config$visualisation$hr$yrange),
                          tags$hr()),

                 tags$div(id = ns("temp_options"),
                          tags$h4("TEMP"),
                          visSeriesOptions$ui(ns("temp"), y_range = constants$app_config$visualisation$temp$yrange),
                          tags$hr()),

                 tags$div(id = ns("move_options"),
                          tags$h4("MOVE"),
                          uiOutput(ns("ui_move")))
          )
        )

      ),

      nav_panel(
        title = "Daily",
        icon = icon("chart-bar"),
        value = ns("plottab"),
        withSpinner(
          echarts4rOutput(ns("daily_graphs1"), height = "300px"),
        ),
        echarts4rOutput(ns("daily_graphs2"), height = "300px"),
        echarts4rOutput(ns("daily_graphs3"), height = "300px"),
        echarts4rOutput(ns("daily_graphs4"), height = "300px"),
        #dygraphOutput(ns("dygraph_current_data1"), height = "140px")
        # ),
        # dygraphOutput(ns("dygraph_current_data2"), height = "140px"),
        # dygraphOutput(ns("dygraph_current_data3"), height = "140px"),
        # dygraphOutput(ns("dygraph_current_data4"), height = "140px"),
        uiOutput(ns("dygraph_notes"))
      ),

      nav_panel(
        title = "Target Behaviour",
        icon = icon("chart-bar"),
        value = ns("plottab2"),
        fluidRow(
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget1")),
          ),
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget2"))
          ),
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget3"))
          )
        )
      ),

      nav_panel(
        title = "Annotations",
        icon = icon("list-ol"),
        value = ns("plotannotations"),
        tags$br(),
        tags$h5("Annotations (selected time window)"),
        DTOutput(ns("dt_annotations_visible"))
      )

    )
  )

}

server <- function(id, data = reactive(NULL), calendar = reactive(NULL),
                   device, r, problemtarget = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Reactive values -------------------------------
    plot_output <- reactiveVal()

    # Modules ---------------------------------------
    helpButton$server("help", helptext = constants$help_config$visualization)
    y_eda <- visSeriesOptions$server("eda")
    y_hr <- visSeriesOptions$server("hr")
    y_temp <- visSeriesOptions$server("temp")

    # Functionality ---------------------------------
    output$ui_move <- renderUI({
      ns <- session$ns
      type <- r$type
      r$load_move <- runif(1)

      if (r$type == "aggregated" && device == "embrace-plus") {
        y_range <- c(0, 0.7)
      } else {
        y_range <- as.numeric(constants$app_config$visualisation$move[[device]][[type]]$yrange)
      }

      visSeriesOptions$ui(ns("move"),
                          y_range = y_range)
    })

    observe({
      req(r$type)
      req(r$load_move)
      type <- r$type
      r$y_move <- visSeriesOptions$server("move",
                                          selected = "custom",
                                          custom_y = constants$app_config$visualisation$move[[device]][[type]]$custom_y)
    })

    observe({
      req(data()$data)

      if (!"HR" %in% names(data()$data)) {
        hide("hr_options")
      }

      if (!"ACC" %in% names(data()$data) &&
          !"MOVE" %in% names(data()$data) &&
          !"ACCELEROMETERS-STD" %in% names(data()$data) &&
          !"COUNT" %in% names(data()$data)) {
        hide("move_options")
      }

    })

    functions$hide_tab(session$ns("plottab"))
    functions$hide_tab(session$ns("plottab2"))
    functions$hide_tab(session$ns("plotannotations"))

    # Collect submodule output in a single reactive
    series_options <- reactive(
      list(
        EDA = y_eda(),
        HR = y_hr(),
        TEMP = y_temp(),
        MOVE = r$y_move()
      )
    )

    # Option to plot aggregated data (or not),
    # only visible if less than 2 hours of data, otherwise the plot will not be responsive.
    data_range_hours <- reactive({
      functions$data_datetime_range(data()$data)
    })

    output$ui_plot_agg_data <- renderUI({

      if(data_range_hours() < 2){
        tagList(
          tags$hr(),
          radioButtons(session$ns("rad_plot_agg"), "Aggregate data",
                       choices = c("Yes","No"), inline = TRUE)
        )

      } else {
        NULL
      }

    })


    # Option to plot tags onto plot.
    # Only visible if there was a tags.csv file in the uploaded ZIP file.
    have_tag_data <- reactive({
      !is.null(data()$data$tags)
    })

    output$ui_plot_tags <- renderUI({
      req(have_tag_data())

      tagList(
        tags$hr(),
        radioButtons(session$ns("rad_plot_tags"), "Add tags to plot",
                     choices = c("Yes","No"), inline = TRUE)
      )

    })


    observe({

      data <- data()

      req(data)

      toastr_info("Plot construction started...")

      # Precalc. timeseries (for viz.)
      if(is.null(input$rad_plot_agg)){
        agg <- "Yes"
      } else {
        agg <- input$rad_plot_agg
      }

      if (agg == "Yes") {
        data <- data$data_agg
      } else {
        data <- data$data
      }

      functions$show_tab(session$ns("plottab"))

      if (isTruthy(calendar())) {
        functions$show_tab(session$ns("plotannotations"))
      }

      nav_select(id = "tabs",
                 selected = session$ns("plottab"))

      if(input$check_add_calendar_annotation){
        annotatedata <- calendar()
      } else {
        annotatedata <- NULL
      }

      if ("ACC" %in% names(data)){
        data$MOVE <- data$ACC
        data$MOVE$MOVE <- data$MOVE$a
      } else if ("ACCELEROMETERS-STD" %in% names(data)){
        data$MOVE <- data$`ACCELEROMETERS-STD`
        data$MOVE$MOVE <- data$MOVE$accelerometers_std_g
      } else if ("MOVE" %in% names(data)){
        if (r$type == "aggregated") {
          # For embraceplus data this is accelerometer_std_g
          data$MOVE$MOVE <- data$MOVE$accelerometers_std_g
        } else if (r$type == "raw") {
          data$MOVE$MOVE <- data$MOVE$a
        }
      } else if ("COUNT" %in% names(data)) {
        data$MOVE <- data$COUNT
        data$MOVE$MOVE <- data$MOVE$COUNT
      } else {
        if ("EDA" %in% names(data)) {
          data$MOVE <- data.frame(data$EDA$DateTime, MOVE = NA)
        } else if ("ACT" %in% names(data)) {
          data$MOVE <- data.frame(data$ACT$DateTime, MOVE = NA)
        }
      }

      yearMonthDate <- JS('function (value) {
        var d = new Date(value);
        var datestring = d.getFullYear() + "-" + ("0"+(d.getMonth()+1)).slice(-2) + "-" + ("0" + d.getDate()).slice(-2)
        return datestring
      }')

      output$daily_graphs1 <- renderEcharts4r({

        req(data$EDA)

        if (series_options()$EDA$line_type == "mean") {
          line_val <- mean(data$EDA$EDA, na.rm = TRUE)
        } else {
          line_val <- series_options()$EDA$custom_y_val
        }

        chart <- data$EDA |>
          e_charts(DateTime) |>
          e_line(EDA,
                 name = "EDA",
                 symbol = "none",
                 lineStyle = list(
                   width = 1,
                   color = constants$app_config$visualisation$eda$color
                 )) |>
          e_title(input$txt_plot_main_title,
                  left = "40%") |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "EDA",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = constants$app_config$visualisation$eda$yrange[1],
            max = constants$app_config$visualisation$eda$yrange[2]
          ) |>
          e_datazoom(show = FALSE) |>
          e_tooltip(trigger = "axis") |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_grid(
            top = 60,
            bottom = 20
          ) |>
          e_mark_line(data = list(yAxis = line_val), title = line_val)

        chart <- functions_devices$create_echarts4r_events(chart,
                                                           annotatedata,
                                                           yrange = constants$app_config$visualisation$eda$yrange)

        chart

      })

      output$daily_graphs2 <- renderEcharts4r({

        if (series_options()$HR$line_type == "mean") {
          line_val <- mean(data$HR$HR, na.rm = TRUE)
        } else {
          line_val <- series_options()$HR$custom_y_val
        }

        chart <- data$HR |>
          e_charts(DateTime) |>
          e_line(HR,
                 name = "HR",
                 symbol = "none",
                 lineStyle = list(
                   width = 1,
                   color = constants$app_config$visualisation$hr$color
                 )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "HR",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = constants$app_config$visualisation$hr$yrange[1],
            max = constants$app_config$visualisation$hr$yrange[2]
          ) |>
          e_datazoom(show = FALSE) |>
          e_tooltip(trigger = "axis") |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_grid(
            top = 10,
            bottom = 20
          ) |>
          e_mark_line(data = list(yAxis = line_val), title = line_val)

        chart <- functions_devices$create_echarts4r_events(chart,
                                                           annotatedata,
                                                           yrange = constants$app_config$visualisation$hr$yrange)

        chart

      })

      output$daily_graphs3 <- renderEcharts4r({

        if (series_options()$TEMP$line_type == "mean") {
          line_val <- mean(data$TEMP$TEMP, na.rm = TRUE)
        } else {
          line_val <- series_options()$TEMP$custom_y_val
        }

        chart <- data$TEMP |>
          e_charts(DateTime) |>
          e_line(TEMP,
                 name = "Temperature",
                 symbol = "none",
                 lineStyle = list(
                   width = 1,
                   color = constants$app_config$visualisation$temp$color
                 )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "Temperature",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = constants$app_config$visualisation$temp$yrange[1],
            max = constants$app_config$visualisation$temp$yrange[2]
          ) |>
          e_datazoom(show = FALSE) |>
          e_tooltip(trigger = "axis") |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_grid(
            top = 10,
            bottom = 20
          ) |>
          e_mark_line(data = list(yAxis = line_val), title = line_val)

        chart <- functions_devices$create_echarts4r_events(chart,
                                                           annotatedata,
                                                           yrange = constants$app_config$visualisation$temp$yrange)

        chart

      })

      output$daily_graphs4 <- renderEcharts4r({

        if (series_options()$MOVE$line_type == "mean") {
          line_val <- mean(data$MOVE$MOVE, na.rm = TRUE)
        } else {
          line_val <- series_options()$MOVE$custom_y_val
        }

        chart <- data$MOVE |>
          e_charts(DateTime) |>
          e_line(MOVE,
                 name = "MOVE",
                 symbol = "none",
                 lineStyle = list(
                   width = 1,
                   color = constants$app_config$visualisation$move[[device]][[r$type]]$color
                 )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "Movement",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = constants$app_config$visualisation$move[[device]][[r$type]]$yrange[1],
            max = constants$app_config$visualisation$move[[device]][[r$type]]$yrange[2]
          ) |>
          e_datazoom(type = "slider") |>
          e_tooltip(trigger = "axis") |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_connect_group("daily") |>
          e_grid(
            top = 10,
            bottom = 60
          ) |>
          e_mark_line(data = list(yAxis = line_val), title = line_val)

        chart <- functions_devices$create_echarts4r_events(chart,
                                                           annotatedata,
                                                           yrange = constants$app_config$visualisation$move[[device]][[r$type]]$yrange)

        chart <- chart |>
          onRender(
            sprintf("function(el, x) {
               var chart = this.getChart();
               chart.on('datazoom', function(e) {
                 var xbounds = chart.getModel().getComponent('xAxis', 0).axis.scale.getExtent();
                 // convert xbounds to date/time
                 xbounds = xbounds.map(function(x) {
                   return new Date(x);
                 });
                 Shiny.setInputValue('%s', xbounds);
               });
            }", session$ns("datazoom_bounds"))
          )

        chart

      })

      toastr_success("Plot constructed, click on the 'Plot' tab!")
      updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("sync"))

      if(r$type == "raw"){
        functions$enable_link(menu = device,
                              name = "Analysis")
      }

    }) |> bindEvent(input$btn_make_plot)

    observe({

      data <- data()

      req(data)
      req(problemtarget())

      functions$show_tab(session$ns("plottab2"))

      # Group move data by 1 hour
      df <- data$MOVE |>
        mutate(active = ifelse(!is.na(activity_counts) & activity_counts > 0, 1, 0)) |>
        group_by(DateTime = lubridate::floor_date(DateTime, "1 hour")) |>
        summarise(
          activity_level = sum(activity_counts, na.rm = TRUE),
          activity_time = sum(active, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(hour = as.numeric(format(DateTime, "%H")),
               hour = ifelse(hour < 12, paste0(hour, "am"), paste0(hour, "pm")),
               date = as.Date(DateTime)) |>
        # scale activity level as number between 0 and 10
        mutate(activity_level = round(scales::rescale(activity_level, to = c(0, 10)))) |>
        # merge problemtarget() data based on Date
        left_join(problemtarget(), by = join_by(date == Date))

      # Calculate weekly average of activity time
      week_data <- df |>
        group_by(date) |>
        mutate(activity_time = sum(activity_time, na.rm = TRUE)) |>
        ungroup() |>
        mutate(week = format(DateTime, "%W")) |>
        group_by(week) |>
        summarise(weekly_activity_time = mean(activity_time),
                  date = max(date))

      yearMonthDate <- htmlwidgets::JS('function (value) {
        var d = new Date(value);
        var datestring = d.getFullYear() + "-" + ("0"+(d.getMonth()+1)).slice(-2) + "-" + ("0" + d.getDate()).slice(-2)
        return datestring
      }')

      output$echarts_problemtarget1 <- renderEcharts4r({
        df |>
          e_charts(hour) |>
          e_heatmap(date, activity_level, label = list(show = TRUE)) |>
          e_y_axis(name = "Date") |>
          e_title("Activity Level") |>
          e_visual_map(activity_level,
                       orient = "horizontal") |>
          e_grid(left = 70)
      })

      output$echarts_problemtarget2 <- renderEcharts4r({
        df |>
          group_by(date) |>
          summarise(activity_time = sum(activity_time, na.rm = TRUE)) |>
          e_charts(date) |>
          e_bar(activity_time) |>
          e_data(week_data) |>
          e_line(weekly_activity_time) |>
          e_y_axis(name = "Minutes") |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = yearMonthDate
            )
          ) |>
          e_title("Activity Time") |>
          e_flip_coords() |>
          e_grid(left = 70)
      })

      output$echarts_problemtarget3 <- renderEcharts4r({

        title <- df$`Problem or Target Behavior` |> unique()

        df |>
          group_by(date) |>
          summarise(score = mean(Score, na.rm = TRUE)) |>
          e_charts(date) |>
          e_line(score) |>
          e_y_axis(name = "Score") |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = yearMonthDate
            )
          ) |>
          e_title(paste(title, "Score")) |>
          e_flip_coords() |>
          e_grid(left = 70)
      })

      updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("sync"))

    }) |> bindEvent(input$btn_make_plot)


    current_visible_annotations <- reactive({

      if (!is.null(input$datazoom_bounds)) {
        print(input$datazoom_bounds)
        # !!!! WATCH THE TIMEZONE!
        # Problem: cannot read timezone info from rv$calendar$Start, should
        # be saved there (as tzone attribute) when reading calendar
        ran <- suppressWarnings({
          ymd_hms(input$datazoom_bounds, tz = "CET")
        })

        calendar() |>
          filter(
            (Start <= ran[[2]] & End <= ran[[2]]) &
              (Start >= ran[[1]] & End >= ran[[1]])
          )
      } else {
        calendar()
      }

    })

    output$dygraph_notes <- renderUI({
      if (device == "embrace-plus"&& r$type == "raw") {
        p("Note: the Embrace Plus device does not have HR per second
          available when data is in raw format. The aggregated HR is available
          from the digital biomarkers and the BVP signal is available from the raw data.")
      } else if (device == "embrace-plus" && r$type == "aggregated") {
        p("Note: the aggregated data of the Embrace Plus device uses
          the standard deviation of the accelerometer readings in terms of gravitational force (g)
          as a proxy for movement. This differs from the
          raw data, that uses the geometric mean acceleration.")
      }
    })


    observe({

      show("thispanel")

    }) |> bindEvent(input$btn_panel_float)

    output$dt_panel_annotations <- renderDT({

      current_visible_annotations() |>
        mutate(Date = format(Date, "%Y-%m-%d"),
               Start = format(Start, "%H:%M:%S"),
               End = format(End, "%H:%M:%S")
        ) |>
        datatable(width = 500)

    })

    output$dt_annotations_visible <- renderDT({

      current_visible_annotations() |>
        mutate(Date = format(Date, "%Y-%m-%d"),
               Start = format(Start, "%H:%M:%S"),
               End = format(End, "%H:%M:%S")
        ) |>
        datatable(width = 500)

    })

    return(plot_output)

  })
}
