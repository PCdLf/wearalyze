
box::use(
  bslib[navset_tab, nav_panel, nav_select, card, card_header],
  DT[datatable, DTOutput, renderDT],
  dplyr[arrange, filter, group_by, join_by, left_join, mutate, select, summarise,
        ungroup],
  dygraphs[dygraphOutput, renderDygraph],
  echarts4r[e_bar, e_charts, e_connect_group, e_data, e_datazoom,
            e_flip_coords, e_grid, e_group,
            e_legend, e_line, e_mark_area, e_mark_line,
            e_x_axis, e_y_axis, e_title, e_tooltip,
            echarts4rOutput, renderEcharts4r],
  glue[glue],
  htmltools[htmlEscape],
  htmlwidgets[JS, onRender],
  lubridate[ymd_hms],
  shiny[actionButton, bindEvent, br, checkboxInput, column, dateRangeInput, div,
        fluidRow, hr,
        icon, isTruthy, moduleServer, NS, observe, radioButtons,
        reactive, reactiveVal, renderUI, req, tagList, tags, textInput, uiOutput,
        updateActionButton, p, tagAppendAttributes],
  shinycssloaders[withSpinner],
  shinyjs[hide, show],
  shinytoastr[toastr_info, toastr_success, toastr_warning],
  shinyWidgets[pickerInput, updatePickerInput],
  stats[runif],
  tidyr[complete]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/logic/functions_devices,
  app/logic/stress_algorithm/predict_stress,
  app/view/components/helpButton,
  app/view/components/visSeriesOptions
)

ui <- function(id) {

  ns <- NS(id)

  tagList(
    navset_tab(
      id = ns("tabs"),
      # Settings -------------------------------------
      nav_panel(
        title = "Settings",
        icon = icon("cogs"),
        value = "settingstab",
        fluidRow(
          column(5,
                 textInput(ns("txt_plot_main_title"), "Title"),
                 functions$side_by_side(
                   tags$div(
                     tags$label(class = "control-label", "Annotations"),
                     checkboxInput(
                       ns("check_add_calendar_annotation"),
                       label = "Calendar events",
                       value = TRUE
                     )
                   ),
                   uiOutput(ns("ui_calendar_event_labels"))
                 ),

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
                          uiOutput(ns("ui_move")),
                          tags$hr()),

                 tags$div(id = ns("stress_options"),
                          tags$h4("STRESS ALGORITHM"),
                          checkboxInput(
                            ns("incl_stress_algorithm"),
                            "Include stress algorithm",
                            value = FALSE
                          )
                 )
          )
        )

      ),

      # Daily graphs ---------------------------------
      nav_panel(
        title = "Overview",
        icon = icon("chart-bar"),
        value = ns("plottab"),
        # The echarts grid is inset by 10% on both sides (the echarts default,
        # e_grid() for this plot only sets top and bottom), so inset everything
        # around the plot by the same amount to line it up with the plot.
        div(
          style = "padding: 15px 10% 0 10%;",
          uiOutput(ns("ui_overview_date_range")),
          tags$h5("Predicted stress level", style = "margin-bottom: 0;")
        ),
        withSpinner(
          echarts4rOutput(ns("predicted_stress_level_plot"), height = "400px")
        ),
        div(
          style = "padding-left: 10%; padding-right: 10%;",
          uiOutput(ns("predicted_stress_level_notes")),
          uiOutput(ns("ui_calendar_overview"))
        )
      ),

      # Parameters -----------------------------------
      nav_panel(
        title = "Parameters",
        icon = icon("wave-square"),
        value = ns("parameters_tab"),
        fluidRow(
          column(
            width = 3,
            offset = 1,
            pickerInput(
              ns("date_picker"),
              label = "Select day",
              choices = "All",
              selected = "All",
              width = "100%"
            )
          ),
          column(
            width = 1,
            tagAppendAttributes(
              style = "margin-top:30px",
              actionButton(
                ns("btn_update_dates"),
                "",
                icon = icon("sync")
              )
            )
          )
        ),
        # Either the stress algorithm spinner is shown, or the daily graphs 1 spinner
        withSpinner(
          id = ns("stress_algorithm_plot_spinner"),
          echarts4rOutput(ns("stress_algorithm_plot"), height = "220px")
        ),
        withSpinner(
          id = ns("daily_graphs1_spinner"),
          echarts4rOutput(ns("daily_graphs1"), height = "220px")
        ),
        # Add spinner, but don't show, this gives a better loading UX
        withSpinner(
          type = 0,
          echarts4rOutput(ns("daily_graphs2"), height = "220px")
        ),
        withSpinner(
          type = 0,
          echarts4rOutput(ns("daily_graphs3"), height = "220px")
        ),
        withSpinner(
          type = 0,
          echarts4rOutput(ns("daily_graphs4"), height = "220px")
        ),
        uiOutput(ns("echarts_notes"))
      ),

      # Problem target behaviour --------------------
      nav_panel(
        title = "Target Behaviour",
        icon = icon("chart-bar"),
        value = ns("plottab2"),
        uiOutput(ns("problemtarget_plots"))
      ),

      # Annotations ---------------------------------
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

    ns <- session$ns

    # Reactive values -------------------------------
    dailygraphs1 <- reactiveVal()
    dailygraphs2 <- reactiveVal()
    dailygraphs3 <- reactiveVal()
    dailygraphs4 <- reactiveVal()

    # Modules ---------------------------------------
    helpButton$server("help", helptext = constants$help_config$visualization)
    y_eda <- visSeriesOptions$server("eda")
    y_hr <- visSeriesOptions$server("hr")
    y_temp <- visSeriesOptions$server("temp")

    # Functionality ---------------------------------
    ## Init -----------------------------------------
    functions$hide_tab(ns("plottab"))
    functions$hide_tab(ns("parameters_tab"))
    functions$hide_tab(ns("plottab2"))
    functions$hide_tab(ns("plotannotations"))

    ## Collect series options -----------------------
    series_options <- reactive(
      list(
        EDA = y_eda(),
        HR = y_hr(),
        TEMP = y_temp(),
        MOVE = r$y_move()
      )
    )

    ## Date picker ----------------------------------
    observe({
      if (r$more_than_24h == TRUE) {
        show("date_picker")
        show("btn_update_dates")
        updatePickerInput(session,
                          "date_picker",
                          choices = c("All", as.character(unique(as.Date(data()$data[[1]]$DateTime)))))
      } else {
        hide("date_picker")
        hide("btn_update_dates")
        r$chosen_dates <- "All"
      }

    })

    observe({
      req(input$date_picker)
      r$chosen_dates <- input$date_picker
    })

    ## Overview date filter -------------------------
    # Start and end date above the overview plot. Filters both the plot and the
    # calendar table below it. Rendered from the data, so the bounds and the
    # initial selection always cover the full measurement period.
    output$ui_overview_date_range <- renderUI({
      req(data()$data)

      datetimes <- data()$data[[1]]$DateTime
      req(datetimes)

      dates <- as.Date(range(datetimes, na.rm = TRUE), tz = Sys.timezone())

      dateRangeInput(
        ns("overview_date_range"),
        label = "Filter on date",
        start = dates[1],
        end = dates[2],
        min = dates[1],
        max = dates[2],
        format = "yyyy-mm-dd",
        separator = " to "
      )
    })

    overview_date_range <- reactive({
      selected <- input$overview_date_range

      # No selection (yet), or a half filled in range: don't filter.
      if (!isTruthy(selected) || any(is.na(selected))) {
        return(NULL)
      }

      as.Date(selected)
    })

    ## Settings -------------------------------------
    output$ui_move <- renderUI({
      r$load_move <- runif(1)

      if (r$type == "aggregated" && device == "embrace-plus") {
        y_range <- c(0, 0.7)
      } else {
        y_range <- as.numeric(constants$app_config$visualisation$move[[device]][[r$type]]$yrange)
      }

      visSeriesOptions$ui(ns("move"),
                          y_range = y_range)
    })

    observe({
      req(r$type)
      req(r$load_move)
      r$y_move <- visSeriesOptions$server("move",
                                          selected = "custom",
                                          custom_y = constants$app_config$visualisation$move[[device]][[r$type]]$custom_y)
    })

    observe({
      req(data()$data)

      if (!"HR" %in% names(data()$data)) {
        hide("hr_options")
      }

      # Different datasets have different names for movement data
      # if no match was found, hide move options.
      if (!"ACC" %in% names(data()$data) &&
          !"MOVE" %in% names(data()$data) &&
          !"ACCELEROMETERS-STD" %in% names(data()$data) &&
          !"COUNT" %in% names(data()$data)) {
        hide("move_options")
      }

    })

    # Show label setting only when calendar data is displayed.
    output$ui_calendar_event_labels <- renderUI({
      if (input$check_add_calendar_annotation) {
        tagList(
          radioButtons(
            ns("show_calendar_event_labels"),
            label = "Show calendar event labels",
            choices = c("Yes" = TRUE, "No" = FALSE),
            inline = TRUE,
            selected = TRUE
          )
        )
      } else {
        NULL
      }
    })

    output$ui_plot_agg_data <- renderUI({

      if(r$more_than_24h){

        # Aggregating by 5 minutes in case of large data will speed
        # up the rendering of the plot significantly
        if(r$more_than_2weeks) {
          label <- "Aggregate data by 5 minutes"
        } else {
          label <- "Aggregate data by 1 minute"
        }

        tagList(
          tags$hr(),
          radioButtons(ns("rad_plot_agg"),
                       label = label,
                       choices = c("Yes","No"),
                       inline = TRUE,
                       selected = "Yes")
        )

      } else {
        NULL
      }

    })


    # Plotting -------------------------------------
    ## Daily graphs --------------------------------
    observe({

      data <- data()

      req(data$data)

      toastr_info("Plot construction started...")

      # Use aggregated data if needed
      if (is.null(input$rad_plot_agg) || input$rad_plot_agg == "Yes") {
        data <- data$data_agg
      } else {
        data <- data$data
      }

      functions$show_tab(ns("plottab"))
      functions$show_tab(ns("parameters_tab"))

      if (isTruthy(calendar())) {
        functions$show_tab(ns("plotannotations"))
      }

      nav_select(
        id = "tabs",
        selected = ns("plottab")
      )

      if(input$check_add_calendar_annotation){
        annotatedata <- calendar()
      } else {
        annotatedata <- NULL
      }

      # subset all data elements to chosen dates
      if (r$chosen_dates != "All") {
        data <- lapply(data, function(x) {
          # skip when no DateTime column found
          if (!"DateTime" %in% names(x)) {
            return(x)
          }
          x$date <- as.Date(x$DateTime, tz = Sys.timezone())
          x <- x[x$date %in% as.Date(r$chosen_dates),]
          x$date <- NULL
          x
        })
      }

      # if less than 24 hours of data, or viewing one particular day, don't complete data
      if(r$more_than_24h & r$chosen_dates == "All"){
        data <- lapply(data, function(x) {
          # skip when no DateTime column found
          if (!"DateTime" %in% names(x)) {
            return(x)
          }
          x <- x |>
            complete(DateTime = seq.Date(as.Date(min(x$DateTime)), as.Date(max(x$DateTime)), by = "1 day")) |>
            arrange(DateTime)
          x
        })
      }

      if(!"EDA" %in% names(data)){
        if ("SKIN_CONDUCTANCE" %in% names(data)){
          data$EDA <- data$SKIN_CONDUCTANCE
          data$EDA$EDA <- data$EDA$SKIN_CONDUCTANCE
        }
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

      # create empty list for plots
      plot_list <- list()

      ## Stress predictions --------------------------
      stress_predictions <- reactive({

        tryCatch({

          toastr_info("Applying stress algorithm 🚀")

          predicted_data <- predict_stress$return_predictions(
            data,
            types = c("TEMP", "MOVE", "EDA", "HR")
          )

          predictions <- predict_stress$combine_predictions(predicted_data)

          if (is.null(predictions)) {
            available_data <- paste(names(data), collapse = ", ")

            message(glue(
              "stress_predictions: no predictions for {device} ({r$type}), ",
              "available data: [{available_data}]."
            ))
            toastr_warning("Could not compute the predicted stress level")

            NULL
          } else {
            toastr_success("Got predictions!")
            toastr_info("Rendering graphs...")

            predictions
          }

        }, error = function(e) {
          message(glue(
            "stress_predictions: failed for {device} ({r$type}): {conditionMessage(e)}"
          ))
          toastr_warning("Could not compute the predicted stress level")

          NULL
        })

      })

      # One line per parameter (TEMP, MOVE, EDA, HR) with the stress level that
      # the model predicts for that parameter on its own.
      output$stress_algorithm_plot <- renderEcharts4r({
        # Guard before stress_predictions(), so the models are not run when the
        # stress algorithm is switched off.
        req(input$incl_stress_algorithm)

        predictions <- stress_predictions()
        req(predictions)

        chart <- predictions |>
          e_charts(DateTime) |>
          e_line(
            TEMP,
            name = "TEMP",
            symbolSize = "0.01",
            color = constants$app_config$visualisation$temp$color,
            lineStyle = list(
              width = 1
            )
          ) |>
          e_line(
            MOVE,
            name = "MOVE",
            symbolSize = "0.01",
            color = constants$app_config$visualisation$move[[device]][[r$type]]$color,
            lineStyle = list(
              width = 1
            )
          ) |>
          e_line(
            EDA,
            name = "EDA",
            symbolSize = "0.01",
            color = constants$app_config$visualisation$eda$color,
            lineStyle = list(
              width = 1
            )
          ) |>
          e_line(
            HR,
            name = "HR",
            symbolSize = "0.01",
            color = constants$app_config$visualisation$hr$color,
            lineStyle = list(
              width = 1
            )
          ) |>
          e_title(
            input$txt_plot_main_title,
            left = "50%",
            top = 0
          ) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "Predicted Stress Level",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = 0,
            max = 10
          ) |>
          e_datazoom(show = FALSE) |>
          e_tooltip(trigger = "item", extraCssText = constants$tooltip_css) |>
          e_legend(
            show = TRUE,
            top = 30
          ) |>
          e_group("daily") |>
          e_grid(
            top = 60,
            bottom = 20
          )

        functions_devices$create_echarts4r_events(
          chart,
          annotatedata,
          yrange = c(0, 10),
          label = input$show_calendar_event_labels
        )

      })

      ## Predicted stress level ----------------------
      # The predictions the overview is based on: only the selected date range,
      # so the plot and the note underneath it describe the same measurements.
      overview_stress_predictions <- reactive({
        predictions <- stress_predictions()
        req(predictions)

        # Filter on start and end date.
        functions$filter_dates(predictions, overview_date_range())
      })

      # One line with the overall stress level: the weighted average over the
      # per parameter predictions of the stress algorithm.
      output$predicted_stress_level_plot <- renderEcharts4r({
        plot_data <- overview_stress_predictions()
        req(nrow(plot_data) > 0)

        # Only annotate the events within the selected range, events outside of
        # it would stretch the x axis beyond the filtered data.
        overview_annotations <- functions$filter_dates(annotatedata, overview_date_range(), "Start")
        if (!is.null(overview_annotations) && nrow(overview_annotations) == 0) {
          overview_annotations <- NULL
        }

        plot_data$predicted_stress <- predict_stress$weighted_stress_score(plot_data)

        yrange <- as.numeric(constants$app_config$visualisation$predicted_stress$yrange)

        # Only keep room above the plotting area for the title when a title was
        # filled in, otherwise it is a gap between the heading and the plot.
        grid_top <- if (isTruthy(input$txt_plot_main_title)) 40 else 10

        chart <- plot_data |>
          e_charts(DateTime) |>
          e_line(
            predicted_stress,
            name = "Predicted stress level",
            symbolSize = "0.01",
            color = constants$app_config$visualisation$predicted_stress$color,
            lineStyle = list(
              width = 1
            )
          ) |>
          e_title(
            input$txt_plot_main_title,
            left = "50%",
            top = 0
          ) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "Predicted stress level",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = yrange[1],
            max = yrange[2]
          ) |>
          e_tooltip(trigger = "item", extraCssText = constants$tooltip_css) |>
          e_legend(show = FALSE) |>
          e_grid(
            top = grid_top,
            bottom = 30
          )

        # No labels or arrow heads on the events here: the table below the plot
        # already names every event, and the tooltip gives the full text.
        chart <- functions_devices$create_echarts4r_events(
          chart,
          overview_annotations,
          yrange = yrange,
          label = FALSE,
          arrow = FALSE,
          color_lines = TRUE
        )

        # Highlight the calendar event that is hovered on in the table below the
        # plot.
        chart |>
          onRender(
            sprintf("function(el, x) {
              var chart = this.getChart();
              var tableId = '%s';
              var highlighting = false;

              // Only highlight while the mouse is on the event itself. The few
              // pixels of slack are there because an event without an end time
              // is drawn as a line rather than as an area, and a line of a
              // couple of pixels wide is otherwise impossible to point at.
              var reach = 3;

              function rows() {
                var table = document.getElementById(tableId);
                return table ? table.querySelectorAll('tbody tr') : [];
              }

              function clear() {
                if (!highlighting) return;
                highlighting = false;
                rows().forEach(function(row) {
                  row.classList.remove('activity-hovered');
                });
              }

              // Distance in pixels between the mouse and the band the event is
              // drawn as, 0 when the mouse is on the event itself. Events
              // without an end time are a single moment in the plot.
              function distance(row, atX) {
                var start = parseFloat(row.getAttribute('data-start'));
                var end = parseFloat(row.getAttribute('data-end'));
                if (isNaN(start)) return Infinity;
                if (isNaN(end)) end = start;

                var from = chart.convertToPixel({xAxisIndex: 0}, start);
                var to = chart.convertToPixel({xAxisIndex: 0}, end);

                return Math.max(from - atX, atX - to, 0);
              }

              chart.getZr().on('mousemove', function(e) {
                if (!chart.containPixel('grid', [e.offsetX, e.offsetY])) {
                  clear();
                  return;
                }

                var all = rows();
                var distances = [];
                var nearest = Infinity;

                all.forEach(function(row) {
                  var d = distance(row, e.offsetX);
                  distances.push(d);
                  nearest = Math.min(nearest, d);
                });

                if (nearest > reach) {
                  clear();
                  return;
                }

                // Events that overlap are equally near, highlight them together.
                highlighting = true;
                all.forEach(function(row, i) {
                  row.classList.toggle('activity-hovered', distances[i] === nearest);
                });
              });

              chart.getZr().on('globalout', clear);
            }", ns("dt_calendar_overview"))
          )

      })

      # Which parameters actually contributed. With the weights renormalised
      # per row, a device that misses a signal still gets a score, so name the
      # parameters it is based on. Judged on the same filtered predictions as
      # the plot: a parameter that is only measured outside the selected date
      # range does not contribute to the line that is shown.
      output$predicted_stress_level_notes <- renderUI({
        predictions <- overview_stress_predictions()

        weights <- predict_stress$stress_weights

        used <- names(weights)[vapply(names(weights), function(type) {
          any(!is.na(predictions[[type]]))
        }, logical(1))]

        if (length(used) == 0) {
          return(p("No parameters available to compute a predicted stress level."))
        }

        p(paste0("Weighted average of ",
                 paste(paste0(used, " (", weights[used], ")"), collapse = ", "),
                 ". Each parameter is predicted on a 1-7 scale."))
      })

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
                 symbolSize = "0.01",
                 color = constants$app_config$visualisation$eda$color,
                 lineStyle = list(
                   width = 1
                 )) |>
          # If there is a stress algorith plot, don't show title here
          e_title(ifelse(input$incl_stress_algorithm, "", input$txt_plot_main_title),
                  left = "50%") |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "EDA",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = series_options()$EDA$yaxis_range[1],
            max = series_options()$EDA$yaxis_range[2]
          ) |>
          e_datazoom(show = FALSE) |>
          e_tooltip(trigger = "item", extraCssText = constants$tooltip_css) |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_connect_group("daily") |>
          e_grid(
            top = 60,
            bottom = 20
          ) |>
          e_mark_line(data = list(yAxis = line_val),
                      title = line_val,
                      tooltip = list(formatter = "")
          )

        chart <- functions_devices$create_echarts4r_events(
          chart,
          annotatedata,
          yrange = series_options()$EDA$yaxis_range,
          label = input$show_calendar_event_labels
        )

        dailygraphs1(chart)

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
                 symbolSize = "0.01",
                 color = constants$app_config$visualisation$hr$color,
                 lineStyle = list(
                   width = 1
                 )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "HR",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = series_options()$HR$yaxis_range[1],
            max = series_options()$HR$yaxis_range[2]
          ) |>
          e_datazoom(show = FALSE) |>
          e_tooltip(trigger = "item", extraCssText = constants$tooltip_css) |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_connect_group("daily") |>
          e_grid(
            top = 10,
            bottom = 20
          ) |>
          e_mark_line(data = list(yAxis = line_val), title = line_val)

        chart <- functions_devices$create_echarts4r_events(
          chart,
          annotatedata,
          yrange = series_options()$HR$yaxis_range,
          label = input$show_calendar_event_labels
        )

        dailygraphs2(chart)

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
                 symbolSize = "0.01",
                 color = constants$app_config$visualisation$temp$color,
                 lineStyle = list(
                   width = 1
                 )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "Temperature",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = series_options()$TEMP$yaxis_range[1],
            max = series_options()$TEMP$yaxis_range[2]
          ) |>
          e_datazoom(show = FALSE) |>
          e_tooltip(trigger = "item", extraCssText = constants$tooltip_css) |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_connect_group("daily") |>
          e_grid(
            top = 10,
            bottom = 20
          ) |>
          e_mark_line(data = list(yAxis = line_val), title = line_val)

        chart <- functions_devices$create_echarts4r_events(
          chart,
          annotatedata,
          yrange = series_options()$TEMP$yaxis_range,
          label = input$show_calendar_event_labels
        )

        dailygraphs3(chart)

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
                 symbolSize = "0.01",
                 color = constants$app_config$visualisation$move[[device]][[r$type]]$color,
                 lineStyle = list(
                   width = 1
                 )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_y_axis(
            name = "Movement",
            nameLocation = "center",
            nameRotate = 90,
            nameGap = 30,
            min = series_options()$MOVE$yaxis_range[1],
            max = series_options()$MOVE$yaxis_range[2]
          ) |>
          e_datazoom(type = "slider") |>
          e_tooltip(trigger = "item", extraCssText = constants$tooltip_css) |>
          e_legend(show = FALSE) |>
          e_group("daily") |>
          e_connect_group("daily") |>
          e_grid(
            top = 10,
            bottom = 60
          ) |>
          e_mark_line(data = list(yAxis = line_val), title = line_val)

        chart <- functions_devices$create_echarts4r_events(
          chart,
          annotatedata,
          yrange = series_options()$MOVE$yaxis_range,
          label = input$show_calendar_event_labels
        )

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
            }", ns("datazoom_bounds"))
          )

        dailygraphs4(chart)

        chart

      })

      updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("sync"))

      if(r$type == "raw" && device == "e4"){
        functions$enable_link(menu = device,
                              name = "Analysis")
      }

    }) |> bindEvent(c(input$btn_make_plot, input$btn_update_dates))

    observe({

      if (input$incl_stress_algorithm) {
        show("stress_algorithm_plot")
        show("stress_algorithm_plot_spinner")
        hide("daily_graphs1_spinner")
      } else {
        hide("stress_algorithm_plot")
        hide("stress_algorithm_plot_spinner")
        show("daily_graphs1_spinner")
      }

    }) |> bindEvent(input$incl_stress_algorithm)

    ## Problem target behaviour ---------------------
    output$problemtarget_plots <- renderUI({

      data <- data()

      req(data)
      req(problemtarget())

      if ("STRESS" %in% names(data$data) && "SLEEP" %in% names(data$data)) {
        tagList(
          fluidRow(
            column(6,
                   echarts4rOutput(ns("echarts_problemtarget_act_time"))
            ),
            column(6,
                   echarts4rOutput(ns("echarts_problemtarget_stress"))
            )
          ),
          fluidRow(
            column(6,
                   echarts4rOutput(ns("echarts_problemtarget_sleep")),
            ),
            column(6,
                   echarts4rOutput(ns("echarts_problemtarget_behaviour"))
            )
          )
        )
      } else if ("STRESS" %in% names(data$data)) {
        fluidRow(
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget_act_time"))
          ),
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget_stress"))
          ),
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget_behaviour"))
          )
        )
      } else if ("SLEEP" %in% names(data$data)) {
        fluidRow(
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget_act_time"))
          ),
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget_sleep"))
          ),
          column(4,
                 echarts4rOutput(ns("echarts_problemtarget_behaviour"))
          )
        )
      } else {
        fluidRow(
          column(6,
                 echarts4rOutput(ns("echarts_problemtarget_act_time"))
          ),
          column(6,
                 echarts4rOutput(ns("echarts_problemtarget_behaviour"))
          )
        )
      }
    })

    observe({

      req(data())
      req(problemtarget())

      # Use non-aggregated data for target behavior visuals.
      data <- data()$data

      functions$show_tab(ns("plottab2"))

      # Different devices have different data that can be used
      # for an indication of movement
      if ("ACTIVITY-COUNTS" %in% names(data)) {
        data$MOVE <- data$`ACTIVITY-COUNTS`
      } else if ("COUNT" %in% names(data)) {
        data$MOVE <- data$COUNT
        data$MOVE$activity_counts <- data$MOVE$COUNT
      } else {
        data$MOVE <- data$MOVE
      }

      # Group move data by 1 hour
      df_activity <- data$MOVE |>
        mutate(active = ifelse(!is.na(activity_counts) & activity_counts > 0, 1, 0)) |>
        group_by(DateTime = lubridate::floor_date(DateTime, "1 hour")) |>
        summarise(
          activity_time = sum(active, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(date = as.Date(DateTime)) |>
        # merge problemtarget() data based on Date
        left_join(problemtarget(), by = join_by(date == Date)) |>
        arrange(desc(DateTime))

      # Calculate weekly average of activity time
      week_data <- df_activity |>
        group_by(date) |>
        mutate(activity_time = sum(activity_time, na.rm = TRUE)) |>
        ungroup() |>
        mutate(week = format(DateTime, "%W")) |>
        group_by(week) |>
        summarise(
          # Convert to hours instead of minutes
          weekly_activity_time = mean(activity_time) / 60,
          date = max(date)
        )

      if ("STRESS" %in% names(data)) {
        df_stress <- data$STRESS |>
          group_by(DateTime = lubridate::floor_date(DateTime, "1 hour")) |>
          summarise(
            STRESS = mean(STRESS, na.rm = TRUE),
            .groups = "drop"
          ) |>
          mutate(date = as.Date(DateTime)) |>
          arrange(desc(DateTime))
      }

      if ("SLEEP" %in% names(data)) {
        # Different devices have different data for sleep
        # Two options:
        # 1. There's a start and end time, and sleep time has to be calculated
        # 2. There are sleep detection stages available, and every minute in
        #    a stage (>0) counts as an minute slept.
        if ("start_timestamp" %in% names(data$SLEEP)) {
          df_sleep <- data$SLEEP |>
            mutate(start_timestamp = as.POSIXct(start_timestamp, origin = "1970-01-01", tz = "UTC"),
                   end_timestamp = as.POSIXct(end_timestamp, origin = "1970-01-01", tz = "UTC"),
                   DateTime = as.Date(start_timestamp),
                   SLEEP = as.numeric(end_timestamp - start_timestamp, units = "hours")) |>
            group_by(DateTime) |>
            summarise(
              SLEEP = as.numeric(sum(SLEEP, na.rm = TRUE)),
              .groups = "drop"
            ) |>
            mutate(date = as.Date(DateTime)) |>
            arrange(desc(DateTime))

          # Calculate weekly average of sleep
          week_data_sleep <- df_sleep |>
            group_by(date) |>
            mutate(SLEEP = as.numeric(sum(SLEEP, na.rm = TRUE))) |>
            ungroup() |>
            mutate(week = format(DateTime, "%W")) |>
            group_by(week) |>
            summarise(weekly_sleep = mean(SLEEP),
                      date = max(date))

        } else if ("sleep_detection_stage" %in% names(data$SLEEP)) {
          df_sleep <- data$SLEEP |>
            mutate(SLEEP = ifelse(!is.na(sleep_detection_stage) & sleep_detection_stage > 0, 1, 0),
                   date = as.Date(DateTime)) |>
            group_by(date) |>
            summarise(
              SLEEP = sum(SLEEP, na.rm = TRUE),
              .groups = "drop"
            ) |>
            # conver to hours instead of minutes
            mutate(SLEEP = SLEEP / 60) |>
            arrange(desc(date))

          # Calculate weekly average of sleep
          week_data_sleep <- df_sleep |>
            mutate(week = format(date, "%W")) |>
            group_by(week) |>
            summarise(weekly_sleep = mean(SLEEP),
                      date = max(date))
        } else {
          df_sleep <- NULL
        }
      }

      output$echarts_problemtarget_act_time <- renderEcharts4r({
        df_activity |>
          group_by(date) |>
          # Convert to hours instead of minutes
          summarise(activity_time = sum(activity_time, na.rm = TRUE) / 60) |>
          arrange(desc(date)) |>
          mutate(date = as.character(date)) |>
          e_charts(date) |>
          e_bar(activity_time,
                name = "Activity",
                color = constants$app_config$visualisation$target_behaviour$bar_color) |>
          e_data(week_data) |>
          e_line(weekly_activity_time,
                 name = "Weekly avg",
                 color = constants$app_config$visualisation$target_behaviour$line_color,
                 lineStyle = list(
                   width = 3
                 )) |>
          e_y_axis(name = "Hours",
                   nameGap = 0,
                   nameLocation = "end",
                   nameTextStyle = list(
                     align = "right"
                   )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_title("Activity Time") |>
          e_flip_coords() |>
          e_grid(left = 70) |>
          e_legend(
            left = "left",
            top = 30
          )
      })

      output$echarts_problemtarget_behaviour <- renderEcharts4r({

        title <- setdiff(df_activity$`Problem or Target Behavior` |> unique(), NA)

        df_activity |>
          group_by(date) |>
          summarise(score = mean(as.numeric(Score), na.rm = TRUE)) |>
          arrange(desc(date)) |>
          mutate(date = as.character(date)) |>
          e_charts(date) |>
          e_line(score,
                 lineStyle = list(
                   width = 3
                 )) |>
          e_y_axis(
            name = "Score",
            nameGap = 0,
            nameLocation = "end",
            nameTextStyle = list(
              align = "right"
            )
          ) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_title(paste("Target:", title, "Score")) |>
          e_flip_coords() |>
          e_grid(left = 70)  |>
          e_legend(
            show = FALSE
          )
      })

      output$echarts_problemtarget_stress <- renderEcharts4r({

        req(df_stress)

        df_stress |>
          group_by(date) |>
          summarise(STRESS = mean(STRESS, na.rm = TRUE)) |>
          arrange(desc(date)) |>
          mutate(date = as.character(date)) |>
          e_charts(date) |>
          e_line(STRESS) |>
          e_y_axis(name = "Stress",
                   nameGap = 0,
                   nameLocation = "end",
                   nameTextStyle = list(
                     align = "right"
                   )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_title("Measured Stress") |>
          e_flip_coords() |>
          e_grid(left = 70)  |>
          e_legend(
            show = FALSE
          )
      })

      output$echarts_problemtarget_sleep <- renderEcharts4r({

        req(df_sleep)

        df_sleep |>
          group_by(date) |>
          summarise(SLEEP = sum(SLEEP, na.rm = TRUE)) |>
          arrange(desc(date)) |>
          mutate(date = as.character(date)) |>
          e_charts(date) |>
          e_bar(SLEEP,
                name = "Hours of sleep",
                color = constants$app_config$visualisation$target_behaviour$bar_color) |>
          e_data(week_data_sleep) |>
          e_line(weekly_sleep,
                 name = "Weekly avg",
                 color = constants$app_config$visualisation$target_behaviour$line_color,
                 lineStyle = list(
                   width = 3
                 )) |>
          e_y_axis(name = "Hours",
                   nameGap = 0,
                   nameLocation = "end",
                   nameTextStyle = list(
                     align = "right"
                   )) |>
          e_x_axis(
            axisPointer = list(show = TRUE),
            axisLabel = list(
              formatter = constants$yearMonthDate
            )
          ) |>
          e_title("Sleep") |>
          e_flip_coords() |>
          e_grid(left = 70) |>
          e_legend(
            left = "left",
            top = 30
          )
      })

      updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("sync"))

    }) |> bindEvent(input$btn_make_plot)

    ## Calendar overview ---------------------------
    # Calendar events below the overview plot, in the same colors the events
    # are drawn with in the plot.
    output$ui_calendar_overview <- renderUI({
      req(calendar())

      message("visualization - ui_calendar_overview")

      tagList(
        tags$hr(),
        tags$h5("Calendar"),
        DTOutput(ns("dt_calendar_overview"))
      )
    })

    output$dt_calendar_overview <- renderDT({
      req(calendar())

      message("visualization - dt_calendar_overview")

      calendar() |>
        ungroup() |>
        functions$filter_dates(overview_date_range(), "Start") |>
        arrange(Start) |>
        mutate(
          # The time window of the event, in the same unit as the echarts time
          # axis (milliseconds since epoch) so the hover handler on the overview
          # plot can compare them directly. As text, to keep the large numbers
          # out of scientific notation. Events without an end time stay empty.
          StartMillis = sprintf("%.0f", as.numeric(Start) * 1000),
          EndMillis = ifelse(is.na(End),
                             "",
                             sprintf("%.0f", as.numeric(End) * 1000)),
          Start = format(Start, "%Y-%m-%d %H:%M"),
          # Color is user-supplied (via the calendar upload), so it is HTML-
          # escaped before being embedded in the style attribute to prevent
          # it from breaking out of the attribute or injecting markup.
          Dot = paste0("<span style='display:inline-block; width:12px; ",
                       "height:12px; border-radius:50%; background-color:",
                       htmlEscape(Color, attribute = TRUE), ";'></span>"),
          Activity = Text
        ) |>
        select(Start, Dot, Activity, StartMillis, EndMillis) |>
        datatable(
          # Only the Dot column holds real HTML (the color swatch); the rest
          # comes from the uploaded calendar file and must stay escaped.
          # Column order: Start, Dot, Activity, StartMillis, EndMillis.
          escape = c(TRUE, FALSE, TRUE, TRUE, TRUE),
          rownames = FALSE,
          selection = "none",
          colnames = c("Start date", "Color", "Activity", "", ""),
          options = list(
            lengthChange = FALSE,
            searching = FALSE,
            paging = FALSE,
            info = FALSE,
            columnDefs = list(
              list(targets = "Start", width = "120px"),
              list(targets = "Dot", orderable = FALSE, width = "20px"),
              # The time window is only there for the hover handler on the
              # overview plot, don't show it.
              list(targets = c("StartMillis", "EndMillis"), visible = FALSE)
            ),
            # Put the time window on the row itself, so the hover handler on the
            # overview plot can find the event it is hovering on.
            rowCallback = JS(
              "function(row, data) {",
              "  row.setAttribute('data-start', data[3]);",
              "  row.setAttribute('data-end', data[4]);",
              "}"
            )
          )
        )

    })

    ## Annotations ---------------------------------
    current_visible_annotations <- reactive({

      if (!is.null(input$datazoom_bounds)) {
        # !! WATCH THE TIMEZONE !!
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

    output$dt_annotations_visible <- renderDT({

      current_visible_annotations() |>
        mutate(`Start Date` = format(`Start Date`, "%Y-%m-%d"),
               Start = format(Start, "%H:%M:%S"),
               `End Date` = format(`End Date`, "%Y-%m-%d"),
               End = format(End, "%H:%M:%S")
        ) |>
        datatable(width = 500)

    })

    ## Notes ---------------------------------------
    output$echarts_notes <- renderUI({
      if (r$more_than_2weeks == TRUE) {
        week_comment <- "Note: the data is aggregated by 5 minutes as it contains more than 2 weeks of data."
      } else {
        week_comment <- ""
      }
      if (device == "embrace-plus"&& r$type == "raw") {
        device_comment <- "Note: the Embrace Plus device does not have HR per second
          available when data is in raw format. The aggregated HR is available
          from the digital biomarkers and the BVP signal is available from the raw data."
      } else if (device == "embrace-plus" && r$type == "aggregated") {
        device_comment <- "Note: the aggregated data of the Embrace Plus device uses
          the standard deviation of the accelerometer readings in terms of gravitational force (g)
          as a proxy for movement. This differs from the
          raw data, that uses the geometric mean acceleration. Currently, the stress algorithmworkswith
          geometric mean acceleration only."
      } else {
        device_comment <- ""
      }

      # render as div with paragraphs
      div(p(device_comment),
          br(),
          p(week_comment)
      )
    })

    # Return values -------------------------------
    # These return values are used in the analysis section of the
    # application. Only applicable for the "legacy" E4 device.
    return(list(
      dailygraphs1 = dailygraphs1,
      dailygraphs2 = dailygraphs2,
      dailygraphs3 = dailygraphs3,
      dailygraphs4 = dailygraphs4
    ))

  })
}
