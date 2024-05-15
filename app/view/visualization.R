
box::use(
  bslib[navset_tab, nav_panel, nav_select, card, card_header],
  DT[datatable, DTOutput, renderDT],
  dplyr[filter, mutate],
  dygraphs[dygraphOutput, renderDygraph],
  lubridate[ymd_hms],
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
        title = "Plot",
        icon = icon("chart-bar"),
        value = "plottab",
        withSpinner(
          dygraphOutput(ns("dygraph_current_data1"), height = "140px")
        ),
        dygraphOutput(ns("dygraph_current_data2"), height = "140px"),
        dygraphOutput(ns("dygraph_current_data3"), height = "140px"),
        dygraphOutput(ns("dygraph_current_data4"), height = "140px"),
        uiOutput(ns("dygraph_notes"))
      ),
      
      nav_panel(
        title = "Problem/Target Relationship",
        icon = icon("chart-bar"),
        value = "plottab2",
        dygraphOutput(ns("dygraph_problemtarget"), height = "100%")
      ),
      
      nav_panel(
        title = "Annotations",
        icon = icon("list-ol"),
        value = "plotannotations",
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
      visSeriesOptions$ui(ns("move"),
                          y_range = as.numeric(constants$app_config$visualisation$move[[device]][[type]]$yrange))
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
      
      if (!"ACC" %in% names(data()$data) && !"MOVE" %in% names(data()$data)) {
        hide("move_options")
      }
      
    })
    
    functions$hide_tab("plottab")
    functions$hide_tab("plottab2")
    functions$hide_tab("plotannotations")
    
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
      
      req(data$data)
      
      toastr_info("Plot construction started...")
      
      # Precalc. timeseries (for viz.)
      if(is.null(input$rad_plot_agg)){
        agg <- "Yes"
      } else {
        agg <- input$rad_plot_agg
      }
      
      if (agg == "Yes") {
        timeseries <- functions_devices$make_timeseries(data$data_agg)
      } else {
        timeseries <- functions_devices$make_timeseries(data$data)
      }
      
      functions$show_tab("plottab")
      
      if (isTruthy(calendar())) {
        functions$show_tab("plotannotations")
      }
      
      nav_select(id = "tabs", 
                 selected = "plottab")
      
      if(input$check_add_calendar_annotation){
        annotatedata <- calendar()
      } else {
        annotatedata <- NULL
      }
      
      plots <- functions_devices$timeseries_plot(timeseries,
                                                 main_title = input$txt_plot_main_title,
                                                 calendar_data = annotatedata,
                                                 plot_tags = isTRUE(input$rad_plot_tags == "Yes"),
                                                 tags = data$data$tags,
                                                 series_options = series_options()
      )
      
      plot_output(
        plots
      )
      
      output$dygraph_current_data1 <- renderDygraph(plots[[1]])
      output$dygraph_current_data2 <- renderDygraph(plots[[2]])
      output$dygraph_current_data3 <- renderDygraph(plots[[3]])
      output$dygraph_current_data4 <- renderDygraph(plots[[4]])
      
      toastr_success("Plot constructed, click on the 'Plot' tab!")
      updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("sync"))
      
      if(r$type == "raw"){
        functions$enable_link(menu = device,
                              name = "Analysis")
      }
      
    }) |> bindEvent(input$btn_make_plot)
    
    observe({
      
      data <- data()
      
      req(data$data)
      req(problemtarget())
      
      browser()
      
      functions$show_tab("plottab2")
      
      output$dygraph_problemtarget <- renderDygraph({})
      
      updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("sync"))
      
    }) |> bindEvent(input$btn_make_plot)
    
    
    current_visible_annotations <- reactive({
      
      # !!!! WATCH THE TIMEZONE!
      # Problem: cannot read timezone info from rv$calendar$Start, should
      # be saved there (as tzone attribute) when reading calendar
      ran <- suppressWarnings({
        ymd_hms(input$dygraph_current_data4_date_window, tz = "CET")
      })
      
      calendar() |> 
        filter(
          (Start <= ran[[2]] & End <= ran[[2]]) &
            (Start >= ran[[1]] & End >= ran[[1]])
        )
      
    })
    
    output$dygraph_notes <- renderUI({
      if (device == "embrace-plus"&& r$type == "raw") {
        p("Note: the Embrace Plus device does not have HR per second 
          available when data is in raw format. The aggregated HR is available
          from the digital biomarkers and the BVP signal is available from the raw data.")
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
