
box::use(
  bslib[navset_tab, nav_panel, nav_select, card, card_header],
  DT[DTOutput, renderDT],
  dygraphs[dygraphOutput, renderDygraph],
  lubridate[ymd_hms],
  shiny[actionButton, bindEvent, br, checkboxInput, column, fluidRow, hr, 
        icon, moduleServer, NS, observe, radioButtons,
        reactive, reactiveVal, renderUI, req, tagList, tags, textInput, uiOutput,
        updateActionButton],
  shinycssloaders[withSpinner],
  shinyjs[show],
  shinytoastr[toastr_info, toastr_success]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/logic/functions_e4,
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
                 tags$h4("EDA"),
                 visSeriesOptions$ui(ns("eda"), y_range = constants$app_config$visualisation$eda$yrange),
                 tags$hr(),
                 
                 tags$h4("HR"),
                 visSeriesOptions$ui(ns("hr"), y_range =constants$app_config$visualisation$hr$yrange),
                 tags$hr(),
                 
                 tags$h4("TEMP"),
                 visSeriesOptions$ui(ns("temp"), y_range = constants$app_config$visualisation$temp$yrange),
                 tags$hr(),
                 
                 tags$h4("MOVE"),
                 visSeriesOptions$ui(ns("move"), y_range = constants$app_config$visualisation$move$yrange)
                 
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
        dygraphOutput(ns("dygraph_current_data4"), height = "140px")
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

server <- function(id, data = reactive(NULL), calendar = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values -------------------------------
    plot_output <- reactiveVal()
    
    # Modules ---------------------------------------
    helpButton$server("help", helptext = constants$help_config$visualization)
    y_eda <- visSeriesOptions$server("eda")
    y_hr <- visSeriesOptions$server("hr")
    y_temp <- visSeriesOptions$server("temp")
    y_move <- visSeriesOptions$server("move", selected = "custom", custom_y = constants$app_config$visualisation$move$custom_y)
    
    # Functionality ---------------------------------
    
    functions$hide_tab("plottab")
    functions$hide_tab("plotannotations")
    
    # Collect submodule output in a single reactive
    series_options <- reactive(
      list(
        EDA = y_eda(),
        HR = y_hr(),
        TEMP = y_temp(),
        MOVE = y_move()
      )
    )
    
    # Option to plot aggregated data (or not),
    # only visible if less than 2 hours of data, otherwise the plot will not be responsive.
    data_range_hours <- reactive({
      functions$e4_data_datetime_range(data()$data)
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
      
      if(agg == "Yes"){
        timeseries <- functions_e4$make_e4_timeseries(data$data_agg)
      } else {
        timeseries <- functions_e4$make_e4_timeseries(data$data)
      }
      
      functions$show_tab("plottab")
      
      if(isTRUE(nrow(calendar()))){
        functions$show_tab("plotannotations")
      }
      
      nav_select(id = "tabs", 
                 selected = "plottab")
      
      if(input$check_add_calendar_annotation){
        annotatedata <- calendar()
      } else {
        annotatedata <- NULL
      }
      
      plots <- functions_e4$e4_timeseries_plot(timeseries,
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
      
      
      # 
      toastr_success("Plot constructed, click on the 'Plot' tab!")
      updateActionButton(session, "btn_make_plot", label = "Update plot", icon = icon("sync"))
      functions$enable_link("tabAnalysis")
      
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
          !((Start > ran[[2]] && End > ran[[2]]) |
              (Start < ran[[1]] && End < ran[[1]]))
        )
      
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
