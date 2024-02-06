
box::use(
  bslib[card, card_header],
  shiny[actionButton, column, bindEvent, br, dateInput, div, fluidRow, icon, incProgress,
        moduleServer, numericInput, NS, observe, p, reactive, reactiveVal, renderText,
        renderUI, req, tagList, tags, uiOutput, updateDateInput, updateNumericInput,
        verbatimTextOutput, withProgress],
  shinyFiles[shinyDirButton, shinyDirChoose],
  shinyjs[disable, enable, hidden, hide, show],
  shinytoastr[toastr_info, toastr_success],
  wearables[read_and_process_e4]
)

box::use(
  app/logic/constants,
  app/logic/functions,
  app/view/components/helpButton
)

ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_header("Batch Analysis"),
      fluidRow(
        
        tags$div(style = "width: 100%;",
                 tags$div(style = "float: right;",
                          helpButton$ui(ns("help"))
                 )
        ),
        
        column(12,
               tags$p("Select the input and output folder for a batch analysis of multiple ZIP files.")
        ),
        
        column(6, 
               style = "padding: 16px;",
               shinyDirButton(ns("btn_select_folder_input"), 
                              label = "Select input folder",
                              title = "Select input folder",
                              icon = icon("folder-open"), 
                              class = "btn-light"),
               
               uiOutput(ns("ui_folder_in")),
               
               tags$br(),
               uiOutput(ns("ui_n_files_found"))
        ),
        column(6,
               style = "padding: 16px; border:",
               shinyDirButton(ns("btn_select_folder_output"), 
                              label = "Select output folder",
                              title = "Select output folder",
                              icon = icon("folder-open"), 
                              class = "btn-light"),
               
               hidden(
                 tags$div(id = ns("div_same_as_input"),
                          tags$br(),
                          actionButton(ns("btn_output_same_input"), "Same as input folder", 
                                       icon = icon("arrow-alt-circle-left"), class = "btn-secondary")
                 )
               ),
               
               uiOutput(ns("ui_folder_out"))
        )
      ),  
      
      tags$br(),
      tags$hr(),
      
      hidden(
        actionButton(ns("btn_run_batch"), "Run batch analysis", icon = icon("check"), class = "btn-success")
      ),
      
      verbatimTextOutput(ns("txt_out"))
      
    )
  )
  
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values -------------------------------
    folder_in <- reactiveVal()
    folder_out <- reactiveVal()
    
    zip_files <- reactive({
      if(isTRUE(folder_in())){
        list.files(folder_in(), pattern = "[.]zip$", recursive = TRUE, full.names = TRUE)
      }
    })
    
    n_zip_files <- reactive({
      if(isTRUE(zip_files())){
        length(zip_files())
      }
    })
    
    # Modules --------------------------------------
    helpButton$server("help", helptext = constants$help_config$batch)
    
    # Functionality ---------------------------------
    
    shinyDirChoose(input, 
                   "btn_select_folder_input",
                   roots = c(home = ifelse(.Platform$OS.type == "windows", "C:", "~"), 
                             wd = "."))
    
    shinyDirChoose(input, 
                   "btn_select_folder_output",
                   roots = c(home = ifelse(.Platform$OS.type == "windows", "C:", "~"), 
                             wd = "."))
    
    observe({
      
      req(input$btn_select_folder_input)
      
      if(length(input$btn_select_folder_input) > 1){
        # if windows, use \, otherwise use /
        if (.Platform$OS.type == "windows") {
          chc <- paste0(ifelse(input$btn_select_folder_input$root == "home", "C:", "."), 
                        paste0(input$btn_select_folder_input$path, collapse = "\\")
          )
        } else {
          chc <- paste0(ifelse(input$btn_select_folder_input$root == "home", "~", "."), 
                        paste0(input$btn_select_folder_input$path, collapse = "/")
          )
        }
      } else {
        chc <- NA
      }
      
      if(!is.na(chc)){
        folder_in(chc)  
      }
      
    }) 
    
    observe({
      if(!is.null(folder_in())){
        show("div_same_as_input")
      }
    })
    
    observe({
      
      req(input$btn_select_folder_output)
      
      if(length(input$btn_select_folder_output) > 1){
        # if windows, use \, otherwise use /
        if (.Platform$OS.type == "windows") {
          chc <- paste0(ifelse(input$btn_select_folder_output$root == "home", "C:", "."), 
                        paste0(input$btn_select_folder_output$path, collapse = "\\")
          )
        } else {
          chc <- paste0(ifelse(input$btn_select_folder_output$root == "home", "~", "."), 
                        paste0(input$btn_select_folder_output$path, collapse = "/")
          )
        }
      } else {
        chc <- NA
      }
      
      if(!is.na(chc)){
        folder_out(chc)  
      }
      
    })
    
    observe({
      
      folder_out(folder_in())
      
    }) |> bindEvent(input$btn_output_same_input)
    
    
    output$ui_folder_in <- renderUI({
      tags$p(folder_in(),
             style = "font-style: italic; font-size : 0.9em;")
    })
    
    output$ui_folder_out <- renderUI({
      tags$p(folder_out(),
             style = "font-style: italic; font-size : 0.9em;")
    })
    
    observe({
      
      have_in <- !is.null(folder_in())
      have_out <- !is.null(folder_out())
      
      if(have_in & have_out){
        show("btn_run_batch")  
      }
      
    })
    
    output$ui_n_files_found <- renderUI({
      
      req(n_zip_files())
      tagList(
        tags$p(
          tags$span(icon("info-circle"), style = "color: #27AE60;"),
          paste(n_zip_files(), "ZIP files found in the selected folder.")),
        
        tags$p(tags$span(icon("exclamation-triangle"), style = "color: #CB4335;"),
               "Select output folder to continue.")
      )
      
    })
    
    observe({
      
      toastr_info("Batch analysis started, this can take a while!")
      
      disable("btn_run_batch")
      
      zips <- zip_files() 
      path_out <- folder_out()
      
      withProgress(message = "Running batch analysis...", value = 0, {
        
        for(i in seq_along(zips)){
          
          out <- read_and_process_e4(zips[i])
          
          if(is.null(out)){
            toastr_info(paste("Problem with",zips[i],"- skipping."))
            next
          }
          
          fn_root <- basename(tools::file_path_sans_ext(zips[i]))
          out_file <- file.path(path_out, paste0(fn_root, ".rds"))
          
          tm <- try(saveRDS(out, out_file))
          
          if(inherits(tm, "try-error")){
            err <- attr(tm, "condition")$message
            toastr_error(paste("Some problem with batch analysis, error message:", err))
            break
          }
          
          incProgress(1/length(zips), detail = basename(out_file))
          
        }
        
      })
      
      toastr_info("Batch analysis completed!")
      enable("btn_run_batch")
      
    }) |> bindEvent(input$btn_run_batch)
    
  })
}
