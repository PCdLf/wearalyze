
box::use(
  shiny[moduleServer, NS, numericInput, radioButtons, reactive, renderUI, req, tags, uiOutput,
        updateRadioButtons],
  shinyWidgets[numericRangeInput]
)

box::use(
  app/logic/functions
)

ui <- function(id, y_range){
  
  ns <- NS(id)
  
  functions$side_by_side(
    numericRangeInput(ns("num_yaxis"), "Y-axis range", value = y_range),
    tags$div(style = "padding-left: 30px;",
             radioButtons(ns("check_line_type"), "Horizontal line", 
                          choices = c("Mean line" = "mean", "Custom" = "custom"),
                          selected = character(0), 
                          inline = TRUE)
    ),
    tags$div(style = "padding-left: 25px;",
             uiOutput(ns("ui_custom_line_val"), 
                      inline = TRUE)
    )
  )
  
}


server <- function(id, selected = c("mean", "custom"), custom_y = 0) {
  moduleServer(id, function(input, output, session) {
    
    updateRadioButtons(session, 
                       "check_line_type", 
                       selected = selected[1])
    
    output$ui_custom_line_val <- renderUI({
      
      req(input$check_line_type)
      
      if(input$check_line_type == "custom"){
        numericInput(session$ns("num_custom_y"), "Y-value", value = custom_y, width = 100)
      } else {
        NULL
      }
      
    })
    
    reactive({
      list(
        yaxis_range = input$num_yaxis,
        line_type = input$check_line_type,
        custom_y_val = input$num_custom_y
      )
    })
    
  })
}
