
box::use(
  shiny[actionButton, bindEvent, br, HTML, icon, modalDialog,
        moduleServer, NS, observe, removeModal, showModal, tagList]
)

ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("btn_help"), 
               label = "Help", 
               icon = icon("question-circle"), 
               class = "btn-help")
} 

server <- function(id, helptext) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      showModal(
        modalDialog(
          title = tagList(
            icon("question-circle"), 
            "Help"),
          size = "m",
          easyClose = TRUE,
          
          HTML(helptext),
          
          footer = actionButton(session$ns("close_modal"), 
                                "Close", 
                                class = "btn-success",
                                `data-dismiss`="modal")
          
        )
      )
    }) |> bindEvent(input$btn_help)
    
    observe({
      removeModal()
    }) |> bindEvent(input$close_modal)
    
  })
}
