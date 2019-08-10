# Server.R
server <- shinyServer(function(input, output,session) {
  
  
  
  
  output$sampletable <- DT::renderDataTable({
    iris
  }, server = TRUE,selection = 'single')  
  
  output$selectedrow <- DT::renderDataTable({
    
    selectedrowindex <<-     input$sampletable_rows_selected[length(input$sampletable_rows_selected)]
    selectedrowindex <<- as.numeric(selectedrowindex)
    selectedrow <- (iris[selectedrowindex,])
    selectedrow
    
    
    
  })
  
  output$plots <- renderPlot({
    
    variable <- sampletable[selectedrowindex,1]
    #write your plot function
    
    
  })
  
  
})

#ui.R 
ui <- shinyUI(navbarPage( "Single Row Selection",
                    
                    
                    
                    tabPanel("Row selection example",
                             sidebarLayout(sidebarPanel("Parameters"),
                                           mainPanel(
                                             DT::dataTableOutput("selectedrow"),   
                                             DT::dataTableOutput("sampletable")
                                             
                                           ))
                             
                    )
                    
))

shinyApp(ui=ui, server = server)