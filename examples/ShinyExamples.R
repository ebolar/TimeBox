# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# Time management in R for busy people, and a good excuse for me to 
# learn more about R.
# 
# ===================================================================
# 
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

AB1UI <- pageWithSidebar(
  headerPanel("actionButton test"),
  sidebarPanel(
    numericInput("n", "N:", min = 0, max = 100, value = 50),
    br(),
    actionButton("AddIdea", "Go!"),
    p("Click the button to update the value displayed in the main panel.")
  ),
  mainPanel(
    verbatimTextOutput("nText")
  )
)

AB1Server <- function(input, output) {
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  ntext <- eventReactive(input$AddIdea, {
    print("Button pusher!!!")
    input$n
  })
  
  output$nText <- renderText({
    ntext()
  })
}

AB2UI <-fluidPage(
  sliderInput("obs", "Number of observations", 0, 1000, 500),
  actionButton("goButton", "Go!"),
  plotOutput("distPlot")
)

AB2UI <- function(input, output) {
  output$distPlot <- renderPlot({
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$goButton
    
    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(rnorm(input$obs))
    hist(dist)
  })
}