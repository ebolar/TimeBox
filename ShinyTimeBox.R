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

source("TimeBoxData.R")
source("HelperFunctions.R")

# Define UI 
TimeBoxUI <- navbarPage(
  "Time Box",

  # Panels
  tabPanel("Brain dump", 
             sidebarLayout(
               sidebarPanel(textOutput('IdeaStuff')),
               mainPanel(dataTableOutput('Idea'))
             )
	   ),
  tabPanel("Strategy / Planning", 
             sidebarLayout(
               sidebarPanel(textOutput('BucketStuff')),
               mainPanel(dataTableOutput('Bucket'))
             )
	   ),
  tabPanel("Tasks", 
             sidebarLayout(
               sidebarPanel(textOutput('TaskStuff')),
               mainPanel(dataTableOutput('Task'))
             )
           ),
  tabPanel("Timekeeping", 
             sidebarLayout(
               sidebarPanel(textOutput('WorkStuff')),
               mainPanel(dataTableOutput('Work'))
             )
	   )
)

SimpleUI <- fluidPage(
  fluidRow(
    column(4, verbatimTextOutput('TaskStuff')),
    column(8, dataTableOutput('Task'))
  )
)

# Only need to make the display features Reactive. 
TimeBoxServer <- function(input, output) {
  cat("XXX-Opening datasoure\n")
  Configuration.Init() -> Env
  Datasource.Init(Env)
  print(ls(Env))

  output$IdeaStuff <- renderPrint(cat("Rapid capture of ideas"))
  output$BucketStuff <- renderPrint(cat("Where will I spend my time"))
  output$TaskStuff <- renderPrint(cat("Getting shit done"))
  output$WorkStuff <- renderPrint(cat("What just happened bro?"))
  
  output$desc <- renderPrint(ls(Env$Cache))
  
  output$Idea <- renderDataTable(Datasource.Read(Env, "Idea"))
  output$Bucket <- renderDataTable(Datasource.Read(Env, "Bucket"))
  output$Task <- renderDataTable(Datasource.Read(Env, "Task"))
  output$Work <- renderDataTable(Datasource.Read(Env, "Work"))
  
  onStop(function(){
    cat("XXX-Closing data source\n")
    Datasource.Close(Env)
    print(ls(Env, pos = parent.env(environment())))
  })
}

# Run the application
shinyApp(ui = TimeBoxUI, server = TimeBoxServer)

