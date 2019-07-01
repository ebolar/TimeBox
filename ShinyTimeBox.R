# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# Time management in R for busy people, and a good excuse for me to 
# learn more about R.
# 
# -------------------------------------------------------------------
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# TBD: 
# - Use Nouns for the UI and Verbs for the Server.
# - Create add routines for Buckets and Tasks
# - Create generic routines to create all data types
#
# - Add timestamps: Created + Last Modification
# - Add search criteria for the list
# - Do I need row names?
# - When will it make sense to split this into ui.R and server.R?
# - How quickly can I get my to-do for this tool into the tool?

rm (list = ls())

library(shiny)
library(shinyjs)
library(DT)

source("TimeBoxData.R")
source("HelperFunctions.R")

# Define UI 
TimeBoxUI <- tagList(
  useShinyjs(),

  navbarPage(
    "Time Box",
  
    # Panels

    # ===================================================================
    # Ideas
    # ===================================================================
    tabPanel(
      "Brain dump", 

      sidebarLayout(
        sidebarPanel(
          textOutput('IdeaStuff'),
          div(
            id="New.IdeaFrame",
            textInput("New.IdeaName", "Name"),
            textAreaInput("New.IdeaNotes","Notes",
              rows = 5, 
              placeholder = "come come elucidate your thoughts"),
            selectInput("New.IdeaBucket", "Assign to:", choices = NULL)
          ),
          actionButton("New.IdeaButton", "Add"),
          actionButton("Update.IdeaButton", "Update"),
          actionButton("Delete.IdeaButton", "Delete"),
          actionButton("Reset.IdeaButton", "Reset")
        ),
        mainPanel(
          verbatimTextOutput('Display.IdeaSelected'),
          dataTableOutput('Display.Idea')
        )
      )
    ),

    # ===================================================================
    # Buckets
    # ===================================================================
    tabPanel(
      "Strategy / Planning", 
      sidebarLayout(
        sidebarPanel(
          textOutput('BucketStuff'),
          div(
            id="New.BucketFrame",
            textInput("New.BucketName", "Name"),
            textAreaInput("New.BucketDescription","Description",
              rows = 5, 
              placeholder = "come come elucidate your thoughts")
          ),
          actionButton("New.BucketButton", "Add"),
          actionButton("Delete.BucketButton", "Delete"),
          actionButton("Reset.BucketButton", "Reset")
        ),
        mainPanel(
          verbatimTextOutput('Display.BucketSelected'),
          dataTableOutput('Display.Bucket')
        )
      )
    ),

    # ===================================================================
    # Tasks
    # ===================================================================
    tabPanel(
      "Tasks", 
      sidebarLayout(
        sidebarPanel(
          textOutput('TaskStuff'),
          div(
            id="New.TaskFrame",
            textInput("New.TaskName", "Name"),
            textAreaInput("New.TaskDescription","Description",
              rows = 5, 
              placeholder = "come come elucidate your thoughts")
          ),
          actionButton("New.TaskButton", "Add"),
          actionButton("Delete.TaskButton", "Delete"),
          actionButton("Reset.TaskButton", "Reset")
	),
        mainPanel(
          verbatimTextOutput('Display.TaskSelected'),
          dataTableOutput('Display.Task')
        )
      )
    ),

    # ===================================================================
    # Work
    # ===================================================================
    tabPanel(
      "Timekeeping", 
      sidebarLayout(
        sidebarPanel(textOutput('WorkStuff')),
        mainPanel(
          verbatimTextOutput('Display.WorkSelected'),
          dataTableOutput('Display.Work')
        )
      )
    ),

    # ===================================================================
    # More stuff
    # ===================================================================
    tabPanel(
      "...",
      actionButton("Save.all", "Save"),
      actionButton("Archive.all", "Archive")
    )
  )
)

TimeBoxServer <- function(input, output, session) {
  # ===================================================================
  # Setup 
  # ===================================================================
  #
  cat("XXX-Opening datasoure\n")
  
  Configuration.Init() -> Env
  FALSE -> Env$traceData
  TRUE -> Env$traceUI
  
  Datasource.Init(Env)

  # ===================================================================
  # Static content
  # ===================================================================
  #
  output$IdeaStuff <- renderPrint(cat("Rapid capture of ideas"))
  output$BucketStuff <- renderPrint(cat("Where will I spend my time"))
  output$TaskStuff <- renderPrint(cat("Getting shit done"))
  output$WorkStuff <- renderPrint(cat("What just happened dude?"))
  
  # ===================================================================
  # Ideas
  # ===================================================================
  # Helpers
  # ------------------------------------------------
  #
  New.Idea <- reactive({
    if (Env$traceUI) cat("XXX - New.Idea - extraction\n")
    
    Idea(
      Name=input$New.IdeaName, 
      Notes=input$New.IdeaNotes,
      Bucket=input$New.IdeaBucket
    )
  })

  Selected.Idea <- reactive({
    input$Display.Idea_rows_selected
  })

  Reset.Idea <- function() {
    # Initialise the bucket list
    updateSelectInput(session, "New.IdeaBucket", 
      choices = {
        if (Env$traceUI) cat("XXX - Initialising the bucket list\n")
        c("-", sort(unique(Datasource.Read(Env, "Bucket")$Name)))
      },
      selected = "-"
    )
    
    reset("New.IdeaFrame")
  }

  # ------------------------------------------------
  # Events
  # ------------------------------------------------
  #
  observeEvent(input$New.IdeaButton, {
    if (Env$traceUI) cat(sprintf("XXX - New.IdeaButton - %d\n", input$New.IdeaButton))
    Datasource.Add(Env, "Idea", New.Idea())
    Reset.Idea()
  })

  observeEvent(input$Update.IdeaButton, {
    if (Env$traceUI) cat(sprintf("XXX - Update.IdeaButton - %d\n", input$Update.IdeaButton))
  })
  
  observeEvent(input$Delete.IdeaButton, {
    if (Env$traceUI) cat(sprintf("XXX - Delete.IdeaButton - %d\n", input$Delete.IdeaButton))
    
    selected <- Selected.Idea()

    if (length(selected) > 0) {
      Datasource.Read(Env, "Idea") -> rows
      rows[-selected,] -> rows
      Datasource.Replace(Env, "Idea", rows)
    }

    Reset.Idea()
  })
  
  observeEvent(input$Reset.IdeaButton, {
    if (Env$traceUI) cat(sprintf("XXX - Reset.IdeaButton - %d\n", input$Reset.IdeaButton))
    Reset.Idea()
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Idea <- DT::renderDataTable({
    input$New.IdeaButton
    input$Delete.IdeaButton
    input$Reset.IdeaButton
    Reset.Idea()
    format.Idea(Datasource.Read(Env, "Idea"))
  }, server = TRUE)

  output$Display.IdeaSelected <- renderPrint({
    selected <- Selected.Idea()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    }
  })

  # ===================================================================
  # Buckets
  # ===================================================================
  # Helpers
  # ------------------------------------------------
  #
  New.Bucket <- reactive({
    if (Env$traceUI) cat("XXX - New.Bucket - extraction\n")
    
    Bucket(
      Name=input$New.BucketName, 
      Description=input$New.BucketDescription
      # ChargeTo
    )
  })

  Selected.Bucket <- reactive({
    input$Display.Bucket_rows_selected
  })

  Reset.Bucket <- function() {
    # Initialise the bucket list
    updateSelectInput(session, "New.BucketBucket", 
      choices = {
        if (Env$traceUI) cat("XXX - Initialising the bucket list\n")
        c("-", sort(unique(Datasource.Read(Env, "Bucket")$Name)))
      },
      selected = "-"
    )
    
    reset("New.BucketFrame")
  }

  # ------------------------------------------------
  # Events
  # ------------------------------------------------
  #
  observeEvent(input$New.BucketButton, {
    if (Env$traceUI) cat(sprintf("XXX - New.BucketButton - %d\n", input$New.BucketButton))
    Datasource.Add(Env, "Bucket", New.Bucket())
    Reset.Idea()
    Reset.Bucket()
    Reset.Task()
  })

  observeEvent(input$Update.BucketButton, {
    if (Env$traceUI) cat(sprintf("XXX - Update.BucketButton - %d\n", input$Update.BucketButton))
  })
  
  observeEvent(input$Delete.BucketButton, {
    if (Env$traceUI) cat(sprintf("XXX - Delete.BucketButton - %d\n", input$Delete.BucketButton))
    
    selected <- Selected.Bucket()

    if (length(selected) > 0) {
      Datasource.Read(Env, "Bucket") -> rows
      rows[-selected,] -> rows
      Datasource.Replace(Env, "Bucket", rows)
    }

    Reset.Idea()
    Reset.Bucket()
    Reset.Task()
  })
  
  observeEvent(input$Reset.BucketButton, {
    if (Env$traceUI) cat(sprintf("XXX - Reset.BucketButton - %d\n", input$Reset.BucketButton))
    Reset.Bucket()
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Bucket <- DT::renderDataTable({
    input$New.BucketButton
    input$Delete.BucketButton
    input$Reset.BucketButton
    Reset.Bucket()
    format.Bucket(Datasource.Read(Env, "Bucket"))
  }, server = TRUE)

  output$Display.BucketSelected <- renderPrint({
    selected <- Selected.Bucket()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    }
  })

  # ===================================================================
  # Tasks
  # ===================================================================
  # Helpers
  # ------------------------------------------------
  #
  New.Task <- reactive({
    if (Env$traceUI) cat("XXX - New.Task - extraction\n")
    
    Task(
      Name=input$New.TaskName, 
      Description=input$New.TaskDescription, 
      Bucket=input$New.TaskBucket
      # Status
      # Today
    ) 
  })

  Selected.Task <- reactive({
    input$Display.Task_rows_selected
  })

  Reset.Task <- function() {
    # Initialise the bucket list
    updateSelectInput(session, "New.TaskBucket", 
      choices = {
        if (Env$traceUI) cat("XXX - Initialising the bucket list\n")
        c("-", sort(unique(Datasource.Read(Env, "Bucket")$Name)))
      },
      selected = "-"
    )
    
    reset("New.TaskFrame")
  }

  # ------------------------------------------------
  # Events
  # ------------------------------------------------
  #
  observeEvent(input$New.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - New.TaskButton - %d\n", input$New.TaskButton))
    Datasource.Add(Env, "Task", New.Task())
    Reset.Task()
  })

  observeEvent(input$Update.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - Update.TaskButton - %d\n", input$Update.TaskButton))
  })
  
  observeEvent(input$Delete.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - Delete.TaskButton - %d\n", input$Delete.TaskButton))
    
    selected <- Selected.Task()

    if (length(selected) > 0) {
      Datasource.Read(Env, "Task") -> rows
      rows[-selected,] -> rows
      Datasource.Replace(Env, "Task", rows)
    }

    Reset.Task()
  })
  
  observeEvent(input$Reset.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - Reset.TaskButton - %d\n", input$Reset.TaskButton))
    Reset.Task()
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Task <- DT::renderDataTable({
    input$New.TaskButton
    input$Delete.TaskButton
    input$Reset.TaskButton
    Reset.Task()
    format.Task(Datasource.Read(Env, "Task"))
  }, server = TRUE)

  output$Display.TaskSelected <- renderPrint({
    selected <- Selected.Task()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    }
  })

  # ===================================================================
  # Work
  # ===================================================================
  # Helpers
  # ------------------------------------------------
  #
  New.Work <- reactive({
    if (Env$traceUI) cat("XXX - New.Work - extraction\n")
    
    Work(
      Date=input$New.WorkName, 
      Duration=input$New.WorkDescription, 
      Task=input$New.WorkBucket
      # ChargeTo
    )
  })

  Selected.Work <- reactive({
    input$Display.Work_rows_selected
  })

  Reset.Work <- function() {
    # Initialise the bucket list
    updateSelectInput(session, "New.WorkBucket", 
      choices = {
        if (Env$traceUI) cat("XXX - Initialising the bucket list\n")
        c("-", sort(unique(Datasource.Read(Env, "Bucket")$Name)))
      },
      selected = "-"
    )
    
    reset("New.WorkFrame")
  }

  # ------------------------------------------------
  # Events
  # ------------------------------------------------
  #
  observeEvent(input$New.WorkButton, {
    if (Env$traceUI) cat(sprintf("XXX - New.WorkButton - %d\n", input$New.WorkButton))
    Datasource.Add(Env, "Work", New.Work())
    Reset.Work()
  })

  observeEvent(input$Update.WorkButton, {
    if (Env$traceUI) cat(sprintf("XXX - Update.WorkButton - %d\n", input$Update.WorkButton))
  })
  
  observeEvent(input$Delete.WorkButton, {
    if (Env$traceUI) cat(sprintf("XXX - Delete.WorkButton - %d\n", input$Delete.WorkButton))
    
    selected <- Selected.Work()

    if (length(selected) > 0) {
      Datasource.Read(Env, "Work") -> rows
      rows[-selected,] -> rows
      Datasource.Replace(Env, "Work", rows)
    }

    Reset.Work()
  })
  
  observeEvent(input$Reset.WorkButton, {
    if (Env$traceUI) cat(sprintf("XXX - Reset.WorkButton - %d\n", input$Reset.WorkButton))
    Reset.Work()
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Work <- DT::renderDataTable({
    input$New.WorkButton
    input$Delete.WorkButton
    input$Reset.WorkButton
    Reset.Work()
    format.Work(Datasource.Read(Env, "Work"))
  }, server = TRUE)

  output$Display.WorkSelected <- renderPrint({
    selected <- Selected.Work()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    }
  })

  # ===================================================================
  # Other stuff
  # ===================================================================
  #
  observeEvent(input$Save.all, {
    if (Env$traceUI) cat(sprintf("XXX - Save.all button - %d\n", input$Save.all))

    Datasource.Sync(Env)
  })

  observeEvent(input$Archive.all, {
    if (Env$traceUI) cat(sprintf("XXX - Archive.all button - %d\n", input$Archive.all))

    # Update the sync call to save to another source
    Datasource.Sync(Env, "Archive")
  })


  # ===================================================================
  # Cleanup
  # ===================================================================
  #
  onStop(function(){
    cat("XXX-Closing data source\n")
    Datasource.Close(Env)
    # print(ls(Env, pos = parent.env(environment())))
  })
}

# Run the application
shinyApp(ui = TimeBoxUI, server = TimeBoxServer)

