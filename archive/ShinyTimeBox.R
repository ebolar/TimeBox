# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# V 0.1 - MVP useful application
# -------------------------------------------------------------------
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
    "Time Box", selected = "Tasks", 
  
    # Panels

    # ===================================================================
    # Ideas
    # ===================================================================
    tabPanel(
      "Brain dump", 

      sidebarLayout(
        sidebarPanel(
          textOutput('IdeaStuff'),
          verbatimTextOutput('Display.IdeaSelected'),
          div(
            id="Idea.Frame",
            textInput("Idea.Name", "Name", 
              placeholder = "A name for your brainwave"),
            textAreaInput("Idea.Notes","Notes",
              rows = 10, 
              placeholder = "Come come elucidate your thoughts"),
            selectInput("Idea.Bucket", "Assign to:", choices = NULL)
          ),
          actionButton("New.IdeaButton", "Add"),
          actionButton("Update.IdeaButton", "Update"),
          actionButton("Delete.IdeaButton", "Delete"),
          actionButton("Reset.IdeaButton", "Reset")
        ),
        mainPanel(
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
          verbatimTextOutput('Display.BucketSelected'),
          div(
            id="Bucket.Frame",
            textInput("Bucket.Name", "Name", placeholder = "Group name"),
            textAreaInput("Bucket.Description","Description",
              rows = 2, 
              placeholder = "Short description"),
	    textInput("Bucket.ChargeTo", "Project", 
              placeholder = "Project code for timesheeting")
          ),
          actionButton("New.BucketButton", "Add"),
          actionButton("Update.BucketButton", "Update"),
          actionButton("Delete.BucketButton", "Delete"),
          actionButton("Reset.BucketButton", "Reset")
        ),
        mainPanel(
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
          verbatimTextOutput('Display.TaskSelected'),
          div(
            id="Task.Frame",
            textInput("Task.Name", "Name", placeholder = "Task name"),
            textAreaInput("Task.Description","Description",
              rows = 10, 
              placeholder = "Description and sub-tasks"),
            selectInput("Task.Bucket", "Assign to:", choices = NULL),
	    textInput("Task.Status", "Status")
	    # Task.Today
          ),
          actionButton("New.TaskButton", "Add"),
          actionButton("Update.TaskButton", "Update"),
          actionButton("Delete.TaskButton", "Delete"),
          actionButton("Reset.TaskButton", "Reset")
	),
        mainPanel(
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
        sidebarPanel(
          textOutput('WorkStuff'),
          verbatimTextOutput('Display.WorkSelected')
        ),
        mainPanel(
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
  cat("TimeBox V0.1\n")
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
      Name=input$Idea.Name, 
      Notes=input$Idea.Notes,
      Bucket=input$Idea.Bucket
    )
  })

  Selected.Idea <- reactive({
    input$Display.Idea_rows_selected
  })

  Reset.Idea <- function(bucket="-") {
    # Initialise the bucket list
    updateSelectInput(session, "Idea.Bucket", 
      choices = {
        c("-", sort(unique(Datasource.Read(Env, "Bucket")$Name)))
      },
      selected = "-"
    )
    
    reset("Idea.Frame")
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

    selected <- Selected.Idea()

    if (length(selected) == 1) {
      Datasource.Read(Env, "Idea") -> rows
      
      input$Idea.Name -> rows[selected, "Name"]
      input$Idea.Notes -> rows[selected, "Notes"]
      input$Idea.Bucket -> rows[selected, "Bucket"]

      if (Env$traceUI) {
        cat(sprintf("XXX - Updating row %d with\n", selected))
        print(rows[selected,])
      }

      Datasource.Replace(Env, "Idea", rows)
      Reset.Idea()
    }
  })
  
  observeEvent(input$Delete.IdeaButton, {
    if (Env$traceUI) cat(sprintf("XXX - Delete.IdeaButton - %d\n", input$Delete.IdeaButton))
    
    selected <- Selected.Idea()

    if (length(selected) > 0) {
      # better to read this directly from the table.
      Datasource.Read(Env, "Idea") -> rows
      
      if (Env$traceUI) {
        cat(sprintf("XXX - Removing %d rows\n", length(selected)))
        print(rows[selected,])
      }

      rows[-selected,] -> rows
      Datasource.Replace(Env, "Idea", rows)
    }

    Reset.Idea()
  })
  
  observeEvent(input$Reset.IdeaButton, {
    if (Env$traceUI) cat(sprintf("XXX - Reset.IdeaButton - %d\n", input$Reset.IdeaButton))
    Reset.Idea()
  })

  observeEvent(input$Display.Idea_rows_selected, {
    selected <- Selected.Idea()
    Datasource.Read(Env, "Idea") -> rows

    if (Env$traceUI) {
      cat(sprintf("XXX - Display.Idea_rows_selected (%d): ", length(selected)))
      if (length(selected) > 0) {
        cat(selected, sep = ", ")
      }
      cat("\n")

      print(rows[selected,])
    }

    # Populate the entry fields for update
    if (length(selected) == 1) {
      updateTextInput(session, "Idea.Name", value = rows[selected, "Name"])
      updateTextAreaInput(session, "Idea.Notes", value = rows[selected, "Notes"])
      updateSelectInput(session, "Idea.Bucket", selected = rows[selected, "Bucket"])
    } else Reset.Idea()
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Idea <- DT::renderDataTable({
    input$New.IdeaButton
    input$Delete.IdeaButton
    input$Update.IdeaButton
    input$Reset.IdeaButton
    Reset.Idea()
    format.Idea(Datasource.Read(Env, "Idea"))
  }, server = TRUE, filter = "top")

  output$Display.IdeaSelected <- renderPrint({
    selected <- Selected.Idea()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    } else Reset.Idea()
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
      Name=input$Bucket.Name, 
      Description=input$Bucket.Description,
      ChargeTo=input$Bucket.ChargeTo
    )
  })

  Selected.Bucket <- reactive({
    input$Display.Bucket_rows_selected
  })

  Reset.Bucket <- function(bucket="-") {
    reset("Bucket.Frame")
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

    selected <- Selected.Bucket()

    if (length(selected) == 1) {
      Datasource.Read(Env, "Bucket") -> rows
      
      input$Bucket.Name -> rows[selected, "Name"]
      input$Bucket.Description -> rows[selected, "Description"]
      input$Bucket.ChargeTo -> rows[selected, "ChargeTo"]

      if (Env$traceUI) {
        cat(sprintf("XXX - Updating row %d with\n", selected))
        print(rows[selected,])
      }

      Datasource.Replace(Env, "Bucket", rows)
      Reset.Bucket()
    }
  })
  
  observeEvent(input$Delete.BucketButton, {
    if (Env$traceUI) cat(sprintf("XXX - Delete.BucketButton - %d\n", input$Delete.BucketButton))
    
    selected <- Selected.Bucket()

    if (length(selected) > 0) {
      # better to read this directly from the table.
      Datasource.Read(Env, "Bucket") -> rows
      
      if (Env$traceUI) {
        cat(sprintf("XXX - Removing %d rows\n", length(selected)))
        print(rows[selected,])
      }

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

  observeEvent(input$Display.Bucket_rows_selected, {
    selected <- Selected.Bucket()
    Datasource.Read(Env, "Bucket") -> rows

    if (Env$traceUI) {
      cat(sprintf("XXX - Display.Bucket_rows_selected (%d): ", length(selected)))
      if (length(selected) > 0) {
        cat(selected, sep = ", ")
      }
      cat("\n")

      print(rows[selected,])
    }

    # Populate the entry fields for update
    if (length(selected) == 1) {
      updateTextInput(session, "Bucket.Name", value = rows[selected, "Name"])
      updateTextAreaInput(session, "Bucket.Description", value = rows[selected, "Description"])
      updateTextInput(session, "Bucket.ChargeTo", value = rows[selected, "ChargeTo"])
    } else Reset.Bucket()
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Bucket <- DT::renderDataTable({
    input$New.BucketButton
    input$Delete.BucketButton
    input$Update.BucketButton
    input$Reset.BucketButton
    Reset.Bucket()
    format.Bucket(Datasource.Read(Env, "Bucket"))
  }, server = TRUE, filter = "top")

  output$Display.BucketSelected <- renderPrint({
    selected <- Selected.Bucket()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    } else Reset.Bucket()
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
      Name=input$Task.Name, 
      Description=input$Task.Description,
      Bucket=input$Task.Bucket,
      Status=input$Task.Status
      # Task.Today
    )
  })

  Selected.Task <- reactive({
    input$Display.Task_rows_selected
  })

  Reset.Task <- function(bucket="-") {
    # Initialise the bucket list
    updateSelectInput(session, "Task.Bucket", 
      choices = {
        c("-", sort(unique(Datasource.Read(Env, "Bucket")$Name)))
      },
      selected = "-"
    )
    
    reset("Task.Frame")
  }

  # ------------------------------------------------
  # Events
  # ------------------------------------------------
  #
  observeEvent(input$New.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - New.TaskButton - %d\n", input$New.TaskButton))
    print(New.Task())
    print(Datasource.Read(Env, "Task"))

    Datasource.Add(Env, "Task", New.Task())
    Reset.Task()
  })

  observeEvent(input$Update.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - Update.TaskButton - %d\n", input$Update.TaskButton))

    selected <- Selected.Task()

    if (length(selected) == 1) {
      Datasource.Read(Env, "Task") -> rows
      
      input$Task.Name -> rows[selected, "Name"]
      input$Task.Description -> rows[selected, "Description"]
      input$Task.Bucket -> rows[selected, "Bucket"]
      input$Task.Status -> rows[selected, "Status"]
      # Task.Today

      if (Env$traceUI) {
        cat(sprintf("XXX - Updating row %d with\n", selected))
        print(rows[selected,])
      }

      Datasource.Replace(Env, "Task", rows)
      Reset.Task()
    }
  })
  
  observeEvent(input$Delete.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - Delete.TaskButton - %d\n", input$Delete.TaskButton))
    
    selected <- Selected.Task()

    if (length(selected) > 0) {
      # better to read this directly from the table.
      Datasource.Read(Env, "Task") -> rows
      
      if (Env$traceUI) {
        cat(sprintf("XXX - Removing %d rows\n", length(selected)))
        print(rows[selected,])
      }

      rows[-selected,] -> rows
      Datasource.Replace(Env, "Task", rows)
    }

    Reset.Task()
  })
  
  observeEvent(input$Reset.TaskButton, {
    if (Env$traceUI) cat(sprintf("XXX - Reset.TaskButton - %d\n", input$Reset.TaskButton))
    Reset.Task()
  })

  observeEvent(input$Display.Task_rows_selected, {
    selected <- Selected.Task()
    Datasource.Read(Env, "Task") -> rows

    if (Env$traceUI) {
      cat(sprintf("XXX - Display.Task_rows_selected (%d): ", length(selected)))
      if (length(selected) > 0) {
        cat(selected, sep = ", ")
      }
      cat("\n")

      print(rows[selected,])
    }

    # Populate the entry fields for update
    if (length(selected) == 1) {
      updateTextInput(session, "Task.Name", value = rows[selected, "Name"])
      updateTextAreaInput(session, "Task.Description", value = rows[selected, "Description"])
      updateSelectInput(session, "Task.Bucket", selected = rows[selected, "Bucket"])
      updateTextInput(session, "Task.Status", value = rows[selected, "Status"])
    } else Reset.Task()
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Task <- DT::renderDataTable({
    input$New.TaskButton
    input$Delete.TaskButton
    input$Update.TaskButton
    input$Reset.TaskButton
    Reset.Task()
    format.Task(Datasource.Read(Env, "Task"))
  }, server = TRUE, filter = "top")

  output$Display.TaskSelected <- renderPrint({
    selected <- Selected.Task()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    } else Reset.Task()
  })


  # ===================================================================
  # Work
  # ===================================================================
  # Helpers
  # ------------------------------------------------
  #

  Selected.Work <- reactive({
    input$Display.Work_rows_selected
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Work <- DT::renderDataTable({
    # Reset.Work()
    format.Work(Datasource.Read(Env, "Work"))
  }, server = TRUE, filter = "top")

  output$Display.WorkSelected <- renderPrint({
    selected <- Selected.Work()
    if (length(selected) > 0) {
      cat("Selected rows: ")
      cat(selected, sep = ", ")
    } # else Reset.Work()
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
    Datasource.Sync(Env, paste("Archive",Sys.Date(), sep = "-"))
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

