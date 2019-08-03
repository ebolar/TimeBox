# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# server.R
#
# V 0.1 - MVP useful application
# V 0.2 - Split into global.R, server.R and ui.R to make maintenance easier
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
# TBD
#   UI improvements
#   - Doubleclick to update/add an idea/bucket/task/work
#   - Collapse/drilldown the tables
#   - Sort, order and filter tables
#
#   Use the shiny logging functions
# 

TimeBoxServer <- function(input, output, session) {
  # ===================================================================
  # Setup 
  # ===================================================================
  #
  set_logging_session()

  log_event("===================================================================", type="Harness")
  log_event(sprintf("TimeBox %s [%s], %s, %s", TimeBox.Version, TimeBox.Username, TimeBox.Fullname, TimeBox.email), type="Harness")
  log_event("XXX-Opening datasoure", type="Harness")
  
  Configuration.Init() -> Env
  FALSE -> Env$traceData
  TRUE -> Env$traceUI
  
  Datasource.Init(Env)

  # ===================================================================
  # Static content
  # ===================================================================
  #
  # output$Display.Version <- renderPrint(cat("TimeBox V0.2"))
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
    if (Env$traceUI) log_event("XXX - New.Idea - extraction", type="Idea")
    
    Idea(
      Name=input$Idea.Name, 
      Notes=input$Idea.Notes,
      Bucket=input$Idea.Bucket
    )
  })

  Selected.Idea <- reactive({
    if (Env$traceUI) log_event("XXX - Selected.Idea", type="Idea")
    
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
    if (Env$traceUI) log_event(sprintf("XXX - New.IdeaButton - %d", input$New.IdeaButton), type="Idea")

    Datasource.Add(Env, "Idea", New.Idea())
    Reset.Idea()
  })

  observeEvent(input$Update.IdeaButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Update.IdeaButton - %d", input$Update.IdeaButton), type="Idea")

    selected <- Selected.Idea()

    if (length(selected) == 1) {
      Datasource.Read(Env, "Idea") -> rows
      
      input$Idea.Name -> rows[selected, "Name"]
      input$Idea.Notes -> rows[selected, "Notes"]
      input$Idea.Bucket -> rows[selected, "Bucket"]

      if (Env$traceUI) {
        log_event(sprintf("XXX - Updating row %d", selected), type="Idea")
        log_output(print(rows[selected,]), type="Idea")
      }

      Datasource.Replace(Env, "Idea", rows)
      Reset.Idea()
    }
  })
  
  observeEvent(input$Delete.IdeaButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Delete.IdeaButton - %d", input$Delete.IdeaButton), type="Idea")
    
    selected <- Selected.Idea()

    if (length(selected) > 0) {
      # better to read this directly from the table.
      Datasource.Read(Env, "Idea") -> rows
      
      if (Env$traceUI) {
        log_event(sprintf("XXX - Removing %d rows", length(selected)), type="Idea")
        log_output(print(rows[selected,]), type="Idea")
      }

      rows[-selected,] -> rows
      Datasource.Replace(Env, "Idea", rows)
    }

    Reset.Idea()
  })
  
  observeEvent(input$Reset.IdeaButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Reset.IdeaButton - %d", input$Reset.IdeaButton), type="Idea")
    Reset.Idea()
  })

  observeEvent(input$Display.Idea_rows_selected, {
    selected <- Selected.Idea()
    Datasource.Read(Env, "Idea") -> rows

    if (Env$traceUI) {
      log_event(sprintf("XXX - Display.Idea_rows_selected (%d): ", length(selected)), type="Idea")
      if (length(selected) > 0) {
        log_output(cat(selected, sep = ", "), type="Idea")
      }

      log_output(print(rows[selected,]), type="Idea")
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
    if (Env$traceUI) log_event("XXX - Display.Idea(s)", type="Idea")

    input$New.IdeaButton
    input$Delete.IdeaButton
    input$Update.IdeaButton
    input$Reset.IdeaButton
    Reset.Idea()
    format.Idea(Datasource.Read(Env, "Idea"))
  }, server = TRUE, filter = "top")

  output$Display.IdeaSelected <- renderPrint({
    if (Env$traceUI) log_event("XXX - Display.IdeaSelected", type="Idea")

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
    if (Env$traceUI) log_event("XXX - New.Bucket - extraction", type="Bucket")
    
    Bucket(
      Name=input$Bucket.Name, 
      Description=input$Bucket.Description,
      ChargeTo=input$Bucket.ChargeTo
    )
  })

  Selected.Bucket <- reactive({
    if (Env$traceUI) log_event("XXX - Selected.Bucket", type="Bucket")

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
    if (Env$traceUI) log_event(sprintf("XXX - New.BucketButton - %d", input$New.BucketButton), type="Bucket")

    Datasource.Add(Env, "Bucket", New.Bucket())
    Reset.Idea()
    Reset.Bucket()
    Reset.Task()
  })

  observeEvent(input$Update.BucketButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Update.BucketButton - %d", input$Update.BucketButton), type="Bucket")

    selected <- Selected.Bucket()

    if (length(selected) == 1) {
      Datasource.Read(Env, "Bucket") -> rows
      
      input$Bucket.Name -> rows[selected, "Name"]
      input$Bucket.Description -> rows[selected, "Description"]
      input$Bucket.ChargeTo -> rows[selected, "ChargeTo"]

      if (Env$traceUI) {
        log_event(sprintf("XXX - Updating row %d with", selected), type="Bucket")
        log_output(print(rows[selected,]), type="Bucket")
      }

      Datasource.Replace(Env, "Bucket", rows)
      Reset.Bucket()
    }
  })
  
  observeEvent(input$Delete.BucketButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Delete.BucketButton - %d", input$Delete.BucketButton), type="Bucket")
    
    selected <- Selected.Bucket()

    if (length(selected) > 0) {
      # better to read this directly from the table.
      Datasource.Read(Env, "Bucket") -> rows
      
      if (Env$traceUI) {
        log_event(sprintf("XXX - Removing %d rows", length(selected)), type="Bucket")
        log_output(print(rows[selected,]), type="Bucket")
      }

      rows[-selected,] -> rows
      Datasource.Replace(Env, "Bucket", rows)
    }

    Reset.Idea()
    Reset.Bucket()
    Reset.Task()
  })
  
  observeEvent(input$Reset.BucketButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Reset.BucketButton - %d", input$Reset.BucketButton), type="Bucket")
    Reset.Bucket()
  })

  observeEvent(input$Display.Bucket_rows_selected, {
    selected <- Selected.Bucket()
    Datasource.Read(Env, "Bucket") -> rows

    if (Env$traceUI) {
      log_event(sprintf("XXX - Display.Bucket_rows_selected (%d): ", length(selected)), type="Bucket")
      if (length(selected) > 0) {
        log_output(cat(selected, sep = ", "), type="Bucket")
      }

      log_output(print(rows[selected,]), type="Bucket")
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
    if (Env$traceUI) log_event("XXX - Display.Bucket", type="Bucket")

    input$New.BucketButton
    input$Delete.BucketButton
    input$Update.BucketButton
    input$Reset.BucketButton
    Reset.Bucket()
    format.Bucket(Datasource.Read(Env, "Bucket"))
  }, server = TRUE, filter = "top")

  output$Display.BucketSelected <- renderPrint({
    if (Env$traceUI) log_event("XXX - Display.BucketSelected", type="Bucket")

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
    if (Env$traceUI) log_event("XXX - New.Task - extraction", type="Task")
    
    Task(
      Name=input$Task.Name, 
      Description=input$Task.Description,
      Bucket=input$Task.Bucket,
      Status=input$Task.Status
      # Task.Today
    )
  })

  Selected.Task <- reactive({
    if (Env$traceUI) log_event("XXX - Selected.Task", type="Task")

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
    if (Env$traceUI) log_event(sprintf("XXX - New.TaskButton - %d", input$New.TaskButton), type="Task")

    print(New.Task())
    print(Datasource.Read(Env, "Task"))

    Datasource.Add(Env, "Task", New.Task())
    Reset.Task()
  })

  observeEvent(input$Update.TaskButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Update.TaskButton - %d", input$Update.TaskButton), type="Task")

    selected <- Selected.Task()

    if (length(selected) == 1) {
      Datasource.Read(Env, "Task") -> rows
      
      input$Task.Name -> rows[selected, "Name"]
      input$Task.Description -> rows[selected, "Description"]
      input$Task.Bucket -> rows[selected, "Bucket"]
      input$Task.Status -> rows[selected, "Status"]
      # Task.Today

      if (Env$traceUI) {
        log_event(sprintf("XXX - Updating row %d with", selected), type="Task")
        log_output(print(rows[selected,]), type="Task")
      }

      Datasource.Replace(Env, "Task", rows)
      Reset.Task()
    }
    # else ignore - cannot update what is not selected
  })
  
  observeEvent(input$Delete.TaskButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Delete.TaskButton - %d", input$Delete.TaskButton), type="Task")
    
    selected <- Selected.Task()

    if (length(selected) > 0) {
      # better to read this directly from the table.
      Datasource.Read(Env, "Task") -> rows
      
      if (Env$traceUI) {
        log_event(sprintf("XXX - Removing %d rows", length(selected)), type="Task")
        log_output(print(rows[selected,]), type="Task")
      }

      rows[-selected,] -> rows
      Datasource.Replace(Env, "Task", rows)
    }

    Reset.Task()
  })
  
  observeEvent(input$Reset.TaskButton, {
    if (Env$traceUI) log_event(sprintf("XXX - Reset.TaskButton - %d", input$Reset.TaskButton), type="Task")

    Reset.Task()
  })

  observeEvent(input$Display.Task_rows_selected, {
    selected <- Selected.Task()
    Datasource.Read(Env, "Task") -> rows

    if (Env$traceUI) {
      log_event(sprintf("XXX - Display.Task_rows_selected (%d): ", length(selected)), type="Task")

      if (length(selected) > 0) {
        log_output(cat(selected, sep = ", "), type="Task")
      }

      log_output(rows[selected,], type="Task")
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
    if (Env$traceUI) log_event("XXX - Display.Task", type="Task")

    input$New.TaskButton
    input$Delete.TaskButton
    input$Update.TaskButton
    input$Reset.TaskButton
    Reset.Task()
    format.Task(Datasource.Read(Env, "Task"))
  }, server = TRUE, filter = "top")

  output$Display.TaskSelected <- renderPrint({
    if (Env$traceUI) log_event("XXX - Display.TaskSelected", type="Task")

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
    if (Env$traceUI) log_event("XXX - Selected.Work", type="Work")

    input$Display.Work_rows_selected
  })
  
  # ------------------------------------------------
  # Reactive updates
  # ------------------------------------------------
  #
  output$Display.Work <- DT::renderDataTable({
    if (Env$traceUI) log_event("XXX - Display.Work", type="Work")

    # Reset.Work()
    format.Work(Datasource.Read(Env, "Work"))
  }, server = TRUE, filter = "top")

  output$Display.WorkSelected <- renderPrint({
    if (Env$traceUI) log_event("XXX - Display.WorkSelected", type="Work")

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
    if (Env$traceUI) log_event(sprintf("XXX - Save.all button - %d", input$Save.all))

    Datasource.Sync(Env)
  })

  observeEvent(input$Archive.all, {
    if (Env$traceUI) log_event(sprintf("XXX - Archive.all button - %d", input$Archive.all))

    # Update the sync call to save to another source
    Datasource.Sync(Env, paste("Archive",Sys.Date(), sep = "-"))
  })


  # ===================================================================
  # Cleanup
  # ===================================================================
  #
  onStop(function(){
    log_event("XXX-Closing data source", type="Harness")
    Datasource.Close(Env)
  })
}


