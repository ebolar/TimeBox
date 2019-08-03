# ===================================================================
# Time Box - planner and task tracker
# ===================================================================
# ui.R
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
# TBD:
# Multiuser capability
# - Display username
# - Save and exit button
# - Login/logout
# - Project teams and private information on each tab
# 
# Title bar with captions, status and controls.  May require restyling from navbarPage to tabPage.
#
# UI improvements
# - Doubleclick to update/add an idea/bucket/task/work
# - Collapse/drilldown the tables
# - Sort, order and filter tables
#

# Define UI 
TimeBoxUI <- tagList(
  useShinyjs(),	# Load ShinyJS routines for improved UI capabilities.
  log_init(),	# Enable logging to the browser JS console.

  # ===================================================================
  # Title bar
  # ===================================================================


  # ===================================================================
  # Content
  # ===================================================================
  navbarPage(
    sprintf("Time Box %s [%s]", TimeBox.Version, TimeBox.Username), selected = "Tasks", 
  
    # Panels

    # -------------------------------------------------------------------
    # Ideas
    # -------------------------------------------------------------------
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

    # -------------------------------------------------------------------
    # Buckets
    # -------------------------------------------------------------------
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

    # -------------------------------------------------------------------
    # Tasks
    # -------------------------------------------------------------------
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

    # -------------------------------------------------------------------
    # Work
    # -------------------------------------------------------------------
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

    # -------------------------------------------------------------------
    # More stuff
    # -------------------------------------------------------------------
    tabPanel(
      "...",
      actionButton("Save.all", "Save"),
      actionButton("Archive.all", "Archive")
    )
  )
)

