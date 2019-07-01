# ------------------------------
# ShinyDataAccess.R
# ------------------------------
# Accessing an external data source from Shiny
#
# I think this works because of the fundamental difference between Observers and Reactive elements
# - Observers are not called during creation, Reactives are.
# - Observers are only called explicitly, eg when an event occurs.  Contents are already isolated.
#
# Questions
# Q. How do you avoid race conditions with the reading and updating of external data
#    A. Observers are called explicitly whereas reactions use lazy evaluation.
#      o Write from an Observer and read from a Reaction.  
#      o Use a reaction to aggregate data from a form.
# Q. Why is the reactive() FormData not called when information in the form 
#    is updated?  Surely this would require an isolate()?
#    A. It is because the FormData output is not on a displayed.  I am assuming the flush() 
#       graph starts at the UI output values.
# Q. Why does the actionButton not trigger observeEvent when it is created?
#    A. I can only guess that Observers are not called during creation, 
#       Only reactive components are called during creation.
# Q. What is triggering the initial output$responses?  
#    - Is it the UI page or the creation of the actionButton?
#    - Why is this not triggered twice?
#      o When created, everything is marked invalid.  
#        On the first call, the output$responses executes and is marked valid
#        On the second call, output$responses just returns its value
# Q. How do you identify if a reactive() is valid?  
#    How do you know if you ran it for the first time?

library("shiny")

# Define the fields we want to save from the form
fields <- c("name", "used_shiny", "r_num_years")

saveData <- function(data) {
  data <- as.data.frame(t(data))

  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    titlePanel("Example"),
    hr(),

    textInput("name", "Name", ""),
    checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    sliderInput("r_num_years", "Number of years using R",
                0, 25, 2, ticks = FALSE),
    actionButton("submit", "Submit"),
    
    hr(),
    dataTableOutput("responses"), tags$hr()
    
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    # Use a reactive value to aggregate the form data.  
    # Data cache is not reactive
    formData <- reactive({
      print("XXX formData")
      print(paste("... ", input$name, input$used_shiny, input$r_num_years, sep="-"))
      data <- sapply(fields, function(x) input[[x]])
      data
    })

    buttonPusher <- reactive({
      print(paste("XXX buttonPusher: ", input$submit))
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      print(paste("XXX submit button pressed: ", input$submit))
      saveData(formData())
    })

    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- renderDataTable({
      print("XXX rendering table")
      input$submit
      buttonPusher()
      loadData()
    })     
  }
)
