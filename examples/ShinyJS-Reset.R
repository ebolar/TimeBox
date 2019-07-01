# An example of the ShinyJS reset functionality

shinyApp(
  ui = tagList(
    useShinyjs(),
    navbarPage(
    div(
      id = "form",
      textInput("name", "Name", "Dean"),
      radioButtons("gender", "Gender", c("Male", "Female")),
      selectInput("letter", "Favourite letter", LETTERS)
    ),
    actionButton("resetAll", "Reset all"),
    actionButton("resetName", "Reset name"),
    actionButton("resetGender", "Reset Gender"),
    actionButton("resetLetter", "Reset letter")
  )),
  server = function(input, output) {
    observeEvent(input$resetName, {
      reset("name")
    })
    observeEvent(input$resetGender, {
      reset("gender")
    })
    observeEvent(input$resetLetter, {
      reset("letter")
    })
    observeEvent(input$resetAll, {
      reset("form")
    })
  }
)
