library(shiny)
library(shinyjs)

ui <- tagList(
  useShinyjs(),
  navbarPage(
    "shinyjs with navbarPage",
    tabPanel(
      "tab1",
      actionButton("button", "Click me"),
      div(id = "hello", "Hello!")
    ),
    tabPanel("tab2",
      sidebarLayout(
        sidebarPanel(
          textInput("The.Stuff", "Give me some good stuff"),
          actionButton("Go", "GoGo"),
          actionButton("Reset", "Reset")),
        mainPanel(
          h2("The good stuff!"),
          textOutput("GoodStuff"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$button, {
    toggle("hello")
  })
  
  output$GoodStuff <- renderText({
    "" -> MyStuff
    if(input$Go > 0)
      isolate(MyStuff <- input$The.Stuff)
    reset("The.Stuff")
    MyStuff
  })
  
  observeEvent(input$Reset, {
    print("XXX - Reset my stuff")
    # Nb: Only resets inputs
    # How do you reset outputs (if you need to)?
    reset("The.Stuff")
  })
}

shinyApp(ui, server)
