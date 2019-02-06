ui <- fluidPage(
  sliderInput("obs", "Number of observations:", animate=TRUE,
              min = 1, max = 100, value = 1, step = 5, 
  ),
  plotOutput("distPlot")
)

# Server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
}

# Complete app with UI and server components
shinyApp(ui, server)