library(shiny)

# Read the survey questions
Qlist <- read.csv("Qlist.csv")
# Qlist <- Qlist[1,]
shinyServer(function(input, output) {
  
  # You can access the values of the widget (as a vector)
  # with input$radio, e.g.
  output$value <- renderPrint({ input$survey })
  
})