
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  mainPanel(
    textInput("page", "Seite eingeben:"),
    plotOutput("stars")
    )
  )
)
