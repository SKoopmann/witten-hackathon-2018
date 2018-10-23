
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Testpilot Scrape Analysis"),
  
  mainPanel(
    textInput("page", "Seite eingeben:", value = "adblockplus.org"),
    actionButton("los", "Los!"),
    plotOutput("stars"),
    plotOutput("charactercount"),
    plotOutput("references"),
    plotOutput("average_characters"),
    plotOutput("average_references")
    )
  )
)
