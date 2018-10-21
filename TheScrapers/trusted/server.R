
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  max_page <- reactive({
    url <- paste0("https://de.trustpilot.com/review/www.", input$page, "?languages=all")
  
    max_page <- url %>%
      read_html() %>%
      html_nodes(.,".pagination-page") %>%
      html_attr("data-page-number") %>%
      head(-1) %>%
      as.numeric() %>%
      max()
    
    if (max_page == -Inf) {
      max_page <- 1
    }
  })
  
  kroenung <- reactive({
    kroenung <- map_df(1:max_page(), scraper, seite = input$page)
    kroenung
  })
  
  output$stars <- renderPlot({
    averages_daily <- kroenung() %>%
      group_by(Datum) %>%
      summarize(Sterne = mean(Sterne), Zeichenzahl = mean(Zeichenzahl), Referenzen = mean(Referenzen))
    
    #Grafiken
    #Sterne im Zeitverlauf 
    ggplot(averages_daily, aes(Datum, Sterne)) + 
      geom_smooth() +
      ylab("Sterne") + 
      xlab("") + 
      ggtitle("Sterne im Zeitverlauf")
    
  })
  
})
