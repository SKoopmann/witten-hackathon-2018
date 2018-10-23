
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  max_page <- reactive({
    
    page <- isolate(input$page)

    url <- paste0("https://de.trustpilot.com/review/www.", page, "?languages=all")
  
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
    
    max_page
  })
  
  kroenung <- reactive({
    page <- isolate(input$page)
    input$los
    kroenung <- map_df(1:max_page(), scraper, seite = page)
    kroenung
  })
  
  #Grafiken
  
  #Sterne im Zeitverlauf
  output$stars <- renderPlot({
    averages_daily <- kroenung() %>%
      group_by(Datum) %>%
      summarize(Sterne = mean(Sterne), Zeichenzahl = mean(Zeichenzahl), Referenzen = mean(Referenzen))
    
    ggplot(averages_daily, aes(Datum, Sterne)) + 
      geom_smooth() +
      ylab("Sterne") + 
      xlab("") + 
      ggtitle("Sterne im Zeitverlauf")
    
  })
  #Zeichenzahl je Bewertung im Zeitverlauf 
  output$charactercount <- renderPlot({
    averages_daily <- kroenung() %>%
      group_by(Datum) %>%
      summarize(Sterne = mean(Sterne), Zeichenzahl = mean(Zeichenzahl), Referenzen = mean(Referenzen))
    
    ggplot(averages_daily, aes(Datum, Zeichenzahl)) + 
      geom_smooth() + 
      ylab("Zeichenzahl") + 
      xlab("") + 
      ggtitle("Zeichenzahl je Bewertung im Zeitverlauf")
  })
  
  #Referenzen je Bewerter im Zeitverlauf 
  output$references <- renderPlot({
    averages_daily <- kroenung() %>%
      group_by(Datum) %>%
      summarize(Sterne = mean(Sterne), Zeichenzahl = mean(Zeichenzahl), Referenzen = mean(Referenzen))
    
    ggplot(averages_daily, aes(Datum, Referenzen)) + 
      geom_smooth() + 
      ylab("Referenzen") + 
      xlab("") + 
      ggtitle("Referenzen je Bewerter im Zeitverlauf")
  })  
  
  
  #Durchschnittliche Zeichenzahl je Sterne-Bewertung
  output$average_characters <- renderPlot({
    averages_per_star_rating <- kroenung() %>%
      group_by(Sterne) %>%
      summarize(Zeichenzahl = mean(Zeichenzahl), Referenzen = mean(Referenzen)) %>%
      round(.,digits = 0)
    
    ggplot(averages_per_star_rating, aes(Sterne, Zeichenzahl)) + 
      geom_bar(stat="identity") + 
      ggtitle("Durchschnittliche Zeichenzahl je Sterne-Bewertung")
  })  
  
  #Durchschnittliche Referenzen je Sterne-Bewertung
  output$average_references <- renderPlot({
    averages_per_star_rating <- kroenung() %>%
      group_by(Sterne) %>%
      summarize(Zeichenzahl = mean(Zeichenzahl),Referenzen = mean(Referenzen)) %>%
      round(.,digits = 0)
    
    ggplot(averages_per_star_rating, aes(Sterne, Referenzen)) + 
      geom_bar(stat="identity") + 
      ggtitle("Durchschnittliche Referenzen je Sterne-Bewertung")
  })  
  
})
