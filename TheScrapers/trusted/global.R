#Deutsche Umlaute einlesen
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#Libraries einlesen
library("rvest")
library("tidyverse")

scraper <- function(seitenzahl,seite) {
  
  url <- paste0("https://de.trustpilot.com/review/www.",seite,"?languages=all&page=",seitenzahl)
  
  website <- url %>%
    read_html()
  
  progress <- seitenzahl
  cat(progress)
  
  #########################################################################
  
  #Auslesen Zeichen/Text
  data_html <- website %>%
    html_nodes(.,".review-info__body__text") %>%
    html_text()
  
  for(i in 1:length(data_html)){
    data_html[i] <-  
      data_html[i] %>%
      gsub("\n *(.*)\n *","\\1", .) %>%
      nchar()
  }
  
  data_html <- data_html %>% as.numeric() 
  
  #########################################################################
  
  #Auslesen der Referenzen
  data_reviewcount <- website %>%
    html_nodes(.,".consumer-info__details__review-count") %>%
    html_text() %>%
    gsub(".*?([[:digit:]]+).*","\\1", .) %>%
    as.numeric()
  
  #########################################################################
  
  #Sterne Bewertung
  data_stars_raw <- website %>%
    html_nodes(.,".review-info__header__verified")
  
  data_stars <- c()
  
  for(i in 1:length(data_stars_raw)){
    data_stars[i] <- data_stars_raw[[i]] %>%
      html_nodes("div") %>% 
      html_attr("class") %>% 
      head(1) %>%
      gsub(".*([1-5]).*","\\1", .) %>%
      as.numeric()
  }
  
  #########################################################################
  
  #Zeiten
  data_time <- website %>%
    html_nodes(.,".header__verified__date") %>%
    html_nodes("time:not(.updatedDate)") %>% 
    html_attr("datetime") %>%
    as.Date()
  
  #########################################################################
  
  #Zusammenfügen zum Dataframe
  data <- data.frame(Sterne=data_stars, Zeichenzahl=data_html, Referenzen=data_reviewcount, Datum=data_time)
  
  return(data)
}