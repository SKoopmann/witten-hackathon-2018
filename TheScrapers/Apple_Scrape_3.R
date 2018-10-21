#Umgebung säubern
rm(list = ls())

#Deutsche Umlaute einlesen
Sys.setlocale("LC_ALL", "en_US.UTF-8")

#Libraries einlesen
library("rvest")
library("tidyverse")



scraper <- function(seitenzahl,seite) {
  #Hier muss noch append eingefügt werden
  url <- paste0("https://de.trustpilot.com/review/www.",seite,"?languages=all&page=",seitenzahl)
  website<-url %>%
    read_html()
  
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
  
  #Auslesen der Anzahl Bewertungen der Bewerter
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
  
  #Zusammenfügen zum Dataframe
  data <- data.frame(Sterne=data_stars,Zeichenzahl=data_html,Referenzen=data_reviewcount)
  
  return(data)
}

page <- "apple.com" #Hier ändern
url <- paste0("https://de.trustpilot.com/review/www.",page,"?languages=all")
website<-url %>%
  read_html()

max_page <- website %>%
  html_nodes(.,".pagination-page")

max_page <- max_page %>%
    html_attr("data-page-number") %>%
    head(-1) %>%
    as.numeric() %>%
    max()

kroenung <- map_df(1:max_page, scraper, seite = page)