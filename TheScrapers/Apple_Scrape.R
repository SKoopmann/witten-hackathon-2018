#Umgebung säubern
rm(list = ls())

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
#Ende Funktion Scraper

  #########################################################################

#Initialisierung Input-Daten
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

#Dataframe der Formel aus Seitenzahl von bis, Scraper Funktion und Internetseite
kroenung <- map_df(1:max_page, scraper, seite = page)

#########################################################################

#Durchschnittswerte
averages_overall <- kroenung[,1:3] %>%
  colMeans() %>%
  round(.,digits = 2)

averages_overall[2:3] <- averages_overall[2:3] %>%
  round(.,digits = 0)

averages_daily <- kroenung %>%
  group_by(Datum) %>%
  summarize(Sterne = mean(Sterne), Zeichenzahl = mean(Zeichenzahl), Referenzen = mean(Referenzen))

#Durchschnittliche Bewertungen je Sterne-Bewertung
averages_per_star_rating <- kroenung %>%
  group_by(Sterne) %>%
  summarize(Zeichenzahl = mean(Zeichenzahl),Referenzen = mean(Referenzen)) %>%
  round(.,digits = 0)

#########################################################################

#Grafiken
#Sterne im Zeitverlauf 
ggplot(averages_daily, aes(Datum, Sterne)) + 
  geom_smooth() +
  ylab("Sterne") + 
  xlab("") + 
  ggtitle("Sterne im Zeitverlauf")

#Zeichenzahl im Zeitverlauf 
ggplot(averages_daily, aes(Datum, Zeichenzahl)) + 
  geom_smooth() + 
  ylab("Zeichenzahl") + 
  xlab("") + 
  ggtitle("Zeichenzahl je Bewertung im Zeitverlauf")

#Referenzen im Zeitverlauf 
ggplot(averages_daily, aes(Datum, Referenzen)) + 
  geom_smooth() + 
  ylab("Referenzen") + 
  xlab("") + 
  ggtitle("Referenzen je Bewerter im Zeitverlauf")

#Durchschnittliche Zeichenzahl je Sterne-Bewertung
ggplot(averages_per_star_rating, aes(Sterne, Zeichenzahl)) + 
  geom_bar(stat="identity") + 
  ggtitle("Durchschnittliche Zeichenzahl je Sterne-Bewertung")

#Durchschnittliche Referenzen je Sterne-Bewertung
ggplot(averages_per_star_rating, aes(Sterne, Referenzen)) + 
  geom_bar(stat="identity") + 
  ggtitle("Durchschnittliche Referenzen je Sterne-Bewertung")

#########################################################################

