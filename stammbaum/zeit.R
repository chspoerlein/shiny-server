## zeit

library(tidyverse)
library(lubridate)
library(gganimate)
library(ggrepel)

setwd("C:/Users/Christoph/Google Drive/misc/STAMMBAUM/stammbaum")
      
zeit <- read.csv("historisches.csv", sep=";", stringsAsFactors=FALSE) 
zeit$beginn <- dmy(zeit$beginn)
zeit$ende <- dmy(zeit$ende)

input <- as.data.frame(rep(NA,2), ncol=1)
input$direkt <- 1
input$zeit[1] <- 1500
input$zeit[2] <- 2019

people <- read.csv("personen_prep.csv", sep=";", stringsAsFactors=FALSE) %>% 
  filter(start!="", (end!="" | newbirthdate>=1900)) %>% 
  filter(newbirthdate>=input$zeit[1] & newbirthdate<=input$zeit[2]) %>%
  filter(direkte==input$direkt[1]) %>%
  mutate(end2=replace(end2, is.na(end2), "2019-01-01")) %>%
  arrange(start2) %>%
  mutate(id=seq(1,nrow(people),1)) 


ggplot() + theme_minimal() + 
  geom_segment(data=people, aes(y = year(start2), x = as.factor(id), yend = year(end2), xend = as.factor(id))) + 
  scale_x_discrete(labels=people$name) +
  ylab("") + 
  xlab("") + 
  ylim(input$zeit[1],input$zeit[2]) +
  geom_rect(aes(ymin = year(zeit$beginn) , xmin = -Inf, ymax = year(zeit$ende), xmax = Inf, fill=zeit$level, color=zeit$level), alpha=.5, size=2) +
  theme(legend.position = "None", axis.text.x = element_text(angle = 90, hjust = 0)) +
  geom_label_repel(aes(x = as.character(length(unique(people$id))), 
                       y = year(zeit$beginn)+floor((year(zeit$ende)-year(zeit$beginn))/2), 
                       label = zeit$ereignis),
                   force=100,
                   hjust=0,
                   vjust=0,
                   direction="both")


  
  
  
## animation irgendwann
## 100 Jahr fenster über die Zeit verschieben
## 1 1530-1630
## 2 1540-1640
## 3 1550-1650
## etc.
