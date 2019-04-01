
library(shiny)
library(visNetwork)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(ggwordcloud)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
library(DT)


ui <- navbarPage("Stammbaum Familie Spörlein",
                 tabPanel("Stammbaum", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("hierarch", 
                                           "Hierarchischer Stammbaum?", c("ja"=1,"nein"=0), 1),
                              selectInput("auswahl",
                                          "Stammbaum beschränken auf Nachfahren von", choices = list(
                                            "keine Einschränkung"=c(4),
                                            "Spörlein:"=c("Christoph"=1, "Wilhelm"=2, "Karola"=3),
                                            "Feigl:"=c("Eliana"=-1,"Ulrich"=-2,"Susanne"=-3))),
                              radioButtons("direkt", 
                                           "Nur direkte Nachfahren?", c("ja"=1,"nein"=0), 1),
                              sliderInput("zeit", "Geburtsjahr:",
                                          min = 1530, max = 2020, step=10, sep="",
                                          value = c(1840,2020)),
                              br(),
                              helpText(a(href="https://cspoerlein.com", target="_blank", "© Christoph Spörlein"))
                            ), 
                            mainPanel(
                              visNetworkOutput("network")
                            ))
                 ),
                 tabPanel("Regionale Herkunft", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("auswahl",
                                          "Stammbaum beschränken auf Nachfahren von", choices = list(
                                            "keine Einschränkung"=c(4),
                                            "Spörlein:"=c("Christoph"=1, "Wilhelm"=2, "Karola"=3),
                                            "Feigl:"=c("Eliana"=-1,"Ulrich"=-2,"Susanne"=-3))),
                              radioButtons("direkt", 
                                           "Nur direkte Nachfahren?", c("ja"=1,"nein"=0), 1),
                              sliderInput("zeit", "Geburtsjahr:",
                                          min = 1530, max = 2020, step=10, sep="",
                                          value = c(1840,2020)),
                              br(),
                              helpText(a(href="https://cspoerlein.com", target="_blank", "© Christoph Spörlein"))
                            ), 
                            mainPanel(
                              leafletOutput("herkunft")
                            ))
                 ),
                 tabPanel("Vornamen", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("auswahl",
                                          "Stammbaum beschränken auf Nachfahren von", choices = list(
                                            "keine Einschränkung"=c(4),
                                            "Spörlein:"=c("Christoph"=1, "Wilhelm"=2, "Karola"=3),
                                            "Feigl:"=c("Eliana"=-1,"Ulrich"=-2,"Susanne"=-3))),
                              radioButtons("direkt", 
                                           "Nur direkte Nachfahren?", c("ja"=1,"nein"=0), 1),
                              sliderInput("zeit", "Geburtsjahr:",
                                          min = 1530, max = 2020, step=10, sep="",
                                          value = c(1840,2020)),
                              br(),
                              helpText(a(href="https://cspoerlein.com", target="_blank", "© Christoph Spörlein"))
                            ), 
                            mainPanel(
                              plotOutput('table')
                            ))
                 ),
                 tabPanel("Historische Ereignisse", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("auswahl",
                                          "Stammbaum beschränken auf Nachfahren von", choices = list(
                                            "keine Einschränkung"=c(4),
                                            "Spörlein:"=c("Christoph"=1, "Wilhelm"=2, "Karola"=3),
                                            "Feigl:"=c("Eliana"=-1,"Ulrich"=-2,"Susanne"=-3))),
                              radioButtons("direkt", 
                                           "Nur direkte Nachfahren?", c("ja"=1,"nein"=0), 1),
                              sliderInput("zeit", "Geburtsjahr:",
                                          min = 1530, max = 2020, step=10, sep="",
                                          value = c(1840,2020)),
                              radioButtons("zeitebene", 
                                           "Historische Ereignisse:", c("alle"="alle",
                                                                        "international"="international",
                                                                        "national"="national",
                                                                        "regional"="regional"), "alle"),
                              br(),
                              helpText(a(href="https://cspoerlein.com", target="_blank", "© Christoph Spörlein"))
                            ), 
                            mainPanel(
                              plotOutput("historie")
                            ))
                 ),
                 tabPanel("Suche", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("auswahl",
                                          "Stammbaum beschränken auf Nachfahren von", choices = list(
                                            "keine Einschränkung"=c(4),
                                            "Spörlein:"=c("Christoph"=1, "Wilhelm"=2, "Karola"=3),
                                            "Feigl:"=c("Eliana"=-1,"Ulrich"=-2,"Susanne"=-3))),
                              radioButtons("direkt", 
                                           "Nur direkte Nachfahren?", c("ja"=1,"nein"=0), 1),
                              sliderInput("zeit", "Geburtsjahr:",
                                          min = 1530, max = 2020, step=10, sep="",
                                          value = c(1840,2020)),
                              br(),
                              helpText(a(href="https://cspoerlein.com", target="_blank", "© Christoph Spörlein"))
                            ), 
                            mainPanel(
                              dataTableOutput("suche")
                            ))
                 )
)


server <- function(input, output) {
   

  
   output$network <- renderVisNetwork({
     
     
     people <- read.csv("personen_prep.csv", sep=";", stringsAsFactors=FALSE) %>% 
       filter(newbirthdate>=input$zeit[1] & newbirthdate<=input$zeit[2]) %>%
       filter(direkte>=input$direkt)
     marriages <- read.csv("heiraten.csv", sep=";", stringsAsFactors=FALSE)
     births <- read.csv("geburten.csv", sep=";", stringsAsFactors=FALSE) %>% left_join(people, by=c("kind_new"="id")) %>%
       select(mutter_new,kind_new,start) %>% rename(bdate=start)
     
     if (input$auswahl==-1){
       people <- people %>% filter(vergleich<=-1)
     } else if (input$auswahl==-2){
       people <- people %>% filter(vergleich==-2)
     } else if (input$auswahl==-3){
       people <- people %>% filter(vergleich==-3)
     } else if (input$auswahl==1){
       people <- people %>% filter(vergleich>=1)
     } else if (input$auswahl==2){
       people <- people %>% filter(vergleich==2)
     } else if (input$auswahl==3){
       people <- people %>% filter(vergleich==3)
     } else {
       people <- people
     }
     
     
     if (input$hierarch==1){
       test <- TRUE
     } else {
       test <- FALSE
     }
     
     # converting dates
     births$bdate2 = dmy(births$bdate) 
     births <- births %>% rename(date=bdate2)
     labeldate <- 1
     labelplace <- 1
     labelocc <- 1
     nodes <- people %>% mutate(label=paste(name), label_long=paste(name,"\n",labeldate,"\n",labelplace,"\n",labelocc)) %>% 
       mutate(female=female+1)
     
     link1 <- births %>% rename(from=mutter_new, to=kind_new) %>% select(from,to)
     link2 <- births %>% left_join(marriages, by=c("mutter_new"="P2_new")) %>% select(kind_new, P1_new) %>%
       rename(from=P1_new, to=kind_new)
     links <- rbind(link1,link2)
     
     
     path_to_images <- "https://raw.githubusercontent.com/chspoerlein/stammbaumbilder/master/"
     vis.nodes <- nodes %>% mutate(image=paste0(path_to_images, id, ".png")) %>%
       mutate(image=replace(image, female==2 & missbild==1 , "https://raw.githubusercontent.com/chspoerlein/stammbaumbilder/master/female.png")) %>%
       mutate(image=replace(image, female==1 & missbild==1 , "https://raw.githubusercontent.com/chspoerlein/stammbaumbilder/master/male.png")) %>%
       mutate(shape="circularImage") %>%
       mutate(size_w=30) %>%
       mutate(size_w=replace(size_w, missbild==0, 60))
     
     
     vis.links <- links
     
     vis.nodes$color.border <- "black"
     vis.links$color <- "black"
     
     vis.nodes$size <- vis.nodes$size_w
     
     
     visNetwork(vis.nodes, vis.links, height="1500px", width="100%") %>% 
       visEdges(arrows="to") %>% 
       visLayout(hierarchical = test) %>% 
       visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
       visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = F)) %>%
       visPhysics(stabilization = FALSE, enabled = T) %>%
       visInteraction(navigationButtons = TRUE) %>%
       visInteraction(hover = T) %>%
       #visHierarchicalLayout(nodeSpacing=150) %>%
       visEvents(hoverNode  = "function(e){
            var label_info = this.body.data.nodes.get({
              fields: ['label', 'label_long'],
              filter: function (item) {
                return item.id === e.node
              },
              returnType :'Array'
            });
            this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
            }") %>% 
       visEvents(blurNode  = "function(e){
            var label_info = this.body.data.nodes.get({
              fields: ['label', 'label_long'],
              filter: function (item) {
                return item.id === e.node
              },
              returnType :'Array'
            });
            this.body.data.nodes.update({id: e.node, label : label_info[0].label_long, label_long : label_info[0].label});
            }")
     
   })
   
   

   
   output$herkunft <- renderLeaflet({
     
     pdat <- read.csv2("places_prep.csv", stringsAsFactors =F) %>%
       filter(lat!="")
     pdat$lat <- str_replace(pdat$lat,",",".")
     pdat$lon <- str_replace(pdat$lon,",",".")
     
     people <- read.csv2("personen_prep.csv", stringsAsFactors =F)  %>% 
       filter(newbirthdate>=input$zeit[1] & newbirthdate<=input$zeit[2]) %>%
       filter(direkte>=input$direkt)
     
     if (input$auswahl==-1){
       people <- people %>% filter(vergleich<=-1)
     } else if (input$auswahl==-2){
       people <- people %>% filter(vergleich==-2)
     } else if (input$auswahl==-3){
       people <- people %>% filter(vergleich==-3)
     } else if (input$auswahl==1){
       people <- people %>% filter(vergleich>=1)
     } else if (input$auswahl==2){
       people <- people %>% filter(vergleich==2)
     } else if (input$auswahl==3){
       people <- people %>% filter(vergleich==3)
     } else {
       people <- people
     }
     
     pdata <- people %>%  #filter(vergleich==input$auswahl) %>%
       left_join(pdat, by=c("bplace"="bplace")) %>%
       filter(lat!="NA") %>%
       mutate(lat=as.numeric(lat),
              long=as.numeric(lon))
     
     leaflet(data=pdata) %>% 
       addTiles(group="OSM") %>% 
       addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner") %>%
       addMarkers(lng = ~lat,
                        lat = ~long,
                        label=paste(pdata$name),
                  popup = paste(pdata$name,"<br>",pdata$labeldate,"<br>",pdata$labelplace,"<br>",pdata$labelocc),
                        clusterOptions = markerClusterOptions()) %>%
       addLayersControl(baseGroups = c("OSM", "NatGeo", "Toner"),
         options = layersControlOptions(collapsed = FALSE)) #%>%
       #addHeatmap(lng = ~lat,
                  #lat = ~long,
                  #blur = 15, max = 0.15, radius = 15)
   })
   
   
   output$table <- renderPlot({
     people <- read.csv2("personen_prep.csv", stringsAsFactors =F)  %>% 
       filter(newbirthdate>=input$zeit[1] & newbirthdate<=input$zeit[2]) %>%
       filter(direkte>=input$direkt)
     
     if (input$auswahl==-1){
       people <- people %>% filter(vergleich<=-1)
     } else if (input$auswahl==-2){
       people <- people %>% filter(vergleich==-2)
     } else if (input$auswahl==-3){
       people <- people %>% filter(vergleich==-3)
     } else if (input$auswahl==1){
       people <- people %>% filter(vergleich>=1)
     } else if (input$auswahl==2){
       people <- people %>% filter(vergleich==2)
     } else if (input$auswahl==3){
       people <- people %>% filter(vergleich==3)
     } else {
       people <- people
     }
     
    
     x111 <-people %>% filter(vorname!="NA", female==1) %>% count(vorname, sort=T) %>% mutate(female="Frauen")
     y111 <-people %>% filter(vorname!="NA", female==0) %>% count(vorname, sort=T) %>% mutate(female="Männer")
     dat111 <- rbind(x111,y111)
     
     ggplot(dat111, aes(label=vorname, size=n)) +
       geom_text_wordcloud(eccentricity = 1) +
       scale_size_area(max_size = 15) +
       theme_minimal() +
       facet_wrap(~female, ncol=2)
          })
   
   
   output$historie <- renderPlot({
     
     
     zeit <- read.csv("historisches.csv", sep=";", stringsAsFactors=FALSE) 
     zeit$beginn <- dmy(zeit$beginn)
     zeit$ende <- dmy(zeit$ende) 
     zeit <- zeit %>% filter(year(ende)>=input$zeit[1]) %>%
       mutate(ende=replace(ende, (year(ende)>input$zeit[2]) & (year(ende)>input$zeit[1]),paste0(input$zeit[2],"-01-01"))) %>%
       mutate(beginn=replace(beginn, (year(beginn)<input$zeit[1]),paste0(input$zeit[1],"-01-01"))) 
       
     
       
     
     people <- read.csv("personen_prep.csv", sep=";", stringsAsFactors=FALSE) %>%
       filter(start!="", (end!="" | newbirthdate>=1900)) %>% 
       #filter(newbirthdate>=input$zeit[1] & newbirthdate<=input$zeit[2]) %>%
       filter(direkte==input$direkt) 
    
          
     people <- people %>%
       mutate(end2=replace(end2, is.na(end2) & newbirthdate>=1900, as.character(Sys.Date()))) %>%
       mutate(end2=replace(end2, end2>input$zeit[2],paste0(input$zeit[2],"-01-01"))) %>%
       mutate(newbirthdate=replace(newbirthdate, start2<input$zeit[1] & end2>input$zeit[1],input$zeit[1])) %>%
       mutate(start2=replace(start2, start2<input$zeit[1] & end2>input$zeit[1],paste0(input$zeit[1],"-01-01"))) %>%
       filter(newbirthdate>=input$zeit[1] & newbirthdate<=input$zeit[2])
     
     if (input$auswahl==-1){
       people <- people %>% filter(vergleich<=-1)
     } else if (input$auswahl==-2){
       people <- people %>% filter(vergleich==-2)
     } else if (input$auswahl==-3){
       people <- people %>% filter(vergleich==-3)
     } else if (input$auswahl==1){
       people <- people %>% filter(vergleich>=1)
     } else if (input$auswahl==2){
       people <- people %>% filter(vergleich==2)
     } else if (input$auswahl==3){
       people <- people %>% filter(vergleich==3)
     } else {
       people <- people
     }
     
     if (input$zeitebene=="international"){
       zeit <- zeit %>% filter(level=="international")
     } else if (input$zeitebene=="national") {
         zeit <- zeit %>% filter(level=="national")
     } else if (input$zeitebene=="regional") {
       zeit <- zeit %>% filter(level=="regional")
     } else {
       zeit <- zeit
     }
     
     
     people <- people  %>%
       arrange(start2) %>%
       mutate(id=seq(1,nrow(people),1)) 
     
     
     ggplot() + theme_minimal() + 
       geom_segment(data=people, aes(x = year(start2), y = as.factor(id), xend = year(end2), yend = as.factor(id))) + 
       scale_y_discrete(labels=people$name) +
       ylab("") + 
       xlab("") + 
       xlim(input$zeit[1],input$zeit[2]) +
       geom_rect(aes(xmin = year(zeit$beginn) , ymin = -Inf, xmax = year(zeit$ende), ymax = Inf, fill=colorRampPalette(brewer.pal(11,"Spectral"))(nrow(zeit)), colour=colorRampPalette(brewer.pal(11,"Spectral"))(nrow(zeit))), alpha=.5, size=2) +
       theme(legend.position = "None") +
       geom_label_repel(aes(y = as.character(length(unique(people$id))), 
                            x = year(zeit$beginn)+floor((year(zeit$ende)-year(zeit$beginn))/2), 
                            label = zeit$ereignis),
                        force=90#,
                        #hjust=0,
                        #vjust=0
                        )
     
     
   })
   
   
   
   output$suche <- DT::renderDataTable({
     people <- read.csv2("personen_prep.csv", stringsAsFactors =F)  %>% 
       filter(newbirthdate>=input$zeit[1] & newbirthdate<=input$zeit[2])%>%
       filter(direkte>=input$direkt)
    
      if (input$auswahl==-1){
       people <- people %>% filter(vergleich<=-1)
     } else if (input$auswahl==-2){
       people <- people %>% filter(vergleich==-2)
     } else if (input$auswahl==-3){
       people <- people %>% filter(vergleich==-3)
     } else if (input$auswahl==1){
       people <- people %>% filter(vergleich>=1)
     } else if (input$auswahl==2){
       people <- people %>% filter(vergleich==2)
     } else if (input$auswahl==3){
       people <- people %>% filter(vergleich==3)
     } else {
       people <- people
     }
     
     people <- people %>% select(name, start, end, bplace, dplace, occ, birname, Partner, newbirthdate, infos) %>%
       rename(Name=name,
              Geburtsdatum=start,
              Sterbedatum=end,
              Geburtsort=bplace,
              Sterbeort=dplace,
              Beruf=occ,
              Geburtsname=birname,
              Zusatzmaterialien=infos) %>%
       arrange(newbirthdate) %>%
       select(-newbirthdate)
     
     DT::datatable({people}, escape=F)
     
   }, options = list(
     autoWidth = TRUE
     #columnDefs = list(list(width = '400px', targets = c(0)))
   ))
}

# Run the application 
shinyApp(ui = ui, server = server)

