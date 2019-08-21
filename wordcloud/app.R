
library(shiny)
library(tidyverse)
library(wordcloud)
library(viridis)


# Data Lennart
data_L <- readxl::read_excel("Woerter_L.xlsx")

data_new <- gather(data_L, month, wort, starts_with("Lenny_")) %>%
  mutate(mon=substr(month,7,8))
data_new2 <-gather(data_L, month, freq, starts_with("Freq_"))
data_new3 <-gather(data_L, month, deutsch, starts_with("Deutsch"))

# combine data sets
data_NEW_L <- as_tibble(c(data_new[,c("mon","wort")],data_new2[,"freq"],data_new3[,"deutsch"]))  %>% 
  filter(wort!="NA") %>% 
  mutate(mon=as.numeric(mon)) %>% 
  mutate(realmon=NA,
         realmon=replace(realmon,mon==12,"September 2017"),
         realmon=replace(realmon, mon==13, "Oktober 2017"),
         realmon=replace(realmon, mon==14, "November 2017"),
         realmon=replace(realmon, mon==15, "Dezember 2017"),
         realmon=replace(realmon, mon==16, "Januar 2018"),
         realmon=replace(realmon, mon==17, "Februar 2018"),
         realmon=replace(realmon, mon==18, "März 2018"),
         realmon=replace(realmon, mon==19, "April 2018"),
         realmon=replace(realmon, mon==20, "Mai 2018"),
         realmon=replace(realmon, mon==21, "Juni 2018"),
         realmon=replace(realmon, mon==22, "Juli 2018"),
         realmon=replace(realmon, mon==23, "August 2018"),
         realmon=replace(realmon, mon==24, "September 2018")) %>%
  mutate(kind=1)



# Data Josch
data_L <- readxl::read_excel("Woerter_J.xlsx")

data_new <- gather(data_L, month, wort, starts_with("Lenny_")) %>%
  mutate(mon=substr(month,7,8))
data_new2 <-gather(data_L, month, freq, starts_with("Freq_"))
data_new3 <-gather(data_L, month, deutsch, starts_with("Deutsch"))

# combine data sets
data_NEW_J <- as_data_frame(c(data_new[,c("mon","wort")],data_new2[,"freq"],data_new3[,"deutsch"]))  %>% 
  filter(wort!="NA") %>% 
  mutate(mon=as.numeric(mon)) %>% 
  mutate(realmon=NA,
         realmon=replace(realmon,mon==12,"Juli 2019"),
         realmon=replace(realmon, mon==13, "August 2019"),
         realmon=replace(realmon, mon==14, "September 2019"),
         realmon=replace(realmon, mon==15, "Oktober 2019"),
         realmon=replace(realmon, mon==16, "November 2019"),
         realmon=replace(realmon, mon==17, "Dezember 2019"),
         realmon=replace(realmon, mon==18, "Januar 2020"),
         realmon=replace(realmon, mon==19, "Februar 2020"),
         realmon=replace(realmon, mon==20, "März 2020"),
         realmon=replace(realmon, mon==21, "April 2020"),
         realmon=replace(realmon, mon==22, "Mai 2020"),
         realmon=replace(realmon, mon==23, "Juni 2020"),
         realmon=replace(realmon, mon==24, "Juli 2020")) %>%
  mutate(kind=2)


########## alle Daten kombinieren

dat_full <- rbind(data_NEW_L,data_NEW_J)


ui <- navbarPage("Wörtentwicklung von Lennart und Joscha",
                 tabPanel("Wortentwicklung", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("kind1",
                                           "Wähle ein Kind:",
                                           choices = c("Lennart"= 1,
                                                       "Joscha" = 2),
                                           selected=1),
                              sliderInput("month1",
                                          "Lebensmonat:",
                                          min = 12,
                                          max = 20,
                                          value = 13),
                              radioButtons("sprache1",
                                           "Wähle eine Sprache:",
                                           choices = c("Kindersprech"= "wort",
                                                       "Deutsch" = "deutsch"),
                                           selected="wort")),
                            mainPanel(plotOutput("wordcloud", width = "100%", height = "600px"),
                                      plotOutput("wordcount")
                            ))),
                 
                 
                 tabPanel("Wortsuche", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("kind2",
                                           "Wähle ein Kind:",
                                           choices = c("Lennart"= 1,
                                                       "Joscha" = 2),
                                           selected=1),
                              sliderInput("month2",
                                          "Lebensmonat:",
                                          min = 12,
                                          max = 20,
                                          value = 13),
                              textInput("suche2", "Wortsuche:", value="Papa"),
                              actionButton("los2", "Suchen!")
                              ),
                          
                          mainPanel(br(),
                                    textOutput("suchbegriff"),
                                    br(),
                                    tableOutput("woerterbuch")
                          ))),
                 
                 
                 tabPanel("Bilder", fluid=T,
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("kind3",
                                           "Wähle ein Kind:",
                                           choices = c("Lennart"= 1,
                                                       "Joscha" = 2),
                                           selected=1),
                              sliderInput("month3",
                                          "Lebensmonat:",
                                          min = 12,
                                          max = 20,
                                          value = 13)
                            ),
                          
                          mainPanel(imageOutput("Lennpic", width = "60px",height = "600px")
                          )))
                 
                 
                 
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$wordcloud <- renderPlot({
    # adding random noise to frequency variables
    set.seed(30091985)
    dat_full$freq<- abs(dat_full$freq+rnorm(n=nrow(dat_full),mean=0,sd=1))
    data2 <- dat_full %>% 
      filter(mon==input$month1) %>% 
      rename(woerter=input$sprache1) %>%
      filter(kind==input$kind1)
    
    # draw the wordcloud
    wordcloud(data2$woerter, 
              freq=data2$freq, 
              min.freq=1, 
              random.order=FALSE, 
              rot.per=0, 
              #colors=brewer.pal(8,"RdYlBu"),
              colors=viridis(20),
              use.r.layout=TRUE)
    
  })
  
  output$wordcount <- renderPlot({
    # generate bins based on input$bins from ui.R
    data2 <- dat_full %>% 
      filter(kind==input$kind1) %>%
      group_by(mon) %>%
      count()
    # draw the wordcount
    ggplot(data=data2, aes(x=mon, y=n, label=n)) + 
      geom_line(group = 1, size=2, color="grey") + 
      geom_point(size=5) + 
      labs(x="Lebensmonat", y="kumulative Wortanzahl") + 
      theme_minimal() + 
      ylim(0,155) +
      xlim(12,20) +
      scale_shape_manual(values=16) +
      geom_line(data=data2 %>% filter(mon<=input$month1), aes(x=mon, y=n), group=1, color="black", size=2) +
      geom_label() +
      theme(legend.position = "None", axis.text=element_text(size=15, face="bold"), axis.title = element_text(size=15, face="bold"))
    
  })
  
  output$suchbegriff <- renderText({
    #mon_data <- 
    dat_full2 <- dat_full %>%
      filter(kind==input$kind2)
      if (min(dat_full2$mon[which(dat_full2$deutsch==input$suche2)])!=Inf){
        paste0(input$suche2," sagt er seit dem ",min(dat_full2$mon[which(dat_full2$deutsch==input$suche2)]),". Lebensmonat.")
      } else {
        paste0("Dieses Wort kennt er entweder noch nicht oder du hast dich verschrieben!")
      }
  })
  
  observeEvent(input$los2, {
    dat_full2 <- dat_full %>%
      filter(kind==input$kind2)
    output$woerterbuch <- renderTable({
      if (min(dat_full2$mon[which(dat_full2$deutsch==input$suche2)])!=Inf){
        worttable <- data.frame(Wort=input$suche2, 
                                Kindersprech=dat_full2$wort[dat_full2$deutsch==input$suche2][1],
                                Lebensmonat=as.character(min(dat_full2$mon[which(dat_full2$deutsch==input$suche2)])),
                                Monat=dat_full2$realmon[dat_full2$mon==min(dat_full2$mon[which(dat_full2$deutsch==input$suche2)])][1])     
        worttable
      }
    })
  })
  
  output$Lennpic <- renderImage({
    
    if (input$kind3==1) {
      picname <- normalizePath(file.path(paste0("L",input$month3,".jpg")))
      list(src=picname) #, width="60%", height="60%")
    } else {
      picname <- normalizePath(file.path(paste0("J",input$month3,".jpg")))
      list(src=picname) #, width="60%", height="60%")
    }
  }, deleteFile=FALSE
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

