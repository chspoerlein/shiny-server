require(dplyr)
require(shiny)
require(googlesheets)
#require(shinyjs)
#require(V8)

pam_pics <- c("P_Cat_1.jpeg","P_cat_2.jpeg","P_cat_3.jpeg","P_racoon_1.jpeg","P_racoon_2.jpeg","P_racoon_3.jpeg")

gs_auth(new_user = FALSE, gs_auth(token = "shiny_app_token.rds"))

dat <- gs_key("1d6c-IT-AKqdmj2JUfm0LziqXLYOPXB7sBplvBVnViZE")
dat_pam <- gs_key("1H7Bs-XHhRmkl5UUaA5nDivWYbvL4IJuHhr6HaruqQTA")

load("add_data.RData")

ui <- navbarPage("Feedback in der Lehre",
                 

   # Application title
   #titlePanel("Feedback in der Lehre"),
   
   # Sidebar with a slider input for number of bins 
   tabPanel("Evaluation", fluid=T,
    sidebarLayout(
       sidebarPanel(
       
         selectInput("sitzung", "Titel der heutigen Sitzung", c(Auswählen = "",
             "Wiederholung II", "Bildungsungleichheit I: Kompetenzerwerb","Bildungsungleichheit II: Bildungsentscheidungen",
             "Bildungsungleichheit III: Ethnische Ungleichheit","Bildung und Berufseinstieg","Arbeitsmarktungleichheit I: Einkommen",
             "Arbeitsmarktungleichheit II: Geschlechterungleichheit","Arbeitsmarktungleichheit III: Ethnische Ungleichheit",
             "Partnerwahl und Heiratsmuster","Wandel der Haushalts- und Familienstruktur",
             "Wiederholung III")),
         helpText("Wie bewerten Sie die folgenden Aspekte der heutigen Veranstaltung?"),
        sliderInput("slider1","Gliederung",0,10,5),
        sliderInput("slider2","Inhalte sind nachvollziehbar",0,10,5),
        sliderInput("slider3","Der Inhalt unterstützt mich bei der Erreichung der Lernziele.",0,10,5),
        textInput("text1", "Mit diesen Inhalten hatte ich Probleme:"),
        sliderInput("slider4","Der Dozent stellt die Inhalt verständlich dar.",0,10,5),
        textInput("text2","Was sollte der Dozent ändern?"),       
        sliderInput("slider5","Wie zufrieden waren Sie mit der heutigen Sitzung insgesamt?",0,10,5),
        textInput("text3","Was möchten Sie noch los werden?"),   
        actionButton("saveBtn","Abschicken")),
     
      mainPanel(imageOutput("danke")
         ))),
    
    tabPanel("Lernkontrolle", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 #shinyjs::useShinyjs(),
                 selectInput("sitzung2", "Titel der heutigen Sitzung", c(Auswählen = "",
                                                                        "Wiederholung II", "Bildungsungleichheit I: Kompetenzerwerb","Bildungsungleichheit II: Bildungsentscheidungen",
                                                                        "Bildungsungleichheit III: Ethnische Ungleichheit","Bildung und Berufseinstieg","Arbeitsmarktungleichheit I: Einkommen",
                                                                        "Arbeitsmarktungleichheit II: Geschlechterungleichheit","Arbeitsmarktungleichheit III: Ethnische Ungleichheit",
                                                                        "Partnerwahl und Heiratsmuster","Wandel der Haushalts- und Familienstruktur",
                                                                        "Wiederholung III")),
                 div(
                   id = "sidepanel",
                 selectInput("frage1", "Frage 1", choices= c("","A", "B","C","D")), # selected = character(0)),
                 selectInput("frage2", "Frage 2", choices= c("","A", "B","C","D")),#selected = character(0)),
                 selectInput("frage3", "Frage 3", choices= c("","A", "B","C","D"))),#selected = character(0)),
                 actionButton("saveBtn2","Antworten abgeben")#,
                 #actionButton("resetinput","Reset")
                 

       ), 
       mainPanel(htmlOutput("fragetext1"),
                 br(),
                 htmlOutput("antwort1"),
                 br(),
                 #br(),
                 htmlOutput("fragetext2"),
                 br(),
                 htmlOutput("antwort2"),
                 #br(),
                 br(),
                 htmlOutput("fragetext3"),
                 br(),
                 htmlOutput("antwort3"))


   )
),
tabPanel("Evaluation Tutorium", fluid=T,
          sidebarLayout(
            sidebarPanel(
              
              selectInput("sitzung3", "Titel der heutigen Sitzung", c(Auswählen = "","Bildungsungleichheit I: Kompetenzerwerb","Bildungsungleichheit II: Bildungsentscheidungen",
                                                                     "Bildungsungleichheit III: Ethnische Ungleichheit","Bildung und Berufseinstieg","Arbeitsmarktungleichheit I: Einkommen",
                                                                     "Arbeitsmarktungleichheit II: Geschlechterungleichheit","Arbeitsmarktungleichheit III: Ethnische Ungleichheit",
                                                                     "Partnerwahl und Heiratsmuster","Wandel der Haushalts- und Familienstruktur")),
              radioButtons("gruppe", "Gruppe", choices= c("Mittwoch", "Donnerstag"), selected = character(0)),
              helpText("Wie bewerten Sie die folgenden Aspekte der heutigen Veranstaltung?"),
              sliderInput("slider21","Gliederung",0,10,5),
              sliderInput("slider22","Inhalte sind nachvollziehbar",0,10,5),
              sliderInput("slider23","Der Inhalt unterstützt mich bei der Erreichung der Lernziele.",0,10,5),
              textInput("text21", "Mit diesen Inhalten hatte ich Probleme:"),
              sliderInput("slider24","Pamina stellt die Inhalt verständlich dar.",0,10,5),
              textInput("text22","Was sollte Pamina ändern?"),       
              sliderInput("slider25","Wie zufrieden waren Sie mit der heutigen Sitzung insgesamt?",0,10,5),
              textInput("text23","Was möchten Sie noch los werden?"),   
              actionButton("saveBtn3","Abschicken")),
            
            mainPanel(imageOutput("danke_pamina")#, width = "60px",height = "600px")
            ))))



# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  #store the results
  Results <- reactive(c(
    input$sitzung, input$slider1, input$slider2, input$slider3, input$text1, input$slider4, input$text2, input$slider5, input$text3, Sys.time()
  ))
  
  #This will add the new row at the bottom of the dataset in Google Sheets.
  
  observeEvent(input$saveBtn, {                                                                 
    dat <- dat %>%                                                                     
      gs_add_row(ws = "Data", input = Results()) 
    
    output$danke <- renderImage({
      picname <- "thankyoucat-680x680.jpg"
      list(src=picname) #, width="60%", height="60%")
    }, deleteFile=FALSE
    )
    
  }
  )
  
  
  ############## MC Idee
  
  output$fragetext1 <- renderUI({
    t1 <- labs %>% 
      filter(sitzung==input$sitzung2,
             frage=="frage1")
    
    HTML(paste("<b>",t1$text,"</b>",t1$antwort1,t1$antwort2,t1$antwort3,t1$antwort4, sep="<br/>"))
  })
  
  
  output$fragetext2 <- renderUI({
    t2 <- labs %>% 
      filter(sitzung==input$sitzung2,
             frage=="frage2")

    HTML(paste("<b>",t2$text,"</b>",t2$antwort1,t2$antwort2,t2$antwort3,t2$antwort4, sep="<br/>"))
  })
  
  
  output$fragetext3 <- renderUI({
    t3 <- labs %>% 
      filter(sitzung==input$sitzung2,
             frage=="frage3")
    
    HTML(paste("<b>",t3$text,"</b>",t3$antwort1,t3$antwort2,t3$antwort3,t3$antwort4, sep="<br/>"))
  })
  
  
 
    
    observeEvent(input$saveBtn2, { 
      
      output$antwort1 <- renderUI({
        t <- labs %>% 
          filter(sitzung==input$sitzung2,
                 frage=="frage1")
        
        if (input$frage1==t$loesung) {
          HTML(paste0("<font color=\"green\">","Korrekt!","</font>"))
        } else if ((input$frage1!=t$loesung) & input$frage1!="") {
          HTML(paste0("<font color=\"red\">","Leider inkorrekt!","</font>"))
        } else if (input$frage1=="") {
          HTML("Wähle erst eine Antwort")
        } else {
          HTML("Hm...")
        }
      })
      
      
      output$antwort2 <- renderUI({
        t <- labs %>% 
          filter(sitzung==input$sitzung2,
                 frage=="frage2")
        
        if (input$frage2==t$loesung) {
          HTML(paste0("<font color=\"green\">","Korrekt!","</font>"))
        } else if ((input$frage2!=t$loesung) & input$frage2!="") {
          HTML(paste0("<font color=\"red\">","Leider inkorrekt!","</font>"))
        } else if (input$frage2=="") {
          HTML("Wähle erst eine Antwort")
        } else {
          HTML("Hm...")
        }
      })
      
      
      output$antwort3 <- renderUI({
        t <- labs %>% 
          filter(sitzung==input$sitzung2,
                 frage=="frage3")
        
        if (input$frage3==t$loesung) {
          HTML(paste0("<font color=\"green\">","Korrekt!","</font>"))
        } else if ((input$frage3!=t$loesung) & input$frage3!="") {
          HTML(paste0("<font color=\"red\">","Leider inkorrekt!","</font>"))
        } else if (input$frage3=="") {
          HTML("Wähle erst eine Antwort")
        } else {
          HTML("Hm...")
        }
      })
  
      

  
})
    
    #observeEvent(input$resetinput, {
    #  shinyjs::reset("sidepanel")
    #})
     
    
    ######### Pamina
    
    Results2 <- reactive(c(
      input$sitzung3, input$gruppe,input$slider21, input$slider22, input$slider23, input$text21, input$slider24, input$text22, input$slider25, input$text23, Sys.time()
    ))
    
    #This will add the new row at the bottom of the dataset in Google Sheets.
    
    observeEvent(input$saveBtn3, {                                                                 
      dat_pam <- dat_pam %>%                                                                     
        gs_add_row(ws = "Data", input = Results2()) 
      
      output$danke_pamina <- renderImage({
        
        picname <- sample(pam_pics,1)
        list(src=picname) #, width="60%", height="60%")
      }, deleteFile=FALSE
      )
      
    }
    )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
