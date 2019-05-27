require(dplyr)
require(shiny)
#library(rdrop2)
#require(googlesheets)
#require(shinyjs)
#require(V8)
#data <- readRDS("dat.rds")


#token <- drop_auth()
#saveRDS(token, "droptoken.rds")
# Upload droptoken to your server
# ******** WARNING ********
# Losing this file will give anyone 
# complete control of your Dropbox account
# You can then revoke the rdrop2 app from your
# dropbox account and start over.
# ******** WARNING ********
# read it back with readRDS
#token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
#drop_acc(dtoken = token)

labs <- as.data.frame(matrix(c("Wiederholung II","frage1","Frage 1: Warum setzt sich die deutsche Bevölkerung zu 51% aus Frauen zusammen?",
                               "A) weil mehr Mädchen als Jungs geboren werden.",
                               "B) weil Frauen im Schnitt länger leben.",
                               "C) Fangfrage: früher war die deutsche Bevölkerung tatsächlich >50 Prozent weiblich, heute ist sie aber >50 Prozent männlich.",
                               "D) Fangfrage: die deutsche Bevölkerung besteht zu 50% aus Frauen.","B",
                               "Wiederholung II","frage2","Frage 2: Ulrich Beck begründet den Bedeutungsverlust von Klassen- und Schichtkonzepten mit dem „Fahrstuhleffekt“. Was ist damit gemeint? Durch den allgemeinen Zuwachs von Einkommen, Bildung, Rechtssicherheit, Konsum oder Mobilität...",
                               "A) werden Klassenidentitäten und -bindungen aufgelöst.",
                               "B) kommt es zu Kooperation der verschiedenen Klassen und Schichten.",
                               "C) der unteren Schichten ist das Konfliktpotenzial der Klassen- und Schichtgegensätze stark zurückgegangen.",
                               "D) der oberen Schichten ist die Neigung zur Ausbeutung der unteren Schichten gesunken.","A",
                               "Wiederholung II","frage3","Frage 3: Welche Aussage beschreibt „soziale Ungleichheit“ am besten?",
                               "A) Regional unterschiedliche Verteilung von Sozialeinrichtungen.",
                               "B) Unterscheide zwischen Gesellschaften hinsichtlich ihrer sozialen Strukturen.",
                               "C) Die ungleiche Verteilung von Gütern aufgrund gesellschaftlicher Positionen.",
                               "D) Das berechtigte Gefühl der Benachteiligung ärmerer Bevölkerungsgruppen.","C",
                               "Bildungsungleichheit I: Kompetenzerwerb","frage1","Frage 1: Was wird mit dem sozialen Gradienten beschrieben?",
                               "A) Der Umfang der Bildungsexpansion im Zeitverlauf.",
                               "B) Die Verringerung des Ausmaßes sozialer Ungleichheit im Kompetenzerwerb.",
                               "C) Die durch „Reibungsverluste“ im Schulsystem entstehende Reserve an Ungebildeten.",
                               "D) Der Zusammenhang von sozialer Herkunft und Kompetenzniveau.","D",
                               "Bildungsungleichheit I: Kompetenzerwerb","frage2","Frage 2: Was ist mit dem Begriff der Bildungsexpansion NICHT gemeint?",
                               "A) Der Ausbau der sekundären und tertiären Bereiche des Bildungswesens.",
                               "B) Immer mehr junge Menschen verweilen immer länger im Bildungssystem.",
                               "C) Die durchschnittliche Höherqualifizierung der Bevölkerung.",
                               "D) Die Expansion der sozialen Ungleichheit durch die Diversifikation von Wissen.","D",
                               "Bildungsungleichheit I: Kompetenzerwerb","frage3","Frage 3: Wozu dient die Unterscheidung von primären und sekundären Effekten?",
                               "A) Um zu zeigen, warum es zu ungleichen Bildungserfolgen trotz gleicher Leistungen kommen kann.",
                               "B) Die Unterscheidung ist notwendig, um den Umfang der tertiären Effekte schätzen zu können.",
                               "C) Hierüber kann der Einfluss von Bildungseinrichtungen auf den Bildungserfolg bestimmt werden.",
                               "D) Hierüber lassen sich die wichtigen und die weniger wichtigen Determinanten des Bildungserfolgs bestimmen.","A",
                               "Bildungsungleichheit II: Bildungsentscheidungen","frage1","Was zeigt das IEO-Modell nicht?",
                               "A) Geringe Differenzen in den Übergangswahrscheinlichkeiten können zu deutlichen Bildungsungleichheiten führen",
                               "B) Unterschiede zwischen den Schichten nehmen über die einzelnen Bildungsübergänge hinweg ab.",
                               "C) Unterschiede zwischen den Schichten nehmen über die einzelnen Bildungsübergänge hinweg zu.",
                               "D) Eine höhere Bildungsbeteiligung führt nicht zwangsläufig zu einer Verringerung der Bildungsungleichheiten, weil die verschiedenen Schichten von unterschiedlichen Positionen aus starten.","B",
                               "Bildungsungleichheit II: Bildungsentscheidungen","frage2","Wo wirken sekundäre Effekte?",
                               "A) Bei der ungleichen Behandlung durch Lehrerinnen und Lehrer",
                               "B) Beim Kompetenzerwerb",
                               "C) Bei Entscheidungen an den einzelnen Bildungsübergängen.",
                               "D) Beim Erwerb von Nachhilfeunterricht","C",
                               "Bildungsungleichheit II: Bildungsentscheidungen","frage3","Wie wirkt sich die Modernisierung des Bildungssystems potenziell auf Selektionsprozesse aus?",
                               "A) Der Einfluss der sozialen Herkunft verändert sich im Zeitverlauf nicht.",
                               "B) Der Einfluss der sozialen Herkunft nimmt im Zeitverlauf zu.",
                               "C) Der Einfluss der sozialen Herkunft nimmt im Zeitverlauf ab.",
                               "D) In den unteren Stufen des Bildungssystems nimmt die Chancenungleichheit zu.","C",
                               "Bildungsungleichheit III: Ethnische Ungleichheit","frage1","Welches Phänomen findet man häufig in Migrantenfamilien?",
                               "A) Höheres Kulturelles Kapital",
                               "B) Niedrigere Bildungsmotivation ",
                               "C) Höhere Bildungsmotivation",
                               "D) Höhere Lesefähigkeiten","C",
                               "Bildungsungleichheit III: Ethnische Ungleichheit","frage2","Auf was lassen sich Bildungsnachteile von Kindern und Jugendlichen aus Zuwandererfamilien am stärksten zurückführen?",
                               "A) Auf die soziale Lage",
                               "B) Auf migrationsspezifische Ungleichheiten",
                               "C) Auf fehlende kognitive Fähigkeiten von Migranten",
                               "D) Auf Diskriminierung","A",
                               "Bildungsungleichheit III: Ethnische Ungleichheit","frage3","Für welche ethnischen Gruppen lassen sich im Deutschen Schulsystem keine große Unterschiede in der Bildungsbeteiligung feststellen?",
                               "A) Türken",
                               "B) Italiener",
                               "C) Russen",
                               "D) Griechen","D",
                               "Bildung und Berufseinstieg","frage1","Was bedeutet Stratifizierung?",
                               "A) Grad nach dem die Bildung den gleichen nationalen Standards entspricht.",
                               "B) Die Entwertung der Bildungsabschlüsse.",
                               "C) Die gesteigerte Bedeutung von Bildungszertifikaten.",
                               "D) Ausmaß, nach dem Schüler/innen in einer frühen Bildungsphase auf verschiedene Bildungswege verteilt werden.","D",
                               "Bildung und Berufseinstieg","frage2","Von was ist die Entlohnung laut Humankapitaltheorie abhängig?",
                               "A) Von der sozialen Herkunft",
                               "B) Von der Produktivität",
                               "C) Von der ethnischen Herkunft",
                               "D) Von der Sympathie zwischen Arbeitgeber und Arbeitnehmer","B",
                               "Bildung und Berufseinstieg","frage3","Was wird unter sozialer Schließung auf dem Arbeitsmarkt verstanden?",
                               "A) Der Ausschluss von Personen mit sozial abweichendem Verhalten",
                               "B) Der Zugang zu begehrten Positionen nur über bestimmte Bildungszertifikate",
                               "C) Der Zugang zu begehrten Positionen über Kontakte",
                               "D) Der Zugang zu begehrten Positionen über Produktivität","B",
                               "Arbeitsmarktungleichheit I: Einkommen","frage1","Was ist das Äquivalenzeinkommen?",
                               "A) Alle Einkommensquellen, die einem Haushalt zufließen",
                               "B) Alle Einkommensquellen abzüglich von Steuern und Sozialversicherungsbeiträgen ",
                               "C) Das Netto-Haushaltseinkommen geteilt durch Anzahl der Personen im Haushalt ",
                               "D) Das Netto-Haushaltseinkommen geteilt durch die gewichtete Anzahl der Personen im Haushalt","D",
                               "Arbeitsmarktungleichheit I: Einkommen","frage2","Was gibt der Gini-Koeffizient an?",
                               "A) Das Ausmaß der Einkommenskonzentration",
                               "B) Das durchschnittliche Haushaltseinkommen ",
                               "C) Das durchschnittliche Äquivalenzeinkommen",
                               "D) Die Armut einer Gesellschaft ","A",
                               "Arbeitsmarktungleichheit I: Einkommen","frage3","Wer gilt nicht als Risikogruppe für Armut?",
                               "A) Alte Menschen",
                               "B) Bildungsferne Schichten",
                               "C) Alleinerziehende Menschen",
                               "D) Kinderlose Personen","D",
                               "Arbeitsmarktungleichheit II: Geschlechterungleichheit","frage1","Was bedeutet horizontale Geschlechtersegregation auf dem Arbeitsmarkt?",
                               "A) Die Verteilung der Geschlechter auf Berufsfelder und Tätigkeiten.",
                               "B) Die Verteilung der Geschlechter auf Positionen in der Berufshierarchie.",
                               "C) Frauen sind in denselben Berufen und Berufsfeldern häufig in rangniedrigeren Statusgruppen platziert.",
                               "D) Frauen verdienen in den selben Positionen weniger als Männer.","A",
                               "Arbeitsmarktungleichheit II: Geschlechterungleichheit","frage2","Was wird unter Diskriminierung verstanden?",
                               "A) Die Schlechterstellung von Frauen aufgrund ihrer Leistung.",
                               "B) Die Schlechter- oder Besserstellung aufgrund eines askriptiven Merkmals.",
                               "C) Die Schlechter- oder Besserstellung aufgrund eines deskriptiven Merkmals.",
                               "D) Die geringere Erwerbstätigenquote bei Frauen als bei Männern.","B",
                               "Arbeitsmarktungleichheit II: Geschlechterungleichheit","frage3","Was trägt zu Geschlechtsunterschieden auf dem Arbeitsmarkt nicht bei?",
                               "A) Unterschiedliche Präferenzen der Geschlechter.",
                               "B) Geringere Bildungsabschlüsse von Frauen.",
                               "C) Diskriminierung.",
                               "D) Erwerbsunterbrechungen von Frauen durch Familienplanung.","B",
                               "Arbeitsmarktungleichheit III: Ethnische Ungleichheit","frage1","Was ist unter statistischer Diskriminierung zu verstehen?",
                               "A) Durch die Präferenzen der Arbeitgeber werden Migranten seltener eingestellt.",
                               "B) Informationsdefizite führen dazu, dass die Produktivität eines Bewerbers auf Basis eine Gruppenzugehörigkeit bewertet wird.",
                               "C) Informationsdefizite führen dazu, dass die Produktivität einer Gruppe auf Basis einzelner Personen aus dieser Gruppe bewertet wird.",
                               "D) Aufgrund von besseren Bildungsabschlüssen wird ein Innländer statt eines Migranten eingestellt.","B",
                               "Arbeitsmarktungleichheit III: Ethnische Ungleichheit","frage2","Was wird unter Arbeitgeberdiskriminierung verstanden?",
                               "A) Diskriminierung bei der Einstellung von Arbeitnehmer/innen, geringere Löhne für bestimmte Gruppen",
                               "B) Diskriminierung von Unternehmen, Arbeitgeber/innen bestimmter Gruppen müssen höhere Löhne zahlen.",
                               "C) Segregation der Gruppen in unterschiedliche Betriebe.",
                               "D) Abneigung gegenüber Produkten, die von der diskriminierten Gruppe hergestellt oder verkauft werden.","A",
                               "Arbeitsmarktungleichheit III: Ethnische Ungleichheit","frage3","?",
                               "A) ",
                               "B) ",
                               "C) ",
                               "D) ","A",
                               "Partnerwahl und Heiratsmuster","frage1","Wie verändert sich potenziell die Bildungshomogamie in der Kohortenfolge?",
                               "A) Sie nimmt zu.",
                               "B) Sie nimmt ab, da traditionelle geschlechtsspezifische Vorstellungen an Rückhalt verlieren.",
                               "C) Sie bleibt unverändert.",
                               "D) Sie nimmt ab, da Die Präferenzen von Männern sich verändert.","A",
                               "Partnerwahl und Heiratsmuster","frage2","?",
                               "A) ",
                               "B) ",
                               "C) ",
                               "D) ","A",
                               "Partnerwahl und Heiratsmuster","frage3","?",
                               "A) ",
                               "B) ",
                               "C) ",
                               "D) ","A",
                               "Wandel der Haushalts- und Familienstruktur","frage1","Welche Phänomene umfasst der Wandel der Haushalts- und Familienstrukturen nicht?",
                               "A) Ein Anstieg von Alleinerziehenden",
                               "B) Rückgang von Eheschließungen",
                               "C) Anstieg der Wiederverheiratungen ",
                               "D) Rückgang von Nicht-Ehelichen Lebensgemeinschaften","D",
                               "Wandel der Haushalts- und Familienstruktur","frage2","Wie verändern sich partnerschaftliche Lebensformen im Zeitverlauf nicht?",
                               "A) Das Zusammenleben mit einem Partner verliert an Bedeutung.",
                               "B) Die Ehe verliert an Bedeutung.",
                               "C) Das Leben ohne Partner nimmt in den jüngeren Altersgruppen zu.",
                               "D) Nicht-eheliche Lebensgemeinschaften sind unter den jüngeren Altersgruppen am verbreitetsten","A",
                               "Wandel der Haushalts- und Familienstruktur","frage3","Was wird unter Kohabitation verstanden?",
                               "A) Das Zusammenleben eines Ehepaars",
                               "B) Ein Paar was an verschiedenen Orten wohnt",
                               "C) Ein nicht-eheliches Zusammenleben",
                               "D) Das Wohnen bei den eigenen Eltern.","C"
                               
), ncol=8, byrow=T))

colnames(labs) <- c("sitzung","frage","text","antwort1","antwort2","antwort3","antwort4","loesung")

pam_pics <- c("P_Cat_1.jpeg","P_cat_2.jpeg","P_cat_3.jpeg","P_racoon_1.jpeg","P_racoon_2.jpeg","P_racoon_3.jpeg")


#gs_auth(new_user = FALSE, gs_auth(token = "shiny_app_token.rds", cache=FALSE), cache=FALSE)

#dat <- gs_key("1d6c-IT-AKqdmj2JUfm0LziqXLYOPXB7sBplvBVnViZE")
#dat_pam <- gs_key("1H7Bs-XHhRmkl5UUaA5nDivWYbvL4IJuHhr6HaruqQTA")

#load("add_data.RData")

#dat2 <- as.data.frame(matrix(c(0,0,0,0,0,0,0,0,0,0,0), ncol=11))
#colnames(dat2) <- c("sitzung3","gruppe","slider21","slider22","slider23","text21","slider24","text22","slider25","text23","system.time")


dat <- readRDS("dat.rds")
dat2 <- readRDS("dat2.rds")



ui <- navbarPage("Feedback in der Lehre",
                 

   # Application title
   #titlePanel("Feedback in der Lehre"),
   
   # Sidebar with a slider input for number of bins 
   tabPanel("Evaluation", fluid=T,
    sidebarLayout(
       sidebarPanel(
       
         selectInput("sitzung", "Titel der heutigen Sitzung", c(Auswählen = "","Bildung und Berufseinstieg","Arbeitsmarktungleichheit I: Einkommen",
             "Arbeitsmarktungleichheit II: Geschlechterungleichheit","Arbeitsmarktungleichheit III: Ethnische Ungleichheit",
             "Partnerwahl und Heiratsmuster","Wandel der Haushalts- und Familienstruktur"), selected="Bildung und Berufseinstieg"),
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
              
              selectInput("sitzung3", "Titel der heutigen Sitzung", c(Auswählen = "","Bildungsungleichheit II: Bildungsentscheidungen",
                                                                     "Bildungsungleichheit III: Ethnische Ungleichheit","Bildung und Berufseinstieg","Arbeitsmarktungleichheit I: Einkommen",
                                                                     "Arbeitsmarktungleichheit II: Geschlechterungleichheit","Arbeitsmarktungleichheit III: Ethnische Ungleichheit",
                                                                     "Partnerwahl und Heiratsmuster","Wandel der Haushalts- und Familienstruktur"), selected="Bildungsungleichheit II: Bildungsentscheidungen"),
              radioButtons("gruppe", "Gruppe", choices= c("Mittwoch", "Donnerstag")),
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
    dat <- rbind(dat,Results())
    #write.csv(dat, file = "sozstruk19.csv",row.names = FALSE)
   #drop_upload("sozstruk19.csv", path = "responses")
    saveRDS(dat,"dat.rds")
    #dat <- dat %>%                                                                     
      #gs_add_row(ws = "Data", input = Results()) 
    
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
      dat2 <- rbind(dat2,Results2())
      saveRDS(dat2,"dat2.rds")
      #dat_pam <- dat_pam %>%                                                                     
       # gs_add_row(ws = "Data", input = Results2()) 
      
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


