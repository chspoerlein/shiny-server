library(sf)
library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)

# 49.84-49.95
# 10.8-10.98


#setwd("C:/Users/ba3ef8/Google Drive/Buergerbefragung/2019/BB2019")

data <- read.csv2("bb2019.csv", sep=";")
data$geschlecht <- str_replace(data$geschlecht, "ae", "ä")
data$beruf <- str_replace(data$beruf, "ae", "ä")

data17 <- read.csv2("bb2017.csv", sep=";")
data17$geschlecht <- str_replace(data17$geschlecht, "ae", "ä")
data17$beruf <- str_replace(data17$beruf, "ae", "ä")

load("add_data.RData")
source("plotfuc.R", local=T)
source("plotfuc2.R", local=T)

bil<- c("keinen Abschluss","Hauptschulabschluss","Mittlere Reife","(Fach-)Abitur","Hochschule")
ber <- c("in Ausbildung","(Fach-)Arbeiter/in","Angestelle/r", "Beamter/in", "Selbständige/r","Freiberufler/in")
data$bildung <- fct_relevel(data$bildung,bil)
data$beruf <- fct_relevel(data$beruf,ber)

data17$bildung <- fct_relevel(data17$bildung,bil)
data17$beruf <- fct_relevel(data17$beruf,ber)




# Define UI for application that draws a histogram
ui <- navbarPage("Bamberger Bürgerbefragung 2019",
   
   # Application title
   #titlePanel("Bamberger Bürgerbefragung 2019"),

   
   # Sidebar with a slider input for number of bins 
   #tabsetPanel(
     tabPanel("Bamberg und seine Eigenschaften", fluid=T,
              sidebarLayout(
                sidebarPanel(
                  img(src="Download.png", height=72, width=72, align="left"),
                  img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                  selectInput("INPUTVAR", 
                              "Wählen Sie eine Frage:", 
                              choices = list(
                                "Wie bewerten Sie Bambergs "=c("Lebensqualität"="v_1"),
                                "Welche Eigenschaft besitzt Bamberg?"=c("Jung"="v_9", "Sauber"="v_10","Modern"="v_11","Sicher"="v_12", "Dynamisch"="v_13", "Weltoffen"="v_14", "Gepflegt"="v_15","Interessant"="v_16","Preiswert"="v_17","Attraktiv"="v_18","Traditionsbewusst"="v_19","Lebendig"="v_20"),
                                "Wie attraktiv bewerten Sie folgende Ort Bambergs?"=c("Stadtkern"="v_26","Wohngegend"="v_27","Parkanlagen"="v_28","Naherholungsgebiete"="v_29"),
                                "Wie attraktiv sind diese Orte für"=c("Kinder"="v_36","Jugendliche"="v_37","Familien"="v_38","Senioren"="v_39","Junge Erwachsene"="v_40","Neubürger"="v_41","Pendler"="v_42","Menschen mit Behinderung"="v_43"))
                  ),
                  radioButtons("Vergleich","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil", "Zeit"="jahr")),
                  br(),
                  helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
                  ), 
              mainPanel(
                plotOutput("distPlot",  width = "100%")
              ))
   ),
   tabPanel("Zufriedenheit", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR2", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Wie zufrieden sind Sie mit"=c("ihrem gegenwärtigen Leben"="v_46", "ihrer finanziellen Lage"="v_52", "ihrer Wohnsituation"="v_53", "ihrer Arbeit"="v_54", "ihren sozialen Kontakten"="v_55", "ihrer Freizeit"="v_56", "ihrer Gesundheit"="v_57"),
                              "Wie bewerten Sie folgende Aspekte Bambergs:"=c("Wohnungsmarkt"="v_63", "Einkaufsmoeglichkeiten"="v_64","Gastronomie"="v_65","Kulturelle Veranstaltungen"="v_66", "Öffentliche Verkehrsmittel"="v_67", "Arbeitsmarkt"="v_68", "Öffentliche Sicherheit"="v_69","Parkmöglichkeiten"="v_70","Medizinische Versorgung"="v_71","Freizeitangebot"="v_72","Wirtschaftskraft"="v_73","Bildungsangebote"="v_74", "Kinderbetreuungsangebot"="v_75", "Seniorenbetreuungsangebot"="v_76"))
                ),
                radioButtons("Vergleich2","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil", "Zeit"="jahr")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
                            ),
              
              mainPanel(
                plotOutput("distPlot2",  width = "100%")
              ))
      ),
   tabPanel("Probleme", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR3", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Wie gross schätzen Sie folgende Probleme in Bamberg ein:"=c("Müll/Schmutz"="v_82", "Belästigung/Rühestörung"="v_83", "Vandalismus"="v_84", "Kriminalität"="v_85"))
                ),
                radioButtons("Vergleich3","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil", "Zeit"="jahr")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot3",  width = "100%")
              ))
   ),
   tabPanel("Integration", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR4", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Wie gut funktioniert das Zusammenleben mit Menschen aus"=c("unterschiedlichen Ländern?"="v_111"),
                              "Das Zusammenleben mit Menschen aus anderen Ländern..."=c("erweitert meinen Horizont"="v_117", "fördert den kulturellen Austausch"="v_118","ist eine Bereicherung für Bamberg"="v_119","fördert Toleranz und Offenheit"="v_120", "schafft neue Probleme"="v_121", "ist mit Kosten verbunden"="v_122"),
                              "Die Integration ausländischer Personen klappt"=c("in Bamberg"="v_124"),
                              "Wie gross ist der Beitrag den folgende Akteure für die Integration in Bamberg leisten?"=c("Ausländische Personen"="v_142"))
                ),
                radioButtons("Vergleich4","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil", "Zeit"="jahr")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot4",  width = "100%")
              ))
   ),
   tabPanel("Bürgerbeteiligung", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR5", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Für die Bürger gibt es zahlreiche Möglichkeiten sich in die Entscheidungsfindung der Politik einzubeziehen. Habens Sie an den folgenden Formaten bereits teilgenommen?"=c("Bürgerbegehren"="v_268","Bürgerentscheid"="v_0ab75","Bürgerversammlung"="v_b4933","Mitgliedschaft in einer Bürgerinitiative"="v_bcfd1","Bürgerforen oder Themenwerkstatt (Zukunftswerkstatt)"="v_272","Informationsveranstaltung"="v_f9027","Arbeitskreis oder Workshop"="v_c054d","Runder Tisch"="v_aaa14","Online-Dialog"="v_276"),
                              "Würden sich die folgenden Änderungen positiv auf die Bürgerbeteiligung in Bamberg auswirken?"=c("regelmäßig durchgeführte Bürgerbefragungen nach wissenschaftlichen Kriterien"="v_277","Sensibilisierung bei den im Stadtrat vertretenen Parteien"="v_278","Einrichtung eines Budgets für Bürgerbeteiligungsmaßnahmen"="v_279","Verabschiedung von offiziellen Leitlinien für die Bürgerbeteiligung"="v_280"),
                              "Wie wichtig ist es Ihnen, im Bereich der Bürgerbeteiligung..."=c("... informiert zu werden?"="v_284","... um Rat gefragt zu werden?"="v_285","... mitentscheiden zu können?"="v_286"),
                              "Wie stark stimmen Sie den folgenden Aussagen zu?"=c("An Volks- oder Bürgerentscheiden beteiligen sich all Bevölkerungsgruppen gleichermaßen"="v_289","Durch Bürgerentscheide kann es bei geringer Beteiligung passieren, dass Minderheiten Ansichten gegen den Willen der Mehrheit durchbringen"="v_290","Ich könnte mir vorstellen, mich längere Zeit regelmäßig politisch zu engarieren"="v_291"))
                ),
                radioButtons("Vergleich5","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot5",  width = "100%")
              ))
   ),
   tabPanel("Verkehr", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR6", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Wie häufig nutzen Sie die folgenden Verkehrsmittel, wenn Sie in Bamberg unterwegs sind?"=c("Zu Fuß"="v_294","Fahrrad (inkl. E-Bike)"="v_295","Auto (inkl. Roller und Motorrad)"="v_296","Bus"="v_297"),
                              "Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder als Fußgänger...?"=c("auf straßenbegleitenden Geh- und Radwegen"="v_299","in der Fußgängerzone"="v_300","in verkehrsberuhigten Bereichen (Spielstraßen)"="v_301","auf straßenbegleitenden Gehwegen mit angrenzendem Radweg"="v_302", "auf gemeinsamen Geh- und Radwegen"="v_303","auf straßenbegleitenden Gehwegen, die für den Radverkehr freigegeben sind"="v_304","insgesamt"="v_305"),
                              "Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder als Radfahrer...?"=c("auf straßenbegleitenden Radwegen"="v_306","auf Schutzstreifen auf der Fahrbahn"="v_307","in Gegenrichtung in dafür freigegeben Einbahnstraßen"="v_308","auf Radfahrstreifen auf der Fahrbahn"="v_309","auf der Fahrbahn in Tempo 30-Zonen"="v_310","auf gemeinsamen Geh- und Radwegen"="v_311","auf Fahrradstraßen"="v_312","insgesamt"="v_313"),
                              "Wie bewerten Sie die folgenden Vorschläge zum Thema Verkehr in Bamberg?"=c("Rückbau von Parkplätzen zugunsten von Fußgängern und Fahrradfahrern"="v_314","Umbau der Langen Straße in Bamberg zu einem Verkehrsraum mit gleichberechtigter Nutzung für alle Verkehrsteilnehmer (Shared Space)"="v_315"),
                              "Benötigt Bamberg einen grundsätzlich besser ausgebauten Busverkehr?"=c("Ja oder nein?"="v_319"),
                              "Stimmen Sie den folgenden Aussagen zu? Ein Schwachpunk des Bamberger Busangebots ist..."=c("die Taktung"="v_320","die fehlende Ringverbindung/nur Anfahrt ZOB"="v_321","die Fahrzeitenregelung unter der Woche"="v_322","die Fahrzeitenregelung am Wochenende"="v_323"),
                              "Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?"=c("Radler nehmen Rücksicht auf Personen zu Fuß"="v_325","Radler nehmen Rücksicht auf den Autoverkehr"="v_326","Autofahrer/innen nehmen Rücksicht auf Fußgänger"="v_327","Autofahrer/innen nehmen Rücksicht auf den Radverkehr"="v_328","Radler fahren häufig auf den Gehwegen"="v_329","Fußgänger/innen laufen häufig auf den Radwegen"="v_330","Fahrräder werden häufig störend auf Gehwegen abgestellt"="v_331","Autos werden häufig störend auf Geh- und Radwegen abgestellt"="v_332"))
                ),
                radioButtons("Vergleich6","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot6",  width = "100%")
              ))
   ),
   tabPanel("Zukunftsaussichten", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR7", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "In den kommenden zehn Jahren, wie sehen Sie Bambergs"=c("Zukunftsaussichten?"="v_177","wirtschaftliche Lage?"="v_179"),
                              "Wie sehen Sie aktuell Bambergs"=c("wirtschaftliche Lage?"="v_178"))
                ),
                radioButtons("Vergleich7","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil", "Zeit"="jahr")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot7",  width = "100%")
              ))
   ),
   tabPanel("Universität Bamberg", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR8", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Wie sehr stimmen Sie folgenden Aussagen zur Universität Bamberg zu?"=c("Ohne die Universität stünde Bamberg schlechter da"="v_186","Die Universität ist wichtig für die Entwicklung der Stadt"="v_187","Die Universität ist ein wichtiger Wirtschaftsfaktor"="v_188","Die Studierenden beleben das Stadtleben"="v_189","Universität breitet sich zu stark aus"="v_190","Die Universität nutzt vor allem Geschäften und Vermietern, weniger den Bürgern"="v_191"),
                              "Wie schätzen Sie die Wirkung der Universität Bamberg ein?"=c("als Arbeitgeber"="v_197", "als Anbieter von Weiterbildung"="v_198","als Auftraggeber für die Wirtschaft"="v_199","für das kulturelle Leben"="v_200", "für das Image der Stadt"="v_201", "für den Zuzug von Fachkräften"="v_202","für den Zuzug junger Menschen"="v_203","für den Tourismus"="v_204","für den Wohnungsmarkt"="v_205","für den Verkehr"="v_206"),
                              "Haben Sie an den folgenden Veranstaltungsangebote der Universität Bamberg bereits teilgenommen?"=c("Kinder-Uni"="v_333","Girls Day"="v_334","Bamberger Hegelwoche"="v_335","Bamberger Informatik Tag (BIT)"="v_336","Musik in der Universität"="v_337","Literatur in der Universität/Poetik-Professur"="v_338","Öffentliche Vortragsreihen und Ringvorlesungen (z.B. Arachäologisches Kolloquium, KulturPLUS-Ringvorlesung, Ringvorlesung des Mittelalterzentrums, Theologisches Forum, WegE-Lectures, Bayerisches Orientkolloquium)"="v_339"))
                ),
                radioButtons("Vergleich8","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="alter", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot8",  width = "100%")
              ))
   )
 )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
   output$distPlot <- renderPlot({
     
     plotfuc(input$INPUTVAR, input$Vergleich)
     
     
   }, height = 600, width = 600)
   
   
   
   output$distPlot2 <- renderPlot({
     
     plotfuc(input$INPUTVAR2, input$Vergleich2)
     
     
   }, height = 600, width = 600)
   
   
   
   output$distPlot3 <- renderPlot({
     
     plotfuc(input$INPUTVAR3, input$Vergleich3)
     
     
   }, height = 600, width = 600)
   
   
   
   output$distPlot4 <- renderPlot({
     
     plotfuc(input$INPUTVAR4, input$Vergleich4)
     
     
   }, height = 600, width = 600)
   
   
   output$distPlot5 <- renderPlot({
     
     plotfuc2(input$INPUTVAR5, input$Vergleich5)
     
     
   }, height = 600, width = 600)
   
   
   
   output$distPlot6 <- renderPlot({
     
     plotfuc2(input$INPUTVAR6, input$Vergleich6)
     
     
   }, height = 600, width = 600)
   
   output$distPlot7 <- renderPlot({
     
     plotfuc(input$INPUTVAR7, input$Vergleich7)
     
     
   }, height = 600, width = 600)
   
   output$distPlot8 <- renderPlot({
     
     plotfuc2(input$INPUTVAR8, input$Vergleich8)
     
     
   }, height = 600, width = 600)
   
}

# Run the application 
shinyApp(ui = ui, server = server)

