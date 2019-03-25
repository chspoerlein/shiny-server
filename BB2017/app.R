library(sf)
library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(stringr)

# 49.84-49.95
# 10.8-10.98


#setwd("C:/Users/ba3ef8/Google Drive/Buergerbefragung/2017/BB2017/")

#data <- read.dta("bb20172.dta", convert.factors=F)

data <- read.csv2("test.csv", sep=";")
data$geschlecht <- str_replace(data$geschlecht, "ae", "ä")
data$beruf <- str_replace(data$beruf, "ae", "ä")

load("add_data.RData")
source("plotfuc.R", local=T)

bil<- c("keinen Abschluss","Hauptschulabschluss","Mittlere Reife","(Fach-)Abitur","Hochschule")
ber <- c("in Ausbildung","(Fach-)Arbeiter/in","Angestelle/r", "Beamter/in", "Selbständige/r","Freiberufler/in")
data$bildung <- fct_relevel(data$bildung,bil)
data$beruf <- fct_relevel(data$beruf,ber)




# Define UI for application that draws a histogram
ui <- navbarPage("Bamberger Bürgerbefragung 2017",
   
   # Application title
   #titlePanel("Bamberger Bürgerbefragung 2017"),

   
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
                                "Wie bewerten Sie Bambergs "=c("Lebensqualität"="V1"),
                                "Welche Eigenschaft besitzt Bamberg?"=c("Jung"="V2A", "Sauber"="V2B","Modern"="V2C","Sicher"="V2D", "Dynamisch"="V2E", "Weltoffen"="V2F", "Gepflegt"="V2G","Interessant"="V2H","Preiswert"="V2I","Attraktiv"="V2J","Traditionsbewusst"="V2K","Lebendig"="V2L"),
                                "Wie attraktiv bewerten Sie folgende Ort Bambergs?"=c("Stadtkern"="V3A","Wohngegend"="V3B","Parkanlagen"="V3C","Naherholungsgebiete"="V3D"),
                                "Wie attraktiv sind diese Orte für"=c("Kinder"="V4A","Jugendliche"="V4B","Familien"="V4C","Senioren"="V4D","Junge Erwachsene"="V4E","Neubürger"="V4F","Pendler"="V4G","Menschen mit Behinderung"="V4H"))
                  ),
                  radioButtons("Vergleich","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="V26", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
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
                              "Wie zufrieden sind Sie mit"=c("ihrem gegenwärtigen Leben"="V5", "ihrer finanziellen Lage"="V6A", "ihrer Wohnsituation"="V6B", "ihrer Arbeit"="V6C", "ihren sozialen Kontakten"="V6D", "ihrer Freizeit"="V6E", "ihrer Gesundheit"="V6F"),
                              "Wie bewerten Sie folgende Aspekte Bambergs:"=c("Wohnungsmarkt"="V7A", "Einkaufsmoeglichkeiten"="V7B","Gastronomie"="V7C","Kulturelle Veranstaltungen"="V7D", "Öffentliche Verkehrsmittel"="V7E", "Arbeitsmarkt"="V7F", "Öffentliche Sicherheit"="V7G","Parkmöglichkeiten"="V7H","Medizinische Versorgung"="V2I","Freizeitangebot"="V7J","Wirtschaftskraft"="V7K","Bildungsangebote"="V7L", "Kinderbetreuungsangebot"="V7M", "Seniorenbetreuungsangebot"="V7N"))
                ),
                radioButtons("Vergleich2","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="V26", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
                            ),
              
              mainPanel(
                plotOutput("distPlot2",  width = "100%")
              ))
      ),
   tabPanel("Probleme, Polizei, Sicherheit", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR3", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Wie gross schätzen Sie folgende Probleme in Bamberg ein:"=c("Müll/Schmutz"="V8A", "Belästigung/Rühestörung"="V8B", "Vandalismus"="V8C", "Kriminalität"="V8D"),
                              "Wie stark stimmen Sie folgenden Aussagen zu:"=c("Die Polizei ist bei Bedarf gleich zur Stelle"="V9A", "Die Polizei zeigt ausreichend Präsenz"="V9B","Ich habe vertrauen in die Polizei"="V9C","Die Polizei verfügt über genügend Personal"="V9D", "Die Polizei sorgt für Sicherheit"="V9E", "Polizisten sind freundlich und hilfsbereit"="V9F"),
                              "Wie sicher fühlen Sie sich ..."=c("ganz allgemein in Bamberg?"="V10A","in Ihrer Wohnung?"="V10B","in Ihrer Wohngegend?"="V10C","im Stadtzentrum?"="V10D","im Bus/in der Bahn?"="V10E","in den Grünanlagen der Stadt?"="V10F","bei Nacht?"="V10G","wenn Sie alleine unterwegs sind?"="V10H"))
                ),
                radioButtons("Vergleich3","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="V26", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
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
                              "Wie gut funktioniert das Zusammenleben mit Menschen aus"=c("unterschiedlichen Ländern?"="V11"),
                              "Das Zusammenleben mit Menschen aus anderen Ländern..."=c("erweitert meinen Horizont"="V12A", "fördert den kulturellen Austausch"="V12B","ist eine Bereicherung für Bamberg"="V12C","fördert Toleranz und Offenheit"="V12D", "schafft neue Probleme"="V12E", "ist mit Kosten verbunden"="V12F"),
                              "Die Integration ausländischer Personen klappt"=c("in Bamberg"="V13"),
                              "Wie gross ist der Beitrag den folgende Akteure für die Integration in Bamberg leisten?"=c("Bamberger Bevölkerung"="V14A","Bamberger Stadtverwaltung"="V14B","Kirchen"="V14C","Arbeitgeber"="V14D","Sozialverbände"="V14E","Ehrenamtliche"="V14F","Ausländische Personen"="V15"),
                              "Wie oft haben Sie persönlichen Kontakt zu Personen mit Migrationshintergrund?"=c("in der Arbeit"="V16A","in der Nachbarschaft"="V16B","im Freundeskreis"="V16C","in der Familie"="V16D","in der Freizeit"="V16E"),
                              "Wie oft haben Sie persönlichen Kontakt zu Asylsuchenden und Flüchtlingen?"=c("in der Arbeit"="V17A","in der Nachbarschaft"="V17B","im Freundeskreis"="V17C","in der Familie"="V17D","in der Freizeit"="V17E"),
                              "Wie sehr stimmen Sie den folgenden Aussagen zu?"=c("Die AEO bringt für Bamberg Vorteile"="V18A","Die derzeitige Kapizität ist zu hoch"="V18B","Das Gelände soll als Wohnraum für Bamberger und Asylberechtigte genutzt werden"="V18C","Die neue AEO-Buslinie ist zu begrüssen"="V18D"))
                ),
                radioButtons("Vergleich4","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="V26", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot4",  width = "100%")
              ))
   ),
   tabPanel("Zukunftsaussichten", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR5", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "In den kommenden zehn Jahren, wie sehen Sie Bambergs"=c("Zukunftsaussichten?"="V19","wirtschaftliche Lage?"="V21"),
                              "Wie sehen Sie aktuell Bambergs"=c("wirtschaftliche Lage?"="V20"))
                ),
                radioButtons("Vergleich5","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="V26", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot5",  width = "100%")
              ))
   ),
   tabPanel("Universität Bamberg", fluid=T,
            sidebarLayout(
              sidebarPanel(
                img(src="Download.png", height=72, width=72, align="left"),
                img(src="logo_stadt-bamberg.png", height=72, width=72, align="right"),
                selectInput("INPUTVAR6", 
                            "Wählen Sie eine Frage:", 
                            choices = list(
                              "Wie sehr stimmen Sie folgenden Aussagen zur Universität Bamberg zu?"=c("Ohne die Universität stünde Bamberg schlechter da"="V22A","Die Universität ist wichtig für die Entwicklung der Stadt"="V22B","Die Universität ist ein wichtiger Wirtschaftsfaktor"="V22C","Die Studierenden beleben das Stadtleben"="V22D","Universität breitet sich zu stark aus"="V22E","Die Universität nutzt vor allem Geschäften und Vermietern, weniger den Bürgern"="V22F"),
                              "Wie schätzen Sie die Wirkung der Universität Bamberg ein?"=c("als Arbeitgeber"="V23A", "als Anbieter von Weiterbildung"="V23B","als Auftraggeber für die Wirtschaft"="V23C","für das kulturelle Leben"="V23D", "für das Image der Stadt"="V23E", "für den Zuzug von Fachkräften"="V23F","für den Zuzug junger Menschen"="V23G","für den Tourismus"="V23H","für den Wohnungsmarkt"="V23I","für den Verkehr"="V23J"))
                ),
                radioButtons("Vergleich6","Wählen Sie eine Vergleichsgruppe:", choices = c("Keine"="gg","Geschlecht"="geschlecht", "Alter"="V26", "Bildung"="bildung","Beruf"="beruf", "Stadtteile"="stadt_teil")),
                br(),
                helpText(a(href="https://www.uni-bamberg.de/sozstruk/personen/team/dr-rer-pol-christoph-spoerlein/", target="_blank", "© Christoph Spörlein")," und ",a(href="https://www.uni-bamberg.de/stat-oek/team/lehrstuhlteam/dr-martin-messingschlager/", target="_blank", "Martin Messingschlager"))
              ),
              mainPanel(
                plotOutput("distPlot6",  width = "100%")
              ))
   )
  # )
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
     
     plotfuc(input$INPUTVAR5, input$Vergleich5)
     
     
   }, height = 600, width = 600)
   
   
   
   output$distPlot6 <- renderPlot({
     
     plotfuc(input$INPUTVAR6, input$Vergleich6)
     
     
   }, height = 600, width = 600)
   
}

# Run the application 
shinyApp(ui = ui, server = server)

