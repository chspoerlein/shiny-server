library(rdrop2)
library(shiny)
library(rhandsontable)
library(ggplot2)
library(dplyr)

geodata <- readRDS("geodata.rds") %>% filter(long>-12, long<30, lat>30)
cnames <- readRDS("cnames.rds")

#geodata <- get_eurostat_geospatial(output_class = "df", resolution = "60", nuts_level = 0)
#dat <- get_eurostat("tps00203", time_format = "num") %>% 
#  filter(time==2017, unit=="PC_ACT", str_length(geo)==2) %>%
#  select(geo, values) %>% rename(land=geo, Arbeitslosenquote=values)


ui <- fluidPage(
  
  # Application title
  titlePanel("Begleitapp für Seminare"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Werte in das Datenblatt eingeben und \"Speichern\" klicken"),
      actionButton("saveBtn","Speichern"),
      br(),
      br(),
      selectInput(
        "char", "Was soll über die Länder verglichen werden?",
        c(Waehle = "",
          Arbeitslosenquote = "Arbeitslosenquote",
          Integrationsindex = "MIPEX" ,
          SozialerGradient = "SozialerGradient"
          )),
      br(),
      helpText(a(href="https://cspoerlein.com", target="_blank", "© Christoph Spörlein"))
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datenblatt", 
          rHandsontableOutput("table")),
        tabPanel("Visualisierung",
                 plotOutput("europe",  width = "100%"))
    )
  )
))




df1 <- drop_read_csv("responses/cntry.csv")

server <- function(input, output) {
  
  showModal(modalDialog(
    title = "Bitte beachten:",
    HTML("Danke, dass Sie sich die Zeit nehmen, meine Kursapp zu testen. <br>
        <br>
         Studierende können in dieser kursbegleitenden App die Werte von Charakteristika des ihnen im Vorfeld zugeteilten Landes eingeben 
         und sehen über den \"Visualisierungs\"-Tab sofort dessen Rang im europäischen Gefüge. <br>
         <br>
         Für diese Demonstration sind die Daten der Arbeitslosenquote bereits vollständig eingegeben und Werte andere Charakteristika teilweise. <br> 
         <br>
        In der nächsten Sitzung werden wir Multikulturalismus im europäischen Vergleich behandeln.<br>
        <br>
          Geben Sie dafür den recherchierten Werte des \"Migrant Integration Policy Index (MIPEX)\" eines Landes Ihrer Wahl in das Datenblatt ein (Werte zwischen 0 und 100) und visualisieren Sie diesen anschließend!"),
    easyClose = FALSE
    ))
  
  output$table <- renderRHandsontable({
    rhandsontable(df1) %>%
      hot_col("Land", readOnly = TRUE) %>%
      hot_col("Arbeitslosenquote", readOnly = TRUE)
  })
  
  # on click of button the file will be saved to the working directory
  observeEvent(input$saveBtn, 
               write.csv(hot_to_r(input$table), file = "cntry.csv",row.names = FALSE))
  
  observeEvent(input$saveBtn, 
               drop_upload("cntry.csv", path = "responses"))
  
  
  output$europe <- renderPlot({
    df1 <- drop_read_csv("responses/cntry.csv")
       map_data <- geodata %>% 
      left_join(df1, by =  c("CNTR_CODE" = "Land")) %>% filter(long>-12, long<30, lat>30)
      if (input$char!=""){
      ggplot() + geom_polygon(data = map_data, aes_string(fill = input$char, x = "long",  y = "lat", group = "group")) +
        geom_path(data = map_data, aes(x = long,  y = lat, group = group), color = "black", size = 0.1) +
        geom_text(data=cnames, aes(long, lat, label = FID), size=5, color="white")+
        theme_classic() + 
        coord_fixed(1.3) +
        labs(title="", x="", y="", fill=input$char) +
        scale_fill_viridis_c(direction=-1, na.value="white") +
        theme(axis.ticks = element_blank(), axis.line=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
      }
  }, height = 700, width = 700 )
}

shinyApp(ui = ui, server = server)

