#### reading data from numerous sources
library(sf)
library(dplyr)
library(ggmap)

setwd("C:/Users/ba3ef8/Google Drive/Buergerbefragung/2017/BB2017/")

noten <- c("sehr gut","gut","befriedigend","ausreichend","mangelhaft","ungenügend") #6
zehner <- c("völlig zufrieden","2","3","4","5","6","7","8","9","völlig unzufrieden") #10
grossgering <- c("sehr gross","eher gross","teils, teils","eher gering","sehr gering") #5
vollundganz <- c("voll und ganz","eher","teils, teils","eher weniger","überhaupt nicht") #5
unsicher <- c("völlig sicher","2","3","4","5","6","7","8","9","10","völlig unsicher") #11 
sehrgut <- c("sehr gut","gut","mittelmässig","schlecht","sehr schlecht") #5
hauefig <- c("sehr häufig","häufig","gelegentlich","selten","nie") #5
positiv <- c("sehr positiv","eher positiv","teils, teils","eher negativ","sehr negativ") #5

lab_list <- list("noten"=noten, "zehner"=zehner, "grossgering"=grossgering, "vollundganz"=vollundganz,"unsicher"=unsicher,"sehrgut"=sehrgut,"hauefig"=hauefig,"positiv"=positiv)

col_list <- list("5"=c('#ca0020','#f4a582','#f7f7f7','#92c5de','#0571b0'),
                 "6"=c('#b2182b','#ef8a62','#fddbc7','#d1e5f0','#67a9cf','#2166ac'),
                 "10"=c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'),
                 "11"=c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))

shape <- read_sf(dsn = ".", layer = "Statistische_Stadtteile_Stadt_Bamberg_2014") %>% 
  mutate(small=case_when(St_Nr_Stad=="01" ~ 1,
                         St_Nr_Stad=="02" ~ 2,
                         St_Nr_Stad %in% c("03","04","05")~3,
                         St_Nr_Stad=="06"~4,
                         St_Nr_Stad=="07"~5,
                         St_Nr_Stad=="08"~6,
                         St_Nr_Stad=="09"~7,
                         St_Nr_Stad=="10"~8)) %>%
  select(-St_Nr_Stad)

shape <- st_transform(shape, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

wp <- cbind(shape, st_coordinates(st_centroid(shape$geometry))) %>%
  mutate(stadt_name=case_when(small==1 ~ "Mitte/\nWunderburg",
                              small==2 ~ "Nord",
                              small==3 ~ "Gartenstadt/\nOst",
                              small==4 ~ "Gereuth",
                              small==6 ~ "Berg",
                              small==5 ~ "Bug",
                              small==7 ~ "Gaustadt",
                              small==8 ~ "Wildensorg"))

wp[c(4,5,10),3] <- 10.91719 
wp[c(4,5,10),4] <- 49.9087



register_google(key="AIzaSyDEVZ1hH3_-UAyWOLmg4Zs2kAow8ABgs34", write=T)
map <- get_stamenmap(c(left = 10.82637, bottom = 49.84269, right = 10.96076, top = 49.92888), #"Bamberg",
                     maptype = "toner-lite", zoom=13, source="stamen", color="bw", force=T)



a <- unlist(attr(map,"bb")[1, ])
bb <- st_bbox(shape)



labs <- as.data.frame(matrix(c("V1","Wie bewerten Sie die Lebensqualität in der Stadt Bamberg ganz allgemein?","","sehrgut",2,
                               "V2A","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Jung","vollundganz",1,
                               "V2B","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Sauber","vollundganz",1,
                               "V2C","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Modern","vollundganz",1,
                               "V2D","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Sicher","vollundganz",1,
                               "V2E","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Dynamisch","vollundganz",1,
                               "V2F","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Weltoffen","vollundganz",1,
                               "V2G","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Gepflegt","vollundganz",1,
                               "V2H","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Interessant","vollundganz",1,
                               "V2I","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Preiswert","vollundganz",1,
                               "V2J","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Attraktiv","vollundganz",1,
                               "V2K","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Traditionsbewusst","vollundganz",1,
                               "V2L","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Lebendig","vollundganz",1,
                               "V3A","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Attraktivität des Stadtkerns","noten",2,
                               "V3B","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Attraktivität Ihrer Wohngegend","noten",2,
                               "V3C","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Parkanlagen","noten",2,
                               "V3D","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Naherholungsgebiete (z.B. Bruderwald, Altenburg)","noten",2,
                               "V4A","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Kinder","noten",2,
                               "V4B","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Jugendliche","noten",2,
                               "V4C","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Familien","noten",2,
                               "V4D","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Senioren","noten",2,
                               "V4E","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Junge Erwachsene","noten",2,
                               "V4F","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Neubürger/Zugezogene","noten",2,
                               "V4G","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Pendler","noten",2,
                               "V4H","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Menschen mit Behinderung","noten",2,
                               "V5","Wie zufrieden sind Sie im Großen und Ganzen mit Ihrem \ngegenwärtigen Leben?","","zehner",4,
                               "V6A","Und wie zufrieden sind Sie mit ...?","Ihrer finanziellen Lage","zehner",4,
                               "V6B","Und wie zufrieden sind Sie mit ...?","Ihrer Wohnsituation","zehner",4,
                               "V6C","Und wie zufrieden sind Sie mit ...?","Ihrer Arbeit/Ausbildung","zehner",4,
                               "V6D","Und wie zufrieden sind Sie mit ...?","Ihren sozialen Kontakten","zehner",4,
                               "V6E","Und wie zufrieden sind Sie mit ...?","Ihrer Freizeit","zehner",4,
                               "V6F","Und wie zufrieden sind Sie mit ...?","Ihrer Gesundheit","zehner",4,
                               "V7A","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Wohnungsmarkt","noten",2,
                               "V7B","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Einkaufsmöglichkeiten","noten",2,
                               "V7C","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Gastronomie","noten",2,
                               "V7D","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Kulturelle Veranstaltungen","noten",2,
                               "V7E","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Öffentliche Verkehrsmittel","noten",2,
                               "V7F","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Arbeitsmarkt","noten",2,
                               "V7G","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Öffentliche Sicherheit","noten",2,
                               "V7H","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Parkmöglichkeiten","noten",2,
                               "V7I","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Medizinische Versorgung","noten",2,
                               "V7J","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Freizeitangebote","noten",2,
                               "V7K","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Wirtschaftskraft","noten",2,
                               "V7L","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Bildungsangebote","noten",2,
                               "V7M","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Betreuungsangebot für Kinder und Jugendliche","noten",2,
                               "V7N","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Betreuungsangebot für Senioren","noten",2,
                               "V8A","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Müll/Schmutz","grossgering",1,
                               "V8B","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Belästigung/Ruhestörung","grossgering",1,
                               "V8C","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Sachbeschädigung, Vandalismus, Schmierereien","grossgering",1,
                               "V8D","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Kriminalität","grossgering",1,
                               "V9A","Wie stark stimmen Sie den folgenden Aussagen zu?","Die Polizei ist bei Bedarf gleich zur Stelle.","vollundganz",1,
                               "V9B","Wie stark stimmen Sie den folgenden Aussagen zu?","Die Polizei zeigt ausreichend Präsenz.","vollundganz",1,
                               "V9C","Wie stark stimmen Sie den folgenden Aussagen zu?","Ich habe Vertrauen in die Polizei.","vollundganz",1,
                               "V9D","Wie stark stimmen Sie den folgenden Aussagen zu?","Die Polizei verfügt über genügend Personal.","vollundganz",1,
                               "V9E","Wie stark stimmen Sie den folgenden Aussagen zu?","Die Polizei sorgt für Sicherheit.","vollundganz",1,
                               "V9F","Wie stark stimmen Sie den folgenden Aussagen zu?","Polizisten sind freundlich und hilfsbereit.","vollundganz",1,
                               "V10A","Wie sicher fühlen Sie sich ... ?","ganz allgemein in Bamberg","unsicher",5,
                               "V10B","Wie sicher fühlen Sie sich ... ?","in Ihrer Wohnung","unsicher",5,
                               "V10C","Wie sicher fühlen Sie sich ... ?","in Ihrer Wohngegend","unsicher",5,
                               "V10D","Wie sicher fühlen Sie sich ... ?","im Stadtzentrum","unsicher",5,
                               "V10E","Wie sicher fühlen Sie sich ... ?","im Bus/in der Bahn","unsicher",5,
                               "V10F","Wie sicher fühlen Sie sich ... ?","in Parks/Grünanlagen der Stadt","unsicher",5,
                               "V10G","Wie sicher fühlen Sie sich ... ?","bei Nacht","unsicher",5,
                               "V10H","Wie sicher fühlen Sie sich ... ?","wenn Sie alleine unterwegs sind","unsicher",5,
                               "V11","Bamberg ist eine Stadt, in der viele Menschen unterschiedlichster \nHerkunft leben. Wie funktioniert Ihrer Meinung nach in Bamberg das \nZusammenleben mit Menschen aus unterschiedlichen Ländern?","","sehrgut",2,	
                               "V12A","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","erweitert meinen Horizont.","vollundganz",1,
                               "V12B","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","fördert den kulturellen Austausch.","vollundganz",1,
                               "V12C","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","ist eine Bereicherung für Bamberg.","vollundganz",1,
                               "V12D","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","fördert Toleranz und Offenheit.","vollundganz",1,
                               "V12E","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","schafft neue Probleme.","vollundganz",1,
                               "V12F","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","ist mit Kosten verbunden.","vollundganz",1,
                               "V13","Die Integration ausländischer Personen klappt in Bamberg \nalles in allem ...","","sehrgut",2,
                               "V14A","Wie groß schätzen Sie den Beitrag folgender Akteure für die \nerfolgreiche Integration ausländischer Personen in Bamberg ein?","Bamberger Bevölkerung","grossgering",1,
                               "V14B","Wie groß schätzen Sie den Beitrag folgender Akteure für die \nerfolgreiche Integration ausländischer Personen in Bamberg ein?","Bamberger Stadtverwaltung","grossgering",1,
                               "V14C","Wie groß schätzen Sie den Beitrag folgender Akteure für die \nerfolgreiche Integration ausländischer Personen in Bamberg ein?","Kirchen","grossgering",1,
                               "V14D","Wie groß schätzen Sie den Beitrag folgender Akteure für die \nerfolgreiche Integration ausländischer Personen in Bamberg ein?","Arbeitgeber","grossgering",1,
                               "V14E","Wie groß schätzen Sie den Beitrag folgender Akteure für die \nerfolgreiche Integration ausländischer Personen in Bamberg ein?","Sozialverbände","grossgering",1,
                               "V14F","Wie groß schätzen Sie den Beitrag folgender Akteure für die \nerfolgreiche Integration ausländischer Personen in Bamberg ein?","Ehrenamtliche","grossgering",1,
                               "V15","Wie groß ist der Beitrag ausländischer Personen für ihre \nerfolgreiche Integration?","","grossgering",1,
                               "V16A","Wie häufig haben Sie persönliche Kontakte zu \nPersonen mit Migrationshintergrund ...?","in der Arbeit/Ausbildung","hauefig",2,
                               "V16B","Wie häufig haben Sie persönliche Kontakte zu \nPersonen mit Migrationshintergrund ...?","in der Nachbarschaft","hauefig",2,
                               "V16C","Wie häufig haben Sie persönliche Kontakte zu \nPersonen mit Migrationshintergrund ...?","im Freundeskreis","hauefig",2,
                               "V16D","Wie häufig haben Sie persönliche Kontakte zu \nPersonen mit Migrationshintergrund ...?","in der Familie","hauefig",2,
                               "V16E","Wie häufig haben Sie persönliche Kontakte zu \nPersonen mit Migrationshintergrund ...?","in der Freizeit / im Verein","hauefig",2,
                               "V17A","Wie häufig haben Sie persönlichen Kontakt zu \nAsylsuchenden und Flüchtlingen?","in der Arbeit/Ausbildung","hauefig",2,
                               "V17B","Wie häufig haben Sie persönlichen Kontakt zu \nAsylsuchenden und Flüchtlingen?","in der Nachbarschaft","hauefig",2,
                               "V17C","Wie häufig haben Sie persönlichen Kontakt zu \nAsylsuchenden und Flüchtlingen?","im Freundeskreis","hauefig",2,
                               "V17D","Wie häufig haben Sie persönlichen Kontakt zu \nAsylsuchenden und Flüchtlingen?","in der Familie","hauefig",2,
                               "V17E","Wie häufig haben Sie persönlichen Kontakt zu \nAsylsuchenden und Flüchtlingen?","in der Freizeit / im Verein","hauefig",2,
                               "V18A","Wie sehr stimmen Sie den folgenden Aussagen zu?","Die AEO (Aufnahmeeinrichtung Oberfranken) für Asylbewerber auf dem ehemaligen \nUS-Kasernen-Gelände bringt für Bamberg Vorteile.","vollundganz",1,
                               "V18B","Wie sehr stimmen Sie den folgenden Aussagen zu?","Die derzeitige Kapazität der AEO ist mit 4.500 Personen zu hoch.","vollundganz",1,
                               "V18C","Wie sehr stimmen Sie den folgenden Aussagen zu?","Das Gelände soll als Wohnraum für Bamberger, Asylberechtigte und anerkannte \nFlüchtlinge genutzt werden.","vollundganz",1,
                               "V18D","Wie sehr stimmen Sie den folgenden Aussagen zu?","Die neue AEO-Shuttle Buslinie für die Bewohner der Aufnahmeeinrichtung ist zu begrüßen.","vollundganz",1,
                               "V19","Wie sind die Zukunftsaussichten der Stadt Bamberg in den \nkommenden zehn Jahren?","","sehrgut",2,
                               "V20","Wie schätzen Sie die derzeitige wirtschaftliche Lage in Bamberg ein?","","sehrgut",2,
                               "V21","Und wird sich die wirtschaftliche Lage in Bamberg in den \nnächsten zehn Jahren ... ?","","sehrgut",2,
                               "V22A","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Ohne die Universität stünde Bamberg (wesentlich) schlechter da.","vollundganz",1,
                               "V22B","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität ist wichtig für die Weiterentwicklung der Stadt.","vollundganz",1,
                               "V22C","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität ist in Bamberg ein wichtiger Wirtschaftsfaktor.","vollundganz",1,
                               "V22D","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Studierenden beleben das Stadtleben.","vollundganz",1,
                               "V22E","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität breitet sich in der Innenstadt zu stark aus.","vollundganz",1,
                               "V22F","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität nutzt vor allem Geschäften und Vermietern und weniger den \nBamberger Bürgern.","vollundganz",1,
                               "V23A","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","als Arbeitgeber für den lokalen Arbeitsmarkt","positiv",1,
                               "V23B","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","als Anbieter von Weiterbildungsmöglichkeiten","positiv",1,
                               "V23C","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","als Auftraggeber für die lokale Wirtschaft","positiv",1,
                               "V23D","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für das kulturelle Leben","positiv",1,
                               "V23E","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für das Image der Stadt","positiv",1,
                               "V23F","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Zuzug von Fachkräften","positiv",1,
                               "V23G","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Zuzug junger Menschen","positiv",1,
                               "V23H","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Tourismus","positiv",1,
                               "V23I","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Wohnungsmarkt","positiv",1,
                               "V23J","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Verkehr","positiv",1
                               ), ncol=5, byrow=T))

colnames(labs) <- c("var","titel","subtitel","label","cutoff")
labs$cutoff <- as.numeric(labs$cutoff)


filllabel <- as.data.frame(matrix(c("noten","Prozentangaben \"sehr gut\" und \"gut\"",
                                    "zehner","Prozentangaben \"völlig zufrieden\" bis \"3\"",
                                    "grossgering","Prozentangaben \"sehr gross\"",
                                    "vollundganz","Prozentangaben \"voll und ganz\"",
                                    "unsicher","Prozentangaben \"völlig sicher\" bis \"4\"",
                                    "sehrgut","Prozentangaben \"sehr gut\" und \"gut\"",
                                    "hauefig","Prozentangaben \"sehr häufig\" und \"häufig\"",
                                    "positiv","Prozentangaben \"sehr positiv\""), ncol=2, byrow=T))
colnames(filllabel) <- c("label","filllabel")

labs <- labs %>% left_join(filllabel, by="label")

save(a,bb,map,col_list,lab_list,labs, shape, wp, grossgering, hauefig,noten, positiv, sehrgut, unsicher, vollundganz, zehner, 
     file = "add_data.RData")

