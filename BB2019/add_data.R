#### reading data from numerous sources
library(sf)
library(dplyr)
library(ggmap)

setwd("C:/Users/ba3ef8/Google Drive/Buergerbefragung/2019/BB2019/")

noten <- c("sehr gut","gut","befriedigend","ausreichend","mangelhaft","ungenügend") #6
zehner <- c("völlig zufrieden","2","3","4","5","6","7","8","9","völlig unzufrieden") #10
grossgering <- c("sehr gross","eher gross","teils, teils","eher gering","sehr gering") #5
vollundganz <- c("voll und ganz","eher","teils, teils","eher weniger","überhaupt nicht") #5
unsicher <- c("völlig sicher","2","3","4","5","6","7","8","9","10","völlig unsicher") #11 
sehrgut <- c("sehr gut","gut","mittelmässig","schlecht","sehr schlecht") #5
hauefig <- c("sehr häufig","häufig","gelegentlich","selten","nie") #5
positiv <- c("sehr positiv","eher positiv","teils, teils","eher negativ","sehr negativ") #5
wieoft <- c("Ja, mehrfach","Ja, einmalig","Nein","Nein, aber evtl. zukünftig") #4
wichtig <- c("sehr wichtig","wichtig","teilweise","unwichtig","sehr unwichtig") #5
sicher <- c("sehr sicher","eher sicher","teilweise","eher unsicher","sehr unsicher") #5
janein <- c("Ja","Nein")
wieoft2 <- c("Ja, mehrfach","Ja, einmalig","Nein","Kenne ich nicht") #4

lab_list <- list("wieoft2"=wieoft2,"janein"=janein,"sicher"=sicher,"wichtig"=wichtig,"wieoft"=wieoft,"noten"=noten, "zehner"=zehner, "grossgering"=grossgering, "vollundganz"=vollundganz,"unsicher"=unsicher,"sehrgut"=sehrgut,"hauefig"=hauefig,"positiv"=positiv)

col_list <- list("2"=c("#ca0020","#0571b0"),
                 "4"=c("#ca0020","#f4a582","#92c5de","#0571b0"),
                 "5"=c('#ca0020','#f4a582','#f7f7f7','#92c5de','#0571b0'),
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



map <- get_stamenmap(c(left = 10.82637, bottom = 49.84269, right = 10.96076, top = 49.92888), #"Bamberg",
                     maptype = "toner-lite", zoom=13, source="stamen", color="bw", force=T)



a <- unlist(attr(map,"bb")[1, ])
bb <- st_bbox(shape)



labs <- as.data.frame(matrix(c("v_1","Wie bewerten Sie die Lebensqualität in der Stadt Bamberg ganz allgemein?","","sehrgut",2,
                               "v_9","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Jung","vollundganz",1,
                               "v_10","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Sauber","vollundganz",1,
                               "v_11","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Modern","vollundganz",1,
                               "v_12","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Sicher","vollundganz",1,
                               "v_13","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Dynamisch","vollundganz",1,
                               "v_14","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Weltoffen","vollundganz",1,
                               "v_15","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Gepflegt","vollundganz",1,
                               "v_16","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Interessant","vollundganz",1,
                               "v_17","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Preiswert","vollundganz",1,
                               "v_18","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Attraktiv","vollundganz",1,
                               "v_19","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Traditionsbewusst","vollundganz",1,
                               "v_20","Inwieweit treffen folgende Eigenschaften auf die Stadt Bamberg zu?","Lebendig","vollundganz",1,
                               "v_26","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Attraktivität des Stadtkerns","noten",2,
                               "v_27","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Attraktivität Ihrer Wohngegend","noten",2,
                               "v_28","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Parkanlagen","noten",2,
                               "v_29","Wie bewerten Sie folgende Aspekte des Bamberger Stadtbilds?","Naherholungsgebiete (z.B. Bruderwald, Altenburg)","noten",2,
                               "v_36","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Kinder","noten",2,
                               "v_37","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Jugendliche","noten",2,
                               "v_38","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Familien","noten",2,
                               "v_39","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Senioren","noten",2,
                               "v_40","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Junge Erwachsene","noten",2,
                               "v_41","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Neubürger/Zugezogene","noten",2,
                               "v_42","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Pendler","noten",2,
                               "v_43","Wie schätzen Sie die Angebote in Bamberg für folgende \nBevölkerungsgruppen ein?","Menschen mit Behinderung","noten",2,
                               "v_46","Wie zufrieden sind Sie im Großen und Ganzen mit Ihrem \ngegenwärtigen Leben?","","zehner",4,
                               "v_52","Und wie zufrieden sind Sie mit ...?","Ihrer finanziellen Lage","zehner",4,
                               "v_53","Und wie zufrieden sind Sie mit ...?","Ihrer Wohnsituation","zehner",4,
                               "v_54","Und wie zufrieden sind Sie mit ...?","Ihrer Arbeit/Ausbildung","zehner",4,
                               "v_55","Und wie zufrieden sind Sie mit ...?","Ihren sozialen Kontakten","zehner",4,
                               "v_56","Und wie zufrieden sind Sie mit ...?","Ihrer Freizeit","zehner",4,
                               "v_57","Und wie zufrieden sind Sie mit ...?","Ihrer Gesundheit","zehner",4,
                               "v_63","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Wohnungsmarkt","noten",2,
                               "v_64","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Einkaufsmöglichkeiten","noten",2,
                               "v_65","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Gastronomie","noten",2,
                               "v_66","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Kulturelle Veranstaltungen","noten",2,
                               "v_67","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Öffentliche Verkehrsmittel","noten",2,
                               "v_68","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Arbeitsmarkt","noten",2,
                               "v_69","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Öffentliche Sicherheit","noten",2,
                               "v_70","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Parkmöglichkeiten","noten",2,
                               "v_71","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Medizinische Versorgung","noten",2,
                               "v_72","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Freizeitangebote","noten",2,
                               "v_73","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Wirtschaftskraft","noten",2,
                               "v_74","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Bildungsangebote","noten",2,
                               "v_75","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Betreuungsangebot für Kinder und Jugendliche","noten",2,
                               "v_76","Wenn Sie an Bamberg denken, wie bewerten Sie die folgenden \nAspekte?","Betreuungsangebot für Senioren","noten",2,
                               "v_82","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Müll/Schmutz","grossgering",1,
                               "v_83","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Belästigung/Ruhestörung","grossgering",1,
                               "v_84","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Sachbeschädigung, Vandalismus, Schmierereien","grossgering",1,
                               "v_85","Wie groß schätzen Sie folgende Probleme in Bamberg ein?","Kriminalität","grossgering",1,
                               "v_111","Bamberg ist eine Stadt, in der viele Menschen unterschiedlichster \nHerkunft leben. Wie funktioniert Ihrer Meinung nach in Bamberg das \nZusammenleben mit Menschen aus unterschiedlichen Ländern?","","sehrgut",2,	
                               "v_117","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","erweitert meinen Horizont.","vollundganz",1,
                               "v_118","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","fördert den kulturellen Austausch.","vollundganz",1,
                               "v_119","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","ist eine Bereicherung für Bamberg.","vollundganz",1,
                               "v_120","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","fördert Toleranz und Offenheit.","vollundganz",1,
                               "v_121","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","schafft neue Probleme.","vollundganz",1,
                               "v_122","Und wie sehr stimmen Sie den folgenden Aussagen zu? \nDas Zusammenleben mit Menschen aus anderen Ländern ...","ist mit Kosten verbunden.","vollundganz",1,
                               "v_124","Die Integration ausländischer Personen klappt in Bamberg \nalles in allem ...","","sehrgut",2,
                               "v_142","Wie groß ist der Beitrag ausländischer Personen für ihre \nerfolgreiche Integration?","","grossgering",1,
                               "v_268","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Bürgerbegehren","wieoft",2,
                               "v_0ab75","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Bürgerentscheid","wieoft",2,
                               "v_b4933","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Bürgerversammlung","wieoft",2,
                               "v_bcfd1","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Mitgliedschaft in einer Bürgerinitiative","wieoft",2,
                               "v_272","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Bürgerforen oder Themenwerkstatt (Zukunftswerkstatt)","wieoft",2,
                               "v_f9027","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Informationsveranstaltung","wieoft",2,
                               "v_c054d","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Arbeitskreis oder Workshop","wieoft",2,
                               "v_aaa14","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Runder Tisch","wieoft",2,
                               "v_276","Für die Bürger gibt es zahlreiche Möglichkeiten sich in die \nEntscheidungsfindung der Politik einzubeziehen.\nHaben Sie an den folgenden Formaten bereits teilgenommen?","Online-Dialog","wieoft",2,
                               "v_277","Würden sich die folgenden Änderungen positiv auf die \nBürgerbeteiligung in Bamberg auswirken?","regelmäßig durchgeführte Bürgerbefragungen nach \nwissenschaftlichen Kriterien","vollundganz",1,
                               "v_278","Würden sich die folgenden Änderungen positiv auf die \nBürgerbeteiligung in Bamberg auswirken?","Sensibilisierung bei den im Stadtrat vertretenen Parteien","vollundganz",1,
                               "v_279","Würden sich die folgenden Änderungen positiv auf die \nBürgerbeteiligung in Bamberg auswirken?","Einrichtung eines Budgets für Bürgerbeteiligungsmaßnahmen","vollundganz",1,
                               "v_280","Würden sich die folgenden Änderungen positiv auf die \nBürgerbeteiligung in Bamberg auswirken?","Verabschiedung von offiziellen Leitlinien für die Bürgerbeteiligung","vollundganz",1,
                               "v_284","Wie wichtig ist es Ihnen, im Bereich der Bürgerbeteiligung...","informiert zu werden?","wichtig",1,
                               "v_285","Wie wichtig ist es Ihnen, im Bereich der Bürgerbeteiligung...","um Rat gefragt zu werden?","wichtig",1,
                               "v_286","Wie wichtig ist es Ihnen, im Bereich der Bürgerbeteiligung...","mitentscheiden zu können?","wichtig",1,
                               "v_289","Wie stark stimmen Sie den folgenden Aussagen zu?","An Volks- oder Bürgerentscheiden beteiligen sich alle \nBevölkerungsgruppen gleichermaßen","vollundganz",1,
                               "v_290","Wie stark stimmen Sie den folgenden Aussagen zu?","Durch Bürgerentscheide kann es bei geringer Beteiligung \npassieren, dass Minderheiten Ansichten gegen den Willen \nder Mehrheit durchbringen","vollundganz",1,
                               "v_291","Wie stark stimmen Sie den folgenden Aussagen zu?","Ich könnte mir vorstellen, mich längere Zeit regelmäßig \npolitisch zu engarieren","vollundganz",1,
                               "v_294","Wie häufig nutzen Sie die folgenden Verkehrsmittel, wenn Sie \nin Bamberg unterwegs sind?","Zu Fuß","hauefig",2,
                               "v_295","Wie häufig nutzen Sie die folgenden Verkehrsmittel, wenn Sie \nin Bamberg unterwegs sind?","Fahrrad (inkl. E-Bike)","hauefig",2,
                               "v_296","Wie häufig nutzen Sie die folgenden Verkehrsmittel, wenn Sie \nin Bamberg unterwegs sind?","Auto (inkl. Roller und Motorrad)","hauefig",2,
                               "v_297","Wie häufig nutzen Sie die folgenden Verkehrsmittel, wenn Sie \nin Bamberg unterwegs sind?","Bus","hauefig",2,
                               "v_299","Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder \nals Fußgänger...?","auf straßenbegleitenden Geh- und Radwegen","sicher",1,
                               "v_300","Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder \nals Fußgänger...?","in der Fußgängerzone","sicher",1,
                               "v_301","Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder \nals Fußgänger...?","in verkehrsberuhigten Bereichen (Spielstraßen)","sicher",1,
                               "v_302","Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder \nals Fußgänger...?","auf straßenbegleitenden Gehwegen mit angrenzendem Radweg","sicher",1,
                               "v_303","Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder \nals Fußgänger...?","auf gemeinsamen Geh- und Radwegen","sicher",1,
                               "v_305","Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder \nals Fußgänger...?","insgesamt","sicher",1,
                               "v_304","Wie sicher fühlen Sie sich in Bamberg als Fußgängerin oder \nals Fußgänger...?","auf straßenbegleitenden Gehwegen, die für \nden Radverkehr freigegeben sind","sicher",1,
                               "v_306","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","auf straßenbegleitenden Radwegen","sicher",1,
                               "v_307","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","auf Schutzstreifen auf der Fahrbahn","sicher",1,
                               "v_308","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","in Gegenrichtung in dafür freigegeben Einbahnstraßen","sicher",1,
                               "v_309","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","auf Radfahrstreifen auf der Fahrbahn","sicher",1,
                               "v_310","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","auf der Fahrbahn in Tempo 30-Zonen","sicher",1,
                               "v_311","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","auf gemeinsamen Geh- und Radwegen","sicher",1,
                               "v_312","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","auf Fahrradstraßen","sicher",1,
                               "v_313","Wie sicher fühlen Sie sich in Bamberg als Radfahrerin oder \nals Radfahrer...?","insgesamt","sicher",1,
                               "v_314","Wie bewerten Sie die folgenden Vorschläge zum Thema Verkehr \nin Bamberg?","Rückbau von Parkplätzen zugunsten von \nFußgängern und Fahrradfahrern","sehrgut",2,
                               "v_315","Wie bewerten Sie die folgenden Vorschläge zum Thema Verkehr \nin Bamberg?","Umbau der Langen Straße in Bamberg zu \neinem Verkehrsraum mit gleichberechtigter \nNutzung für alle Verkehrsteilnehmer (Shared Space)","sehrgut",2,
                               "v_319","Benötigt Bamberg einen grundsätzlich besser ausgebauten Busverkehr?","","janein",1,
                               "v_320","Stimmen Sie den folgenden Aussagen zu? Ein Schwachpunk des \nBamberger Busangebots ist...","die Taktung","vollundganz",1,
                               "v_321","Stimmen Sie den folgenden Aussagen zu? Ein Schwachpunk des \nBamberger Busangebots ist...","die fehlende Ringverbindung / nur Anfahrt ZOB","vollundganz",1,
                               "v_322","Stimmen Sie den folgenden Aussagen zu? Ein Schwachpunk des \nBamberger Busangebots ist...","die Fahrzeitenregelung unter der Woche","vollundganz",1,
                               "v_323","Stimmen Sie den folgenden Aussagen zu? Ein Schwachpunk des \nBamberger Busangebots ist...","die Fahrzeitenregelung am Wochenende","vollundganz",1,
                               "v_325","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Radler nehmen Rücksicht auf Personen zu Fuß","vollundganz",1,
                               "v_326","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Radler nehmen Rücksicht auf den Autoverkehr","vollundganz",1,
                               "v_327","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Autofahrer/innen nehmen Rücksicht auf Fußgänger","vollundganz",1,
                               "v_328","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Autofahrer/innen nehmen Rücksicht auf den Radverkehr","vollundganz",1,
                               "v_329","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Radler fahren häufig auf den Gehwegen","vollundganz",1,
                               "v_330","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Fußgänger/innen laufen häufig auf den Radwegen","vollundganz",1,
                               "v_331","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Fahrräder werden häufig störend auf Gehwegen abgestellt","vollundganz",1,
                               "v_332","Stimmen Sie den folgenden Aussagen zum Thema Verkehr zu?","Autos werden häufig störend auf Geh- und Radwegen abgestellt","vollundganz",1,
                               "v_177","Wie sind die Zukunftsaussichten der Stadt Bamberg in den \nkommenden zehn Jahren?","","sehrgut",2,
                               "v_178","Wie schätzen Sie die derzeitige wirtschaftliche Lage in Bamberg ein?","","sehrgut",2,
                               "v_179","Und wird sich die wirtschaftliche Lage in Bamberg in den \nnächsten zehn Jahren ... ?","","sehrgut",2,
                               "v_186","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Ohne die Universität stünde Bamberg (wesentlich) schlechter da.","vollundganz",1,
                               "v_187","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität ist wichtig für die Weiterentwicklung der Stadt.","vollundganz",1,
                               "v_188","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität ist in Bamberg ein wichtiger Wirtschaftsfaktor.","vollundganz",1,
                               "v_189","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Studierenden beleben das Stadtleben.","vollundganz",1,
                               "v_190","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität breitet sich in der Innenstadt zu stark aus.","vollundganz",1,
                               "v_191","Bamberg ist eine Universitätsstadt. Wie sehr stimmen Sie den \nfolgenden Aussagen zur Universität Bamberg zu?","Die Universität nutzt vor allem Geschäften und Vermietern und weniger den \nBamberger Bürgern.","vollundganz",1,
                               "v_197","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","als Arbeitgeber für den lokalen Arbeitsmarkt","positiv",1,
                               "v_198","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","als Anbieter von Weiterbildungsmöglichkeiten","positiv",1,
                               "v_199","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","als Auftraggeber für die lokale Wirtschaft","positiv",1,
                               "v_200","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für das kulturelle Leben","positiv",1,
                               "v_201","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für das Image der Stadt","positiv",1,
                               "v_202","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Zuzug von Fachkräften","positiv",1,
                               "v_203","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Zuzug junger Menschen","positiv",1,
                               "v_204","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Tourismus","positiv",1,
                               "v_205","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Wohnungsmarkt","positiv",1,
                               "v_206","Wie schätzen Sie die Wirkung der Universität Bamberg ein ...?","für den Verkehr","positiv",1,
                               "v_333","Haben Sie an den folgenden Veranstaltungsangebote der \nUniversität Bamberg bereits teilgenommen?","Kinder-Uni","wieoft2",1,
                               "v_334","Haben Sie an den folgenden Veranstaltungsangebote der \nUniversität Bamberg bereits teilgenommen?","Girls Day","wieoft2",1,
                               "v_335","Haben Sie an den folgenden Veranstaltungsangebote der \nUniversität Bamberg bereits teilgenommen?","Bamberger Hegelwoche","wieoft2",1,
                               "v_336","Haben Sie an den folgenden Veranstaltungsangebote der \nUniversität Bamberg bereits teilgenommen?","Bamberger Informatik Tag (BIT)","wieoft2",1,
                               "v_337","Haben Sie an den folgenden Veranstaltungsangebote der \nUniversität Bamberg bereits teilgenommen?","Musik in der Universität","wieoft2",1,
                               "v_338","Haben Sie an den folgenden Veranstaltungsangebote der \nUniversität Bamberg bereits teilgenommen?","Literatur in der Universität / Poetik-Professur","wieoft2",1,
                               "v_339","Haben Sie an den folgenden Veranstaltungsangebote der \nUniversität Bamberg bereits teilgenommen?","Öffentliche Vortragsreihen und Ringvorlesungen \n(z.B. Arachäologisches Kolloquium, KulturPLUS-Ringvorlesung, \nRingvorlesung des Mittelalterzentrums, Theologisches Forum, WegE-Lectures, \nBayerisches Orientkolloquium)","wieoft2",1
), ncol=5, byrow=T))

colnames(labs) <- c("var","titel","subtitel","label","cutoff")
labs$cutoff <- as.numeric(labs$cutoff)

### UPDATE
filllabel <- as.data.frame(matrix(c("noten","Prozentangaben \"sehr gut\" und \"gut\"",
                                    "zehner","Prozentangaben \"völlig zufrieden\" bis \"3\"",
                                    "grossgering","Prozentangaben \"sehr gross\"",
                                    "vollundganz","Prozentangaben \"voll und ganz\"",
                                    "unsicher","Prozentangaben \"völlig sicher\" bis \"4\"",
                                    "sehrgut","Prozentangaben \"sehr gut\" und \"gut\"",
                                    "hauefig","Prozentangaben \"sehr häufig\" und \"häufig\"",
                                    "positiv","Prozentangaben \"sehr positiv\"",
                                    "wichtig","Prozentangaben \"sehr wichtig\"",
                                    "wieoft","Prozentangaben \"Ja, mehrfach\"",
                                    "sicher","Prozentangaben \"sehr sicher\"",
                                    "janein","Prozentangaben \"Ja\"",
                                    "wieoft2","Prozentangaben \"Ja, mehrfach\""), ncol=2, byrow=T))
colnames(filllabel) <- c("label","filllabel")

labs <- labs %>% left_join(filllabel, by="label")

save(wieoft2,janein,sicher,wichtig,wieoft,a,bb,map,col_list,lab_list,labs, shape, wp, grossgering, hauefig,noten, positiv, sehrgut, unsicher, vollundganz, zehner, 
     file = "add_data.RData")

