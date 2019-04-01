### MC-Fragen für die Vorlesung

#setwd("C:/Users/ba3ef8/Google Drive/APPS/lehrfeedback/")

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
                               "D) Hierüber lassen sich die wichtigen und die weniger wichtigen Determinanten des Bildungserfolgs bestimmen.","A"
), ncol=8, byrow=T))

colnames(labs) <- c("sitzung","frage","text","antwort1","antwort2","antwort3","antwort4","loesung")

#save(labs, file = "add_data.RData")

