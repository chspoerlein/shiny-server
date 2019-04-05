library(tidyverse)
library(googlesheets)

#shiny_token <- gs_auth()
#saveRDS(shiny_token,"shiny_app_token.rds")

#Data_pamina <- gs_new("Data_pamina") %>% 
#  gs_ws_rename(from = "Tabellenblatt1", to = "Data")      
#Data_pamina <- Data_pamina %>% 
#  gs_edit_cells(ws = "Data", input = cbind("sitzung3","gruppe", "slider21", "slider22", "slider23", "text21",  "slider24", "text22", "slider25", "text23","system.time"), trim = TRUE)


#dat <- gs_key(Data_pamina$sheet_key)
dat <- gs_key("1d6c-IT-AKqdmj2JUfm0LziqXLYOPXB7sBplvBVnViZE")
dat2 <- dat %>% gs_read(ws="Data")




dat <- dat %>% gs_read(ws="Data") %>%                                                                      
  gs_add_row(ws = "Data", input = Results())   