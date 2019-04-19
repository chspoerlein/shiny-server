#library(tidyverse)


plotfuc <- function(inputvar, vergleichsvar) {
  if (vergleichsvar=="gg") {
    
    data_plot <- data %>% 
      rename(plotvar=inputvar) %>% 
      filter(!is.na(plotvar),
             plotvar!="")
    
    plabel <- labs %>% filter(var==inputvar)
    
    ggplot(data_plot, aes(x=as.factor(plotvar))) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), fill="#00457D") +
      labs(x="",y="Prozent", title=plabel$titel, subtitle=paste0(plabel$subtitel," (n=",nrow(data_plot),")")) +
      scale_y_continuous(limits=c(0,0.65),labels=scales::percent) +
      theme_minimal()+ 
      scale_x_discrete(labels=lab_list[[as.character(plabel$label)]]) +
      theme(plot.title = element_text(face="bold"))
    
  } else if (vergleichsvar=="stadt_teil") {
    
    plabel <- labs %>% filter(var==inputvar)
    
    dat <- data %>%
      rename(plotvar=inputvar) %>%
      select(stadt_teil2, plotvar) %>%
      filter(!is.na(plotvar),
             !is.na(stadt_teil2)) %>%
      mutate(plotvar2=case_when(plotvar<=plabel$cutoff ~ 1,
                                plotvar>plabel$cutoff ~ 0),
                faelle=n()) %>%
      group_by(stadt_teil2) %>%
      summarise(plotvar3=mean(plotvar2)*100,
                faelle=max(faelle))
    
    
    shape2 <- shape %>% left_join(dat, by=c("small"="stadt_teil2")) 
    
    ggplot() +
      annotation_raster(map, xmin = a[2], xmax = a[4], ymin = a[1], ymax = a[3], interpolate = T) + 
      geom_sf(data=shape2, aes(fill = plotvar3, group=small), alpha=0.7, lwd=0.5, color="white") +
      geom_text(data=wp, aes(x=X, y=Y, label=stadt_name ),color="black", size=5) +
      theme_minimal() + 
      xlim(c(bb[1], bb[3])) + ylim(c(bb[2], bb[4])) + 
      labs(title=plabel$titel, x="", y="", fill=plabel$filllabel, subtitle=paste0(plabel$subtitel," (n=",max(dat$faelle),")")) +
      scale_fill_distiller(palette = "Spectral", limits=c(0,100)) + 
      theme(axis.ticks = element_blank(), axis.line=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(face="bold"))
    
  } else if (vergleichsvar=="jahr") {
    data <- data %>% select(v_1,v_9,v_10,v_11,v_12,v_13,v_14,v_15,v_16,v_17,v_18,v_19,v_20,v_26,v_27,v_28,v_29,v_36,v_37,v_38,v_39,v_40,v_41,v_42,v_43,v_46,v_52,v_53,v_54,v_55,v_56,v_57,v_63,v_64,v_65,v_66,v_67,v_68,v_69,v_70,v_71,v_72,v_73,v_74,v_75,v_76,v_82,v_83,v_84,v_85,v_111,v_117,v_118,v_119,v_120,v_121,v_122,v_124,v_142,v_177,v_178,v_179,v_186,v_187,v_188,v_189,v_190,v_191,v_197,v_198,v_199,v_200,v_201,v_202,v_203,v_204,v_205,v_206,v_2598,v_223,v_227,v_246,geschlecht,bildung,beruf,stadt_teil,stadt_teil2,alter,jahr) %>%
      bind_rows(data17)
    data_plot <- data %>% 
      rename(plotvar=inputvar,
             groupvar=vergleichsvar) %>% 
      filter(!is.na(plotvar),
             !is.na(groupvar),
             plotvar!="",
             groupvar!="")
    
    plabel <- labs %>% filter(var==inputvar)
    
    ggplot(data_plot, aes(x = as.factor(groupvar), fill=as.factor(plotvar))) + 
      geom_bar(aes(fill=as.factor(plotvar)), position = 'fill', color="black") + 
      labs(x="",y="Prozent", fill="", title=plabel$titel, subtitle=paste0(plabel$subtitel," (n=",nrow(data_plot),")")) +
      theme_minimal() +
      theme(axis.text.x=element_text(size=12, angle=90), 
            plot.title = element_text(face="bold")) +
      scale_y_continuous(labels=scales::percent) + 
      scale_fill_manual(values=col_list[[as.character(length(unique(data_plot$plotvar)))]], labels=lab_list[[as.character(plabel$label)]])
    
  } else {
    data_plot <- data %>% 
      rename(plotvar=inputvar,
             groupvar=vergleichsvar) %>% 
      filter(!is.na(plotvar),
             !is.na(groupvar),
             plotvar!="",
             groupvar!="")
    
    plabel <- labs %>% filter(var==inputvar)
    
    ggplot(data_plot, aes(x = as.factor(groupvar), fill=as.factor(plotvar))) + 
      geom_bar(aes(fill=as.factor(plotvar)), position = 'fill', color="black") + 
      labs(x="",y="Prozent", fill="", title=plabel$titel, subtitle=paste0(plabel$subtitel," (n=",nrow(data_plot),")")) +
      theme_minimal() +
      theme(axis.text.x=element_text(size=12, angle=90), 
            plot.title = element_text(face="bold")) +
      scale_y_continuous(labels=scales::percent) + 
      scale_fill_manual(values=col_list[[as.character(length(unique(data_plot$plotvar)))]], labels=lab_list[[as.character(plabel$label)]])
  }
}








