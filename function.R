library(echarts4r)
library(dplyr)
library(stringr)
library(magrittr)
library(purrr)
library(plotly)
library(ggplot2)
library(DT)
library(glue)
library(haven)
library(reshape2)
library(tibble)
library(tidyr)
library(lubridate)
library(rlang)

###' functions 

      datavector <- function(studyid) {
      datavector <-
        gsub("\\.sas7bdat$",
             "",
             list.files(
               path = str_c('/RBM_JMPCLINICAL/JMP_entimICE/', studyid, "/"),
               pattern = "\\.sas7bdat$"
             ))
      return(datavector)
      }
      
      read.sas <-
      function(studyid, ds)
        haven::read_sas(str_c(
          "/RBM_JMPCLINICAL/JMP_entimICE/" ,
          studyid ,
          "/",
          ds ,
          ".sas7bdat"
        ))
      
      label_strip <- function(x) {
      for (i in names(x))
        attr(x[[i]] , "label") <- NULL
      x <- haven::zap_formats(x)
      }
      
      dt_fltr<-function(ds,studyid){
      
      df<- read.sas(studyid,ds)
      if("USUBJID" %in% names(df)){
        return(df)}else{}
      
      }
      
      dt<-function(study=studyid[1],snap1=""){
      dl<-datavector(study)
      mdl=c()
      dt1<-list()
      if(length(dl) != 0){
        dt<-lapply(dl,dt_fltr,study)
        
        for(i in 1:length(dt)){
          
          if(!is.null(dt[[i]]) && nrow(dt[[i]]) != 0){
            mdl<-c(mdl,dl[[i]])
            dt1<-c(dt1,list(dt[[i]]))
          }
          
        }
        
      }
      return(list("mdl"=mdl,"fd"=dt1))
      
      }
      
      
      # dlist<-function()({
      # fun_op()$mdl
      # })
      # 
      # fun_op<- function()({
      # d<- dt(studyid)
      # })
      # 
      # Entire_data<- function(){   
      # d<- fun_op()$fd
      # return(d)
      # }
      ############ Profile subject functions #############
      plot_fun<-function(Entire_data,datalist,subj_list){
        
        wd<-Entire_data #list of non-null datasets which contains USUBJID variable.
        dtl<-datalist #list of names of non-null datasets which contains USUBJID variable.
        
        
        if(!rlang::is_empty(which(dtl == "ae"))) {ae<-wd[[which(dtl == "ae")]] }else{ ae=data.frame() }
        if(!rlang::is_empty(which(dtl == "cm"))) {cm<-wd[[which(dtl == "cm")]] }else{ cm=data.frame() }
        if(!rlang::is_empty(which(dtl == "dm"))) {dm<-wd[[which(dtl == "dm")]] }else{ dm=data.frame() }
        if(!rlang::is_empty(which(dtl == "ds"))) {ds<-wd[[which(dtl == "ds")]] }else{ ds=data.frame() }
        if(!rlang::is_empty(which(dtl == "lb"))) {lb<-wd[[which(dtl == "lb")]] }else{ lb=data.frame() }
        if(!rlang::is_empty(which(dtl == "mh"))) {mh<-wd[[which(dtl == "mh")]] }else{ mh=data.frame() }
        if(!rlang::is_empty(which(dtl == "sv"))) {sv<-wd[[which(dtl == "sv")]] }else{ sv=data.frame() }
        if(!rlang::is_empty(which(dtl == "vs"))) {vs<-wd[[which(dtl == "vs")]] }else{ vs=data.frame() }
        
        
        mhplot<- function(){
          
          if(!is.null(subj_list))
            if((nrow(dm)>0 && !is.null(dm)) && (nrow(mh)>0 && !is.null(mh))){
              
              dm <- dm %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, BRTHDTC, AGE)
              
              mh <-
                mh %>% label_strip() %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, MHTERM, MHDTC)
              
              
              if(nrow(mh)>0){
                
                dmh <-
                  dm %>% left_join(mh, by = 'USUBJID') %>% group_by(USUBJID) %>%
                  mutate(AGEMH = ifelse(MHDTC != "" &&
                                          BRTHDTC != "", round(
                                            difftime(
                                              ymd(str_sub(MHDTC, 1, 10), truncated = 2),
                                              ymd(str_sub(BRTHDTC, 1, 10), truncated = 2),
                                              units = 'days'
                                            ) / 365.25, 2
                                          ), AGE)) %>% ungroup()
                
                dmh$MHTERM <- sapply(
                  dmh$MHTERM,
                  FUN = function(x) {
                    paste(strwrap(x, width = 32), collapse = "<br>")
                  }
                )
                
                
                dmh$USUBJID <-
                  factor(dmh$USUBJID, levels = subj_list)
                
                dmh %>% plot_ly(
                  x = ~ AGEMH,
                  y = ~ MHTERM,
                  type = 'scatter',
                  mode = 'markers',
                  color = ~ USUBJID,
                  symbol = ~ USUBJID,
                  marker = list(size = 10),
                  colors = c('#606060FF', '#D6ED17FF'),
                  hoverinfo = 'text',
                  text = ~ paste0('Subject: ', USUBJID, '<br>MHTERM: ', MHTERM)
                ) %>% layout(
                  showlegend = FALSE,
                  title = 'Medical History',
                  paper_bgcolor = "#E8D2F550",
                  yaxis = list(
                    title = '',
                    tickangle = 0,
                    tickfont = list(size = 8)
                  ),
                  xaxis = list(title = 'AGEMH (Years)')
                ) %>% config(displayModeBar=FALSE)
                
              }else{
                plot_ly() %>% layout(
                  showlegend = FALSE,
                  title = 'Medical History',
                  paper_bgcolor = "#E8D2F550",
                  yaxis = list(
                    title = '',
                    tickangle = 0,
                    tickfont = list(size = 8)
                  ),
                  xaxis = list(title = 'AGEMH (Years)')
                ) %>% config(displayModeBar=FALSE)  %>%
                  add_annotations(
                    text = if(!nrow(dm)>0 | is.null(dm)){
                      paste0("Unable to plot medical history data \nbecause no demographic data available \nfor the selected study.")
                    }else{
                      
                      if(!nrow(mh)>0 | is.null(mh))
                        paste0("No medical history data available \nfor the selected study.")
                    },
                    x = 0.65,
                    y = 0.65,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "middle",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 15,color="red"))
              }   
            }else{
              plot_ly() %>% layout(
                showlegend = FALSE,
                title = 'Medical History',
                paper_bgcolor = "#E8D2F550",
                yaxis = list(
                  title = '',
                  tickangle = 0,
                  tickfont = list(size = 8)
                ),
                xaxis = list(title = 'AGEMH (Years)')
              ) %>% config(displayModeBar=FALSE)  %>%
                add_annotations(
                  text = if(!nrow(dm)>0 | is.null(dm)){
                    paste0("Unable to plot medical history data \nbecause no demographic data available \nfor the selected study.")
                  }else{
                    
                    if(!nrow(mh)>0 | is.null(mh))
                      paste0("No medical history data available \nfor the selected study.")
                  },
                  x = 0.65,
                  y = 0.65,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "middle",
                  yanchor = "top",
                  showarrow = FALSE,
                  font = list(size = 15,color="red"))
            }
        }
        
        dsplot<- function(){
          
          if(!is.null(subj_list))
            
            
            if((nrow(dm)>0 && !is.null(dm)) && (nrow(ds)>0 && !is.null(ds))){
              
              # validate(need(length(intersect("dm",datalist()))>0,""))
              # validate(need(length(intersect("ds",datalist()))>0,""))
              
              dm <-
                dm %>% label_strip() %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, BRTHDTC)
              
              ds <-
                ds %>% label_strip()
              # validate(need(!is.null(ds) & nrow(ds)>0,""))
              ds <- ds %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, DSDECOD, DSSTDTC, DSSTDY) %>% filter(!is.na(DSSTDY))
              
              
              
              if(nrow(ds)>0){
                
                ds$DSDECOD <- sapply(
                  ds$DSDECOD,
                  FUN = function(x) {
                    paste(strwrap(x, width = 32), collapse = "<br>")
                  }
                )
                
                ds$USUBJID <-
                  factor(ds$USUBJID, levels = subj_list)
                
                
                ds %>% plot_ly(
                  x = ~ DSSTDY,
                  y = ~ DSDECOD,
                  type = 'scatter',
                  mode = 'markers',
                  color = ~ USUBJID,
                  symbol = ~ USUBJID,
                  marker = list(size = 10),
                  colors = c('#606060FF', '#D6ED17FF'),
                  hoverinfo = 'text',
                  text = ~ paste0('Subject: ', USUBJID, '<br>DSDECOD: ', DSDECOD)
                ) %>% layout(
                  showlegend = FALSE,
                  title = 'Disposition Events',
                  paper_bgcolor = "#E8D2F550",
                  yaxis = list(
                    title = '',
                    zeroline = FALSE,
                    showline = FALSE,
                    tickangle = 0,
                    tickfont = list(size = 8)
                  ),
                  xaxis = list(title = 'Disposition Start Day')
                ) %>% config(displayModeBar=FALSE)
                
                
              }else{
                plot_ly() %>% layout(
                  showlegend = FALSE,
                  title = 'Disposition Events',
                  paper_bgcolor = "#E8D2F550",
                  yaxis = list(
                    title = '',
                    zeroline = FALSE,
                    showline = FALSE,
                    tickangle = 0,
                    tickfont = list(size = 8)
                  ),
                  xaxis = list(title = 'Disposition Start Day')
                ) %>% config(displayModeBar=FALSE) %>%
                  add_annotations(
                    text = if(!nrow(dm)>0 | is.null(dm)){
                      paste0("Unable to plot disposition events data \nbecause no demographic data available \nfor the selected study.")
                    }else{
                      if(!nrow(ds)>0 | is.null(ds))
                        paste0("No disposition events data available \nfor the selected study.")
                    },
                    x = 0.65,
                    y = 0.65,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "middle",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 15,color="red"))
              }
              
            }else{
              plot_ly() %>% layout(
                showlegend = FALSE,
                title = 'Disposition Events',
                paper_bgcolor = "#E8D2F550",
                yaxis = list(
                  title = '',
                  zeroline = FALSE,
                  showline = FALSE,
                  tickangle = 0,
                  tickfont = list(size = 8)
                ),
                xaxis = list(title = 'Disposition Start Day')
              ) %>% config(displayModeBar=FALSE) %>%
                add_annotations(
                  text = if(!nrow(dm)>0 | is.null(dm)){
                    paste0("Unable to plot disposition events data \nbecause no demographic data available \nfor the selected study.")
                  }else{
                    if(!nrow(ds)>0 | is.null(ds))
                      paste0("No disposition events data available \nfor the selected study.")
                  },
                  x = 0.65,
                  y = 0.65,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "middle",
                  yanchor = "top",
                  showarrow = FALSE,
                  font = list(size = 15,color="red"))
              
            }
          
          
        }
        
        aeplot<- function(){
          
          
          if(!is.null(subj_list))
            if((nrow(dm)>0 && !is.null(dm)) && (nrow(sv)>0 && !is.null(sv)) && (nrow(ae)>0 && !is.null(ae))){
              
              dm <-
                dm %>% label_strip() %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, BRTHDTC)
              
              sv <-
                sv %>% label_strip() %>%
                filter(USUBJID %in% subj_list) %>%  select(USUBJID, SVSTDY) %>%
                group_by(USUBJID) %>% filter(SVSTDY == max(SVSTDY))
              
              ae <-
                ae %>% label_strip() %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, AEDECOD, AESTDY, AEENDY) %>%
                left_join(sv, by = 'USUBJID') %>% group_by(USUBJID) %>%
                mutate(AEENDY = ifelse(is.na(AEENDY), SVSTDY, AEENDY)) %>% ungroup()
              
              
              
              
              if(nrow(ae)>0){
                
                
                ae$AEDECOD <- sapply(
                  ae$AEDECOD,
                  FUN = function(x) {
                    paste(strwrap(x, width = 32), collapse = "<br>")
                  }
                )
                
                line <- list(
                  type = "line",
                  line = list(color = '#6C7B8B', dash = 'dot'),
                  xref = "x",
                  yref = "y"
                )
                
                lines <- list()
                for (i in 1:nrow(ae)) {
                  line["x0"] <- ae$AESTDY[i]
                  line["x1"] <- ae$AEENDY[i]
                  line[c("y0", "y1")] <- ae$AEDECOD[i]
                  lines <- c(lines, list(line))
                }
                
                ae$USUBJID <-
                  factor(ae$USUBJID, levels = subj_list)
                
                
                ae %>% plot_ly(
                  x = ~ AESTDY,
                  y = ~ AEDECOD,
                  type = 'scatter',
                  mode = 'markers',
                  color = ~ USUBJID,
                  marker = list(size = 10),
                  symbol = ~ USUBJID,
                  colors = c('#606060FF', '#D6ED17FF'),
                  hoverinfo = 'text',
                  text = ~ paste0('Subject: ', USUBJID, '<br>AEDECOD: ', AEDECOD)
                ) %>% layout(
                  showlegend = FALSE,
                  # shapes = lines,
                  paper_bgcolor = "#E8D2F550",
                  title = 'Adverse Events',
                  xaxis = list(title = 'Study Day'),
                  yaxis = list(
                    title = '',
                    zeroline = FALSE,
                    showline = FALSE,
                    tickangle = 0,
                    tickfont = list(size = 8)
                  )
                )%>% config(displayModeBar=FALSE)
                
              }else{
                plot_ly() %>% layout(
                  showlegend = FALSE,
                  paper_bgcolor = "#E8D2F550",
                  title = 'Adverse Events',
                  xaxis = list(title = 'Study Day'),
                  yaxis = list(
                    title = '',
                    zeroline = FALSE,
                    showline = FALSE,
                    tickangle = 0,
                    tickfont = list(size = 8)
                  )
                )%>% config(displayModeBar=FALSE) %>%
                  add_annotations(
                    text = if(!nrow(dm)>0 | is.null(dm)){
                      paste0("Unable to plot adverse events data \nbecause no demographic data available \nfor the selected study.")
                    }else{
                      if(!nrow(sv)>0 | is.null(sv)){
                        paste0("Unable to plot adverse events data \nbecause no study visit data available \nfor the selected study.") 
                      }else{
                        if(!nrow(ae)>0 | is.null(ae))
                          paste0("No adverse events data available \nfor the selected study.")
                      }
                    },
                    x = 0.65,
                    y = 0.65,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "middle",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 15,color="red"))
              }
            }else{
              plot_ly() %>% layout(
                showlegend = FALSE,
                paper_bgcolor = "#E8D2F550",
                title = 'Adverse Events',
                xaxis = list(title = 'Study Day'),
                yaxis = list(
                  title = '',
                  zeroline = FALSE,
                  showline = FALSE,
                  tickangle = 0,
                  tickfont = list(size = 8)
                )
              )%>% config(displayModeBar=FALSE) %>%
                add_annotations(
                  text = if(!nrow(dm)>0 | is.null(dm)){
                    paste0("Unable to plot adverse events data \nbecause no demographic data available \nfor the selected study.")
                  }else{
                    if(!nrow(sv)>0 | is.null(sv)){
                      paste0("Unable to plot adverse events data \nbecause no study visit data available \nfor the selected study.") 
                    }else{
                      if(!nrow(ae)>0 | is.null(ae))
                        paste0("No adverse events data available \nfor the selected study.")
                    }
                  },
                  x = 0.65,
                  y = 0.65,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "middle",
                  yanchor = "top",
                  showarrow = FALSE,
                  font = list(size = 15,color="red"))
            }
          
        }
        
        cmplot<- function(){
          
          if(!is.null(subj_list))
            
            if((nrow(dm)>0 && !is.null(dm)) && (nrow(sv)>0 && !is.null(sv)) && (nrow(cm)>0 && !is.null(cm))){
              
              dm <-dm %>% label_strip() %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, BRTHDTC)
              
              sv <-
                sv %>% label_strip() %>%
                filter(USUBJID %in%subj_list) %>%  select(USUBJID, SVSTDY) %>%
                group_by(USUBJID) %>% filter(SVSTDY == max(SVSTDY))
              
              
              if(!is.null(cm) & nrow(cm)>0)
                
                cm <- cm %>% label_strip() %>%
                filter(USUBJID %in% subj_list) %>%
                select(USUBJID, CMTRT, CMSTDTC, CMENDTC, CMSTDY, CMENDY) %>% filter(CMTRT != "",!is.na(CMSTDY)) %>%
                left_join(sv, by = 'USUBJID') %>% group_by(USUBJID) %>%
                mutate(CMENDY = ifelse(is.na(CMENDY), SVSTDY, CMENDY)) %>% ungroup()
              
              if(nrow(cm)>0){
                
                
                cm$CMTRT <- sapply(
                  cm$CMTRT,
                  FUN = function(x) {
                    paste(strwrap(x, width = 32), collapse = "<br>")
                  }
                )
                
                cm$USUBJID <-
                  factor(cm$USUBJID, levels =subj_list)
                
                line1 <- list(
                  type = "line",
                  line = list(color = '#6C7B8B', dash = 'dot'),
                  xref = "x",
                  yref = "y"
                )
                
                lines <- list()
                for (i in 1:nrow(cm)) {
                  line1["x0"] <- cm$CMSTDY[i]
                  line1["x1"] <- cm$CMENDY[i]
                  line1[c("y0", "y1")] <- cm$CMTRT[i]
                  lines <- c(lines, list(line1))
                }
                
                cm %>% plot_ly(
                  x = ~ CMSTDY,
                  y = ~ CMTRT,
                  type = 'scatter',
                  mode = 'markers',
                  color = ~ USUBJID,
                  symbol = ~ USUBJID,
                  marker = list(size = 10),
                  colors = c('#606060FF', '#D6ED17FF'),
                  hoverinfo = 'text',
                  text = ~ paste0('Subject: ', USUBJID, '<br>CMTRT: ', CMTRT)
                ) %>% layout(
                  showlegend = FALSE,
                  shapes = lines,
                  paper_bgcolor = "#E8D2F550",
                  title = 'Concomitant Medications',
                  xaxis = list(title = 'Study Day'),
                  yaxis = list(
                    title = '',
                    zeroline = FALSE,
                    showline = FALSE,
                    tickangle = 0,
                    tickfont = list(size = 8)
                  )
                ) %>% config(displayModeBar=FALSE)
                
              }else{
                plot_ly() %>% layout(
                  showlegend = FALSE,
                  shapes = lines,
                  paper_bgcolor = "#E8D2F550",
                  title = 'Concomitant Medications',
                  xaxis = list(title = 'Study Day'),
                  yaxis = list(
                    title = '',
                    zeroline = FALSE,
                    showline = FALSE,
                    tickangle = 0,
                    tickfont = list(size = 8)
                  )
                ) %>% config(displayModeBar=FALSE)%>%
                  add_annotations(
                    text =  if(!nrow(dm)>0 | is.null(dm)){
                      paste0("Unable to plot concomitant medication data \nbecause no demographic data available \nfor the selected study.")
                    }else{
                      if(!nrow(sv)>0 | is.null(sv)){
                        paste0("Unable to plot concomitant medication data \nbecause no study visit data available \nfor the selected study.") 
                      }else{
                        if(!nrow(cm)>0 | is.null(cm))
                          paste0("No concomitant medication data available \nfor the selected study.")
                      }
                    },
                    x = 0.65,
                    y = 0.65,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "middle",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 15,color="red"))
                
              }
            }else{
              plot_ly() %>% layout(
                showlegend = FALSE,
                shapes = lines,
                paper_bgcolor = "#E8D2F550",
                title = 'Concomitant Medications',
                xaxis = list(title = 'Study Day'),
                yaxis = list(
                  title = '',
                  zeroline = FALSE,
                  showline = FALSE,
                  tickangle = 0,
                  tickfont = list(size = 8)
                )
              ) %>% config(displayModeBar=FALSE)%>%
                add_annotations(
                  text =  if(!nrow(dm)>0 | is.null(dm)){
                    paste0("Unable to plot concomitant medication data \nbecause no demographic data available \nfor the selected study.")
                  }else{
                    if(!nrow(sv)>0 | is.null(sv)){
                      paste0("Unable to plot concomitant medication data \nbecause no study visit data available \nfor the selected study.") 
                    }else{
                      if(!nrow(cm)>0 | is.null(cm))
                        paste0("No concomitant medication data available \nfor the selected study.")
                    }
                  },
                  x = 0.65,
                  y = 0.65,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "middle",
                  yanchor = "top",
                  showarrow = FALSE,
                  font = list(size = 15,color="red"))
              
              
            }
        }
        
        vsplot<- function(){
          
          if(!is.null(subj_list))
            if(!is.null(vs) && nrow(vs)>0 && 'VSSTRESN' %in% names(vs)){
              vs <- vs %>% label_strip()
              
              
              vs <-
                vs %>% filter(USUBJID %in% subj_list) %>%
                select(USUBJID, VISIT, VISITNUM, VISITDY, VSTESTCD, VSSTRESN) %>%
                filter(!is.na(VSSTRESN),!grepl('UNSCH|DISCONT', toupper(VISIT))) %>%
                group_by(USUBJID, VSTESTCD, VISIT, VISITNUM) %>%
                mutate(VSSTRESN = mean(VSSTRESN)) %>% ungroup() %>%
                arrange(VISITNUM)
              
              vs$USUBJID <-
                factor(vs$USUBJID, levels = subj_list)
              
              vs$VISIT <-
                factor(vs$VISIT, levels = unique(vs$VISIT))
              
              test_name <- unique(vs$VSTESTCD)
              
              prof_vs <- function(test) {
                
                vs_f <- vs %>% filter(VSTESTCD == test)
                
                if(nrow(vs_f)>0){
                  vs_pro<- vs_f %>%
                    plot_ly(
                      x = ~ VISIT,
                      y = ~ VSSTRESN,
                      type = 'scatter',
                      mode = 'markers+lines',
                      color = ~ USUBJID,
                      colors = c('#606060FF', '#D6ED17FF'),
                      symbol = ~ USUBJID,
                      marker = list(size = 8),
                      hoverinfo = 'text',
                      text = ~ paste0('Subject: ', USUBJID),
                      height = 1200
                    ) %>%
                    add_annotations(
                      text = ~ VSTESTCD,
                      x = 0.5,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "middle",
                      yanchor = "top",
                      showarrow = FALSE,
                      font = list(size = 10)
                    ) %>% layout(
                      hovermode = 'compare',
                      paper_bgcolor = "#E8D2F550",
                      xaxis = list(
                        tickfont = list(size = 9),
                        tickangle = 45,
                        categoryorder = "array",
                        categoryarray = unique(vs$VISIT)
                      )
                    )
                  
                }else{
                  vs_pro<- vs_f %>%
                    plot_ly(
                      x = ~ VISIT,
                      y = ~ VSSTRESN,
                      type = 'scatter',
                      mode = 'markers+lines',
                      color = ~ USUBJID,
                      colors = c('#606060FF', '#D6ED17FF'),
                      symbol = ~ USUBJID,
                      marker = list(size = 8),
                      hoverinfo = 'text',
                      text = ~ paste0('Subject: ', USUBJID),
                      height = 1200
                    ) %>%
                    add_annotations(
                      text =test,
                      x = 0.5,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "middle",
                      yanchor = "top",
                      showarrow = FALSE,
                      font = list(size = 10)
                    ) %>% add_annotations(
                      text = "No data available for this test",
                      x = 0.2,
                      y = 0.65,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "middle",
                      yanchor = "top",
                      showarrow = FALSE,
                      font = list(size = 12,color="red"))
                  
                }
                return(vs_pro)
              }
              
              plotvs <- as.list(purrr::map(test_name, prof_vs))
              
              subplot(
                plotvs,
                nrows = ceiling(length(unique(
                  vs$VSTESTCD
                )) / 2),
                # heights = max(ceiling(length(
                #   unique(vs$VSTESTCD)
                # ) / 2) * 300, 900),
                shareX = TRUE,
                margin = 0.01
              ) %>%
                layout(
                  title = 'Vital Signs',
                  showlegend = FALSE,
                  hovermode = 'compare',
                  paper_bgcolor = "#E8D2F550"
                ) %>% config(displayModeBar=FALSE)
              
              
            }else{
              
              plot_ly(height = 1200)  %>%
                layout(
                  title = 'Vital Signs',
                  showlegend = FALSE,
                  paper_bgcolor = "#E8D2F550",
                  xaxis=list(title="VISIT"),
                  yaxis=list(title="VSSTRESN")
                ) %>% add_annotations(
                  text = "No data available for this test",
                  x = 0.65,
                  y = 0.65,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "middle",
                  yanchor = "top",
                  showarrow = FALSE,
                  font = list(size = 15,color="red"))
              
            }
          
        }
        
        lbplot<- function(){
          
          if(!is.null(subj_list))
            if(!is.null(lb) && nrow(lb)>0 && 'LBSTRESN' %in% names(lb)){
              
              lb <-lb %>% label_strip()
              
              
              lb<- lb %>% filter(USUBJID %in% subj_list)
              lb$USUBJID <-
                factor(lb$USUBJID, levels = subj_list)
              
              lb$VISIT <-
                factor(lb$VISIT, levels = unique(lb$VISIT))
              
              test_name <- unique(lb$LBTESTCD)
              
              prof_lb <- function(test) {
                
                lb_f <- lb %>% filter(LBTESTCD == test) %>% filter(!is.na(LBSTRESN))
                
                if(nrow(lb_f)>0){
                  lb_pro <- lb %>% filter(LBTESTCD == test) %>%
                    plot_ly(
                      x = ~ VISIT,
                      y = ~ LBSTRESN,
                      type = 'scatter',
                      mode = 'markers+lines',
                      color = ~ USUBJID,
                      symbol = ~ USUBJID,
                      marker = list(size = 8),
                      line = list(shape = "spline"),
                      hoverinfo = 'text',
                      text = ~ paste0('Subject: ', USUBJID),
                      colors = c('#F46D43', '#5E4FA2'),
                      height = 1200
                    ) %>%
                    add_annotations(
                      text = ~ LBTESTCD,
                      x = 0.5,
                      y = 1,
                      yref = "paper",
                      xref = "paper",
                      xanchor = "middle",
                      yanchor = "top",
                      showarrow = FALSE,
                      font = list(size = 10)
                    ) %>% layout(
                      paper_bgcolor = "#E8D2F550",
                      xaxis = list(
                        tickfont = list(size = 9),
                        categoryorder = "array",
                        categoryarray = unique(lb$VISIT)
                      )
                    ) 
                  
                }else{
                  
                  lb_pro <- lb_f %>% plot_ly(x = ~ VISIT,
                                             y = ~ LBSTRESN,
                                             type = 'scatter',
                                             mode = 'markers+lines',
                                             color = ~ USUBJID,
                                             symbol = ~ USUBJID,
                                             marker = list(size = 8),
                                             line = list(shape = "spline"),
                                             hoverinfo = 'text',
                                             text = ~ paste0('Subject: ', USUBJID),
                                             height = 1200
                  ) %>% add_annotations(
                    text = test,
                    x = 0.5,
                    y = 1,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "middle",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 10)
                  ) %>% add_annotations(
                    text = "No data available for this test",
                    x = 0.3,
                    y = 0.65,
                    yref = "paper",
                    xref = "paper",
                    xanchor = "middle",
                    yanchor = "top",
                    showarrow = FALSE,
                    font = list(size = 12,color="red"))
                  
                }
                return(lb_pro)
                
              }
              
              lbmodal<- as.list(purrr::map(test_name, prof_lb))
              rown=length(
                unique(lb$LBTESTCD)
              ) / 2
              
              subplot(
                lbmodal,
                nrows = ceiling(length(unique(
                  lb$LBTESTCD
                )) / 2),
                shareX = TRUE,
                margin = 0.01
              ) %>%
                layout(
                  title = 'Laboratory Domain',
                  showlegend = FALSE,
                  paper_bgcolor = "#E8D2F550"
                  # height = max(ceiling(length(
                  #   unique(lb$LBTESTCD)
                  # ) / 2) * 300, 1000)
                ) %>% config(displayModeBar=FALSE)
              
              
            }else{
              
              
              plot_ly(height = 1200)  %>%
                layout(
                  title = 'Laboratory Domain',
                  showlegend = FALSE,
                  paper_bgcolor = "#E8D2F550",
                  xaxis=list(title="VISIT"),
                  yaxis=list(title="LBSTRESN")
                ) %>% add_annotations(
                  text = "No data available for this test",
                  x = 0.3,
                  y = 0.65,
                  yref = "paper",
                  xref = "paper",
                  xanchor = "middle",
                  yanchor = "top",
                  showarrow = FALSE,
                  font = list(size = 12,color="red")) %>% config(displayModeBar=FALSE)
              
            }
          
        }
        
        plots<-list("mh"= mhplot(),
                    "ds"= dsplot(),
                    "ae"= aeplot(),
                    "cm"= cmplot(),
                    "vs"= vsplot(),
                    "lb"= lbplot())
        
        return(plots)
      }
      
      datatable_fun<- function(ds_num, #index number of particular dataset
                               Entire_data,
                               datalist,
                               subj_list){
        ds<-Entire_data[[ds_num]] %>% label_strip() %>%
          filter(USUBJID %in% subj_list)
        ds_name<-datalist[[ds_num]]
        datatable(
          ds,
          rownames = FALSE,
          filter = 'top',
          class = 'row_border stripe',
          extensions = 'Buttons',
          caption = htmltools::tags$caption(
            switch(
              ds_name,
              'ae' = str_to_title('adverse event record(s)'),                                                 
              'ag' = str_to_title( 'procedure agents record(s)'),                                       
              'apmh' = str_to_title( 'associated person\'s medical history record(s)'),                  
              'be' = str_to_title( 'biospecimen event record(s)'),                                             
              'bs' = str_to_title( 'biospecimen finding record(s)'),                                           
              'ce' = str_to_title( 'clinical event record(s)'),                                                
              'cm' = str_to_title( 'concomitant medication record(s)'),                                        
              'co' = str_to_title( 'comment record(s)'),                                                       
              'da' = str_to_title( 'drug accountability assessment record(s)'),                                
              'di' = str_to_title( 'device identifier record(s)'),                                             
              'dm' = str_to_title( 'demographic variable record(s)'),                                          
              'ds' = str_to_title( 'disposition event record(s)'),                                             
              'du' = str_to_title( 'device-in-use information record(s)'),                                     
              'dv' = str_to_title( 'protocol deviation record(s)'),                                            
              'ec' = str_to_title( 'exposure (as collected) record(s)'),                                
              'eg' = str_to_title( 'ECG test result record(s)'),                                               
              'ex' = str_to_title( 'exposure record(s)'),                                                      
              'fa' = str_to_title( 'finding about test record(s)'),                                            
              'faae' = str_to_title( 'finding about adverse event record(s)'),                                 
              'facm' = str_to_title( 'finding about concomitant medication record(s)'),                        
              'fads' = str_to_title( 'finding about disposition event record(s)'),                             
              'faec' = str_to_title( 'finding about (as collected) exposure record(s)'),                       
              'famh' = str_to_title( 'finding about medical history event record(s)'),                         
              'fapr' = str_to_title( 'finding about procedure record(s)'),                                     
              'fasu' = str_to_title( 'finding about subject use record(s)'),                                   
              'ho' = str_to_title( 'healthcare encounter record(s)'),                                          
              'ie' = str_to_title( 'iclusion or exclusion criteria not met observation record(s)'),            
              'lb' = str_to_title( 'lab test result record(s)'),                                               
              'mh' = str_to_title( 'medical history event record(s)'),                                         
              'mi' = str_to_title( 'microscopic finding record(s)'),                                           
              'nv' = str_to_title( 'nervous system finding record(s)'),                                        
              'pc' = str_to_title( 'pharmacokinetics concentration record(s)'),                                
              'pe' = str_to_title( 'physical examination result record(s)'),                                   
              'pf' = str_to_title( 'pharmacogenomics finding record(s)'),                                      
              'pg' = str_to_title( 'PGx methods and supporting information record(s)'),                        
              'ph' = str_to_title( 'disease stage history record(s)'),                                  
              'pr' = str_to_title( 'procedure record(s)'),                                                     
              'qs' = str_to_title( 'questionnaire record(s)'),                                                 
              're' = str_to_title( 'respiratory system finding record(s)'),                                    
              'relrec' = str_to_title( 'related record(s)'),                                            
              'rp' = str_to_title( 'reproductive system finding record(s)'),                                   
              'rs' = str_to_title( 'disease response record(s)'),                                              
              'sb' = str_to_title( 'subject biomaker record(s)'),                                              
              'sc' = str_to_title( 'subject characteristic record(s)'),                                        
              'se' = str_to_title( 'subject element observation record(s)'),                                   
              'ss' = str_to_title( 'subject status record(s)'),                                                
              'su' = str_to_title( 'subject use record(s)'),                                                   
              'sv' = str_to_title( 'subject visit record(s)'),                                                 
              'ta' = str_to_title( 'trial arm record(s)'),                                                     
              'te' = str_to_title( 'trial element record(s)'),                                                 
              'ti' = str_to_title( 'trial inclusion/ exclusion criteria record record(s)'),                    
              'tr' = str_to_title( 'tumor result record(s)'),                                                  
              'ts' = str_to_title( 'trial summary information record(s)'),                                     
              'tu' = str_to_title( 'tumor identification record(s)'),                                          
              'tv' = str_to_title( 'trial visit record(s)'),                                                   
              'vs' = str_to_title( 'vital sign parameter record(s)'),                                   
              'xp' = str_to_title( 'surgery or procedure record(s)'),                                   
              'yi' = str_to_title( 'site and investigator information record(s)'),                             
              'za' = str_to_title( 'ethnicity (as collected) record(s)'),                               
              'zb' = str_to_title( 'biomarkers related record(s)'),                                     
              'zd' = str_to_title( 'diagnostic and therapeutic procedure record(s)'),                          
              'faex' = str_to_title( 'finding about exposure record(s)'),                               
              'oe' = str_to_title( 'opthalmic examination record(s)'),                                  
              'cv' = str_to_title( 'cardiovascular physiology record(s)'),                              
              'face' = str_to_title( 'finding about clinical events record(s)'),                        
              'apae' = str_to_title( 'associated person\'s adverse event record(s)'),                    
              'apcm' = str_to_title( 'associated person\'s concomitant medication record(s)'),           
              'apdv' = str_to_title( 'associated person\'s protocol deviation record(s)'),               
              'apfa' = str_to_title( 'associated person\'s finding about record(s)'),                    
              'apie' = str_to_title( 'associated person\'s inclusion/exclusion criteria record(s)'),     
              'aplb' = str_to_title( 'associated person\'s laboratory result record(s)'),                
              'apmb' = str_to_title( 'associated person\'s microbiology specimen record(s)'),            
              'appr' = str_to_title( 'associated person\'s procedure record(s)'),                        
              'apsv' = str_to_title( 'associated person\'s study visit record(s)'),                      
              'apvs' = str_to_title( 'associated person\'s vital signs parameter record(s)'),            
              'mb' = str_to_title( 'Microbiology Specimen record(s)'),                                  
              'apdm' = str_to_title( 'associated person\'s demographic record(s)'),                      
              'apds' = str_to_title( 'associated person\'s disposition event record(s)'),                
              'apyi' = str_to_title( 'associated person\'s site and investigator information record(s)'),
              'zm' = str_to_title( 'Microscopic Methods and Supporting Information record(s)'),         
              'fabe' = str_to_title( 'finding about biospecimen event record(s)'),
            ),
            style = "caption-side: top; color:#514F52; font-size: 24px; text-align: left;"),
          options = list(
            searching = TRUE,
            scrollX = TRUE,
            scrollY="300px",
            fillContainer=TRUE,
            dom = "Blfrtip",
            buttons =
              list(
                "copy",
                list(
                  extend = "collection"
                  ,
                  buttons = c("csv", "excel", "pdf")
                  ,
                  text = "Download"
                )
              ),
            lengthMenu = list(c(5, 10, 20,-1)
                              , c(5, 10, 20, "All"))
          )
        )
      }
      
      
########### Testing area #################
      
  studyid<-c("r40242e","r42133d","r42511e")
      
 TestPS<- function(x) {   
   funop<-dt(x) # loading all data, filtering out null datasets and we will have final datalist and all data
   Entire_data<- funop$fd #data for tables
   subj_list<-read.sas(x,"dm")$USUBJID[1:3]  # selecting first 3 subjects 
   datalist<- funop$mdl 
   
   pts<-plot_fun(Entire_data,datalist,subj_list) #plot outputs
   
   return(list("plots"=pts,"tb_data"=Entire_data))
 }
 
 output<- TestPS(studyid[1]) #single study 
 TestOp<- lapply(studyid,TestPS) # multiple studies
