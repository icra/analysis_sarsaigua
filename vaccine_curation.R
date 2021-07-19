library(tidyverse)
library(lubridate)

get_vaccines <- function(){
  
  setwd("C:/Users/jpueyo/OneDrive - ICRA/Other projects/sarsaigua/analysis_sarsaigua")
  source("functions.R")
  
  download <- download.file("https://dadescovid.cat/static/csv/catalunya_setmanal.zip",
                            destfile = "vacunacio.zip")
  unzip("vacunacio.zip")
  
  vaccines <- read.csv("catalunya_setmanal.csv", sep=";")
  
  vaccines <- vaccines %>% group_by(DATA_FI) %>% 
    summarize(dosi_2 = sum(VACUNATS_DOSI_2, na.rm = T))
  
  data <- load_excel()
  
  # vaccines$DATA_INI_YMD <- ymd(vaccines$DATA_INI)
  vaccines$DATA_FI_YMD <- ymd(vaccines$DATA_FI)
  
  #vaccines$accum_1 <- NA
  vaccines$accum_2 <- NA
  
  vaccines <- vaccines %>% 
    filter(DATA_FI %in% data$Data.mostreig)
  
  for (i in 1:nrow(vaccines)){
    if (i == 1){
      #vaccines$accum_1[i] <- vaccines$VACUNATS_DOSI_1[i]
      vaccines$accum_2[i] <- vaccines$dosi_2[i]
    } else {
      #vaccines$accum_1[i] <- vaccines$accum_1[i-1] + vaccines$VACUNATS_DOSI_1[i]
      vaccines$accum_2[i] <- vaccines$accum_2[i-1] + vaccines$dosi_2[i]
    }
  }
  
  vaccines$accum_2_1000 <- (vaccines$accum_2 / (7.5*10^6)) * 10^3
  
  vaccines$Data.mostreig <- vaccines$DATA_FI
  
  vaccines <- vaccines %>% 
    filter(dosi_2 > 0)
  
  return(vaccines)
}
