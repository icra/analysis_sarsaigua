library(tidyverse)
library(lubridate)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

##LOAD EXCEL

load_excel <- function(filename = "Mostres_newest.csv", 
                       folder=paste(dirname(rstudioapi::getSourceEditorContext()$path), "data", sep="/"), 
                       remove_first = FALSE){
  data <- read.csv2(paste(folder,filename, sep="/"), dec = '.', na.strings=c("","NA", "NaN")) %>% 
    janitor::clean_names()
  
  colnames(data)[str_starts(colnames(data), "no_casos_dia")] <- paste0("casos_lag", -7:7) %>% 
    str_replace_all("-", "_minus_")
  
  #remove samples without data
  data <- data[!is.na(data$data_mostreig),]
  
  #string to dates
  data$YMDmostreig <- ymd(data$data_mostreig)
  
  #move uncorrected dates
  correctDates <- seq(min(data$YMDmostreig), max(data$YMDmostreig), by=7)
  
  for (i in 1:nrow(data)){
    j <- -3
    newDate <- data$YMDmostreig[i]
    while (!(newDate %in% correctDates) && j <= 3){
      newDate <- data$YMDmostreig[i]+j
      j <- j+1
    }
    data$YMDmostreig[i] <- newDate
  }
  data$data_mostreig <- as.character(data$YMDmostreig)
  
  # if remove_first is true, remove the july and august 2020
  if (remove_first){
    data <- filter(data, YMDmostreig >= "2020-09-01")
  }
  
  #string to numeric replacing commas by points
  data$cabal_influent_ultimes_24h_calculat <- as.numeric(gsub(",",".",data$cabal_influent_ultimes_24h_calculat))
  data$cabal_influent_ultimes_24h_calculat[data$cabal_influent_ultimes_24h_calculat == 0] <- NA
  
  #repair labs names
  data <- data %>% 
    mutate(laboratori = case_when(
      str_starts(laboratori, "UB") ~ "UB_Girones",
      str_starts(laboratori, "EURE") ~ "Eurecat",
      str_starts(laboratori, "Virus") ~ "Virus Enterics-UB",
      TRUE ~ "Some lab is wrong"
    ))
  
  #check labs names and stop if some are not right
  stopifnot(all(data$laboratori != "Some lab is wrong"))
  
  #convert labs to factor
  data$laboratori <- factor(data$laboratori)
  
  #convert EDAR to factor
  data$edar <- factor(data$edar)
  
  data <- data %>% 
    #calculate viral load and log10
    mutate(n1_load = n1_concentracio_final * 1000 * cabal_influent_ultimes_24h_calculat,
           n2_load = n2_concentracio_final * 1000 * cabal_influent_ultimes_24h_calculat,
           ip4_load = ip4_concentracio_final * 1000 * cabal_influent_ultimes_24h_calculat
    ) %>% 
    relocate(ip4_concentracio_final, .after = casos_lag7) %>% 
    relocate(n2_concentracio_final, .after = casos_lag7) %>% 
    relocate(n1_concentracio_final, .after = casos_lag7) %>% 
    relocate(n1_load, .after = n1_concentracio_final) %>% 
    relocate(n2_load, .after = n2_concentracio_final) %>% 
    relocate(ip4_load, .after = ip4_concentracio_final) %>% 
    
  
  return(data)
}

load_excel() %>% 
  write_excel_csv2("dades_sarsaigua.csv")
