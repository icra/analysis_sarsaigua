library(tidyverse)
library(lubridate)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

##LOAD EXCEL

load_excel <- function(filename = "Mostres_newest.csv", folder="C:/Users/jpueyo/OneDrive - ICRA/Other projects/sarsaigua", remove_first = FALSE){
  data <- read.csv2(paste(folder,filename, sep="/"), dec = '.', na.strings=c("","NA"))
  
  #remove samples without data
  data <- data[!is.na(data$Data.mostreig),]
  
  #string to dates
  data$YMDmostreig <- ymd(data$Data.mostreig)
  
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
  data$Data.mostreig <- as.character(data$YMDmostreig)
  
  # if remove_first is true, remove the july and august 2020
  if (remove_first){
    data <- filter(data, YMDmostreig >= "2020-09-01")
  }
  
  #string to numeric replacing commas by points
  data$Cabal.influent.últimes.24h.calculat <- as.numeric(gsub(",",".",data$Cabal.influent.últimes.24h.calculat))
  data$Cabal.influent.últimes.24h.calculat[data$Cabal.influent.últimes.24h.calculat == 0] <- NA
  
  #convert labs to factor
  data$Laboratori <- factor(data$Laboratori)
  
  #convert EDAR to factor
  data$EDAR <- factor(data$EDAR)
  
  
  return(data)
}

## CREATE DATAFRAME WITH A SPECIFIC TARGET

target_data <- function(data=data, diana = 'N1', filter_value = 0){
  conc_diana <- paste(diana,".Concentracio.final", sep="")
  
  casos <- data %>%
    #select useful variables
    select(Data.mostreig,
           Nº.casos.darrers.14.dies..acum., 
           Nº.casos.darrers.7.dies..acum., 
           Nº.casos.darrers.7.dies.i.següents.7.dies,
           Mitjana.darrers.7.dies..promig.,
           matches(conc_diana),
           Cabal.influent.últimes.24h.calculat,
           EDAR,
           EDAR.provincia,
           Nº.habitants.municipis.assistits.EDAR,
           Laboratori) %>% 
    
    #remove target concentrations lower than filter value
    filter(.[[6]] > filter_value,
           !is.nan(Cabal.influent.últimes.24h.calculat)) %>%
    
    #calculate viral load and log10
    mutate(load = .[[6]] * 1000 * Cabal.influent.últimes.24h.calculat,
           log_14 = log10(Nº.casos.darrers.14.dies..acum.+1), 
           log_7 = log10(Nº.casos.darrers.7.dies..acum.+1), 
           log_7_7 = log10(Nº.casos.darrers.7.dies.i.següents.7.dies+1),
           log_mean_7 = log10(Mitjana.darrers.7.dies..promig.+1),
           log = log10(load),
           load_inh = load / Nº.habitants.municipis.assistits.EDAR * 10^5,
           darrers_7_inh = Nº.casos.darrers.7.dies..acum. / Nº.habitants.municipis.assistits.EDAR * 10^5
           )
  
  #rename variables
  colnames(casos)[2:5] <- c("darrers_14", "darrers_7", "set_i_set", "mitjana_7")
  
  return(casos)
}

## CREATE DATAFRAME WITH ALL TARGETS

all_targets_data <- function(data=data, filter_value = 0){
  casos <- data %>% 
    #select useful variables
    select(Data.mostreig,
           Nº.casos.darrers.14.dies..acum., 
           Nº.casos.darrers.7.dies..acum., 
           Nº.casos.darrers.7.dies.i.següents.7.dies,
           Mitjana.darrers.7.dies..promig.,
           N1.Concentracio.final,
           N2.Concentracio.final,
           IP4.Concentracio.final,
           Cabal.influent.últimes.24h.calculat,
           Laboratori,
           EDAR) %>% 
    
    #remove samples without water flow volume
    filter(!is.nan(Cabal.influent.últimes.24h.calculat)) %>%
    
    #convert to NA concentrations lower than filter_value
    mutate(N1.Concentracio.final = ifelse(N1.Concentracio.final < filter_value, NA, N1.Concentracio.final),
           N2.Concentracio.final = ifelse(N2.Concentracio.final < filter_value, NA, N2.Concentracio.final),
           IP4.Concentracio.final = ifelse(IP4.Concentracio.final < filter_value, NA, IP4.Concentracio.final),
           
           #calculate viral load and logs
           load_N1 = N1.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
           load_N2 = N2.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
           load_IP4 = IP4.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
           log_14 = log10(Nº.casos.darrers.14.dies..acum.+1), 
           log_7 = log10(Nº.casos.darrers.7.dies..acum.+1), 
           log_7_7 = log10(Nº.casos.darrers.7.dies.i.següents.7.dies+1),
           log_mean_7 = log10(Mitjana.darrers.7.dies..promig.+1),
           log_N1 = log10(load_N1),
           log_N2 = log10(load_N2),
           log_IP4 = log10(load_IP4))
  
  #rename variables
  colnames(casos)[2:5] <- c("darrers_14", "darrers_7", "set_i_set", "mitjana_7")
  
  return(casos)
}
