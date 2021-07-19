library(tidyverse)
library(R.utils)
library(RCurl)
library(lubridate)
source("../../scripts/functions.R")

setwd("C:/Users/jpueyo/OneDrive - ICRA/Other projects/sarsaigua/data/mobility")

#set years and days
years <- c(2020, 2021)
days <- 1:31

#download files
for (year in years){
  if (year == 2020){
    months <- 7:12
  } else {
    months <- 1:5
  }
  for (month in months){
    if (nchar(month) < 2){
      month <- paste("0",month, sep="")
    }
    for (day in days){
      if (nchar(day) < 2){
        day <- paste("0",day, sep="")
      }
      filename <- paste(year,"-",month,"/",year,month,day,"_maestra_1_mitma_municipio.txt.gz", sep="")
      destfile <- paste("files/",year,month,day,"_maestra_1_mitma_municipio.txt.gz", sep="")
      url <- paste("https://opendata-movilidad.mitma.es/maestra1-mitma-municipios/ficheros-diarios", filename, sep="/")
      if (url.exists(url)){
        download.file(
          url = url,
          destfile = destfile)
        
        gunzip(destfile) 
      }
    }
  }
}

#load municipalities of interest
mof <- read.csv("edar_mitima.csv", encoding="UTF-8")

#create empty dataframe to rbind
mobi <- data.frame(fecha=NA, origen=NA, destino=NA, periodo=NA, distancia=NA, viajes=NA, viajes_km=NA)

#filter files
for (file in list.files("files")){
  #afegir comprobació de si acaba amb txt
  data <- read.table(paste("files/", file, sep=""), sep="|", header = T, dec=",")
  filtered <- data %>% 
    filter(origen %in% mof$ID | destino %in% mof$ID)
  
  mobi <- rbind(mobi,filtered)
  print(file)
}

mobi <- mobi[2:nrow(mobi),]

mobi$viajes <- as.numeric(gsub(".", "", mobi$viajes, fixed = T))
mobi$viajes_km <- as.numeric(gsub(".", "", mobi$viajes_km, fixed = T))
  

mobi <- mobi %>% 
  filter(origen!=destino)

# write.csv2(mobi, "mobility_agg.csv", row.names = F)

# mobi <- read.csv2("mobility_agg.csv")

mobi_agg <- mobi %>% 
  filter(origen %in% mof$ID) %>% 
  group_by(fecha, origen) %>% 
  summarise(origin_trips = sum(viajes, na.rm = T))

mobi_destin <- mobi %>% 
  filter(destino %in% mof$ID) %>% 
  group_by(fecha, destino) %>% 
  summarise(destin_trips = sum(viajes, na.rm = T))

mobi_edar <- merge.data.frame(mobi_agg, mobi_destin, 
                              by.x=c("fecha", "origen"), by.y=c("fecha", "destino"),
                              all = T)
mobi_edar <- merge.data.frame(mof, mobi_edar, by.x="ID", by.y="origen", all.y=T)

#convert dates to YMD
mobi_edar$YMD <- ymd(mobi_edar$fecha)

#create a vector with sample dates
samples <- load_excel()
dates <- unique(samples$YMDmostreig)

mobi_edar_dates <- data.frame(municipi = NA,
                              codi_edar = NA,
                              nom_edar = NA,
                              data = NA,
                              origin_trips = NA,
                              destin_trips = NA)

for (edar in mof$codi_edar){
  filtered <- mobi_edar %>% 
    filter(codi_edar == edar) %>% 
    arrange(fecha)
  
  for (i in 1:nrow(filtered)){
    if (filtered$YMD[i] %in% dates){
      #sum trips for edar and date
      if (i > 6){
        origin_t <- sum(filtered$origin_trips[(i-6):i], na.rm = T)
        destin_t <- sum(filtered$destin_trips[(i-6):i], na.rm = T)
      } else{
        origin_t <- sum(filtered$origin_trips[1:i], na.rm = T)
        destin_t <- sum(filtered$destin_trips[1:i], na.rm = T)
      }
      
      #save values to edar_mobi_dates
      df <- data.frame(municipi = filtered$ID[i],
                       codi_edar = filtered$codi_edar[i],
                       nom_edar = filtered$nom_edar[i],
                       data = as.character(filtered$YMD[i]),
                       origin_trips = origin_t,
                       destin_trips = destin_t)
      
      mobi_edar_dates <- rbind(mobi_edar_dates, df)
    }
  }
}

mobi_edar_dates <- mobi_edar_dates[2:nrow(mobi_edar_dates),]

write.csv2(mobi_edar_dates, "mobi_edar.csv")
write.csv(mobi_edar_dates, "mobi_edar_EURECAT.csv", row.names = F)

