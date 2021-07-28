library(tidyverse)
library(lubridate)
library(viridis)
library(patchwork)

path <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(path,"functions.R", sep="/"))
setwd(path)

#load excel
data <- load_excel()

#create casos for N1 and correlation
N1_casos <- N1_data(data)

#generate a field with rowname
N1_casos$row <- as.integer(rownames(N1_casos))

#generate variables where store slopes
N1_casos$slope_log_7 <- NA
N1_casos$slope_log_N1 <- NA

#indicate size of backtime window
n_casos <- 3

#calculate slope for log_7 and log_N1
for (edar in levels(N1_casos$EDAR)){
  casos_edar <- N1_casos %>% 
    filter(EDAR == edar,
           !is.na(log_7),
           !is.na(log_N1)) %>% 
    arrange(Data.mostreig)
  for (i in 1:nrow(casos_edar)){
    #calculate y for log_7
    slope <- create_slope(casos_edar, "log_7", 1, direction = 1)
    N1_casos$slope_log_7[casos_edar$row[i]] <- slope
    
    #calculate y for log_N1
    slope <- create_slope(casos_edar, "log_N1", 3, direction = -1)    
    N1_casos$slope_log_N1[casos_edar$row[i]] <- slope
  }
}

#calculate correlation
cor.test(N1_casos$slope_log_7, N1_casos$slope_log_N1, method="pearson")

#scatterplot with slopes
p0 <- N1_casos %>% 
  filter(!is.na(slope_log_7), !is.na(slope_log_N1)) %>% 
  ggplot(aes(slope_log_7, slope_log_N1, color=log_N1))+
  geom_point()+
  geom_smooth()+
  theme(legend.position = 'none')


##ANALYSING PREDICTION POWER OF N1
forward <- 1

N1_casos$slope_log_7_forward <- NA

for (i in 1:nrow(N1_casos)){
  N1_casos$slope_log_7_forward[i] <- N1_casos$slope_log_7[i+forward]
}

cor.test(N1_casos$slope_log_7_forward, N1_casos$slope_log_N1, method="pearson")

p1 <- N1_casos %>% 
  filter(!is.na(slope_log_7_forward), !is.na(slope_log_N1)) %>%
  ggplot(aes(slope_log_7_forward, slope_log_N1, color=log_N1))+geom_point()+geom_smooth()

p0 + p1

