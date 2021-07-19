library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(patchwork)
library(RColorBrewer)
library(emojifont)

setwd("C:/Users/jpueyo/OneDrive - ICRA/Other projects/sarsaigua/")

#load functions for data collection and curation
source('analysis_sarsaigua/functions.R')
source('analysis_sarsaigua//vaccine_curation.R')

#load data on vaccine doses
vaccines <- get_vaccines()

#load mostres_newest.csv (csv created from sarsaigua data)
data <- load_excel(remove_first = F)


#set plot parameters
theme_set(theme_classic())
color1 <- "darkorange"
color2 <- "grey"
color3 <- "blue3"

#load data on target N1 without detection threshold
filter_value <-  0
N1_data <- target_data(data = data, diana = "N1")

#set breaks of x axis
dates <- unique(N1_data$Data.mostreig)
dates <- dates[order(dates)]

date_breaks <- dates[1]

for (i in 1:length(dates)){
  if((i-1)%%4 == 0 && i !=1){
    date_breaks <- c(date_breaks,dates[i])
  }
}

#function for small plots
getPlot <- function(dataset=N1_data, 
                    min=0, 
                    max=Inf, 
                    title="Evolution of GC N1/day/inh and accumulated 7 days past cases / inh",
                    linesize = 1.2){
  N1_data <- dataset %>% 
    filter(Nº.habitants.municipis.assistits.EDAR > min,
           Nº.habitants.municipis.assistits.EDAR < max)
  
  sum_N1_inh <- N1_data %>% 
    group_by(Data.mostreig) %>%
    summarize(sum_N1 = sum(load, na.rm = T)) %>% 
    mutate(log_sum_N1 = log10(sum_N1))
  
  sum_darrers_7 <- N1_data %>% 
    group_by(Data.mostreig) %>% 
    summarize(sum_7 = sum(darrers_7, na.rm = T)) %>% 
    mutate(log_sum_7 = log10(sum_7))
  
  sum_inh <- N1_data %>% 
    group_by(Data.mostreig) %>% 
    summarise(sum_inh = sum(Nº.habitants.municipis.assistits.EDAR, na.rm = T))
  
  sum_N1_inh <- merge.data.frame(sum_N1_inh, sum_darrers_7, by="Data.mostreig", all = T)
  sum_N1_inh <- merge.data.frame(sum_N1_inh, sum_inh, by="Data.mostreig", all = T)
  sum_N1_inh <- merge.data.frame(sum_N1_inh, vaccines, by="Data.mostreig", all=T)
  
  sum_N1_inh <- sum_N1_inh %>% 
    mutate(sum_N1_inh = sum_N1 / sum_inh * 10^5,
           sum_7_inh = sum_7 / sum_inh * 10^5)
  
  correlation <- cor.test(sum_N1_inh$sum_N1, sum_N1_inh$sum_7, pairwise.obs = T)
  
  corr_text <- paste("R = ", round(correlation$estimate, digits = 3))  
  
  cols <- c("N1"= color1, "cases" = color2, "vaccine doses" = color3)
  event <- "#575757"
  event_line <- "dashed"
  
  ratio <-  max(sum_N1_inh$sum_N1_inh, na.rm=T) / max(sum_N1_inh$sum_7_inh) *0.8
  
  pl <- ggplot(sum_N1_inh, aes(x=Data.mostreig))+
    geom_col(aes(y=sum_7_inh*ratio, fill = "cases"))+
    geom_line(aes(y=sum_N1_inh, group=1, color="N1"), size = linesize)+
    scale_y_continuous(name="Total viral load N1 (GC/day/100.000 inhabitants)", 
                       sec.axis = sec_axis(trans=~./ratio, 
                                           name="COVID-19 cases (cases/100.000 inhabitants)"))+
    scale_x_discrete(breaks=date_breaks)+
    theme(legend.title = element_blank(), text=element_text(size=30))+
    scale_color_manual(name="", values=cols)+
    scale_fill_manual(name="", values=cols)+
    labs(x=title)+
    annotate("text", x= 6, y= max(sum_N1_inh$sum_N1_inh*0.9), 
             label = corr_text, size=8)
  
  return(pl)
}

#create main plot
dataset <- N1_data
min <- 0
max <- Inf 
title <- "Evolution of GC N1/day/inh and accumulated 7 days past cases / inh"
linesize <-  1.2

N1_data <- dataset %>% 
  filter(Nº.habitants.municipis.assistits.EDAR > min,
         Nº.habitants.municipis.assistits.EDAR < max)

sum_N1_inh <- N1_data %>% 
  group_by(Data.mostreig) %>%
  summarize(sum_N1 = sum(load, na.rm = T)) %>% 
  mutate(log_sum_N1 = log10(sum_N1))

sum_darrers_7 <- N1_data %>% 
  group_by(Data.mostreig) %>% 
  summarize(sum_7 = sum(darrers_7, na.rm = T)) %>% 
  mutate(log_sum_7 = log10(sum_7))

sum_inh <- N1_data %>% 
  group_by(Data.mostreig) %>% 
  summarise(sum_inh = sum(Nº.habitants.municipis.assistits.EDAR, na.rm = T))

sum_N1_inh <- merge.data.frame(sum_N1_inh, sum_darrers_7, by="Data.mostreig", all = T)
sum_N1_inh <- merge.data.frame(sum_N1_inh, sum_inh, by="Data.mostreig", all = T)
sum_N1_inh <- merge.data.frame(sum_N1_inh, vaccines, by="Data.mostreig", all=T)

sum_N1_inh <- sum_N1_inh %>% 
  mutate(sum_N1_inh = sum_N1 / sum_inh * 10^5,
         sum_7_inh = sum_7 / sum_inh * 10^5)

correlation <- cor.test(sum_N1_inh$sum_N1, sum_N1_inh$sum_7, pairwise.obs = T)

corr_text <- paste("R = ", round(correlation$estimate, digits = 3))  

cols <- c("N1"= color1, "cases" = color2, "vaccine doses" = color3)
event <- "#575757"
event_line <- "dashed"

ratio <-  max(sum_N1_inh$sum_N1_inh, na.rm=T) / max(sum_N1_inh$sum_7_inh) *0.8

cols <- c("N1"= color1, "cases" = color2, "2nd vaccine doses" = color3, "1st vaccine doses" = "lightblue")
event <- "#575757"
event_line <- "dashed"

p1 <- ggplot(sum_N1_inh, aes(x=Data.mostreig))+
  geom_col(aes(y=sum_7_inh*ratio, fill = "cases"))+
  geom_line(aes(y=sum_N1_inh, group=1, color="N1"), size = linesize)+
  geom_line(aes(y=accum_2_1000*ratio, group=1, color="2nd vaccine doses"), size=0.8)+
  #geom_line(aes(y=accum_1_1000*ratio, group=1, color="1st vaccine doses"), size=0.8)+
  scale_y_continuous(name="Total viral load N1 (GC/day/100.000 inhabitants)", 
                     sec.axis = sec_axis(trans=~./ratio, 
                                         name="COVID-19 cases (cases/100.000 inhabitants)\nVaccines doses / 1.000 inhabitants"))+
  scale_x_discrete(breaks=date_breaks)+
  theme(legend.title = element_blank(), text=element_text(size=30, lineheight = 0.3))+
  scale_color_manual(name="", values=cols)+
  scale_fill_manual(name="", values=cols)+
  labs(x=title)+
  annotate("text", x= 6, y= max(sum_N1_inh$sum_N1_inh*0.9), 
           label = corr_text, size=8)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  geom_vline(xintercept = "2020-10-26", color=event, linetype = event_line)+
  geom_vline(xintercept="2020-12-14", color=event, linetype = event_line)+
  geom_vline(xintercept="2021-01-11", color=event, linetype = event_line)+
  geom_vline(xintercept="2021-05-03", color=event, linetype = event_line)+
  geom_fontawesome("fa-lock", 
                   color=event, size=18,
                   x=17, y= max(sum_N1_inh$sum_N1_inh, na.rm=T))+
  geom_fontawesome("fa-unlock", 
                   color=event, size=18,
                   x=24, y= max(sum_N1_inh$sum_N1_inh, na.rm=T))+
  geom_fontawesome("fa-lock", 
                   color=event, size=18,
                   x=28, y= max(sum_N1_inh$sum_N1_inh, na.rm=T))+
  geom_fontawesome("fa-unlock", 
                   color=event, size=18,
                   x=44, y= max(sum_N1_inh$sum_N1_inh, na.rm=T))+
  theme(text = element_text(size=30))

small_plots <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                     axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                     axis.title.y = element_blank(), legend.position = "none")+
  theme(text = element_text(size=30))

#create small plots  
p2 <- getPlot(min = 150000, title="WWTP larger than 150.000 inhabitants", linesize = 0.8)
p2 <- p2+small_plots
p3 <- getPlot(max = 150000, title ="WWTP smaller than 150.000 inhabitants", linesize = 0.8)
p3 <- p3+small_plots

#patch all plots  
p1 / (p2 + p3) + plot_layout(heights = c(8,2,2))

#save the result
ggsave("results/corr_plot.png", width = 20, height = 15, units = "cm")
