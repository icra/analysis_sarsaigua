library(tidyverse)
library(lubridate)
library(viridis)

path <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(path,"functions.R", sep="/"))
setwd(path)

#load excel
data <- load_excel()

#create casos for N1 and correlation
N1_casos <- target_data(data, diana = "N1")

sim1 = function(load, n = 1000){
  #FEC random matrix
  mu=2.11; sigma=0.25;
  FECmatrix = 10^rnorm(n, mu, sigma) # units; grams feces/person/day
  # shedding rate of SARS-CoV-2 RNA in feces
  # log-uniform distribution from 2.56 to 7.67
  x = c(2.56, 7.67)
  RNAfecmatrix = 10^(x[1] + (x[2]-x[1]) * runif(n)); #RNA copies per gram feces; de 10^2 a 10^7
  median(load / (FECmatrix * RNAfecmatrix)) #el numerador es pot substituir per load_N1
}
interval_ = function(x) c(quantile(x, 0.025, na.rm=T),
                          median(x, na.rm=T),
                          quantile(x, 0.975, na.rm=T))

#create df to store simulation results
sim_2.5 <- c()
sim_median <- c()
sim_97.5 <- c()

for (i in 1:nrow(N1_casos)){
  res = replicate(1000, sim1(load = N1_casos$load_N1[i]))
  sim_2.5 <- c(sim_2.5, interval_(res)[1])
  sim_median <- c(sim_median, interval_(res)[2])
  sim_97.5 <- c(sim_97.5, interval_(res)[3])
  print(i)
}

N1_casos$sim_2.5 <- sim_2.5
N1_casos$sim_median <- sim_median
N1_casos$sim_97.5 <- sim_97.5

correlation <- cor.test(log10(N1_casos$sim_median), N1_casos$log_7)

write.csv2(N1_casos, "N1_simulation_raw.csv")
N1_casos <- read.csv2("N1_simulation_raw.csv")

cols <- c(
  "Simulation"="green",
  "N1" = "blue"
)

correccio <- 0
correccio <- 7.15


ggplot(N1_casos, aes(x=Data.mostreig))+
  geom_line(aes(y=log10(sim_median), group=EDAR, color="Simulation"))+
  geom_line(aes(y=log_N1-correccio, group=EDAR, color="N1"))+
  scale_color_manual(name="", values=cols)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.title = element_blank())



ggplot(N1_casos, aes(log10(sim_median), log_N1, color=log_N1))+
  geom_point()+
  geom_smooth()+
  #annotate("text", x= 2, y= 3.5, label = paste("R = ",round(correlation$estimate, 3)))+
  scale_color_viridis()

#temporal comparison

N1_date <- N1_casos %>%
  group_by(Data.mostreig) %>% 
  summarize(sum_log_N1 = sum(log_N1, na.rm = T))

log_7_date <- N1_casos %>% 
  group_by(Data.mostreig) %>% 
  summarize(sum_log_7 = sum(log_7, na.rm = T))

log_sim <- N1_casos %>% 
  mutate(log_sim = log10(sim_median)) %>% 
  group_by(Data.mostreig) %>% 
  summarize(sum_sim = sum(log_sim, na.rm = T))

N1_date$sum_log_7 <- log_7_date$sum_log_7
N1_date$sum_log_sim <- log_sim$sum_sim

cols <- c("log_7"="tomato","log_sim"="chartreuse4")

ggplot(N1_date, aes(x=Data.mostreig))+
  geom_line(aes(y=sum_log_7, group=1, color="log_7"))+
  geom_line(aes(y=sum_log_sim, group=1, color="log_sim"))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.title = element_blank())+
  labs(x="Comparison of real and simulated cases", y="sum of log(infected)")

cols <- c("log(N1)"="tomato","log(cases)"="chartreuse4")

conversio <- 5

ggplot(N1_date, aes(x=Data.mostreig))+
  geom_col(aes(y=log_sum_7*conversio, fill = "log(cases)"), colour="chartreuse4", width=0.8)+
  geom_line(aes(y=log_sum_N1, group=1, color="log(N1)"), size=1)+
  labs(x="Sum of all WWTP by date")+
  scale_y_continuous(name= "log(N1)", sec.axis = sec_axis(trans=~./conversio, name="log(Cases last 14 days)"))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.title = element_blank())+
  scale_color_manual(name="", values=cols)+
  scale_fill_manual(name="", values=cols)#+
  #annotate("text", x= 6, y= 2, label = paste("R = ",round(cor_N1_date$estimate, 3)))
