library(tidyverse)
library(lubridate)
library(viridis)
library(grid)
library(gridExtra)

path <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(path,"functions.R", sep="/"))
setwd(path)

#load excel
data <- load_excel()

#create casos for N1 and correlation
N1_casos <- N1_data(data)

sim1 = function(load, n = 1000, beta1 = 4, beta2 = 2){
  #FEC random matrix
  mu=2.11; sigma=0.25;
  FECmatrix = 10^rnorm(n, mu, sigma) # units; grams feces/person/day
  # shedding rate of SARS-CoV-2 RNA in feces
  # log-uniform distribution from 2.56 to 7.67
  x = c(2.56, 7.67)
  RNAfecmatrix = 10^(x[1] + (x[2]-x[1]) * rbeta(n, beta1, beta2)); #RNA copies per gram feces; de 10^2 a 10^7
  median(load / (FECmatrix * RNAfecmatrix)) #el numerador es pot substituir per load_N1
}
interval_ = function(x) c(quantile(x, 0.025),
                          median(x),
                          quantile(x, 0.975))

#create a range of parameters for beta distribution
beta1 <- 1:5
beta2 <- 5:1  

#for replication
set.seed(1234)

for (i in 1:5){
  for (j in 1:nrow(N1_casos)){
    result <- replicate(1000, sim1(load = N1_casos$load_N1[j], beta1 = beta1[i], beta2 = beta2[i]))
    sim_median <- median(result)
    field <- paste("sim_median",beta1[i], beta2[i], sep="_")
    N1_casos[j,field] <- sim_median
    print(c(i,j))
  }
}

write.csv2(N1_casos, "data/simulation_calibration.csv")

N1_casos <- read.csv2("data/simulation_calibration.csv")

pal <- c("#ED90A4","#D3A263","#99B657","#33C192","#00BDCE","#94A9EC","#DC91DB")

cols <- c(
  "darrers_7"='red',
  "sim_median_1_5"=pal[2],
  "sim_median_2_4"=pal[3],
  "sim_median_3_3"=pal[4],
  "sim_median_4_2"=pal[5],
  "sim_median_5_1"=pal[6],
  "sim_fix_10^7" = pal[7],
  "N1" = 'black'
          )

alpha_value <- 1

ggplot(N1_casos, aes(x=Data.mostreig))+
  geom_line(aes(y=sim_median_1_5+1, group=EDAR, color="sim_median_1_5"), alpha=alpha_value)+
  geom_line(aes(y=sim_median_2_4+1, group=EDAR, color="sim_median_2_4"), alpha=alpha_value)+
  geom_line(aes(y=sim_median_3_3+1, group=EDAR, color="sim_median_3_3"), alpha=alpha_value)+
  geom_line(aes(y=sim_median_4_2+1, group=EDAR, color="sim_median_4_2"), alpha=alpha_value)+
  geom_line(aes(y=sim_median_5_1+1, group=EDAR, color="sim_median_5_1"), alpha=alpha_value)+
  geom_line(aes(y=darrers_7+1, group=EDAR, color="darrers_7"), alpha=alpha_value)+
  geom_line(aes(y=load_N1, group=EDAR, color="N1"), alpha=alpha_value)+
  scale_color_manual(name="", values=cols)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        legend.title = element_blank())+
  scale_y_log10()

cor.test(N1_casos$log_7,log10(N1_casos$sim_median_5_1))

ggplot(N1_casos, aes(load_N1, sim_median_5_1))+
  geom_point(color='red')+
  geom_smooth()+
  scale_color_viridis()

