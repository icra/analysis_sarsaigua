library(tidyverse)
library(patchwork)
library(lubridate)
setwd("C:/Users/jpueyo/OneDrive - ICRA/Other projects/sarsaigua/")

#load and curate data
data <- read.csv2("data/fecal_normalization_2.csv")
data <- data[data$EDAR != "",]
data$EDAR <- factor(data$EDAR)

#explore correlations
cor.test(log10(data$cases+1), log10(data$N1.total))
cor.test(log10(data$cases+1), log10(data$HAdV.total+1))
cor.test(log10(data$cases+1), log10(data$N1.HAdV+1))

results <- data.frame(edar = levels(data$EDAR), 
                      N1_total = NA, 
                      p_value1 = NA, 
                      HAdV_total = NA, 
                      p_value2 = NA, 
                      N1_HAdV = NA, 
                      p_value3 = NA)

#calculate and save correlation values
for (wwtp in levels(data$EDAR)){

  data_edar <- data %>% 
    filter(EDAR == wwtp)
  
  #whole serie
  results$N1_total[results$edar==wwtp] <- cor.test(log10(data_edar$N1.total+1), log10(data_edar$cases+1))$estimate
  results$p_value1[results$edar==wwtp] <- cor.test(log10(data_edar$N1.total+1), log10(data_edar$cases+1))$p.value
  
  results$HAdV_total[results$edar==wwtp] <- cor.test(log10(data_edar$HAdV.total+1), log10(data_edar$cases+1))$estimate
  results$p_value2[results$edar==wwtp] <- cor.test(log10(data_edar$HAdV.total+1), log10(data_edar$cases+1))$p.value
  
  results$N1_HAdV[results$edar==wwtp] <- cor.test(log10(data_edar$N1.HAdV+1), log10(data_edar$cases+1))$estimate
  results$p_value3[results$edar==wwtp] <- cor.test(log10(data_edar$N1.HAdV+1), log10(data_edar$cases+1))$p.value
  
}

#plot data
plot_data <- results %>% 
  select(-p_value1,-p_value2,-p_value3) %>% 
  pivot_longer(cols = c(N1_total, HAdV_total, N1_HAdV), names_to = "correlation")

plot_data$correlation <- factor(plot_data$correlation, levels = c("N1_total","HAdV_total","N1_HAdV"),
                                ordered = T)

ggplot(plot_data, aes(edar, value, fill=correlation))+
  geom_col(position = position_dodge(width = 0.9))+
  labs(y= "Correlation value")+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
