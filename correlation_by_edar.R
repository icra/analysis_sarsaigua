library(tidyverse)

setwd("C:/Users/jpueyo/OneDrive - ICRA/Other projects/sarsaigua/")

#load functions for data collection and curation
source('analysis_sarsaigua/functions.R')
data <- load_excel(remove_first = F)

theme_set(theme_classic())

color1 <- "darkorange"
color2 <- "grey"
color3 <- "blue3"

filter_value <- 0

casos <- data %>% 
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
  filter(!is.nan(Cabal.influent.últimes.24h.calculat)) %>%
  mutate(N1.Concentracio.final = ifelse(N1.Concentracio.final < filter_value, NA, N1.Concentracio.final),
         N2.Concentracio.final = ifelse(N2.Concentracio.final < filter_value, NA, N2.Concentracio.final),
         IP4.Concentracio.final = ifelse(IP4.Concentracio.final < filter_value, NA, IP4.Concentracio.final),
         load_N2 = N2.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
         load_N1 = N1.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
         load_IP4 = IP4.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
         log_14 = log10(Nº.casos.darrers.14.dies..acum.+1), 
         log_7 = log10(Nº.casos.darrers.7.dies..acum.+1), 
         log_7_7 = log10(Nº.casos.darrers.7.dies.i.següents.7.dies+1),
         log_mean_7 = log10(Mitjana.darrers.7.dies..promig.+1),
         log_N1 = log10(load_N1),
         log_N2 = log10(load_N2),
         log_IP4 = log10(load_IP4))

colnames(casos)[2:5] <- c("darrers_14", "darrers_7", "set_i_set", "mitjana_7")

casos$EDAR <- factor(casos$EDAR)

wwtp <- data.frame(row.names = levels(casos$EDAR))
wwtp$EDAR <- row.names(wwtp)
wwtp$N1 <- NA
wwtp$N1_p <- NA
wwtp$N2 <- NA
wwtp$N2_p <- NA 
wwtp$IP4 <- NA
wwtp$IP4_p <- NA
wwtp$highest <- "N1"

for (i in 1:nrow(wwtp)){
  edar <- casos %>% 
    filter(EDAR == wwtp$EDAR[i])
  
  if (length(which(!is.na(edar$log_N1))) > 2){
    correlation <- cor.test(edar$log_7, edar$log_N1, 
                            method = "pearson", alternative = "greater")
    wwtp$N1[i] <- correlation$estimate
    wwtp$N1_p[i] <- correlation$p.value
  }
  
  for (target in c("N2", "IP4")){
    
    log_target <- paste("log", target, sep="_")
    p_field <- paste(target, "p", sep="_")
    
    if (sum(edar[log_target], na.rm = T)!=0 && length(which(!is.na(edar[log_target]))) > 2){
      correlation <- cor.test(edar[,'log_7'], edar[,log_target], 
                              method = "pearson", alternative = "greater")
      wwtp[i, target] <- correlation$estimate
      wwtp[i, p_field] <- correlation$p.value
      if (!is.na(wwtp[i, target]) && wwtp[i, target] > wwtp$N1[i] && wwtp[i, p_field] < 0.05){
        wwtp$highest[i] <- target
      }
    }
  }
}

wwtp$highest <- factor(wwtp$highest)
highest <- summary(wwtp$highest)
highest <- paste("Highest target N1: ",highest[2],"; N2: ", highest[3],"; IP4: ", highest[1], sep="")

plot_data <- wwtp %>% 
  select(EDAR, N1, N2, IP4) %>% 
  pivot_longer(cols= c(N1, N2, IP4), names_to = "target") %>% 
  filter(!is.na(value))

plot_data$target <- factor(plot_data$target, levels = c("N1", "N2", "IP4"))
plot_data$EDAR <- factor(plot_data$EDAR)

plot_data$inh <- NA

for (i in 1:nrow(plot_data)){
  plot_data$inh[i] <- data %>% 
    filter(EDAR == as.character(plot_data$EDAR[i])) %>% 
    .[1, 'NÃ‚Âº.habitants.municipis.assistits.EDAR']
  plot_data$inh[i] <- as.numeric(plot_data$inh[i])
}

text_size <- 30

ggplot(plot_data, aes(reorder(EDAR, -inh), value, fill=target))+
  geom_col(position = position_dodge(width = 0.9))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1, size=text_size+0.5))+
  labs(x="Correlation between GC and accum. 7 past days", y="R value")+
  annotate("text", x=28, y=-0.05, label=highest, size = text_size/3)+
  theme(text = element_text(size=text_size))
ggsave("Fig6A.png", width = 20, units = "cm")

ggplot(plot_data, aes(target, value, fill = target))+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= "WWTP correlation distribution among targets", y = "R value")
ggsave("Fig6A_violin.png", width = 20, units = "cm")

#plot distribution of correlations among targets
ggplot(plot_data, aes(target, value, fill = target))+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(x= "WWTP correlation distribution among targets/inh", y = "R value")

#plot number of samples per EDAR
ggplot(casos, aes(EDAR))+
  geom_bar(fill=color2)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
