library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(RColorBrewer)

path <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(path,"functions.R", sep="/"))
setwd(path)

data <- load_excel(remove_first = F)

filter_value <- 0

casos <- data %>%
  select(Data.mostreig,
         matches("Nº.casos.dia"),
         N1.Concentracio.final,
         N2.Concentracio.final,
         IP4.Concentracio.final,
         Cabal.influent.últimes.24h.calculat,
         EDAR.provincia,
         Nº.habitants.municipis.assistits.EDAR) %>% 
  
  filter(!is.nan(Cabal.influent.últimes.24h.calculat)) %>%
  
  mutate(N1.Concentracio.final = ifelse(N1.Concentracio.final < filter_value, NA, N1.Concentracio.final),
         N2.Concentracio.final = ifelse(N2.Concentracio.final < filter_value, NA, N2.Concentracio.final),
         IP4.Concentracio.final = ifelse(IP4.Concentracio.final < filter_value, NA, IP4.Concentracio.final),
         load_N1 = N1.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
         load_N2 = N2.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat,
         load_IP4 = IP4.Concentracio.final * 1000 * Cabal.influent.últimes.24h.calculat
  )

colnames(casos)[2:16] <- c("menys7", "menys6", "menys5", 
                           "menys4", "menys3", "menys2",
                           "menys1", "zero", "mes1", 
                           "mes2", "mes3", "mes4", 
                           "mes5", "mes6", "mes7")

correlations <- data.frame(row.names = colnames(casos[2:16]))
sequence <- -7:7
for (i in 2:16){
  correlations$case[i-1] <- sequence[i-1]
  
  correlation <- cor.test(log10(casos$load_N1), log10(casos[,i]+1))
  correlations$corr_value_N1[i-1] <- correlation$estimate
  correlations$p_value_N1[i-1] <- correlation$p.value
  
  correlation <- cor.test(log10(casos$load_N2), log10(casos[,i]+1))
  correlations$corr_value_N2[i-1] <- correlation$estimate
  correlations$p_value_N2[i-1] <- correlation$p.value
  
  correlation <- cor.test(log10(casos$load_IP4), log10(casos[,i]+1))
  correlations$corr_value_IP4[i-1] <- correlation$estimate
  correlations$p_value_IP4[i-1] <- correlation$p.value
  
}

max_corr_N1 <- correlations$case[correlations$corr_value_N1 == max(correlations$corr_value_N1)]
max_corr_N2 <- correlations$case[correlations$corr_value_N2 == max(correlations$corr_value_N2)]
max_corr_IP4 <- correlations$case[correlations$corr_value_IP4 == max(correlations$corr_value_IP4)]

correlations$mean_corr <- rowMeans(correlations[c('corr_value_N1','corr_value_N2','corr_value_IP4')])

max_corr <- correlations$case[correlations$mean_corr == max(correlations$mean_corr)]

ggplot(correlations, aes(case))+
  geom_line(aes(y= corr_value_N1, color="N1"))+#geom_point()+
  geom_line(aes(y= corr_value_N2, color="N2"))+#geom_point()+
  geom_line(aes(y= corr_value_IP4, color="IP4"))+#geom_point()+
  geom_line(aes(y=mean_corr, color="mean"))+
  geom_vline(aes(xintercept=0), linetype="dotted")+
  geom_vline(aes(xintercept=max_corr_N1, color="N1"), linetype="dashed", size = 1)+
  geom_vline(aes(xintercept=max_corr_N2, color="N2"), linetype="dashed", size = 1)+
  geom_vline(aes(xintercept=max_corr_IP4, color="IP4"), linetype="dashed", size = 1)+
  #geom_vline(aes(xintercept=max_corr, color="Mean"), linetype="dashed", size = 1.5)+
  theme(legend.position = "bottom")+
  scale_color_discrete(name="Target")+
  labs(y="Correlation value", x= "Wastewater leads cases by x days")
ggsave("Fig4A_best_corr_day.png", width = 20, units = "cm")
