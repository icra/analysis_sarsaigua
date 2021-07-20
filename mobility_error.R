library(tidyverse)
library(emojifont)

path <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(path,"functions.R", sep="/"))
setwd(path)

mobility <- read.csv2("data/mobi_edar.csv")

N1_data <- load_excel() %>% 
  target_data(diana="N1")

mobility <- merge.data.frame(mobility,N1_data, 
                             by.x=c("data", "codi_edar"), by.y=c("Data.mostreig", "EDAR"), 
                             all = F, sort = F)

mobility <- mobility %>% 
  mutate(ratio_origin = origin_trips/Nº.habitants.municipis.assistits.EDAR,
         ratio_destiny = destin_trips/Nº.habitants.municipis.assistits.EDAR)


model <- lm(log_7~log, mobility)
summary(model)

mobility$errors <- model$residuals

ggplot(mobility, aes(x=log, y=log_7, color = ratio_destiny))+
  geom_point()+
  geom_smooth()+
  theme(legend.position = "bottom")

#test correlation between errors and mobility ratio
cor.test(mobility$ratio_origin, abs(mobility$errors))

mobility %>% 
  # filter(Nº.habitants.municipis.assistits.EDAR < 1000000,
  #        Nº.habitants.municipis.assistits.EDAR > 10000) %>%
ggplot(aes(x=ratio_origin, y=abs(errors)))+
  geom_point(aes(color=NÂº.habitants.municipis.assistits.EDAR))+
  geom_smooth()+
  labs(color="Inhabitants")+
  scale_x_sqrt()

#test correlation between errors and size
cor.test(mobility$NÂº.habitants.municipis.assistits.EDAR, abs(mobility$errors))

mobility %>% 
  ggplot(aes(x=NÂº.habitants.municipis.assistits.EDAR, y=abs(errors)))+
  geom_point(aes(color=NÂº.habitants.municipis.assistits.EDAR))+
  geom_smooth()+
  labs(color="Inhabitants")+
  scale_x_sqrt()


#furthest analysis with all series of Besós
cols = c("Catalunya" = "red", "BesÃ³s" = "blue")

dbss_data <- mobility %>% 
  filter(codi_edar == "DBSS") %>%
  group_by(data) %>% 
  summarise(trips = sum(destin_trips, na.rm=T),
            inh = sum(NÂº.habitants.municipis.assistits.EDAR, na.rm=T))
  
mobility %>% 
  group_by(data) %>% 
  summarise(trips = sum(destin_trips, na.rm=T),
            inh = sum(NÂº.habitants.municipis.assistits.EDAR, na.rm=T)) %>% 
  ggplot()+
    geom_line(aes(x=data, y=trips/inh, color = "Catalunya", group=1))+
    geom_line(data=dbss_data, aes(x=data, y=trips/inh, color = "BesÃ³s", group=1))+
    theme(axis.text.x = element_text(angle = 90, vjust=0.5))+
  geom_vline(xintercept = "2020-10-26", linetype = "dashed")+
  geom_vline(xintercept="2020-12-14", linetype = "dashed")+
  geom_vline(xintercept="2021-01-11", linetype = "dashed")+
  geom_vline(xintercept="2021-05-03", linetype = "dashed")+
  geom_fontawesome("fa-lock", x=17, y= 3, size=8)+
  geom_fontawesome("fa-unlock", x=24, y= 3, size=8)+
  geom_fontawesome("fa-lock", x=28, y= 3, size=8)+
  geom_fontawesome("fa-unlock", x=44, y= 3, size=8)+
  scale_color_manual(values=cols)