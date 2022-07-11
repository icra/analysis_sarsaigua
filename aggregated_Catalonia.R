library(patchwork)
library(RColorBrewer)
#library(emojifont)

path <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste(path,"functions.R", sep="/"), encoding = "UTF-8")
#source(paste(path,"vaccine_curation.R", sep="/"))
setwd(path)

filter_data <- "2021-12-31"

#load data on vaccine doses
# vaccines <- get_vaccines() %>% 
#   filter(DATA_FI_YMD <= filter_data)

#load mostres_newest.csv (csv created from sarsaigua data)
data <- load_excel(remove_first = F) %>% 
  filter(YMDmostreig <= filter_data)


#set plot parameters
theme_set(theme_classic())
color1 <- "darkgreen"
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
  # sum_N1_inh <- merge.data.frame(sum_N1_inh, vaccines, by="Data.mostreig", all=T)
  
  sum_N1_inh <- sum_N1_inh %>% 
    mutate(sum_N1_inh = sum_N1 / sum_inh * 10^5,
           sum_7_inh = sum_7 / sum_inh * 10^5)
  
  correlation <- cor.test(sum_N1_inh$sum_N1, sum_N1_inh$sum_7, pairwise.obs = T)
  print(correlation)
  
  corr_text <- paste("R[Pearson] == ", round(correlation$estimate, digits = 2))  
  
  cols <- c("N1"= color1, "Cases" = color2) #, "vaccine doses" = color3)
  event <- "#575757"
  event_line <- "dashed"
  
  ratio <-  max(sum_N1_inh$sum_N1_inh, na.rm=T) / max(sum_N1_inh$sum_7_inh) *0.8
  
  pl <- ggplot(sum_N1_inh, aes(x=Data.mostreig))+
    geom_col(aes(y=sum_7_inh*ratio, fill = "Cases"))+
    geom_line(aes(y=sum_N1_inh, group=1, color="N1"), size = linesize)+
    scale_y_continuous(name="Total viral load N1 (GC/day/100,000 inhabitants)", 
                       sec.axis = sec_axis(trans=~./ratio, 
                                           name="COVID-19 cases (cases/100,000 inhabitants)"))+
    scale_x_discrete(breaks=date_breaks)+
    theme_classic()+
    theme(legend.title = element_blank())+ #, text=element_text(size=30))+
    scale_color_manual(name="", values=cols)+
    scale_fill_manual(name="", values=cols)+
    annotate("text", x= nrow(sum_N1_inh)-12, y= max(sum_N1_inh$sum_N1_inh*0.9),
              label = corr_text, size=2.5, parse=T)+
    labs(x=title)
  
  return(pl)
}

#create main plot
min <- 0
max <- Inf 
title <- "Evolution of GC N1/day/inh and accumulated 7 days past cases / inh"
linesize <-  1.2

plot_data <- N1_data %>% 
  filter(Nº.habitants.municipis.assistits.EDAR > min,
         Nº.habitants.municipis.assistits.EDAR < max)

sum_N1_inh <- plot_data %>% 
  group_by(Data.mostreig) %>%
  summarize(sum_N1 = sum(load, na.rm = T)) %>% 
  mutate(log_sum_N1 = log10(sum_N1))

sum_darrers_7 <- plot_data %>% 
  group_by(Data.mostreig) %>% 
  summarize(sum_7 = sum(darrers_7, na.rm = T)) %>% 
  mutate(log_sum_7 = log10(sum_7))

sum_inh <- plot_data %>% 
  group_by(Data.mostreig) %>% 
  summarise(sum_inh = sum(Nº.habitants.municipis.assistits.EDAR, na.rm = T))

sum_N1_inh <- merge.data.frame(sum_N1_inh, sum_darrers_7, by="Data.mostreig", all = T)
sum_N1_inh <- merge.data.frame(sum_N1_inh, sum_inh, by="Data.mostreig", all = T)
# sum_N1_inh <- merge.data.frame(sum_N1_inh, vaccines, by="Data.mostreig", all=T)

sum_N1_inh <- sum_N1_inh %>% 
  mutate(sum_N1_inh = sum_N1 / sum_inh * 10^5,
         sum_7_inh = sum_7 / sum_inh * 10^5)

correlation <- cor.test(sum_N1_inh$sum_N1, sum_N1_inh$sum_7, pairwise.obs = T)

wave2 <- dmy(c("28-09-2020", "30-11-2020"))
wave3 <- dmy(c("18-12-2020", "21-02-2021"))
wave4 <- dmy(c("21-03-2021", "17-05-2021"))
wave5 <- dmy(c("14-06-2021", "20-09-2021"))
wave6 <- dmy(c("18-10-2021", "27-12-2021"))

waves <- list(wave2, wave3, wave4, wave5, wave6)


cor_wave <- function(period){
  data <- sum_N1_inh %>% 
    filter(between(ymd(Data.mostreig), period[1], period[2]))
  
  cor.test(data$sum_N1, data$sum_7)$estimate
}

map_dbl(waves, cor_wave)

corr_text <- paste("R[Pearson] == ", round(correlation$estimate, digits = 2))  

cols <- c("N1"= color1, "Cases" = color2) #, "vaccine doses" = color3)
event <- "#575757"
event_line <- "dashed"

ratio <-  max(sum_N1_inh$sum_N1_inh, na.rm=T) / max(sum_N1_inh$sum_7_inh)

cols <- c("N1"= color1, "Cases" = color2) #, "2nd vaccine doses" = color3, "1st vaccine doses" = "lightblue")
event <- "#575757"
event_line <- "dashed"

(p1 <- ggplot(sum_N1_inh, aes(x=Data.mostreig))+
  geom_col(aes(y=sum_7_inh*ratio, fill = "Cases"))+
  geom_line(aes(y=sum_N1_inh, group=1, color="N1"), size = linesize)+
  #geom_line(aes(y=accum_2_1000*ratio, group=1, color="2nd vaccine doses"), size=0.8)+
  #geom_line(aes(y=accum_1_1000*ratio, group=1, color="1st vaccine doses"), size=0.8)+
  scale_y_continuous(name="Total viral load N1 (GC/day/100,000 inhabitants)", 
                     sec.axis = sec_axis(trans=~./ratio, 
                                         name="COVID-19 cases (cases/100,000 inhabitants)"))+
  scale_x_discrete(breaks=date_breaks)+
  theme(legend.title = element_blank())+#, text=element_text(size=30, lineheight = 0.3))+
  scale_color_manual(name="", values=cols)+
  scale_fill_manual(name="", values=cols)+
  labs(x=title)+
  annotate("text", x= 8, y= max(sum_N1_inh$sum_N1_inh*0.9),
          label = corr_text, size=3, parse=T)+
  # geom_vline(xintercept = "2020-12-07", color=event, linetype = event_line)+
  # geom_vline(xintercept = "2021-03-15", color=event, linetype = event_line)+
  annotate("text", x="2020-10-26", y=-10^12.1, label=expression("2"^"nd"~wave), size=3)+
  annotate("text", x="2021-01-18", y=-10^12.1, label=expression("3"^"rd"~wave), size=3)+
  annotate("text", x="2021-04-26", y=-10^12.1, label=expression("4"^"th"~wave), size=3)+
  annotate("text", x="2021-08-02", y=-10^12.1, label=expression("5"^"th"~wave), size=3)+
  annotate("text", x="2021-12-06", y=-10^12.1, label=expression("6"^"th"~wave), size=3)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))+
  theme(axis.title = element_text(size=9)))

small_plots <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                     axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_blank(), legend.position = "none")

#create small plots  
p2 <- getPlot(min = 150000, title="WWTP larger than 150,000 inhabitants", linesize = 0.8)
p2 <- p2+small_plots
p3 <- getPlot(max = 150000, title ="WWTP smaller than 150,000 inhabitants", linesize = 0.8)
p3 <- p3+small_plots

#patch all plots  
p1 / (p2 + p3) + plot_layout(heights = c(8,2,2))

#save the result
ggsave("../results/corr_plot.png", width = 20, height = 15, units = "cm")
