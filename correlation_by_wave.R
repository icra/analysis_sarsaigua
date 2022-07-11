library(tidyverse)
source("functions.R")

data <- load_excel(filename = "Mostres_newest.csv", folder="analysis_sarsaigua/data", remove_first = F)


#load data on target N1 without detection threshold
N1_data <- target_data(data = data, diana = "N1")

# Wave 2: Sept-Oct 2020
# Wave 3: Dec 20 – Feb 21
# Wave 4: March-April 2021
# Wave 5: July-August 2021
# Wave 6: Nov 21 – March 22

waves <- tribble(
  ~Wave, ~Start, ~End,
  2, "2020-09-01", "2020-10-31",
  3, "2020-12-01", "2021-02-28",
  4, "2021-03-01", "2021-04-30",
  5, "2021-07-01", "2021-08-31",
  6, "2021-11-01", "2022-03-31"
)

#function to correlate one edar and one wave
one_one <- function(n_wave, edar){
  wave <- waves %>% 
    filter(Wave == n_wave)
  
  wave_data <- N1_data %>%
    filter(Data.mostreig > wave$Start,
           Data.mostreig < wave$End) %>% 
    filter(EDAR == edar)
  
  if (nrow(wave_data) < 3){
    corr_value <- NA_real_
    p_value <- NA_real_
    comment <- "Not enough observations"
    
  } else if(var(wave_data$log_7, na.rm = T) == 0){
    corr_value <- NA_real_
    p_value <- NA_real_
    comment <- "No cases to estimate the correlation with N1"
    
  } else {
    correlation <- cor.test(wave_data$log_7, wave_data$log)
    corr_value <- correlation$estimate
    p_value <- correlation$p.value
    comment <- ""
  }
  
  wave %>% 
    mutate(EDAR = edar,
           cor_value = corr_value,
           p_value = p_value,
           n = nrow(wave_data),
           comments = comment)
}

#function to iterate waves
all_waves <- function(edar){
  map_dfr(waves$Wave, ~one_one(.x, edar))
}

#iterate all edars and all vawes
correlations <- map_dfr(unique(N1_data$EDAR), all_waves)

correlations %>% 
  write_excel_csv2("../results/correlations_by_wave.csv")
