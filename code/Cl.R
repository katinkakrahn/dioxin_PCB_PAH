# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)

# Raw data ----
Cl <- read_xlsx("raw_data/Cl_data.xlsx")

Cl_summary <- Cl %>% 
  group_by(sample_ID, feedstock, phase, unit) %>% 
  summarise(mean_conc = mean(conc_corr),
            sd_conc = sd(conc_corr))
