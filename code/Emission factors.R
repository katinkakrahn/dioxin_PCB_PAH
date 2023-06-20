# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)

# data sets ----
# see data_wrangling

# Emission factors ----
EF <- gas_particle %>% 
  mutate(emission_conc = conc_blank_corr/V_gas_m3,
         EF_tonne = emission_conc*V_gas_m3kg*1000,
         EF_tonne_prop = emission_conc*V_gas_m3kg_propane*1000,
         emission_conc_TEQ = TEQ/V_gas_m3,
         EF_TEQ = emission_conc_TEQ*V_gas_m3kg*1000,
         EF_TEQ_tonne_prop = emission_conc_TEQ*V_gas_m3kg_propane*1000)

EF_summary <- gas_particle_summary %>% 
  mutate(emission_conc = sum_pollutant/V_gas_m3,
         EF_tonne = emission_conc*V_gas_m3kg*1000,
         EF_tonne_prop = emission_conc*V_gas_m3kg_propane*1000,
         emission_conc_TEQ = sum_TEQ/V_gas_m3,
         EF_TEQ = emission_conc_TEQ*V_gas_m3kg*1000,
         EF_TEQ_tonne_prop = emission_conc_TEQ*V_gas_m3kg_propane*1000)

EF_total_summary <- gas_particle_total_summary %>% 
  mutate(EC =                sum_pollutant/V_gas_m3,
         EC_TEQ =            sum_TEQ/V_gas_m3,
         EC_unit =           ifelse(pollutant_class == "dioxin", "pg/m3", "ng/m3"),
         EF_tonne =          EC*V_gas_m3kg/1000,
         EF_tonne_prop =     EC*V_gas_m3kg_propane/1000,
         EF_TEQ =            EC_TEQ*V_gas_m3kg/1000,
         EF_TEQ_tonne_prop = EC_TEQ*V_gas_m3kg_propane/1000,
         EF_unit =           ifelse(pollutant_class == "dioxin", "ug/tonne", "mg/tonne")
  )

EF_total_summary %>% 
  select(sample_ID, pollutant_class, temperature, feedstock, EC, EC_TEQ, EC_unit, EF_tonne_prop, EF_TEQ_tonne_prop, EF_unit) %>% 
  write_xlsx("processed_data/EF_total_summary.xlsx")
