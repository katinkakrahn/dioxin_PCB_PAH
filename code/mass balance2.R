# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)

# Data sets ----
# see data_wrangling.R for data sets

# mean and summarize, EC and EF ----
data_means <- all_data %>% 
  filter(feedstock != "DWSS",
         LOQ_corr == FALSE) %>% 
  group_by(sample_ID, sample_ID_common, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, LOQ_corr, TEF, TEF_order, unit, 
           source, yield, phase, phase2, V_gas_m3, V_gas_m3kg, V_gas_m3kg_propane, V_gas_m3kg_feedstock) %>% 
  summarise(mean_conc = mean(as.numeric(conc_corr_all)),
            sd_conc = sd(as.numeric(conc_corr_all)),
            mean_TEQ = mean(as.numeric(conc_corr_all)*TEF),
            sd_TEQ = sd(as.numeric(conc_corr_all)*TEF),
            mean_conc_yield_corrected = mean(as.numeric(conc_yield_corrected)),
            sd_conc_yield_corrected = sd(as.numeric(conc_yield_corrected)),
            mean_TEQ_yield_corrected = mean(as.numeric(conc_yield_corrected)*TEF),
            sd_TEQ_yield_corrected = sd(as.numeric(conc_yield_corrected)*TEF),
            n = n())

data_summary_GP_separate <- data_means %>% 
  group_by(sample_ID,  sample_ID_common, pollutant_class, phase2, unit, type, temperature, feedstock, source,
           V_gas_m3, V_gas_m3kg, V_gas_m3kg_propane, V_gas_m3kg_feedstock, yield) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sd_sum_pollutant = sqrt(sum(sd_conc^2)),
            sum_TEQ = sum(mean_TEQ),
            sd_sum_TEQ = sqrt(sum(sd_TEQ^2)),
            sum_conc_yield_corrected = sum(as.numeric(mean_conc_yield_corrected)),
            sd_sum_conc_yield_corrected = sqrt(sum(as.numeric(sd_conc_yield_corrected^2))),
            n())


data_summary_GP_together <- data_means %>% 
  group_by(sample_ID,  sample_ID_common, pollutant_class, phase2, unit, temperature, feedstock, source,
           V_gas_m3, V_gas_m3kg, V_gas_m3kg_propane, V_gas_m3kg_feedstock, yield) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sd_sum_pollutant = sqrt(sum(sd_conc^2)),
            sum_TEQ = sum(mean_TEQ),
            sd_sum_TEQ = sqrt(sum(sd_TEQ^2)),
            sum_conc_yield_corrected = sum(as.numeric(mean_conc_yield_corrected)),
            sd_sum_conc_yield_corrected = sqrt(sum(as.numeric(sd_conc_yield_corrected^2))),
            sum_TEQ_yield_corrected = sum(as.numeric(mean_TEQ_yield_corrected)),
            sd_TEQ_conc_yield_corrected = sqrt(sum(as.numeric(sd_TEQ_yield_corrected^2))),
            n()) %>% 
  mutate(EC =               sum_pollutant/V_gas_m3,
         EC_yield =         sum_conc_yield_corrected/V_gas_m3,
         EC_TEQ =           sum_TEQ/V_gas_m3,
         EC_TEQ_yield =     sum_TEQ_yield_corrected/V_gas_m3,
         EC_unit = case_when(pollutant_class == "dioxin" ~ "ng/m3",
                             pollutant_class == "PCB" ~ "µg/m3",
                             pollutant_class == "PAH" ~ "mg/kg"),
         EC_unit =          ifelse(pollutant_class == "dioxin", "ng/m3", "ng/m3"),
         EF =               EC*V_gas_m3kg,
         EF_prop =          EC*V_gas_m3kg_propane,
         EF_feedstock =     EC*V_gas_m3kg_feedstock,
         EF_yield_feedstock = EC_yield*V_gas_m3kg_feedstock,
         EF_TEQ =           EC_TEQ*V_gas_m3kg,
         EF_TEQ_prop =      EC_TEQ*V_gas_m3kg_propane,
         EF_TEQ_feedstock = EC_TEQ*V_gas_m3kg_feedstock,
         EF_TEQ_yield_feedstock = EC_TEQ_yield*V_gas_m3kg_feedstock,
         EF_unit =          ifelse(pollutant_class == "dioxin", "ug/kg", "mg/kg")
  ) %>% 
  mutate(conc_perkg_feedstock = ifelse(phase2 == "gas", EF_yield_feedstock, sum_conc_yield_corrected),
         TEQ_perkg_feedstock = ifelse(phase2 == "gas", EF_TEQ_yield_feedstock, sum_TEQ_yield_corrected))
  

mass_balance <- data_summary_GP_together %>% 
  ungroup() %>% 
  select(sample_ID_common, pollutant_class, phase2, temperature, feedstock, 
         source, conc_perkg_feedstock, TEQ_perkg_feedstock, EF_unit) %>% 
  pivot_wider(names_from = phase2,
              values_from = c(conc_perkg_feedstock, TEQ_perkg_feedstock))

feedstock_selection <- mass_balance %>% 
  filter(temperature == "feedstock") %>% 
  select(pollutant_class, feedstock, conc_perkg_feedstock_feedstock, TEQ_perkg_feedstock_feedstock)

mass_balance <- mass_balance %>% 
  select(-c(conc_perkg_feedstock_feedstock, TEQ_perkg_feedstock_feedstock))

mass_balance_merge <- merge(feedstock_selection, mass_balance, all = TRUE) %>% 
  filter(temperature != "feedstock")

mass_balance_selection <- mass_balance_merge %>% 
  select(c(sample_ID_common, pollutant_class, EF_unit, feedstock, temperature,
           conc_perkg_feedstock_biochar, conc_perkg_feedstock_gas, conc_perkg_feedstock_oil, conc_perkg_feedstock_feedstock,
           TEQ_perkg_feedstock_biochar, TEQ_perkg_feedstock_gas, TEQ_perkg_feedstock_oil, TEQ_perkg_feedstock_feedstock)) %>% 
  mutate(sum_pollutant = conc_perkg_feedstock_biochar + conc_perkg_feedstock_gas + conc_perkg_feedstock_oil,
         sum_TEQ = TEQ_perkg_feedstock_biochar + TEQ_perkg_feedstock_gas + TEQ_perkg_feedstock_oil,
         perc_bc = conc_perkg_feedstock_biochar/sum_pollutant,
         perc_gas = conc_perkg_feedstock_gas/sum_pollutant,
         perc_oil = conc_perkg_feedstock_oil/sum_pollutant,
         conc_comparison = case_when(pollutant_class == "dioxin" & conc_perkg_feedstock_feedstock > sum_pollutant ~ "dioxin degradation", 
                                  pollutant_class == "PAH" & conc_perkg_feedstock_feedstock < sum_pollutant ~ "PAH formation")
         )

write_xlsx(mass_balance_selection, "processed_data/mass_balance.xlsx")

# EBC limits ----
EBC <- biochar_summary %>% 
  filter(pollutant_class == "PAH",
         sum_pollutant < 12)

cum_feedstock_PAH <- biochar_summary %>% 
  filter(pollutant_class == "PAH") %>% 
  group_by() %>% 
  summarise(mean = mean(sum_pollutant),
            total = sum(sum_pollutant),
            median = median(sum_pollutant))

# Cl correlation
write_xlsx(Cl, "processed_data/Cl.xlsx")
dioxins <- data_summary_GP_together %>% 
  filter(pollutant_class == "dioxin")

Cl_lm <- merge(dioxins, Cl, all = TRUE) %>% 
  select(feedstock, phase2, sum_pollutant, mean_Cl) %>% 
  drop_na(mean_Cl) %>% 
  filter(phase2 == "biochar") %>% 
  lm(sum_pollutant~mean_Cl,.)
 
summary(Cl_lm) 
  
cor(use = "pairwise.complete.obs", # to include the maximum of data even when values are missing
      method = "pearson")

