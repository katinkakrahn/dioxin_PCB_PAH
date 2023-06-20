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
feedstock <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",1) 
biochar <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",2)
oil <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",3)
PUF <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",4)
XAD <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",5) %>% 
  select(-c(comment,conc_div))
GFF <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",6) %>% 
  select(-c(comment,conc_div))
yield_and_gas <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",7)
TEF <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",8)

# TEF merge ----
feedstock <- merge(feedstock, TEF, all.x = TRUE)
biochar <- merge(biochar, TEF, all.x = TRUE)
biochar <- merge(biochar,yield_and_gas, by = "sample_ID_common", all = TRUE)
oil <- merge(oil, TEF, all.x = TRUE)
PUF <- merge(PUF, TEF, all.x = TRUE)
XAD <- merge(XAD, TEF, all.x = TRUE) 
GFF <- merge(GFF, TEF, all.x = TRUE)

# feedstock ----
feedstock_means <- feedstock %>% 
  filter(LOQ_corr==FALSE) %>% 
  group_by(sample_ID, sample_name, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, LOQ_corr, TEF, TEF_order, unit, source) %>% 
  summarise(mean_conc = mean(as.numeric(conc)),
            sd_conc = sd(as.numeric(conc)),
            n = n()) %>% 
  mutate(TEQ = mean_conc*TEF,
         sd_TEQ = sd_conc*TEF)

feedstock_summary <- feedstock_means %>% 
  group_by(sample_ID, sample_name, pollutant_class, unit, temperature, feedstock, source, type) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sum_TEQ = sum(TEQ))

feedstock_LOQ <- feedstock %>% 
  filter(LOQ_corr==TRUE) %>% 
  unique()

feedstock_means_LOQ <- merge(feedstock_means, feedstock_LOQ, all = TRUE) %>% 
  mutate(conc_comb = coalesce(as.character(mean_conc), conc))

feedstock_percent_total <- merge(feedstock_means_LOQ, feedstock_summary, all = TRUE) %>% 
  mutate(perc_comp = ifelse(LOQ_corr == FALSE, mean_conc/sum_pollutant*100, NA))

# feedstock PAH ----
feedstock_PAH_plot <- feedstock_percent_total %>% 
  filter(pollutant_class == "PAH") %>% 
  mutate(across(feedstock, factor, levels=c("DSS-1", "DSS-2", "DWSS",
                                            "LSS", "FWR", "WT", "GW", "CWC"))) %>% 
  ggplot(aes(x = perc_comp, y = reorder(abbreviation, -TEF_order))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.1f", round(perc_comp, digits = 1))),
            hjust = -0.2,
            size = 2.5) +
  facet_wrap(~feedstock,
             ncol=4) +
  labs(x = "% of PAH-16",
       y = "",
       color = "") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  # scale_fill_manual(labels = c("carcinogenic", "non-carcinogenic"),
  #                   breaks = c(TRUE, FALSE),
  #                   values = c("red","black")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.87, 0.08)) +
  guides(fill = guide_legend(title=NULL))
feedstock_PAH_plot
ggsave("figs/feedstock/feedstock_PAH.jpg")

feedstock_PAH_carc <- feedstock_means_LOQ %>% 
  filter(pollutant_class == "PAH") %>% 
  group_by(sample_ID, pollutant_class, unit, carcinogenic, source) %>%
  summarise(sum_pollutant = sum(as.numeric(conc_comb)),
            sum_TEQ = sum(TEQ)) %>% 
  pivot_wider(names_from = carcinogenic,
              values_from = c(sum_pollutant, sum_TEQ)) %>% 
  mutate(sum_pollutant = rowSums(across(c(sum_pollutant_FALSE, sum_pollutant_TRUE))),
         sum_TEQ = rowSums(across(c(sum_TEQ_FALSE, sum_TEQ_TRUE))))

feedstock_PAH_carc2 <- feedstock_percent_total %>% 
  filter(pollutant_class == "PAH") %>%  
  group_by(sample_ID, pollutant_class, unit, carcinogenic, source) %>%
  summarise(sum_pollutant = sum(as.numeric(perc_comp))) %>% 
  pivot_wider(names_from = carcinogenic,
              values_from = sum_pollutant)

# feedstock dioxin ----
feedstock_dioxin_plot <- feedstock_percent_total %>% 
  filter(pollutant_class == "dioxin") %>% 
  mutate(across(feedstock, factor, levels=c("DSS-1", "DSS-2", "DWSS",
                                            "LSS", "FWR", "WT", "GW", "CWC"))) %>% 
  ggplot(aes(x = perc_comp, y = reorder(pollutant, TEF_order))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.2f", round(perc_comp, digits = 2))),
            hjust = -0.2,
            size = 2.5) +
  facet_wrap(~feedstock) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "% of PCDD/F-17",
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
feedstock_dioxin_plot
ggsave("figs/feedstock/feedstock_dioxin.jpg")

# feedstock PCB ----
feedstock_PCB_plot <- feedstock_percent_total %>% 
  filter(pollutant_class == "PCB") %>% 
  mutate(across(feedstock, factor, levels=c("DSS-1", "DSS-2", "DWSS",
                                            "LSS", "FWR", "WT", "GW", "CWC"))) %>% 
  ggplot(aes(x = perc_comp, y = reorder(pollutant, -TEF_order))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.0f", round(perc_comp, digits = 0))),
            hjust = -0.2,
            size = 2.5) +
  facet_wrap(~feedstock) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "% of PCB-7",
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
feedstock_PCB_plot
ggsave("figs/feedstock/feedstock_PCB.jpg")

# biochar ----
biochar_means <- biochar %>% 
  filter(LOQ_corr==FALSE) %>% 
  group_by(sample_ID, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, LOQ_corr, TEF, TEF_order, unit, source, yield_biochar) %>% 
  summarise(mean_conc = mean(as.numeric(conc_corr)),
            sd_conc = sd(as.numeric(conc_corr)),
            n = n()) %>% 
  mutate(TEQ = mean_conc*TEF,
         sd_TEQ = sd_conc*TEF)

biochar_summary <- biochar_means %>% 
  group_by(sample_ID, pollutant_class, unit, temperature, feedstock, source, type, yield_biochar) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sum_TEQ = sum(TEQ))

biochar_LOQ <- biochar %>% 
  filter(LOQ_corr==TRUE) %>% 
  unique()

biochar_means_LOQ <- merge(biochar_means, biochar_LOQ, all = TRUE) %>% 
  mutate(conc_comb = coalesce(as.character(mean_conc), conc_corr))

biochar_percent_total <- merge(biochar_means_LOQ, biochar_summary, all = TRUE) %>% 
  mutate(perc_comp = ifelse(LOQ_corr == FALSE, mean_conc/sum_pollutant*100, NA))

# FB summary ----
feedstock_biochar_summary <- merge(feedstock_summary, biochar_summary, all = TRUE)
write_xlsx(feedstock_biochar_summary, "processed_data/feedstock_biochar_summary.xlsx")
feedstock_biochar_means <- merge(feedstock_means, biochar_means, all = TRUE) 
write_xlsx(feedstock_biochar_means, "processed_data/feedstock_biochar_means.xlsx")
feedstock_biochar_summary_sd <- read_xlsx("processed_data/feedstock_biochar_summary_manual_sd.xlsx")
feedstock_biochar_summary_sd_dummy <- read_xlsx("processed_data/feedstock_biochar_summary_manual_sd_dummy.xlsx")

# FB individual pollutants percent ---- 
FB_perc_comp <- merge(feedstock_percent_total, biochar_percent_total, all = TRUE) %>% 
  mutate(perc_comp_yield = perc_comp*yield_biochar/100)

design_biochar_PCB <- "
ABCDE
FGHIJ
KLM##
"

FB_PCB_plot <- FB_perc_comp %>% 
  filter(pollutant_class == "PCB") %>%
  filter(sample_ID != "FWR-F") %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-800",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-750"))) %>% 
  ggplot(aes(x = perc_comp_yield, y = reorder(pollutant, -TEF_order))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.1f", round(perc_comp_yield, digits = 1))),
            hjust = -0.2) +
  facet_manual(vars(sample_ID),
               design = design_biochar_PCB) +
  labs(x = expression("% of PCB" [tot]),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_PCB_plot
ggsave("figs/FB/FB_PCB_perc_comp.jpg")

design_biochar_dioxin <- "
ABCDE
FGHIJ
KLM##
"

FB_dioxin_plot <- FB_perc_comp %>% 
  filter(pollutant_class == "dioxin",
         feedstock != "LSS") %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-800",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800"))) %>% 
  ggplot(aes(x = perc_comp_yield, y = reorder(pollutant, -TEF_order))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.1f", round(perc_comp_yield, digits = 1))),
            hjust = -0.2) +
  facet_manual(vars(sample_ID),
               design = design_biochar_dioxin) +
  labs(x = expression("% of dioxin" [tot]),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_dioxin_plot
ggsave("figs/FB/FB_dioxin_perc_comp.jpg")

design_FB_PAH_sludge <- "
ABCDE
FGHIJ
KLM##
NOP##
"

FB_PAH_sludge <- FB_perc_comp %>% 
  filter(pollutant_class == "PAH",
         source %in% c("sludge", "reject")) %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-800",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-750",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-500", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
  ggplot(aes(x = perc_comp, y = reorder(abbreviation, -TEF_order))) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label=sprintf("%0.1f", round(perc_comp, digits = 1))),
  #           hjust = -0.2) +
  #facet_wrap(~sample_ID) +
  facet_manual(vars(sample_ID),
               design = design_FB_PAH_sludge) +
  labs(x = TeX(r'($\%\Sigma PAH$-$16$)'),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_PAH_sludge
ggsave("figs/FB/biochar_PAH_sludge.jpg")

design_FB_PAH_wood <- "
QRST#
VWXYZ
12345
"

FB_PAH_wood <- FB_perc_comp %>% 
  filter(pollutant_class == "PAH",
         source == "wood") %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-800",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-750",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-500", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
  ggplot(aes(x = perc_comp, y = reorder(abbreviation, TEF_order))) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label=sprintf("%0.1f", round(perc_comp, digits = 1))),
  #           hjust = -0.2) +
  #facet_wrap(~sample_ID) +
  facet_manual(vars(sample_ID),
               design = design_FB_PAH_wood) +
  labs(x = TeX(r'($\%\Sigma PAH$-$16$)'),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_PAH_wood
ggsave("figs/FB/biochar_PAH_wood.jpg")

# FB individual pollutants concentration ----
design_FB_PAH_sludge <- "
ABCDE
FGHIJ
KLM##
NOP##
"

FB_PAH_sludge_conc <- FB_mean_conc %>% 
  filter(pollutant_class == "PAH",
         source %in% c("sludge", "reject")) %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-800",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-750",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-500", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
  ggplot(aes(x = mean_conc, y = reorder(abbreviation, -TEF_order))) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label=sprintf("%0.1f", round(mean_conc, digits = 1))),
  #           hjust = -0.2) +
  #facet_wrap(~sample_ID) +
  facet_manual(vars(sample_ID),
               design = design_FB_PAH_sludge) +
  labs(x = TeX(r'($\Sigma PAH$-$16~(\mu g~g^{-1})$)'),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_PAH_sludge_conc
ggsave("figs/FB/biochar_PAH_sludge_conc.jpg")

design_FB_PAH_wood <- "
QRST#
VWXYZ
12345
"

FB_PAH_wood_conc <- FB_mean_conc %>% 
  filter(pollutant_class == "PAH",
         source == "wood") %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-800",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-750",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-500", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
  ggplot(aes(x = mean_conc, y = reorder(abbreviation, -TEF_order))) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label=sprintf("%0.1f", round(mean_conc, digits = 1))),
  #           hjust = -0.2) +
  #facet_wrap(~sample_ID) +
  facet_manual(vars(sample_ID),
               design = design_FB_PAH_wood) +
  labs(x = TeX(r'($\Sigma PAH$-$16~(\mu g~g^{-1})$)'),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_PAH_wood_conc
ggsave("figs/FB/biochar_PAH_wood_conc.jpeg")

# FB PAH ----
FB_PAH_summary_plot <- feedstock_biochar_summary_sd_dummy %>% 
  filter(pollutant_class == "PAH",
         feedstock != "DWSS") %>% 
  mutate(feedstock = factor(feedstock, 
                            levels = c("WT","DSS-1", "LSS", 
                                       "DSS-2", "DWSS","GW","FWR", "CWC")
  )) %>% 
  ggplot(aes(x = as.character(temperature), y = sum_pollutant, group = feedstock, fill = feedstock)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(aes(ymin=sum_pollutant-sd_pollutant, 
                    ymax=sum_pollutant+sd_pollutant),
                width = 0.3,
                show.legend = F,
                position = position_dodge(.9)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\Sigma PAH$-$16~(\mu g~g^{-1})$)'),
       fill = "Feedstock") +
  facet_zoom(ylim = c(0,40)) +
  scale_fill_manual(values = c("#a6761d","#d95f02","#e6ab02", "#7570b3",
                                        "#66a61e", "#e7298a","#1b9e77"),
                                        breaks = c("WT","DSS-1","LSS",  "DSS-2", 
                                                   "GW","FWR","CWC")) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12))
FB_PAH_summary_plot
ggsave("figs/FB/FB_PAH_barzoom.jpg")

# FB dioxin ----
FB_dioxin_summary_plot <- feedstock_biochar_summary_sd %>% 
  filter(pollutant_class == "dioxin") %>% 
  mutate(feedstock = factor(feedstock, 
                            levels = c("DSS-1", "LSS", 
                                       "FWR", "DSS-2")
  )) %>% 
  ggplot(aes(x = as.character(temperature), y = sum_pollutant, group = feedstock, fill = feedstock)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(aes(ymin=sum_pollutant-sd_pollutant, 
                    ymax=sum_pollutant+sd_pollutant),
                width = 0.3,
                show.legend = F,
                position = position_dodge(.9)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\Sigma TCDD{/}TCDF$-$15~(pg~TEQ~g^{-1})$)'),
       fill = "Feedstock") +
  facet_zoom(ylim = c(0,15)) +
  scale_fill_manual(values = c("#d95f02", "#e6ab02","#e7298a", "#7570b3"),
                    breaks = c("DSS-1", "LSS", "FWR", "DSS-2")) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12))
FB_dioxin_summary_plot
ggsave("figs/FB/FB_dioxin_barzoom.jpg")

# FB PCB ----
FB_PCB_summary_plot <- feedstock_biochar_summary_sd_dummy %>%
  filter(pollutant_class == "PCB") %>% 
  mutate(feedstock = factor(feedstock, 
                            levels = c("DSS-1", "LSS", 
                                       "FWR", "DSS-2")
  )) %>% 
  ggplot(aes(x = as.character(temperature), 
             y = sum_pollutant, 
             group = feedstock, 
             fill = feedstock)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(aes(ymin=sum_pollutant-sd_pollutant, 
                    ymax=sum_pollutant+sd_pollutant),
                width = 0.3,
                show.legend = F,
                position = position_dodge(.9)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\Sigma PCB$-$7~(ng~g^{-1})$)'),
       fill = "Feedstock") +
  scale_fill_manual(values = c("#d95f02", "#e6ab02","#e7298a", "#7570b3"),
                    breaks = c("DSS-1", "LSS", "FWR", "DSS-2")) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 12)) +
  facet_zoom(ylim = c(0,2))
FB_PCB_summary_plot
ggsave("figs/FB/FB_PCB.jpg")

# oil ----
oil_means <- oil %>% 
  group_by(sample_ID, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, TEF, TEF_order, unit, source) %>% 
  summarise(mean_conc = mean(as.numeric(conc)),
            sd_conc = sd(as.numeric(conc)),
            n = n()) %>% 
  mutate(TEQ = mean_conc*TEF,
         sd_TEQ = sd_conc*TEF)

oil_summary <- oil_means %>% 
  group_by(sample_ID, pollutant_class, unit, temperature, feedstock, source, type) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sum_TEQ = sum(TEQ))

# Gas and particle ----
gas <- merge(PUF, XAD, all = TRUE)
gas_particle <- merge(gas, GFF, all = TRUE)
gas_particle <- merge(gas_particle, yield_and_gas, all = TRUE) %>% 
  mutate(TEQ = TEF*conc_blank_corr)

gas_particle_summary <- gas_particle %>% 
  group_by(sample_ID, pollutant_class, phase, unit, temperature, feedstock, V_gas_m3, V_gas_m3kg) %>% 
  summarise(sum_pollutant = sum(conc_blank_corr),
            sum_TEQ = (sum(TEQ)))

gas_particle_percent <- gas_particle_summary %>% 
  pivot_wider(names_from = phase,
              values_from = c(sum_pollutant, sum_TEQ)) %>% 
  mutate(percent_particle = sum_pollutant_particle/(sum_pollutant_particle+sum_pollutant_gas)*100,
         percent_gas = 100-percent_particle)

write_xlsx(gas_particle_percent, "processed_data/gas_particle_percent.xlsx")

gas_particle_total_summary <- gas_particle %>% 
  group_by(sample_ID, phase2, pollutant_class, unit, temperature, feedstock, V_gas_m3, V_gas_m3kg) %>% 
  summarise(sum_pollutant = sum(conc_blank_corr),
            sum_TEQ = (sum(TEQ)))

write_xlsx(gas_particle_total_summary, "processed_data/gas_particle_total_summary.xlsx")

# Emission factors ----
EF <- gas_particle %>% 
  mutate(emission_conc = conc_blank_corr/V_gas_m3,
         EF = emission_conc*V_gas_m3kg*1000,
         emission_conc_TEQ = TEQ/V_gas_m3,
         EF_TEQ = emission_conc_TEQ*V_gas_m3kg*1000)

EF_summary <- gas_particle_summary %>% 
  mutate(emission_conc = sum_pollutant/V_gas_m3,
         EF = emission_conc*V_gas_m3kg*1000,
         emission_conc_TEQ = sum_TEQ/V_gas_m3,
         EF_TEQ = emission_conc_TEQ*V_gas_m3kg*1000)

EF_total_summary <- gas_particle_total_summary %>% 
  mutate(emission_conc = sum_pollutant/V_gas_m3,
         EF = emission_conc*V_gas_m3kg*1000,
         emission_conc_TEQ = sum_TEQ/V_gas_m3,
         EF_TEQ = emission_conc_TEQ*V_gas_m3kg*1000)

write_xlsx(EF_total_summary, "processed_data/EF_total_summary.xlsx")



