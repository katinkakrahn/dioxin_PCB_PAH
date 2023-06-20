FB_PAH_summary <-feedstock_biochar_summary_sd %>% 
  filter(pollutant_class == "PAH")
FB_dioxin_summary <-feedstock_biochar_summary_sd %>% 
  filter(pollutant_class == "dioxin")
FB_PCB_summary <-feedstock_biochar_summary_sd %>% 
  filter(pollutant_class == "PCB")

FB_PAH_plot <- feedstock_biochar_summary %>% 
  filter(pollutant_class == "PAH") %>% 
  group_by(temperature) %>% 
  ggplot(aes(x = as.character(temperature), y = sum_pollutant)) +
  geom_point(aes(color = feedstock),
             size = 4) +
  geom_line(aes(group = feedstock, color = feedstock),
            linewidth = 1) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($PAH_{tot}~(\mu g~g^{-1})$)'),
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_PAH_plot
ggsave("figs/FB_PAH.jpg")

FB_dioxin_plot <- feedstock_biochar_summary %>% 
  filter(pollutant_class == "dioxin") %>% 
  group_by(temperature) %>% 
  ggplot(aes(x = as.character(temperature), y = sum_pollutant)) +
  geom_point(aes(color = feedstock),
             size = 3) +
  geom_line(aes(group = feedstock, color = feedstock)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($dioxin_{tot}~(pg~g^{-1})$)'),
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_dioxin_plot
ggsave("figs/FB_dioxin.jpg")

FB_PCB_plot <- feedstock_biochar_summary %>% 
  filter(pollutant_class == "PCB") %>% 
  group_by(temperature) %>% 
  ggplot(aes(x = as.character(temperature), y = sum_pollutant)) +
  geom_point(aes(color = feedstock),
             size = 3) +
  geom_line(aes(group = feedstock, color = feedstock)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($PCB_{tot}~(ng~g^{-1})$)'),
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
FB_PCB_plot
ggsave("figs/FB_PCB.jpg")

biochar_dioxin_plot2 <- biochar_dioxin_total %>% 
  group_by(temperature) %>% 
  ggplot(aes(x = temperature, y = sum_pollutant)) +
  geom_point(aes(color = feedstock),
             size = 3) +
  geom_line(aes(color = feedstock)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\Sigma TCDD{/}F$-$15~(p g~g^{-1})$)'),
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
biochar_dioxin_plot2
ggsave("figs/biochar_dioxin2.jpg")

biochar_RE <- biochar_means_LOQ %>% 
  select(-c(pollutant, pollutant_class))
feedstock_RE <- feedstock_means_LOQ %>% 
  select(-c(pollutant, pollutant_class))

RE <- merge(feedstock_means, biochar_means, by = "feedstock") %>% 
  filter(LOQ_corr.x==FALSE | LOQ_corr.y ==FALSE) %>% 
  mutate(RE = 100-(mean_conc.y*yield_percTS/mean_conc.x)) %>% 
  filter(pollutant_class.x != "PAH" | pollutant_class.y != "PAH")

gas_particle_blank_corr <- gas_particle %>% 
  mutate(blank_mean = ifelse(LOQ_blank==FALSE,rowMeans(select(., blank1_corr, blank2_corr, blank3_corr)),blank1_corr),
         blank_sd = ifelse(LOQ_blank==FALSE,rowSds(as.matrix(.[c("blank1_corr", "blank2_corr", "blank3_corr")])),NA),
         conc_bl_corr = ifelse(LOQ_blank==FALSE & LOQ==FALSE,as.numeric(conc)-blank_mean,0),
         TEQ = ifelse(LOQ==FALSE & LOQ==FALSE,TEF*as.numeric(conc),0))

gas_particle_blank_corr <- gas_particle %>% 
  mutate(conc_bl_corr = ifelse(LOQ==FALSE,as.numeric(conc)-blank_mean,0),
         TEQ = ifelse(LOQ==FALSE,TEF*as.numeric(conc),0))

biochar_PAH_plot2 <- biochar_percent_total %>% 
  filter(pollutant_class == "PAH") %>% 
  group_by(temperature) %>% 
  ggplot(aes(x = temperature, y = sum_pollutant)) +
  geom_point(aes(color = feedstock),
             size = 3) +
  geom_line(aes(color = feedstock)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\Sigma PAH$-$16~(\mu g~g^{-1})$)'),
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
biochar_PAH_plot2
ggsave("figs/biochar_PAH2.jpg")

design_biochar_PAH <- "
ABCD
EFGH
IJKL
MN##
OPQ#
RS##
TUVW
"

biochar_PAH_plot <- biochar_percent_total %>% 
  filter(pollutant_class == "PAH") %>%  
  mutate(across(feedstock, factor, levels=c("DSS-1", "DSS-2", "DWSS",
                                            "LSS", "FWR", "WT", "GW", "CWC"))) %>% 
  ggplot(aes(x = perc_comp, y = reorder(pollutant, perc_comp))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.1f", round(perc_comp, digits = 1))),
            hjust = -0.2) +
  facet_manual(vars(sample_ID),
               design = design_biochar_PAH) +
  labs(x = TeX(r'($\%\Sigma PAH$-$16$)'),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
biochar_PAH_plot
ggsave("figs/biochar_PAH.jpg")

design_biochar_dioxin <- "
ABCD
EFGH
IJ##
"

biochar_dioxin_plot <- biochar_percent_total %>% 
  filter(pollutant_class == "dioxin") %>% 
  mutate(across(feedstock, factor, levels=c("DSS-1", "DSS-2", "DWSS",
                                            "LSS", "FWR", "WT", "GW", "CWC"))) %>% 
  ggplot(aes(x = perc_comp, y = reorder(pollutant, perc_comp))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.1f", round(perc_comp, digits = 1))),
            hjust = -0.2) +
  facet_manual(vars(sample_ID),
               design = design_biochar_dioxin) +
  labs(x = TeX(r'($\%\Sigma TCDD{/}F$-$15$)'),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
biochar_dioxin_plot
ggsave("figs/biochar_dioxin.jpg")

biochar_dioxin_temperature <- biochar_percent_total %>% 
  filter(pollutant_class == "dioxin") %>% 
  ggplot(aes(x = as.character(temperature), y = sum_TEQ, group = feedstock, fill = feedstock)) +
  geom_bar(position = position_dodge(),
           stat = "identity") +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\%\Sigma TEQ TCDD{/}F$-$15$)'),
       color = "") +
  theme_bw() +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", 
                                        "#66a61e", "#e6ab02", "#a6761d"),
                                        breaks = c("CWC", "DSS-1", "DSS-2", "FWR", 
                                                   "GW", "LSS", "WT")) +
  theme(panel.grid = element_blank())
biochar_dioxin_temperature 
ggsave("figs/biochar_dioxin_temperature.jpg")

biochar_PAH_temperature <- biochar_percent_total %>% 
  filter(pollutant_class == "PAH") %>% 
  ggplot(aes(x = as.character(temperature), y = sum_TEQ, group = feedstock, fill = feedstock)) +
  geom_bar(position = position_dodge(),
           stat = "identity") +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\Sigma TEQ PAH$-$16~(\mu g~g^{-1})$)'),
       color = "") +
  theme_bw() +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", 
                                        "#66a61e", "#e6ab02", "#a6761d"),
                                        breaks = c("CWC", "DSS-1", "DSS-2", "FWR", 
                                                   "GW", "LSS", "WT")) +
  theme(panel.grid = element_blank())
biochar_PAH_temperature 
ggsave("figs/biochar_PAH_temperature.jpg")

biochar_PCB_temperature <- biochar_percent_total %>% 
  filter(pollutant_class == "PCB") %>% 
  ggplot(aes(x = as.character(temperature), y = sum_pollutant, group = feedstock, fill = feedstock)) +
  geom_bar(position = position_dodge(),
           stat = "identity") +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($\Sigma TEQ~PCB$-$7~(pg~g^{-1})$)'),
       color = "") +
  theme_bw() +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", 
                                        "#66a61e", "#e6ab02", "#a6761d"),
                                        breaks = c("CWC", "DSS-1", "DSS-2", "FWR", 
                                                   "GW", "LSS", "WT")) +
  theme(panel.grid = element_blank())
biochar_PCB_temperature 
ggsave("figs/biochar_PCB_temperature.jpg")

design_biochar_PCB <- "
ABCD
EFGH
IJ##
"

biochar_PCB_plot <- biochar_percent_total %>% 
  filter(pollutant_class == "PCB") %>%
  ggplot(aes(x = perc_comp, y = reorder(pollutant, -TEF_order))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sprintf("%0.1f", round(perc_comp, digits = 1))),
            hjust = -0.2) +
  facet_manual(vars(sample_ID),
               design = design_biochar_PCB) +
  labs(x = expression("% of PCB" [tot]),
       y = "",
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
biochar_PCB_plot
ggsave("figs/biochar_PCB.jpg")


biochar_PCB_plot2 <- biochar_percent_total %>% 
  filter(pollutant_class == "dioxin") %>% 
  group_by(temperature) %>% 
  ggplot(aes(x = temperature, y = sum_pollutant)) +
  geom_point(aes(color = feedstock),
             size = 3) +
  geom_line(aes(color = feedstock)) +
  labs(x = "Pyrolysis temperature (\u00B0C)",
       y = TeX(r'($PCB_{tot}~(ng~g^{-1})$)'),
       color = "") +
  theme_bw() +
  theme(panel.grid = element_blank())
biochar_PCB_plot2
ggsave("figs/biochar_PCB2.jpg")

FB_PAH_summary_plot <- feedstock_biochar_summary_sd %>%
  filter(pollutant_class == "PAH") %>% 
  mutate(feedstock = factor(feedstock,
                            levels = c("WT", "DWSS", "DSS-2", "DSS-1",
                                       "CWC", "GW", "FWR", "LSS")
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
       y = TeX(r'($PAH_{tot}~(pg~g^{-1})$)'),
       fill = "Feedstock") +
  theme(legend.position = "bottom",
        text = element_text(size = 9)) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = guide_legend(override.aes = list(size = 2)))
FB_PAH_summary_plot
ggsave("figs/FB_PAH.jpg")

# FB summary ----
feedstock_biochar_summary <- merge(feedstock_summary, biochar_summary, all = TRUE)
write_xlsx(feedstock_biochar_summary, "processed_data/feedstock_biochar_summary.xlsx")
feedstock_biochar_means <- merge(feedstock_means, biochar_means, all = TRUE) 
write_xlsx(feedstock_biochar_means, "processed_data/feedstock_biochar_means.xlsx")
feedstock_biochar_summary_sd <- read_xlsx("processed_data/feedstock_biochar_summary_manual_sd.xlsx")
feedstock_biochar_summary_sd_dummy <- read_xlsx("processed_data/feedstock_biochar_summary_manual_sd_dummy.xlsx")

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
  # scale_fill_manual(values = c("#a6761d","#d95f02","#e6ab02", "#7570b3",
  #                              "#66a61e", "#e7298a","#1b9e77"),
  #                   breaks = c("WT","DSS-1","LSS",  "DSS-2", 
  #                              "GW","FWR","CWC")) +
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

test <- feedstock_biochar_summary %>% 
  filter(pollutant_class == "PCB",
         source != "wood")

cor <- test %>% 
  filter(temperature != "feedstock") %>% 
  select(c(temperature, sum_pollutant)) %>% 
  transform(temperature = as.numeric(temperature)) %>% 
  cor(use = "everything",
      method = "pearson")
cor

test2 <- feedstock_biochar_summary %>% 
  filter(temperature != "feedstock",
         pollutant_class == "PCB",
         feedstock == "DSS-1") 

res <- lm(as.numeric(temperature) ~ sum_pollutant,
          data = test2)
summary(res)
library(car)
avPlots(res)

GPO <- all_data %>% 
  filter(type %in% c("oil", "GFF", "PUF", "XAD"))

GPO_summary <- GPO %>% 
  group_by(sample_ID, pollutant_class, phase, unit, temperature, feedstock, V_gas_m3, V_gas_m3kg, V_gas_m3kg_propane) %>% 
  summarise(sum_pollutant = ifelse(type != "oil", sum(conc_blank_corr), as.numeric(conc_corr)),
            sum_TEQ = (sum(TEQ)))

EF_GPO_summary <- GPO_summary %>% 
  mutate(emission_conc = sum_pollutant/V_gas_m3,
         EF_tonne = emission_conc*V_gas_m3kg*1000,
         EF_tonne_prop = emission_conc*V_gas_m3kg_propane*1000,
         emission_conc_TEQ = sum_TEQ/V_gas_m3,
         EF_TEQ = emission_conc_TEQ*V_gas_m3kg*1000,
         EF_TEQ_tonne_prop = emission_conc*V_gas_m3kg_propane*1000)

GPO_PAH_total <- EF_GPO_summary %>% 
  filter(pollutant_class == "PAH",
         feedstock != "DWSS") %>% 
  #mutate(emission_conc = emission_conc/1000) %>% 
  ggplot(aes(x = sample_ID, y = emission_conc, fill = sample_ID, color = phase),
         fill = sample_ID) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  labs(x = "",
       y = TeX(r'($\Sigma PAH$-$16~(\mu g~m^{-3})$)'),
       fill = "Feedstock") +
  facet_zoom(ylim = c(0,100000)) +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.grid = element_blank())
GPO_PAH_total
ggsave("figs/GP/G_PAH_total.jpeg")

# PAH distribution BC/oil/gas ----
PAH_summary <- summary %>% 
  filter(pollutant_class == "PAH",
         phase != "feedstock") %>% 
  pivot_wider(names_from = phase,
              values_from = sum_pollutant)

PAH_summary_biochar <- PAH_summary %>% 
  drop_na(biochar) %>% 
  select(-c("oil", "gas", "particle"))

PAH_summary_oil <- PAH_summary %>% 
  drop_na(oil) %>% 
  select(-c("biochar", "gas", "particle"))

PAH_summary_gas <- PAH_summary %>% 
  drop_na(gas) %>% 
  select(-c("oil", "biochar", "particle"))

PAH_summary_particle <- PAH_summary %>% 
  drop_na(particle) %>% 
  select(-c("oil", "gas", "biochar"))

PAH_summary_pivot <- merge(PAH_summary_biochar, PAH_summary_oil, 
                           by = c("sample_ID_common", "temperature", "feedstock", 
                                  "yield_biochar", "yield_oil", "yield_gas"), all = TRUE)
PAH_summary_pivot <- merge(PAH_summary_pivot, PAH_summary_gas, 
                           by = c("sample_ID_common", "temperature", "feedstock", 
                                  "yield_biochar", "yield_oil", "yield_gas"), all = TRUE)
PAH_summary_pivot <- merge(PAH_summary_pivot, PAH_summary_particle, 
                           bby = c("sample_ID_common", "temperature", "feedstock", 
                                   "yield_biochar", "yield_oil", "yield_gas"), all = TRUE)

PAH_summary_pivot <- PAH_summary_pivot %>% 
  select(sample_ID_common, temperature, feedstock, biochar, oil, gas, particle, yield_biochar, yield_oil, yield_gas) %>% 
  drop_na(oil) %>% 
  filter(!(sample_ID_common %in% c("DSS-1-800", "CWC-750"))) %>% 
  replace(is.na(.),0) %>% 
  mutate(biochar_y = biochar*yield_biochar,
         oil_y = oil*yield_oil,
         gp_y = (gas+particle)*yield_gas,
         sum_PAH = biochar_y+oil_y+gp_y,
         perc_biochar = biochar_y/sum_PAH,
         perc_oil = oil_y/sum_PAH,
         perc_gp = gp_y/sum_PAH)

write_xlsx(PAH_summary_pivot, "processed_data/PAH_distribution.xlsx")

FB_dioxin_total <- feedstock_biochar_summary_dummy %>% 
  filter(pollutant_class == "dioxin",
         sample_ID != "LSS-F") %>% 
  ggplot(aes(x = reorder(sample_ID, temp_order), y = sum_TEQ, fill = sample_ID),
         fill = sample_ID) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(aes(ymin=sum_TEQ-sd_sum_TEQ, 
                    ymax=sum_TEQ+sd_sum_TEQ),
                width = 0.3,
                show.legend = F,
                position = position_dodge(.9)) +
  labs(x = "",
       y = TeX(r'($\Sigma PCDD{/}F$-$17~(ng~TEQ~kg^{-1})$)'),
       fill = "Feedstock") +
  facet_zoom(ylim = c(0,0.1)) +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID_BC) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.grid = element_blank())
FB_dioxin_total
