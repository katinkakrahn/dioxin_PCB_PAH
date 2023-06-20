# FB individual pollutants percent ---- 
FB_perc_comp <- merge(feedstock_percent_total, biochar_percent_total, all = TRUE) %>% 
  replace_na(list(yield_biochar = 1)) %>% 
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
  # geom_text(aes(label=sprintf("%0.1f", round(perc_comp_yield, digits = 1))),
  #           hjust = -0.2) +
  facet_manual(vars(sample_ID),
               design = design_biochar_PCB) +
  labs(x = TeX(r'($\% \Sigma PCB$-$7$)'),
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
  # geom_text(aes(label=sprintf("%0.1f", round(perc_comp_yield, digits = 1))),
  #           hjust = -0.2) +
  facet_manual(vars(sample_ID),
               design = design_biochar_dioxin) +
  labs(x = TeX(r'($\% \Sigma 
               PCDD/F$-$17$)'),
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
