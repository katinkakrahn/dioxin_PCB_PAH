# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)
library(gridExtra)

# data ----
feedstock_biochar_summary <- merge(feedstock_summary, biochar_summary, all = TRUE)
write_xlsx(feedstock_biochar_summary, "processed_data/feedstock_biochar_summary.xlsx")
feedstock_biochar_means <- merge(feedstock_means, biochar_means, all = TRUE) 
write_xlsx(feedstock_biochar_means, "processed_data/feedstock_biochar_means.xlsx")
feedstock_biochar_summary_dummy <- read_xlsx("processed_data/feedstock_biochar_summary_dummy.xlsx")

# plotting parameters ----
breaks_ID <- c("CWC-F", "CWC","CWC-530" , "CWC-600" , "CWC-700" , "CWC-750" , 
               "WT-F", "WT","WT-500" , "WT-600" , "WT-700",  "WT-800", 
               "GW-F", "GW","GW-500", "GW-600",  "GW-800", 
               "DSS-1-F", "DSS-1","DSS-1-500","DSS-1-600" ,"DSS-1-700", "DSS-1-770", 
               "DSS-2-F", "DSS-2","DSS-2-500", "DSS-2-600" ,"DSS-2-700", "DSS-2-800" ,
               "LSS-F","LSS","LSS-600" , "LSS-760" ,
               "FWR-F", "FWR","FWR-600","FWR-800")

values_ID <- c('#8c510a','#8c510a','#8c510aD9','#8c510aB3','#8c510a8C','#8c510a66',
               '#d8b365','#d8b365','#d8b365D9','#d8b365B3','#d8b3658C','#d8b36566',
               '#a1d76a','#a1d76a','#a1d76aD9','#a1d76aB3',            '#a1d76a66',
               '#2166ac','#2166ac','#2166acE6','#2166acBF','#2166ac80','#2166ac40',
               '#5ab4ac','#5ab4ac','#5ab4acD9','#5ab4acB3','#5ab4ac8C','#5ab4ac66',
               '#1b7837','#1b7837',            '#1b7837B3',            '#1b783766',
               '#762a83','#762a83',            '#762a83B3',            '#762a8366')


order <- c("CWC-F", "CWC","CWC-BC-530" , "CWC-BC-600" , "CWC-BC-700" , "CWC-BC-750" , 
           "WT-F", "WT","WT-BC-500" , "WT-BC-600" , "WT-BC-700",  "WT-BC-800", 
           "GW-F", "GW","GW-BC-500", "GW-BC-600",  "GW-BC-800", 
           "DSS-1-F", "DSS-BC-1","DSS-BC-1-BC-500","DSS-BC-1-BC-600" ,"DSS-BC-1-BC-700", "DSS-BC-1-BC-770", 
           "DSS-2-F", "DSS-BC-2","DSS-BC-2-BC-500", "DSS-BC-2-BC-600" ,"DSS-BC-2-BC-700", "DSS-BC-2-BC-800" ,
           "LSS-F","LSS","LSS-BC-600" , "LSS-BC-760" ,
           "FWR-F", "FWR","FWR-BC-600","FWR-BC-800")

feedstock_order <- c("CWC", "GW", "WT", "DSS-1", "DSS-2", "LSS", "FWR")
temperature_order <- c("F", "500", "530","600", "700", "750","760","770", "800")

# plots ----
#PAH
feedstock_biochar_summary_dummy %>% 
  filter(pollutant_class == "PAH",
         feedstock != "DWSS") %>% 
  mutate(temperature = factor(temperature, 
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>% 
  ggplot(aes(x = feedstock, y = sum_pollutant, 
             group = temperature, 
             fill = sample_ID_common)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(aes(ymin=sum_pollutant-sd_sum_pollutant, 
                    ymax=sum_pollutant+sd_sum_pollutant),
                width = 0.3,
                show.legend = F,
                position = position_dodge(.9)) +
  geom_text(
    aes(
      label = temperature,
      y = sum_pollutant+sd_sum_pollutant + 0.2
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  labs(x = "",
       y = TeX(r'($\Sigma PAH$-$16~(mg~kg^{-1})$)'),
       fill = "Feedstock") +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  coord_cartesian(ylim=c(0, 40)) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10)) +
  guides(fill = guide_legend(ncol = 7, bycol = TRUE))
ggsave("figs/FB/FB_PAH_total.jpeg")


# Dioxin
feedstock_biochar_summary_dummy %>% 
  filter(pollutant_class == "dioxin",
         sample_ID != "LSS-F") %>% 
  mutate(temperature = factor(temperature, 
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>% 
  ggplot(aes(x = feedstock, y = sum_TEQ, 
             group = temperature, 
             fill = sample_ID_common)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(aes(ymin=sum_TEQ-sd_sum_TEQ, 
                    ymax=sum_TEQ+sd_sum_TEQ),
                width = 0.3,
                show.legend = F,
                position = position_dodge(.9)) +
  labs(x = "",
       y = TeX(r'($\Sigma PCDD{/}PCDF$-$17~(pg~TEQ~g^{-1})$)'),
       fill = "Pyrolysis temperature (\u00B0C)") +
  facet_zoom(ylim = c(0,0.1)) +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))
ggsave("figs/FB/FB_dioxin_total.jpeg")

# PCB
feedstock_biochar_summary_dummy %>% 
  filter(pollutant_class == "PCB",
         sample_ID != "FWR-F") %>% 
  mutate(temperature = factor(temperature, 
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>% 
  ggplot(aes(x = feedstock, y = sum_pollutant, 
             group = temperature, 
             fill = sample_ID_common)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(aes(ymin=sum_pollutant-sd_sum_pollutant, 
                    ymax=sum_pollutant+sd_sum_pollutant),
                width = 0.3,
                show.legend = F,
                position = position_dodge(.9)) +
  labs(x = "",
       y = TeX(r'($\Sigma PCB$-$7~(\mu g~kg^{-1})$)'),
       fill = "Feedstock") +
  facet_zoom(ylim = c(0,2)) +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))
ggsave("figs/FB/FB_PCB_total.jpeg")

# PAH Emission concentration
EF_total_summary %>% 
  filter(pollutant_class == "PAH",
         feedstock != "DWSS") %>% 
  mutate(EC = EC/1000,
         temperature = factor(temperature, 
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>% 
  ggplot(aes(x = feedstock, y = EC, 
             group = temperature,
             fill = sample_ID)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  labs(x = "",
       y = expression(paste(Sigma,"PAH-16 (", mu, "g m"^-3*")")),
       fill = "Feedstock") +
  facet_zoom(ylim = c(0,30)) +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank()) +
  guides(fill = guide_legend(ncol = 7, bycol = TRUE))
ggsave("figs/GP/G_PAH_total.jpeg")

# Dioxin Emission concentration ----
EF_total_summary %>% 
  filter(pollutant_class == "dioxin",
         feedstock != "DWSS") %>% 
  mutate(EC = EC/1000,
         temperature = factor(temperature, 
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>% 
  ggplot(aes(x = feedstock, y = EC, 
             group = temperature,
             fill = sample_ID)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  labs(x = "",
       y = expression(paste(Sigma,"PCDD/F-17 (pg TEQ m"^-3*")")),
       fill = "Feedstock") +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())
ggsave("figs/GP/G_dioxin_total.jpeg")


# Oil ----
breaks_ID_oil_PAH <- c("CWC-F", "CWC","CWC-530-Oil" , "CWC-600-Oil" , "CWC-700-Oil" , "CWC-750-Oil" , 
                       "DSS-1-F", "DSS-1","DSS-1-600-Oil" ,"DSS-1-700-Oil", "DSS-1-770-Oil")

values_ID_oil_PAH <- c('#8c510a','#8c510a','#8c510aB3','#8c510a99','#8c510a66','#8c510a4D',
                       '#2166ac','#2166ac','#2166acB3','#2166ac99','#2166ac66','#2166ac4D')

oil_PAH <- oil_summary %>%
  filter(pollutant_class == "PAH") %>% 
  ggplot(aes(x = feedstock, y = sum_pollutant, fill = sample_ID)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "", y = expression(paste(Sigma, "PAH-16 (mg kg"^-1 * ")")), fill = "") +
  scale_fill_manual(values = values_ID_oil_PAH, breaks = breaks_ID_oil_PAH) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    panel.grid = element_blank(),
    legend.margin = margin(-10, 0, 0, 0),
    legend.box.margin = margin(-10, -10, 0, -10),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.key.size = unit(0.5, "cm")) +
  guides(fill = guide_legend(ncol = 2, bycol = TRUE))
oil_PAH
ggsave("figs/GP/O_PAH_total.jpeg")


# dioxin ----

breaks_ID_oil_dioxin <- c("DSS-1-600-Oil","DSS-2-600-Oil","FWR-800-Oil")

values_ID_oil_dioxin <- c('#2166ac', '#5ab4ac', '#762a83')


oil_dioxin <- oil_summary %>%
  filter(pollutant_class == "dioxin") %>% 
  ggplot(aes(x = sample_ID, 
             y = sum_TEQ,
             fill = sample_ID)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  labs(x = "",
       y = expression(paste(Sigma,"PCDD/F-17 (ng TEQ kg"^-1*")")),
       fill = "") +
  scale_fill_manual(values = values_ID_oil_dioxin,
                    breaks = breaks_ID_oil_dioxin) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "",
        text = element_text(size = 12),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  guides(fill = guide_legend(ncol = 3, bycol = TRUE))
ggsave("figs/GP/O_dioxin_total.jpeg")

# PCB ----

breaks_ID_oil_PCB <- c("DSS-1-600-Oil","DSS-2-600-Oil", "LSS-600-Oil")

values_ID_oil_PCB <- c('#2166ac','#5ab4ac','#1b7837')

oil_PCB <- oil_summary %>%
  filter(pollutant_class == "PCB") %>% 
  ggplot(aes(x = sample_ID, 
             y = sum_pollutant,
             fill = sample_ID)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  labs(x = "",
       y = TeX(r'($\Sigma PCB$-$7~(\mu g~kg^{-1})$)'),
       fill = "") +
  scale_fill_manual(values = values_ID_oil_PCB,
                    breaks = breaks_ID_oil_PCB) +
  # scale_color_manual(values = c("black", "grey"),
  #                    breaks = c("much tar", "very low tar")) +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "",
        text = element_text(size = 12),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  guides(fill = guide_legend(ncol = 1, bycol = TRUE))
ggsave("figs/GP/O_PCB_total.jpeg")

combined_oil_plot <- grid.arrange(oil_PAH, oil_dioxin, oil_PCB, nrow = 1)
ggsave(combined_oil_plot, "figs/GP/oil_total.jpeg")

# facets pollutants ----
# FB individual pollutants percent ---- 
FB_perc_comp <- merge(feedstock_percent_total, biochar_percent_total, all = TRUE) %>% 
  replace_na(list(yield = 1)) %>% 
  mutate(perc_comp_yield = perc_comp*yield/100)

design_biochar_PCB <- "
ABCDE
FGHIJ
KLM##
"

FB_perc_comp %>% 
  filter(pollutant_class == "PCB") %>%
  filter(sample_ID != "FWR-F") %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-770",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-760"))) %>% 
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
ggsave("figs/FB/FB_PCB_perc_comp.jpg")

design_biochar_dioxin <- "
ABCDE
FGHIJ
KLM##
"

FB_perc_comp %>% 
  filter(pollutant_class == "dioxin",
         feedstock != "LSS") %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-770",
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
ggsave("figs/FB/FB_dioxin_perc_comp.jpg")

design_FB_PAH_sludge <- "
ABCDE
FGHIJ
KLM##
NOP##
"

FB_perc_comp %>% 
  filter(pollutant_class == "PAH",
         source %in% c("sludge", "reject")) %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-770",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-760",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-530", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
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
ggsave("figs/FB/biochar_PAH_sludge.jpg")

design_FB_PAH_wood <- "
QRST#
VWXYZ
12345
"

FB_perc_comp %>% 
  filter(pollutant_class == "PAH",
         source == "wood") %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-770",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-760",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-530", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
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
ggsave("figs/FB/biochar_PAH_wood.jpg")

# FB individual pollutants concentration ----
FB_mean_conc <- merge(feedstock_means, biochar_means, all = TRUE)

design_FB_PAH_sludge <- "
ABCDE
FGHIJ
KLM##
NOP##
"

FB_mean_conc %>% 
  filter(pollutant_class == "PAH",
         source %in% c("sludge", "reject")) %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-770",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-750",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-530", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
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
ggsave("figs/FB/biochar_PAH_sludge_conc.jpg")

design_FB_PAH_wood <- "
QRST#
VWXYZ
12345
"

FB_mean_conc %>% 
  filter(pollutant_class == "PAH",
         source == "wood") %>% 
  filter(sample_ID != "DWSS-F") %>% 
  group_by(feedstock) %>% 
  mutate(sample_ID = factor(sample_ID,
                            levels = c("DSS-1-F", "DSS-1-BC-500", "DSS-1-BC-600", "DSS-1-BC-700", "DSS-1-BC-770",
                                       "DSS-2-F", "DSS-2-BC-500", "DSS-2-BC-600", "DSS-2-BC-700", "DSS-2-BC-800",
                                       "LSS-F", "LSS-BC-600", "LSS-BC-760",
                                       "FWR-F", "FWR-BC-600", "FWR-BC-800",
                                       "WT-F", "WT-BC-500", "WT-BC-600", "WT-BC-700", "WT-BC-800",
                                       "GW-F", "GW-BC-500", "GW-BC-600","GW-BC-800",
                                       "CWC-F", "CWC-BC-530", "CWC-BC-600","CWC-BC-700","CWC-BC-750"))) %>% 
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
ggsave("figs/FB/biochar_PAH_wood_conc.jpeg")

# gas ----
EF_total_summary %>% 
  filter(pollutant_class == "PAH",
         feedstock != "DWSS") %>% 
  mutate(EC = EC/1000,
         temperature = factor(temperature,
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>%
  ggplot(aes(x = feedstock, y = EC, 
             group = temperature,
             fill = sample_ID)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  coord_cartesian(ylim=c(0, 30)) +
  labs(x = "",
       y = expression(paste(Sigma,"PAH-16 (", mu, "g m"^-3*")")),
       fill = "Feedstock") +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 7, bycol = TRUE))
ggsave("figs/GP/G_PAH_total.jpeg")

EF_total_summary %>% 
  filter(pollutant_class == "dioxin",
         feedstock != "DWSS") %>% 
  mutate(EC = EC/1000,
         temperature = factor(temperature,
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>%
  ggplot(aes(x = feedstock, y = EC, 
             group = temperature,
             fill = sample_ID)) +
  geom_bar(position=position_dodge(), 
           stat="identity") +
  labs(x = "",
       y = expression(paste(Sigma,"PCDD/F-17 (pg TEQ m"^-3*")")),
       fill = "Feedstock") +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 14),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 14))
ggsave("figs/GP/G_dioxin_total.jpeg")
