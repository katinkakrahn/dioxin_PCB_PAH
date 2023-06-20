# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)

# data ----
feedstock_biochar_summary <- merge(feedstock_summary, biochar_summary, all = TRUE)
write_xlsx(feedstock_biochar_summary, "processed_data/feedstock_biochar_summary.xlsx")
feedstock_biochar_means <- merge(feedstock_means, biochar_means, all = TRUE) 
write_xlsx(feedstock_biochar_means, "processed_data/feedstock_biochar_means.xlsx")
feedstock_biochar_summary_dummy <- read_xlsx("processed_data/feedstock_biochar_summary_dummy.xlsx")

# plotting parameters ----
breaks_ID_BC <- c("CWC-F", "CWC","CWC-BC-500" , "CWC-BC-600" , "CWC-BC-700" , "CWC-BC-750" , 
                  "WT-F", "WT","WT-BC-500" , "WT-BC-600" , "WT-BC-700",  "WT-BC-800", 
                  "GW-F", "GW","GW-BC-500", "GW-BC-600",  "GW-BC-800", 
                  "DSS-1-F", "DSS-1","DSS-1-BC-500","DSS-1-BC-600" ,"DSS-1-BC-700", "DSS-1-BC-800", 
                  "DSS-2-F", "DSS-2","DSS-2-BC-500", "DSS-2-BC-600" ,"DSS-2-BC-700", "DSS-2-BC-800" ,
                  "LSS-F","LSS","LSS-BC-600" , "LSS-BC-750" ,
                  "FWR-F", "FWR","FWR-BC-600","FWR-BC-800")

breaks_ID <- c("CWC-F", "CWC","CWC-500" , "CWC-600" , "CWC-700" , "CWC-750" , 
               "WT-F", "WT","WT-500" , "WT-600" , "WT-700",  "WT-800", 
               "GW-F", "GW","GW-500", "GW-600",  "GW-800", 
               "DSS-1-F", "DSS-1","DSS-1-500","DSS-1-600" ,"DSS-1-700", "DSS-1-800", 
               "DSS-2-F", "DSS-2","DSS-2-500", "DSS-2-600" ,"DSS-2-700", "DSS-2-800" ,
               "LSS-F","LSS","LSS-600" , "LSS-750" ,
               "FWR-F", "FWR","FWR-600","FWR-800")

values_ID <- c('#8c510a','#8c510a','#8c510aB3','#8c510a99','#8c510a66','#8c510a4D',
               '#d8b365','#d8b365','#d8b365B3','#d8b36599','#d8b36566','#d8b3654D',
               '#a1d76a','#a1d76a','#a1d76aB3','#a1d76a99',            '#a1d76a4D',
               '#2166ac','#2166ac','#2166acB3','#2166ac99','#2166ac66','#2166ac4D',
               '#5ab4ac','#5ab4ac','#5ab4acB3','#5ab4ac99','#5ab4ac66','#5ab4ac4D',
               '#1b7837','#1b7837',            '#1b783799',            '#1b78374D',
               '#762a83','#762a83',            '#762a8399',            '#762a834D')

values_ID <- c('#8c510a','#8c510a','#8c510aD9','#8c510aB3','#8c510a8C','#8c510a66',
               '#d8b365','#d8b365','#d8b365D9','#d8b365B3','#d8b3658C','#d8b36566',
               '#a1d76a','#a1d76a','#a1d76aD9','#a1d76aB3',            '#a1d76a66',
               '#2166ac','#2166ac','#2166acE6','#2166acBF','#2166ac80','#2166ac40',
               '#5ab4ac','#5ab4ac','#5ab4acD9','#5ab4acB3','#5ab4ac8C','#5ab4ac66',
               '#1b7837','#1b7837',            '#1b7837B3',            '#1b783766',
               '#762a83','#762a83',            '#762a83B3',            '#762a8366')

values_ID_gudny <- c('#8c510a','#8c510a','#8c510a66','#8c510a8C','#8c510aB3','#8c510aD9',
                     '#d8b365','#d8b365','#d8b36566','#d8b3658C','#d8b365B3','#d8b365D9',
                     '#f6e8c3','#f6e8c3','#f6e8c366','#f6e8c38C','#f6e8c3D9',
                     '#c7eae5','#c7eae5','#c7eae566','#c7eae58C','#c7eae5B3','#c7eae5D9',
                     '#5ab4ac','#5ab4ac','#5ab4ac66','#5ab4ac8C','#5ab4acB3','#5ab4acD9',
                     '#01665e','#01665e','#01665e8C','#01665eD9',
                     '#762a83','#762a83','#762a838C','#762a83D9')

order <- c("CWC-F", "CWC","CWC-BC-500" , "CWC-BC-600" , "CWC-BC-700" , "CWC-BC-750" , 
           "WT-F", "WT","WT-BC-500" , "WT-BC-600" , "WT-BC-700",  "WT-BC-800", 
           "GW-F", "GW","GW-BC-500", "GW-BC-600",  "GW-BC-800", 
           "DSS-1-F", "DSS-BC-1","DSS-BC-1-BC-500","DSS-BC-1-BC-600" ,"DSS-BC-1-BC-700", "DSS-BC-1-BC-800", 
           "DSS-2-F", "DSS-BC-2","DSS-BC-2-BC-500", "DSS-BC-2-BC-600" ,"DSS-BC-2-BC-700", "DSS-BC-2-BC-800" ,
           "LSS-F","LSS","LSS-BC-600" , "LSS-BC-750" ,
           "FWR-F", "FWR","FWR-BC-600","FWR-BC-800")

feedstock_order <- c("CWC", "GW", "WT", "DSS-1", "DSS-2", "LSS", "FWR")
temperature_order <- c("feedstock", "500", "600", "700", "800")

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
  labs(x = "",
       y = TeX(r'($\Sigma PAH$-$16~(mg~kg^{-1})$)'),
       fill = "Feedstock") +
  geom_hline(data = limits %>% 
               filter(pollutant_class == "PAH", 
                      quality == "EBC-Agro",
                      regulator == "EPA"
               ) %>% 
               select(limit, quality),
             mapping = aes(yintercept = as.numeric(limit),
                           linetype = quality),
             linewidth = 1,
             color = "red") +
  # geom_hline(yintercept = 14,
  #            linewidth = 1,
  #            color = "red") +
  #facet_zoom(ylim = c(0,40)) +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  scale_linetype_manual(name = "Limit", values = 2, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
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
ggsave("figs/FB/FB_PAH_total.jpeg")
ggsave("figs/FB/FB_PAH_total_EBC.jpeg")
ggsave("figs/FB/FB_PAH_total_Sormo.jpeg")

# PAH 8 EFSA 
FB_carc <- left_join(feedstock_biochar_means, carcinogens, all = TRUE)

FB_carc_summary <- FB_carc %>% 
  group_by(sample_ID,  sample_ID_common, pollutant_class, unit, temperature, feedstock, source, 
           type, yield_biochar, phase, class) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sd_sum_pollutant = sqrt(sum(sd_conc^2)),
            sum_TEQ = sum(mean_TEQ),
            sd_sum_TEQ = sqrt(sum(sd_TEQ^2))) %>% 
  filter(class == "EFSA")

write_xlsx(FB_carc_summary, "processed_data/FB_EFSA.xlsx")

FB_carc_summary %>% 
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
  labs(x = "",
       y = TeX(r'($\Sigma 8~EFSA~PAH~(mg~kg^{-1})$)'),
       fill = "Feedstock") +
  geom_hline(data = EBC %>% 
               filter(pollutant_class == "PAH", 
                      quality == "EBC-Agro",
                      type == "EFSA"
               ) %>% 
               select(limit, quality),
             mapping = aes(yintercept = as.numeric(limit),
                           linetype = quality),
             linewidth = 1,
             color = "red") +
  # geom_hline(yintercept = 14,
  #            linewidth = 1,
  #            color = "red") +
  facet_zoom(ylim = c(0,2)) +
  scale_fill_manual(values = values_ID,
                    breaks = breaks_ID,
                    name = "") +
  scale_linetype_manual(name = "Limit", values = 2, 
                        guide = guide_legend(override.aes = list(color = "red"))) +
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
ggsave("figs/FB/FB_PAH_EFSA.jpeg")


# Dioxin
feedstock_biochar_summary_dummy %>% 
  filter(pollutant_class == "dioxin",
         sample_ID != "LSS-F") %>% 
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
       y = TeX(r'($\Sigma PCDD{/}PCDF$-$17~(pg~TEQ~g^{-1})$)'),
       fill = "Pyrolysis temperature (\u00B0C)") +
  facet_zoom(ylim = c(0,15)) +
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
