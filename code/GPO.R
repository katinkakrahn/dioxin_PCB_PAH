# library ----
library(gridExtra)

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
        text = element_text(size = 14),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 14)) +
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
        text = element_text(size = 14),
        panel.grid = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 14))
ggsave("figs/GP/G_dioxin_total.jpeg")


# Oil ----
#
breaks_ID_oil_PAH <- c("CWC-F", "CWC","CWC-500-Oil" , "CWC-600-Oil" , "CWC-700-Oil" , "CWC-750-Oil" , 
                       "DSS-1-F", "DSS-1","DSS-1-600-Oil" ,"DSS-1-700-Oil", "DSS-1-800-Oil")

values_ID_oil_PAH <- c('#8c510a','#8c510a','#8c510aB3','#8c510a99','#8c510a66','#8c510a4D',
                       '#2166ac','#2166ac','#2166acB3','#2166ac99','#2166ac66','#2166ac4D')


oil_PAH <- data_summary_GP_together %>%
  filter(phase2 == "oil", pollutant_class == "PAH") %>% 
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
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.key.size = unit(0.5, "cm")) +
  guides(fill = guide_legend(ncol = 2, bycol = TRUE))
oil_PAH
ggsave("figs/GP/O_PAH_total.jpeg")


# dioxin ----

breaks_ID_oil_dioxin <- c("DSS-1-600-Oil","DSS-2-600-Oil","FWR-800-Oil")

values_ID_oil_dioxin <- c('#2166ac', '#5ab4ac', '#762a83')


oil_dioxin <- data_summary_GP_together %>%
  filter(phase2 == "oil",
         pollutant_class == "dioxin") %>% 
  ggplot(aes(x = sample_ID, 
             y = sum_pollutant,
             fill = sample_ID)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  labs(x = "",
       y = expression(paste(Sigma,"PCDD/F-17 (ng kg"^-1*")")),
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

oil_PCB <- data_summary_GP_together %>%
  filter(phase2 == "oil",
         pollutant_class == "PCB") %>% 
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
