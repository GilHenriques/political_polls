library(tidyverse)

df <- read_csv(here::here("Portugal_parties_time/", "parties_over_time.csv"))
df_long <- df %>% rename(time = X1) %>% 
  pivot_longer(-time) %>% 
  mutate(value = if_else(is.na(value), 0, value)) %>% 
  group_by(time, name) %>% 
  summarize(n = sum(value)) %>% 
  mutate(percentage = n / sum(n, na.rm = T))

df_long$name <- factor(df_long$name,
                       levels = c("UDP", "BE", "MDP/CDE",
                                  "PCP", "PEV", "L", "UEDS", "PS",
                                  "ASDI", "PAN", "Ind", "PSN", "PRD",
                                  "PPD/PSD", "IL", "CDS/PP", "ADIM",
                                  "PPM", "CH"))

colors <- c("purple", "firebrick", "darkred", 
            "red2", "limegreen", "chartreuse", "brown2", "palevioletred2", 
            "gray1", "seagreen", "gray72", "yellow", "darkgreen", 
            "darkgoldenrod1", "turquoise2", "royalblue1", "mediumseagreen",
            "blue3", "midnightblue")


df_long %>% 
  ggplot(aes(x = time, y = percentage, fill = name)) +
  geom_col(width = 1) +
  scale_fill_manual(values = colors) +
  scale_x_continuous(expand = expansion(0, 0)) +
  scale_y_continuous(expand = expansion(0, 0),
                     labels = scales::percent_format()) +
  expand_limits(x = 2021) +
  xlab("Year") + ylab("Percentage of Members of Parliament") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.position = "none",
        panel.grid = element_blank()) +
  # UDP
  geom_curve(aes(xend = 1977.5, yend = 0.995, x = 1977, y = 0.95),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1977, y = 0.95, label = "UDP", fill = colors[1]) +
  #BE
  annotate("label", x = 2018, y = 0.96, label = "BE", fill = colors[2]) +
  # MDP/CDE
  geom_curve(aes(xend = 1985, yend = 0.985, x = 1984, y = 0.93),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1984, y = 0.93, label = "MDP/CDE", fill = colors[3]) +
  #PCP
  annotate("label", x = 1992, y = 0.97, label = "PCP", fill = colors[4]) +
  # PEV
  geom_curve(aes(xend = 1993, yend = 0.93, x = 1995, y = 0.90),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1995, y = 0.90, label = "PEV", fill = colors[5]) +
  # L
  geom_curve(aes(xend = 2019.25, yend = 0.86, x = 2018, y = 0.82),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 2018, y = 0.82, label = "L", fill = colors[6]) +
  # UEDS
  geom_curve(aes(xend = 1982, yend = 0.82, x = 1984, y = 0.78),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1984, y = 0.78, label = "UEDS", fill = colors[7]) +
  # PS
  annotate("label", x = 2000, y = 0.73, label = "PS", fill = colors[8]) +
  # ASDI
  geom_curve(aes(xend = 1979, yend = 0.45, x = 1977, y = 0.49),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1977, y = 0.49, label = "ASDI", fill = colors[9], color = "white") +
  # PAN
  geom_curve(aes(xend = 2016.5, yend = 0.47, x = 2018, y = 0.51),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 2018, y = 0.51, label = "PAN", fill = colors[10]) +
  # Ind
  geom_curve(aes(xend = 2019.7, yend = 0.37, x = 2018, y = 0.33),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 2018, y = 0.33, label = "Ind.", fill = colors[11]) +
  # PSN
  geom_curve(aes(xend = 1992.5, yend = 0.61, x = 1991, y = 0.56),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1991, y = 0.56, label = "PSN", fill = colors[12]) +
  # PRD
  annotate("label", x = 1985.5, y = 0.54, label = "PRD", fill = colors[13]) +
  # PPD/PSD
  annotate("label", x = 1996, y = 0.27, label = "PPD/PSD", fill = colors[14]) +
  # IL
  geom_curve(aes(xend = 2019.5, yend = 0.03, x = 2018.5, y = 0.09),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = -0.2, size = 0.05) +
  annotate("label", x = 2018.5, y = 0.09, label = "IL", fill = colors[15]) +
  # CDS-PP
  annotate("label", x = 2000, y = 0.03, label = "CDS-PP", fill = colors[16]) +
  #ADIM
  geom_curve(aes(xend = 1975.25, yend = 0.005, x = 1977.5, y = 0.04),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1977.5, y = 0.04, label = "ADIM", fill = colors[17]) + 
  # PPM
  geom_curve(aes(xend = 1981, yend = 0.025, x = 1983, y = 0.055),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = 0.2, size = 0.05) +
  annotate("label", x = 1983, y = 0.055, label = "PPM", fill = colors[18]) +
  # CH
  geom_curve(aes(xend = 2019.5, yend = 0.005, x = 2016, y = 0.03),
             arrow = arrow(length = unit(0.015, "npc")),
             curvature = -0.2, size = 0.05) +
  annotate("label", x = 2016, y = 0.03, label = "CH", color = "white", fill = colors[19]) 


ggsave("Portugal_parties_time/parties_over_time.pdf", width = 10, height = 7)
