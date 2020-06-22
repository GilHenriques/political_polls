library(tidyverse)
library(lubridate)

link <- "https://filipvanlaenen.github.io/eopaod/pt-N.csv" # Constantly updated database of national polls
polls <- read_csv(link) %>% 
  select(-c(`Polling Firm`, Commissioners, `Fieldwork Start`, `Scope`, `Sample Size Qualification`,
            `Participation`, `Precision`)) %>% 
  pivot_longer(cols = -c(`Fieldwork End`, `Sample Size`)) %>% 
  filter(value != "Not Available") %>% 
  separate(value, into = c("Percent", NA), sep = "%") %>% 
  mutate(Percent = as.numeric(Percent)) %>% 
  janitor::clean_names()

polls$name <- as.factor(polls$name)
levels(polls$name) <- c("A", "BE", "CDS", "CH", "CDU", "IL",
                        "L", "Other", "PSD", "PS", "PAN")

polls <- polls %>% 
  filter(! (name %in% c("Other", "A"))) 

polls$name <- factor(polls$name) # remove unused factor levels
levels(polls$name) <- c("BE", "CDS", "CH",  "CDU", "IL",  "L",   "PSD", "PS",  "PAN")

colors <- c("brown4", "dodgerblue2", "darkorchid4",
            "red2", "deepskyblue", "green3",
            "orange", "indianred1",   "mediumseagreen")

all_combinations <- expand(polls, fieldwork_end, name)

polls %>% 
  right_join(all_combinations) %>% 
  mutate(time = floor_date(fieldwork_end, "month"),
         month = month(time), year = year(time),
         label = paste(month, year, sep = "-")) %>% 
  group_by(time, name) %>% 
  summarise(percent = weighted.mean(percent, sample_size, na.rm = T), label = unique(label)) %>% 
  ungroup() %>% 
  mutate(percent = if_else(is.na(percent), 0, percent)) %>% 
  group_by(time) %>% 
  arrange(time, -percent) %>% # for each date, assign a rank
  mutate(rank = 1:n()) %>% 
  ungroup() %>% 
  mutate(rank = if_else(percent == 0, 99L, rank)) -> ranked_by_date

ranked_by_date %>% 
  filter(rank <= 7) %>% 
  ggplot(aes(xmin = 0, xmax = percent/100,
             ymin = rank - 0.45,
             ymax = rank + 0.45,
             y = rank,
             fill = name,
             group = name)) +
  geom_rect(alpha = 0.7) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(-0.03, 0.45), breaks = seq(0, 0.4, by = 0.1), 
                     labels = scales::percent_format(accuracy = 1)) +
  scale_y_reverse() +
  geom_text(hjust = "right", aes(label = name, color = name), x = -0.01) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  geom_text(x = 0.35, y = -7,
            aes(label = label),
            size = 10, color = "darkgray") +
  gganimate::transition_time(time)


gganimate::anim_save(filename = "Portugal_legislative/barchart_race.gif")
