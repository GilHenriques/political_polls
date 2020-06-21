library(tidyverse)
library(lubridate)
library(ggforce)

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
  filter(! (name %in% c("Other", "A"))) %>% 
  filter(! (name %in% c("L", "CH", "IL") & 
              fieldwork_end < as.POSIXct("2019-10-01")))  # remove early polls from new parties to avoid big error bars

polls$name <- factor(polls$name) # remove unused factor levels
levels(polls$name) # "BE"  "CDS" "CH"  "CDU" "IL"  "L"   "PSD" "PS"  "PAN"

colors <- c("brown4", "dodgerblue2", "darkorchid4",
            "red2", "deepskyblue", "green3",
            "orange", "indianred1",   "mediumseagreen")

dates <- tribble(~ date, ~ label, ~ percent, ~ size,
                 ymd("2019-10-6"), " Legislative\n Election", max(polls$percent)/100+0.025, 1,
                 ymd("2019-05-25"), " European\n Election", max(polls$percent)/100+0.025, 1,
                 ymd("2020-03-18"), " COVID\n Emergency", max(polls$percent)/100+0.025, 1)

polls %>% 
  ggplot(aes(x = fieldwork_end, y = percent/100, size = sample_size, weight = sample_size, color = name)) +
  geom_vline(xintercept = dates$date, color = "darkgray") +
  geom_point(alpha = 0.5) +
  scale_size(range = c(0.5, 2.5)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgray"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 12.5),
        strip.background = element_rect(fill = "gray93", color = "white")) +
  ylab(NULL) + xlab(NULL) +
  geom_smooth(aes(fill = name), method = "loess", alpha = 0.2, span = 0.35) + # smaller spans = more wiggly. 0.75 is default
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.05)), 
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, max(polls$percent)/100+0.05)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  guides(size = FALSE, color = FALSE, fill = FALSE) +
  directlabels::geom_dl(aes(label = glue::glue("   {name}")), method = "last.bumpup") +
  expand_limits(x = max(polls$fieldwork_end) %m+% months(1)) +
  geom_text(data = dates,
            aes(x = date, y = percent, label = label, size = size, weight = size), 
            color = "black", hjust = 0, cex = 3.5) +
  facet_zoom(xlim = c(dates$date[1], max(polls$fieldwork_end) %m+% days(10)))



