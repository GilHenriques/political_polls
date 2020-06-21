# goal: make shiny app - menu for countries, for ideologies
# could also add additional axes (EU anti pro, general left-right)
# hover over points to see names and highlights other parties of same country?
#data = http://www.parlgov.org/data/table/view_party/

library(tidyverse)
df <- read_csv("EU_political_compass/view_party.csv")
df_el <- read_csv("EU_political_compass/view_election.csv")
df_el %>% glimpse()

european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Republic","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden")

df_el %>% 
  filter(country_name %in% european_union) %>% 
  group_by(country_name) %>% 
  filter(election_type == "parliament") %>% 
  filter(election_date == max(election_date),
         seats >= 1) %>% 
  select(country_name, party_name_short, party_name_english, vote_share) %>% 
  ungroup() %>% 
  left_join(df) %>% 
  mutate(family = case_when(
    family_name == "Communist/Socialist" ~ "Left",
    family_name == "Green/Ecologist" ~ "Green",
    family_name == "Right-wing" ~ "Far-right",
    family_name == "Special issue" ~ "Other",
    family_name == "Agrarian" ~ "Other",
    family_name == "no family" ~ "Other",
    family_name == "to be coded" ~ "Other",
    TRUE ~ family_name
  )) %>% 
  mutate(family = factor(family)) %>% 
  mutate(family = fct_reorder(family, left_right, mean)) %>% 
  mutate_at(c("state_market", "liberty_authority"), ~(scale(.) %>% as.vector)) -> df_plot

color_scale <- c("firebrick", "green3", "pink", "darkgoldenrod1", "skyblue", "blue", "black", "white")

p1 <- df_plot %>% 
  ggplot(aes(x = state_market, y = liberty_authority, fill = family, size = vote_share)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 0, color = "darkgrey") +
  geom_point(shape = 21, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = color_scale) +
  scale_color_manual(values = color_scale) +
  scale_size_continuous(guide = F) +
  #ggrepel::geom_text_repel(aes(label = party_name_short), size = 2.5, force = 1) +
  cowplot::theme_minimal_grid() +
  theme(plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(size = 10.5),
        plot.caption = element_text(size = 10)) +
  coord_fixed(ratio=1) +
  labs(x = "Economic dimension", y = "Cultural dimension", 
       title = "European Union political compass",
       subtitle = "Each point represents a political party from an EU member state with at least one MP.<br>
                  **Point size** is proportional to the party's vote share.<br>
                  **Point color** identifies the party's ideology (**<span style = 'color:firebrick;'>left-wing</span>**, 
                  **<span style = 'color:green3;'>green</span>**, **<span style = 'color:pink;'>social democracy</span>**,
                  **<span style = 'color:darkgoldenrod1;'>liberalism</span>**, **<span style = 'color:skyblue;'>Christian democracy</span>**,
                  **<span style = 'color:blue;'>conservatism</span>**, **<span style = 'color:black;'>far-right</span>**, or 
                  **<span style = 'color:lightgray;'>other</span>**).<br>
                  Quantities are measured in units of standard deviations from the mean.<br>") +
  NULL


p2 <- df_plot %>% 
  ggplot(aes(x = state_market, y = liberty_authority, fill = family, size = vote_share)) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = 0, color = "darkgrey") +
  geom_point(shape = 21, color = "black", show.legend = FALSE) +
  facet_wrap(~ country_name, ncol = 4) +
  scale_fill_manual(values = color_scale) +
  scale_color_manual(values = color_scale) +
  scale_size_continuous(guide = F) +
  ggrepel::geom_text_repel(data = filter(df_plot, !is.na(vote_share)), 
                           aes(label = party_name_short), 
                           size = 2, segment.size = 0.25) +
  cowplot::theme_minimal_grid() +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        strip.text = element_text(size = 11),
        plot.title.position = "plot",
        plot.caption = element_text(size = 10)) +
  coord_fixed(ratio=1) +
  labs(caption = "Data from Doring & Manow (ParlGov, 2019).\nVisualization by Gil Henriques (@_Gil_Henriques).") +
  NULL


p3 <- cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(1, 1.7))

ggsave(plot = p3, filename = "EU_political_compass/EU_compass.pdf", height = 19.5, width = 6.5)
