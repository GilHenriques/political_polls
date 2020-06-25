library(tidyverse)
library(DescTools) # convert color to HSV
library(colorspace) # convert HSV to HEX
library(colortools)

colors <- read_csv("Portugal_party_colors/party_colors.csv")

hsv <- ColToHsv(colors$Color) %>% t %>% as.data.frame.matrix()
colors <- bind_cols(colors, hsv)
colors <- colors %>% mutate(Notes = if_else(is.na(X4), "none", X4)) %>% select(-X4) %>% drop_na()


# Plot all party colors as separate facets --------------------------------
(colors %>% 
  ggplot(aes(x = h, y = s)) +
  geom_point(color = colors$Color, size = 2.5) +
  coord_polar() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 1)) +
  facet_wrap(~Party) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank()) -> plot)



# Plot all party colors and means as a list -------------------------------
means <- colors %>% 
  group_by(Party, Notes) %>% 
  summarise(h = atan2(mean(sin(h*2*pi)), 
                      mean(cos(h*2*pi))),
            s = mean(s), v = mean(v)) %>% 
  mutate(h = if_else(h < 0, h + 2*pi, h),
         h = h/(2*pi)) %>% 
  filter(Notes != "exclude") %>% 
  mutate(Notes = case_when(
    Notes == "red" ~ "A",
    Notes == "blue" ~ "B",
    Notes == "black" ~ "B",
    Notes == "none" ~ "A"
  ))

means$mean_color <- means[, -c(1:2)] %>% 
  mutate(h = h*360) %>% 
  as.matrix() %>% HSV() %>% hex()

tmp <- means
colnames(tmp) <- c("Party", "Publication", "h", "s", "v", "Color")
tmp <- colors %>% bind_rows(tmp)
tmp$Facet <- if_else(tmp$Publication %in% c("A", "B"), "2_Mean", "1_Not_Mean")

tmp$Publication <- factor(tmp$Publication)
levels(tmp$Publication) <- c("Mean (1)", "Mean (2)", "DN", "Expresso", "JN", "J. Negócios", "J. Económico", 
                             "Observador", "Pitagórica", "Politico", "Público", "RTP",
                             "Sábado", "Sol", "Visão", "Wikipedia.com", "Wikipedia.pt")

tmp %>% 
  ggplot(aes(y = Party, x = Publication)) +
  geom_tile(fill = tmp$Color, height = .9) +
  facet_grid(~ Facet, scales = "free_x", space = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        strip.text = element_blank()) +
  geom_point(data = filter(tmp, Notes == "exclude"), label = "*", shape = 8) +
  xlab(NULL) + ylab(NULL)


# Plot all party mean colors  ---------------------------------------------
set.seed(2); tmp %>% 
  filter(Publication %in% c("Mean (1)", "Mean (2)")) %>%
  mutate(Party = case_when(Party == "BE" & Publication == "Mean (1)" ~ "BE (1)",
                           Party == "CDU" & Publication == "Mean (1)" ~ "CDU (1)",
                           Party == "BE" & Publication == "Mean (2)" ~ "BE (2)",
                           Party == "CDU" & Publication == "Mean (2)" ~ "CDU (2)",
                           TRUE ~ Party)) %>% 
  ggplot(aes(x = h, y = s)) +
  geom_point(data = colors, shape = 1, color = colors$Color, size = 3, alpha = 0.75) +
  ggrepel::geom_text_repel(aes(label = Party), 
                           point.padding = unit(0.5, 'lines')) +
  geom_point(fill = (filter(tmp, Publication %in% c("Mean (1)", "Mean (2)")))$Color, 
             color = "black", size = 7, shape = 21) +
  coord_polar() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())



