library(tidyverse)
library(DescTools) # convert color to HSV
colors <- read_csv("Portugal_party_colors/party_colors.csv")
hsv <- ColToHsv(colors$Color) %>% t %>% as.data.frame.matrix()
colors <- bind_cols(colors, hsv)
colors <- colors %>% drop_na()

colors %>% 
  ggplot(aes(x = h, y = s)) +
  geom_point(color = colors$Color, size = 2.5) +
  coord_polar() +
  facet_wrap(~Party) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())

