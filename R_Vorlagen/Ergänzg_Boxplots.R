library(tidyverse)
library(plotly)
library(ggthemes)
library(ggstatsplot)
library(EnvStats)
library(ggtext)

top5 <- readRDS("top5.rds")

theme_set(theme_solarized(base_size = 15))

theme_update(axis.text.x = element_text(angle = 90),
             axis.ticks.x = element_blank(),
             panel.grid.major.x = element_blank())

#### Einzelne Datenpunkte mit darstellen ####

ggplot(top5, aes(x = artist, y = indicativerevenue, color = artist)) +
  geom_boxplot(varwidth = TRUE, outlier.color = NA) +
  geom_jitter(alpha = 0.6, width = 0.2, height = 0) +
  labs(x = "", y = "Indicative Revenue",
       title = "Indicative Revenue by Artist",
       subtitle = "Artists sorted by number of songs in Top 100 per year",
       caption = "Source: Chart2000.com, Songs of the year, Version 0-3-0062") +
  scale_color_discrete(guide = NULL) +
  stat_n_text(y.pos = 900)


#### Infos zu weiterer Variable codieren ####

top5 %>% 
  rowwise() %>% 
  mutate(no1 = any(c_across(us:au) == 1, na.rm = TRUE)) %>% 
  ggplot(aes(x = artist, y = indicativerevenue)) +
  geom_boxplot(varwidth = TRUE, outlier.color = NA) +
  geom_jitter(alpha = 0.6, width = 0.2, height = 0,
              aes(shape = no1, color = no1)) +
  labs(x = "", y = "Indicative Revenue",
       title = "Indicative Revenue by Artist",
       subtitle = "Artists sorted by number of songs in Top 100 per year",
       caption = "Source: Chart2000.com, Songs of the year, Version 0-3-0062") +
  scale_color_brewer(palette = "Dark2", 
                     name = "No. 1\n(Any Country)?") +
  scale_shape_discrete(name = "No. 1\n(Any Country)?") +
  stat_n_text(y.pos = 900)


#### Mittelwerte einzeichnen zum Vergleich mit dem Median ####

ggplot(top5, aes(x = artist, y = indicativerevenue, color = artist)) +
  geom_boxplot(varwidth = TRUE) +
  # geom_jitter(alpha = 0.6, width = 0.2, height = 0) +
  stat_summary(fun = "mean", color = "black", shape = 8) +
  labs(x = "", y = "Indicative Revenue",
       title = "Indicative Revenue by Artist",
       subtitle = "Artists sorted by number of songs in Top 100 per year",
       caption = "* Mean\n\nSource: Chart2000.com, Songs of the year, Version 0-3-0062") +
  scale_color_discrete(guide = NULL) +
  stat_n_text(y.pos = 900)


#### Bilder als Achsenbeschriftungen ####

labels <- c(Rihanna = "<img src='Rihanna.png'
              width = '100' /><br>*Rihanna*",
            Pink = "<img src='Pink.jpg'
              width = '100' /><br>*Pink*",
            'Maroon 5' = "<img src='Maroon_5.jpg'
              width = '100' /><br>*Maroon 5*",
            'The Black Eyed Peas' = "<img src='Black_Eyed_Peas.jpeg'
              width = '100' /><br>*Black Eyed Peas*",
            'Ed Sheeran' = "<img src='Ed_Sheeran.jpg'
              width = '100' /><br>*Ed Sheeran*")

ggplot(top5, aes(x = artist, y = indicativerevenue, color = artist)) +
  geom_boxplot(varwidth = TRUE) +
  labs(x = "", y = "Indicative Revenue",
       title = "Indicative Revenue by Artist",
       subtitle = "Artists sorted by number of songs in Top 100 per year",
       caption = "Source: Chart2000.com, Songs of the year, Version 0-3-0062") +
  scale_color_discrete(guide = NULL) +
  scale_x_discrete(name = NULL, labels = labels) +
  stat_n_text(y.pos = 900) +
  theme_solarized(base_size = 14) +
  theme(axis.text.x = element_markdown(color = "black", angle = 0))
