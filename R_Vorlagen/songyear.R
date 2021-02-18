# Ergebnisse berichten mit R
# Vorlage für Top 100 Songs pro Jahr 2000 - 2020
# Datenquelle: chart2000.com

library(tidyverse)
library(plotly)
library(ggthemes)
library(ggstatsplot)
library(EnvStats)

#### Daten laden und aufbereiten ####

songyear <- read_csv(file = "chart2000-songyear-0-3-0062.csv",
                      na = c("", "-"))
attr(songyear, "spec") <- NULL

songyear %>% 
  arrange(desc(indicativerevenue)) %>% 
  head(n = 5)

top3 <- songyear %>% 
  group_by(artist) %>% 
  summarise(total = sum(indicativerevenue)) %>% 
  slice_max(n = 3, order_by = total) %>% 
  pull(artist)

top3 <- songyear %>% 
  filter(artist %in% top3)

top_artists <- songyear %>%
  group_by(artist) %>%
  summarise(total_score = sum(indicativerevenue)) %>% 
  arrange(desc(total_score)) %>% 
  head(n = 5) %>% 
  pull(artist)

top5 <- songyear %>% 
  filter(artist %in% top_artists) %>% 
  mutate(artist = fct_infreq(artist),
         indicativerevenue = round(indicativerevenue))

saveRDS(top5, "top5.rds")

rm(top_artists)


#### Streudiagramm der Top 3 Bands / Künstler ####

p <- ggplot(top3, aes(x = year, y = indicativerevenue, color = artist, shape = artist)) +
  geom_jitter(alpha = 0.8, size = 2) +
  labs(x = "Jahr", y = "Erlös (in 1.000 USD)",
       title = "Songs der Top 3 Bands / Künstler 2000 - 2020") +
  scale_color_brewer(palette = "Set1", name = "Band / Künstler") +
  scale_shape_discrete(name = "Band / Künstler") +
  theme_bw(base_size = 15)  

p

# Zwei Legenden zu einer verschmelzen:
# Beide greifen auf die gleiche Variable zu
# und beide haben den gleichen Titel ("Band / Künstler")

#### Top 3: Interaktives Diagramm mit Mouse-Over ####

p <- ggplot(top3, aes(x = year, y = indicativerevenue, 
                      color = artist, shape = artist, label = song)) +
  geom_jitter(alpha = 0.8, size = 2) +
  labs(x = "Jahr", y = "Erlös (in 1.000 USD)",
       title = "Songs der Top 3 Bands / Künstler 2000 - 2020") +
  scale_color_brewer(palette = "Set1", name = "Band / Künstler") +
  scale_shape_discrete(name = "Band / Künstler") +
  theme_bw(base_size = 15)  

# Ästhetik "label" für die Beschriftung der Songs in plotly

ggplotly(p, tooltip = c("x", "y", "shape", "label"))

#### ggplot2: Theme-Einstellungen ####

theme_set(theme_solarized(base_size = 15))

theme_update(axis.text.x = element_text(angle = 90),
             axis.ticks.x = element_blank(),
             panel.grid.major.x = element_blank())


#### Erstes Boxplot ####

ggplot(top5, aes(x = artist, y = indicativerevenue)) +
  geom_boxplot() +
  labs(x = "", y = "Indicative Revenue",
       title = "Indicative Revenue by Artist",
       subtitle = "Artists sorted by number of songs in Top 100 per year",
       caption = "Source: Chart2000.com, Songs of the year, Version 0-3-0062")

#### Boxplot mit Angabe der Fallzahlen: numerisch und visuell ####

ggplot(top5, aes(x = artist, y = indicativerevenue)) +
  geom_boxplot(varwidth = TRUE) +
  labs(x = "", y = "Indicative Revenue",
       title = "Indicative Revenue by Artist",
       subtitle = "Artists sorted by number of songs in Top 100 per year",
       caption = "Source: Chart2000.com, Songs of the year, Version 0-3-0062") +
  EnvStats::stat_n_text(y.pos = 900)


#### Ausreißer beschriften ####

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

top5 %>% 
  group_by(artist) %>% 
  mutate(outlier = ifelse(is_outlier(indicativerevenue), song, NA)) %>% 
  ggplot(aes(x = artist, y = indicativerevenue, color = artist)) +
  geom_boxplot(varwidth = TRUE) +
  geom_text(aes(label = outlier), na.rm = TRUE, nudge_y = 1500) +
  labs(x = "", y = "Indicative Revenue",
       title = "Indicative Revenue by Artist",
       subtitle = "Artists sorted by number of songs in Top 100 per year",
       caption = "Source: Chart2000.com, Songs of the year, Version 0-3-0062") +
  scale_color_discrete(guide = NULL)


#### Signifikanztest zwischen drei Künstlern: ggstatsplot ####

songs2 <- songyear %>% 
  filter(artist %in% c("Ed Sheeran", "Justin Timberlake", "Miley Cyrus"))

ggstatsplot::ggbetweenstats(
  data = songs2,
  x = artist, xlab = "",
  y = indicativerevenue,
  ylab = "Indicative Revenue",
  plot.type = "box",
  type = "p",
  conf.level = 0.95,
  title = "Indicative Revenue by Artist"
)
