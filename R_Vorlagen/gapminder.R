# Code von Thomas Lin Pedersen
# Daten aus dem Gapminder-Projekt (Hans Rosling - siehe Youtube-Videos)
# https://github.com/thomasp85/gganimate/wiki/Gapminder

library(gapminder)
library(tidyverse)
library(gganimate)
library(plotly)
library(skimr)
library(gghighlight)

data(gapminder)
str(gapminder)

# Wie viele Länder?

gapminder %>% 
  summarise(Anzahl_Länder = length(unique(country)))

# Base R Übersicht

summary(gapminder)

# Paket skimr

skimr::skim(gapminder)

# ggplot-Diagramme

# Entwicklung der Lebenserwartung im Zeitverlauf: Beispiel Deutschland

gapminder %>% 
  filter(country == "Germany") %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  geom_point() +
  labs(title = "Germany", subtitle = "Life Expectancy by Year",
       caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan")

# Europa

gapminder %>% 
  filter(continent == "Europe") %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  labs(title = "Europe", subtitle = "Life Expectancy by Year",
       caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan") +
  scale_color_discrete(name = "")

# Deutschland und Türkei hervorgehoben

gapminder %>% 
  filter(continent == "Europe") %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line(size = 1.5) +
  gghighlight::gghighlight(country %in% c("Germany", "Turkey"),
              unhighlighted_params = list(size = .75),
              use_group_by = FALSE) +
  labs(title = "Europe", subtitle = "Life Expectancy by Year",
       caption = "Source: Gapminder project /
       gapminder R package by Jenny Bryan") +
  scale_color_discrete(name = "")

# Land mit höchster und niedrigster Lebenserwartung hervorgehoben

highlow <- gapminder %>% 
  filter(continent == "Europe") %>% 
  filter(lifeExp == max(lifeExp) | lifeExp == min(lifeExp)) %>% 
  pull(country)

highlow <- gapminder %>% 
  filter(country %in% highlow)

gapminder %>% 
  filter(continent == "Europe") %>% 
  ggplot(aes(x = year, y = lifeExp, group = country)) +
  geom_line(size = 0.75, color = "grey") +
  geom_line(data = highlow, aes(color = country), size = 1.5) +
  labs(title = "Europe", subtitle = "Life Expectancy by Year",
       caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan") +
  scale_color_discrete(name = "") +
  theme(legend.position = c(0.9, 0.15))

# Mehr Infos: Facets

highlow <- gapminder %>% 
  group_by(continent) %>% 
  filter(lifeExp == max(lifeExp) | lifeExp == min(lifeExp)) %>% 
  pull(country)

gapminder <- gapminder %>% 
  mutate(highlight = ifelse(country %in% highlow, 1, 0))

gapminder %>% 
  filter(highlight == 1) %>% 
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  gghighlight(TRUE, use_group_by = FALSE) +
  facet_wrap(~ continent) +
  labs(title = "Life Expectancy by Year", subtitle = "Facets by continent",
       caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan") +
  scale_color_discrete(guide = NULL)

# Mouse-Over

library(plotly)

p <- gapminder %>% 
  ggplot(aes(x = year, y = lifeExp, group = country, color = factor(highlight))) +
  geom_line() +
  facet_wrap(~ continent) +
  labs(title = "Life Expectancy by Year", subtitle = "Facets by continent",
       caption = "Source: Gapminder project /\n gapminder R package by Jenny Bryan") +
  scale_color_manual(values = c("darkgrey", "steelblue")) +
  theme_bw() +
  theme(legend.position = "none")

ggplotly(p, tooltip = c("group", "x", "y"))

#### Interaktives plotly-Diagramm: 2007 ####

p <- gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  theme(legend.position = 'none') +
  labs(title = 'Year: 2007', x = 'GDP per capita', y = 'life expectancy')

ggplotly(p)

# Ideen für Modifikationen: 
# Andere Darstellungen, z. B. x = Jahre, y = Lebenserwartung

#### Animation mit gganimate ####

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year)

# Ideen für Modifikationen:
# Nur einen Kontinent darstellen, auf facets verzichten
