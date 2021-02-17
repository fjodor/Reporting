# Code von Thomas Lin Pedersen
# Daten aus dem Gapminder-Projekt (Hans Rosling - siehe Youtube-Videos)
# https://github.com/thomasp85/gganimate/wiki/Gapminder

library(gapminder)
library(tidyverse)
library(gganimate)
library(plotly)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year)

# Interaktives plotly-Diagramm

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
