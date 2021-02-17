# Ergebnisse berichten mit R
# Vorlage für Top 100 Songs pro Jahr 2000 - 2020
# Datenquelle: chart2000.com

library(tidyverse)
songyear <- read_csv(file = "chart2000-songyear-0-3-0062.csv",
                      na = c("", "-"))
attr(songyear, "spec") <- NULL

songyear %>% 
  arrange(desc(indicativerevenue)) %>% 
  head(n = 5) %>% 
  View()

top3 <- songyear %>% 
  group_by(artist) %>% 
  summarise(total = sum(indicativerevenue)) %>% 
  slice_max(n = 3, order_by = total) %>% 
  pull(artist)

top3 <- songyear %>% 
  filter(artist %in% top3)
