# Paket DataExplorer

library(DataExplorer)
library(gapminder)

introduce(gapminder)
plot_intro(gapminder)
plot_missing(gapminder)

create_report(gapminder)

# Optionen anpassen: Weniger Diagramme

config <- configure_report(
  add_plot_str = FALSE,
  add_plot_qq = FALSE,
  add_plot_prcomp = FALSE,
  add_plot_boxplot = FALSE,
  add_plot_scatterplot = FALSE,
  global_ggtheme = quote(theme_minimal(base_size = 14))
)

create_report(gapminder, config = config)

