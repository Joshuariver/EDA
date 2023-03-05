# How to Automate Exploratory Analysis Plots
# https://www.r-bloggers.com/2020/10/how-to-automate-exploratory-analysis-plots/


# Getting Started

# libraries
library(tidyverse) # include ggplot2, purrr and some others usefull packages
library(cowplot)
library(readxl)
library(ggsci) # nice palletes =)
library(forcats)

# data
hr_people_analytics_tbl <- read_xlsx("data/telco_train.xlsx")





# selecting character variables
hr_people_analytics_char_tbl <- hr_people_analytics_tbl %>% 
  select_if(is.character)

# setting a named vector
names <- names(hr_people_analytics_char_tbl)
names <- set_names(names)

scale_color <- pal_jco()(10)

hr_people_analytics_char_tbl %>% 
  count(Attrition) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = fct_reorder(Attrition, prop),
             y = prop,
             color = Attrition)) +
  scale_color_manual(values = scale_color) +
  geom_segment(aes(xend = Attrition, yend = 0),
               show.legend = F) +
  geom_point(aes(size = prop),
             show.legend = F) +
  geom_label(aes(label = prop, size = prop*10), 
             fill = "white", 
             hjust = "inward",
             show.legend = F) +
  labs(
    x = "Attrition"
  ) +
  coord_flip() +
  theme_minimal()

# second: plot_function
plot_frequency <- function(x) {
  
  scale_color <- pal_jco()(10)
  
  hr_people_analytics_char_tbl %>% 
    count(.data[[x]]) %>% 
    mutate(prop = n/sum(n)) %>% 
    ggplot(aes(x = forcats:fct_reorder(.data[[x]], prop),
               y = prop,
               color = .data[[x]])) +
    scale_color_manual(values = scale_color) +
    geom_segment(aes(xend = .data[[x]], yend = 0),
                 show.legend = F) +
    geom_point(aes(size = prop),
               show.legend = F) +
    geom_label(aes(label = prop, size = prop*10), 
               fill = "white", 
               hjust = "inward",
               show.legend = F) +
    labs(
      x = x
    ) +
    coord_flip() +
    theme_minimal()
}


all_plots <- map(names, plot_frequency)
cowplot::plot_grid(plotlist = all_plots)

