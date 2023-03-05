rm(list=ls())

#First install devtools to allow you to install inspectdf from github
# install.packages("devtools")
library(devtools)

#install and load the package - https://github.com/alastairrushworth/inspectdf

# devtools::install_github("alastairrushworth/inspectdf")
library(inspectdf)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readr")
library(readr)

# categorial data discriptive analysis using Inspectdf package


library(inspectdf)
library(dplyr)

?starwars


# Tabular summaries using inspect_cat()

starwars %>% inspect_types()

# Using inspect_cat() is very straightforward:

star_cat <- starwars %>% inspect_cat()
star_cat

star_cat$levels$eye_color

# Visualising categorical columns with show_plot()

star_cat %>% show_plot()

# Combining rare entries with show_plot()

star_cat %>% show_plot(high_cardinality = 1)

# Playing with color options in show_plot()

star_cat %>% show_plot(col_palette = 1)

star_cat %>% show_plot(col_palette = 2)

star_cat %>% show_plot(col_palette = 3)

star_cat %>% show_plot(col_palette = 4)

star_cat %>% show_plot(col_palette = 5)

