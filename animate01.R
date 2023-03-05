rm(list=ls())
setwd("~/R/EDA/EDA")

df = data.frame(A=sample(1:75, 50, replace=TRUE),
                B=sample(1:100, 50, replace=TRUE),
                stringsAsFactors = FALSE)

library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)
library(gifski)

p = ggplot(df, aes(A, B)) +
  geom_line() +
  transition_reveal(A) +
  labs(title = 'A: {frame_along}')


# p = ggplot(df, aes(A, B, group = C)) +
#   geom_line() +
#   transition_reveal(A) +
#  labs(title = 'A: {frame_along}')

animate(p, nframes=40)

anim_save("basic_animation.gif", p)

animate(p, nframes=40, fps = 2)


# how to stop loop in the animation?

animate(p, renderer = gifski_renderer(loop = FALSE))

# How to change layout of plot?

animate(p, fps = 10, duration = 14, width = 800, height = 400)



