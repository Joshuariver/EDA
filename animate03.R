rm(list=ls())
setwd("~/R/EDA/EDA")

# Indian general election example

library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)

# Read Data
df = read.table(text = 
                  " Year	Perc_Seats	Party
                1984	0.79	INC
                1989	0.38	INC
                1991	0.45	INC
                1996	0.27	INC
                1998	0.27	INC
                1999	0.22	INC
                2004	0.28	INC
                2009	0.4   INC
                2014	0.09	INC
                2019	0.1	  INC
                1984	0	    BJP
                1989	0.17	BJP
                1991	0.23	BJP
                1996	0.31	BJP
                1998	0.35	BJP
                1999	0.35	BJP
                2004	0.27	BJP
                2009	0.23	BJP
                2014	0.52	BJP
                2019	0.56	BJP
                ", header=TRUE)

# Set Theme
theme_set(theme_minimal())

# Plot and animate
p =  
  ggplot(data = df, aes(x= factor(Year), y=Perc_Seats, group=Party, colour=Party)) +
  geom_line(size=2, show.legend = FALSE) +
  scale_color_manual(values=c("#ff9933", "#006400")) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = 'Lok Sabha Election : % of seats won', 
       x = NULL, y = NULL) +
  geom_text(aes(label=scales::percent(Perc_Seats, accuracy = 1),
                vjust= -2), show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_dl(aes(label=Party), method="last.points") +
  transition_reveal(Year) +
  coord_cartesian(clip = 'off') + 
  ease_aes('cubic-in-out')

animate(p, fps = 10, width = 800, height = 400)
anim_save("election.gif", p)

# save in video file

animate(p, renderer = ffmpeg_renderer(), width = 800, height = 450)
anim_save("nations.mp4")
