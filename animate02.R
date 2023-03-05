rm(list=ls())
setwd("~/R/EDA/EDA")

set.seed(123)
dates = paste(rep(month.abb[1:10], each=10), 2018)
df = data.frame(Product=rep(sample(LETTERS[1:10],10), 10),
                Period=factor(dates, levels=unique(dates)),
                Sales=sample(1:100,100, replace = TRUE))

head(df)

# Ranking by Period and Sales
df = df %>% 
  arrange(Period, Sales) %>% 
  mutate(order = 1:n())

# Animation
p = df %>% 
  ggplot(aes(order, Sales)) +
  geom_bar(stat = "identity", fill = "#ff9933") +
  labs(title='Total Sales in {closest_state}', x=NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  scale_x_continuous(breaks=df$order, labels=df$Product, position = "top") +
  transition_states(Period, transition_length = 1, state_length = 2) +
  view_follow(fixed_y=TRUE) +
  ease_aes('cubic-in-out')

animate(p, nframes=50, fps=4)
anim_save("bar_animation.gif", p)
