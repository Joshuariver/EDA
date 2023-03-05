# Diverging Lollipop Chart: A Visual Tool for Comparing Data with {healthyR}
# https://www.r-bloggers.com/2023/02/diverging-lollipop-chart-a-visual-tool-for-comparing-data-with-healthyr/

diverging_lollipop_plt(
  .data,
  .x_axis,
  .y_axis,
  .plot_title = NULL,
  .plot_subtitle = NULL,
  .plot_caption = NULL,
  .interactive = FALSE
)

library(healthyR)

suppressPackageStartupMessages(library(ggplot2))

data("mtcars")
mtcars$car_name <- rownames(mtcars)
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$car_name <- factor(mtcars$car_name, levels = mtcars$car_name)

# lollipop 비교 차트 만들기

diverging_lollipop_plt(
  .data = mtcars, 
  .x_axis = car_name,
  .y_axis = mpg_z
)

# interactive 차트 만들기

diverging_lollipop_plt(
  .data = mtcars, 
  .x_axis = car_name,
  .y_axis = mpg_z,
  .interactive = TRUE
)
