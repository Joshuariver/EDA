# Learn EDA functions
# https://github.com/bayesball/LearnEDAfunctions


# Installation

library(remotes)
install_github("bayesball/LearnEDAfunctions")

# Loading

library(LearnEDAfunctions)
library(aplpack)
library(vcd)


# Fits a Gaussian curve to binned data.

data <- rt(200,df=5)
bins <- pretty(range(data))
g.mean <- 0
g.sd <- 1
fit.gaussian(data, bins, g.mean, g.sd)


# Half-slope ratio

sx <- c(10,30,50)
sy <- c(5,8,20)
half.slope.ratio(sx, sy, 1, 1)
half.slope.ratio(sx, sy, -0.5, -0.5)


# han.R()function Hanning a sequence.

plot(WWWusage)
plot(smooth(WWWusage, kind="3RSS"))
plot(han(smooth(WWWusage, kind="3RSS")))


# hinkley() function Hinkley's quick method.

raw <- state.x77[, "Population"]
hinkley(raw)
logs <- log(raw)
hinkley(logs)


# lval_plus() function Finds outliers by group using Tukey's rule

lval_plus(beatles, time, album)


# lval() function letter values

lval(rnorm(100))


# mtrans() function

raw <- state.x77[,"Population"]
matched.roots <- mtrans(raw,0.5)
matched.logs <- mtrans(raw,0)
boxplot(data.frame(raw, matched.roots,
                   matched.logs))


# plot2way() function Plot of an additive fit.

temps <- matrix(data=c(50, 30, 35, 21, 38,
                       73, 58, 65, 57, 63,
                       88, 83, 89, 84, 86,
                       73, 62, 68, 59, 66),
                nrow=5,ncol=4,
                dimnames=list(c("Atlanta",
                                "Detroit", "Kansas City",
                                "Minneapolis", "Philadelphia"),
                              c("January", "April",
                                "July", "October")))
fit <- medpolish(temps)
plot2way(fit$row + fit$overall, fit$col,
         dimnames(temps)[[1]],
         dimnames(temps)[[2]])


# power.t() function Power transformation.

power.t(c(3, 6, 5, 4, 7), 0.5)


# rline() function Computation of a resistant line.

df <- data.frame(x = 1:10,
                 y = 3 * (1:10) + rnorm(10))
rline(y ~ x, df, iter=5)



# spread_level_plot() function Spread versus level plot.

spread_level_plot(beatles, time, album)


# symplot() function  Symmetry plot.

symplot(rnorm(100))
# symmetry plot for exponential data
symplot(rexp(100))

