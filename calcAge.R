# calculating age in R with Lubridate package
# https://datacornering.com/how-to-calculate-age-in-r/

rm(list=ls())


birth_date <- as.Date("1966-01-17")
x_date   <- as.Date("2021-12-15")


require(lubridate)

trunc((birth_date %--% x_date) / years(1))


