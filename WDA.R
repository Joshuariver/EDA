# 30 Year Weather Data Analysis
# https://www.r-bloggers.com/2021/01/30-year-weather-data-analysis/



# Libraries
packages <- 
  c("data.table",
    "ggplot2",
    "stringr",
    "skimr",
    "janitor",
    "glue"
  )

if (length(setdiff(packages,rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

invisible(lapply(packages, library, character.only = TRUE))

knitr::opts_chunk$set(
  comment = NA,
  fig.width = 12,
  fig.height = 8,
  out.width = '100%',
  cache = TRUE
)


## Weather Data Extraction

# Years to retrieve
year <- c(1990:2022)
ac <- 
  lapply(
    glue::glue(
      'https://www3.epa.gov/cgi-bin/broker?_service=data&_program=dataprog.Daily.sas&check=void&polname=Ozone&debug=0&year={year}&site=09-001-0017'
    ),
    fread
  )

# Bind lists to data.table
ac_dt <- rbindlist(ac)

# Clean names
ac_dt <- janitor::clean_names(ac_dt)


## Exploration and Preparation

# Summarize data.table
skimr::skim(ac_dt)


# Drop unneeded cols
ac_dt <- ac_dt[, c(9:27)][, .SD, .SDcols = !patterns("tribe|nonreg")]

# Convert aqi to integer
ac_dt[, aqi := as.integer(str_extract(aqi, "\\d*"))]


# Separate out aqi
aqi <- ac_dt[!is.na(aqi)]

# Tidy key measures for parameters other than aqi
measures <- c("first_maximum_value", "arithmetic_mean")
ids <- setdiff(names(ac_dt), measures)
ids <- ids[!str_detect(ids, "aqi")]
ac_dt_tidy <-
  ac_dt[, 
        melt(.SD,
             idcols = ids,
             measure.vars = measures),
        .SDcols = !"aqi"]

# Tidy up aqi
aqi <- 
  aqi[, 
      melt(.SD,
           idcols = ids,
           measure.vars = "aqi"),
      .SDcols = !measures]

# Put two tidied data sets back together
ac_dt_tidy <- rbind(ac_dt_tidy, aqi)

# Show sample rows
ac_dt_tidy


# Look for missing periods
ac_dt_tidy[
  variable %in% c("arithmetic_mean", "first_maximum_value"), 
  ggplot(.SD,
         aes(date_local,
             y = value,
             color = variable)) +
    geom_line() +
    facet_wrap( ~ parameter_name, scale = 'free') +
    theme_bw() +
    labs(caption = "Source: EPA Monitor 09-001-0017"
    )]



## Wind is Lowest during the Summer

ac_dt_tidy[
  str_detect(parameter_name, "Wind") &
    variable %in% c("first_maximum_value", "arithmetic_mean"),
  .(avg_speed = mean(value)), by = .(month(date_local), parameter_name, variable)][,
                                                                                   ggplot(.SD, aes(month, avg_speed, color = variable)) +
                                                                                     geom_line() +
                                                                                     facet_wrap( ~ parameter_name, scales = "free_y") +
                                                                                     theme_bw() + 
                                                                                     labs(
                                                                                       x = "Month",
                                                                                       y = "Wind Speed",
                                                                                       caption = "Source: EPA Monitor 09-001-0017"
                                                                                     ) ]    


## Air Quality Has Improved

ac_dt_tidy[
  parameter_name == "Ozone" &
    variable %in% c("arithmetic_mean", "first_maximum_value")][
    ][,
      ggplot(.SD,
             aes(date_local,
                 y = value)) +
        geom_point(aes(color = cut(
          value,
          breaks = c(0, 0.064, 0.3),
          labels = c("Good", "Unhealthy")
        )),
        size = 1) +
        scale_color_manual(
          name = "Ozone",
          values = c("Good" = "green1",
                     "Unhealthy" = "red1")) +
        theme_bw() +
        labs(x = "Year",
             y = "Ozone",
             caption = "Source: EPA Monitor 09-001-0017") +
        facet_wrap(~ variable + duration_description, scales = "free_y")]    



## Temperature

ac_dt_tidy[
  parameter_name == "Outdoor Temperature" &
    variable %in% c("arithmetic_mean", "first_maximum_value")][
    ][,
      ggplot(.SD,
             aes(date_local,
                 y = value)) +
        geom_point(aes(
          color = cut(value, 
                      breaks = c(-20, 32, 50, 65, 85, 95, 120),
                      labels = c("Very Cold", "Cold", "Cool", "Moderate", "Hot", "Very Hot"))),
          size = 1) +
        scale_color_manual(
          name = "Outside Temperature",
          values = c(
            "Very Cold" = "blue",
            "Cold" = "yellow",
            "Cool" = "green1",
            "Moderate" = "green4",
            "Hot" = "orange",
            "Very Hot" = "red"
          )
        ) +
        theme_bw() + 
        labs(
          x = "Year",
          y = "Outside Temperature",
          caption = "Source: EPA Monitor 09-001-0017"
        ) +
        facet_wrap(~ variable)]     


temperature <-
  ac_dt[parameter_name == "Outdoor Temperature",
        c("year",
          "day_in_year_local",
          "arithmetic_mean",
          "first_maximum_value")]

baseline <- 
  temperature[year < 1995,
              .(base_mean = mean(arithmetic_mean),
                base_max = mean(first_maximum_value)), day_in_year_local]

temperature <- 
  baseline[temperature[year > 1994], on = c("day_in_year_local")][, 
                                                                  `:=`(change_avg = arithmetic_mean - base_mean,
                                                                       change_max = first_maximum_value - base_max)]

temperature <-
  temperature[, melt(
    .SD,
    id.vars = c("day_in_year_local", "year"),
    measure.vars = c("change_max", "change_avg")
  )]

temperature[
  year %in% c(1995:2019) &
    !is.na(value), 
  ggplot(.SD,
         aes(year,
             day_in_year_local,
             fill = cut(
               value,
               breaks = c(-100, -15, -5, 5, 15, 100),
               labels = c("Much Colder", "Colder", "Similar", "Warmer", "Much Warmer")
             ))) +
    geom_tile() +
    scale_fill_manual(name = "Temp. Change",
                      values = c("skyblue4", "skyblue", "green", "red", "red4")) +
    theme_bw() +
    labs(
      title = "Days Compared to 1990-1994 Average Temp. on That Day",
      subtitle = "Hotter Days Shown Redder",
      x = "Year",
      y = "Day of Year",
      caption = "Source: EPA"
    ) +
    facet_wrap(~ variable)
]



## Thoughts on Heatmaps

# Uneven hand selected cutoffs to find more balanced counts
lapply(list(cut(temperature[variable == "change_avg" &
                              !is.na(value)]$value, c(-100,-15,-5, 5, 15, 100)), 
            cut(temperature[variable == "change_max" &
                              !is.na(value)]$value, c(-100,-15,-5, 5, 15, 100))), summary)


# Even range limits with less even counts
lapply(list(cut(temperature[variable == "change_avg" &
                              !is.na(value)]$value, 5),
            cut(temperature[variable == "change_max" &
                              !is.na(value)]$value, 5)), summary)


