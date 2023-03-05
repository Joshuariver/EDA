# Slope Graph

rm(list=ls())
setwd("~/R/EDA/EDA")

library(tidyverse)
# library(devtools)
# Get the latest version from GitHub
# install.packages("devtools")
# devtools::install_github("ibecav/CGPfunctions")
library(CGPfunctions)

thedata <- structure(list(
  topic = structure(c(6L, 3L, 5L, 4L, 11L, 13L, 
                      2L, 8L, 9L, 12L, 7L, 1L, 14L, 10L), 
                    .Label = c("Arts & entertainment", 
                               "Business", "Climate change", "Economics", "Education", "Health care", 
                               "Immigration", "National Security", "Politics", "Religion", "Science", 
                               "Sports", "Technology", "U.S. foreign policy"), 
                    class = "factor"), 
  actually_read = c(7L, 5L, 11L, 6L, 10L, 14L, 13L, 1L, 2L, 3L, 4L, 8L, 9L, 12L), 
  say_want_covered = c(1L, 2L, 3L, 4L, 7L, 8L, 11L, 5L, 10L, 14L, 6L, 13L, 9L, 12L)), 
  class = "data.frame", row.names = c(NA, -14L))
thedata

temp <- reshape2::melt(data = thedata,
                       id = "topic",
                       variable.name = "Saydo",
                       value.name = "Rank")
temp$Saydo <- forcats::fct_recode(temp$Saydo, 
                                  "Actually read" = "actually_read",
                                  "Say they want" = "say_want_covered")
temp

newggslopegraph(dataframe = temp, 
                Times = Saydo, 
                Measurement = Rank, 
                Grouping = topic)

## 
## Converting 'Saydo' to an ordered factor


newggslopegraph(dataframe = temp, 
                Times = Saydo, 
                Measurement = Rank, 
                Grouping = topic, 
#                ReverseYAxis = TRUE,
                Title = "14 Topics Ranked by What Americans Read vs Want Covered",
                SubTitle = "'Read' rank from Parse.ly May 2019 data.\n'Want covered' rank from Axios/SurveyMonkey poll conducted May 17-20, 2019",
                Caption = "Source: Axios \nMakeover by @hrbrmstr",
                LineColor = "black")

colorvect <- temp %>% group_by(topic) %>% 
  summarise(difference = diff(Rank)) %>% 
  mutate(whatcolor = case_when(
    difference == 0 ~ "light gray",
    difference > 0 ~ "red",
    difference < 0 ~ "black"
  )) %>%
  select(topic, whatcolor) %>%
  tibble::deframe()
colorvect

newggslopegraph(dataframe = temp,
                Times = Saydo,
                Measurement = Rank,
                Grouping = topic,

                                #ReverseYAxis = TRUE, 
                
                DataTextSize = 3.5, 
                YTextSize = 4, 
                XTextSize = 16,
#                 DataLabelPadding = .2,
                Title = "Topic Rankings Compared Between\nWhat Americans Actually Read vs Want Covered",
                SubTitle = "'Actually Read' rank from Parse.ly May 2019 data.\n'Want covered' rank from Axios/SurveyMonkey poll conducted May 17-20, 2019",
                Caption = "Source: Axios \nMakeover by @hrbrmstr",
                LineColor = colorvect
)


newggslopegraph(dataframe = temp,
                Times = Saydo,
                Measurement = Rank,
                Grouping = topic,
#                 ReverseYAxis = TRUE, 
                DataTextSize = 3.5, 
                YTextSize = 4, 
                XTextSize = 16,
#                DataLabelPadding = .2,
                Title = "Topic Rankings Compared Between\nWhat Americans Actually Read vs Want Covered",
                SubTitle = "'Actually Read' rank from Parse.ly May 2019 data.\n'Want covered' rank from Axios/SurveyMonkey poll conducted May 17-20, 2019",
                Caption = "Source: Axios \nMakeover by @hrbrmstr",
                LineColor = colorvect
)


newggslopegraph(dataframe = temp,
                Times = Saydo,
                Measurement = Rank,
                Grouping = topic,
#                 ReverseYAxis = TRUE, 
                DataTextSize = 3.5, 
                YTextSize = 3.2, 
                XTextSize = 14,
#                DataLabelPadding = .2,
                Title = "Americans Don't Actually Read the News They Say They Want",
                SubTitle = "Many sharp differences in rankings in both directions. Hypocrisy, laziness or gratification?",
                Caption = "Source: Rud.is \nMakeover by @hrbrmstr",
                LineColor = colorvect,
                #ThemeChoice = "ipsum",
                TitleTextSize = 18,
                SubTitleTextSize = 12

#                SubTitleJustify = "right"

)

newggslopegraph(dataframe = temp,
                Times = Saydo,
                Measurement = Rank,
                Grouping = topic,
                ReverseYAxis = TRUE, 
                ReverseXAxis = TRUE,
                DataTextSize = 3.5, 
                YTextSize = 4, 
                XTextSize = 13,
                DataLabelPadding = .2,
                Title = "Americans Don't Actually Read the News They Say They Want",
                SubTitle = "Many sharp differences in rankings in both directions.\nHypocrisy or laziness or gratification?",
                Caption = "Source: Rud.is \nMakeover by @hrbrmstr",
                LineColor = colorvect,
                ThemeChoice = "wsj",
                TitleTextSize = 15,
                CaptionTextSize = 6,
                SubTitleTextSize = 11,
                SubTitleJustify = "right"
)

