# RGoogleFit
# https://github.com/mathesong/rwithings
# 


library(googlesheets4)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(stringr)
options(pillar.sigfig = 4)
gs4_deauth()
ww_weight <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1PGO2ewReghbTrlLISGDMR9AwzjDVWxVlBjNzqaVHQXk/edit?usp=sharing")
withings_weight <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1ZlWmFnrWuTt-vk3goHy8oFvH41AvarqAztojHoEHdBA/edit?usp=sharing",
                                             col_names = FALSE) |>
  select(date = 1, weight = 2) |>
  mutate(date = str_replace(date, "Date: ", "")) |>
  mutate(date = str_replace(date, " at*M", ""))


View(withings_weight)



www <- www |>
  mutate(rollmax = rollmax(weight, 7, fill = NA, na.rm = TRUE),
         rollmean = rollmean(weight, 7, fill = NA, na.rm = TRUE),
         rollmin = rollapply(weight, 7, min, fill = NA, na.rm = TRUE),
         month = month(date),
         week = week(date),
         year = year(date))
ggplot(www, aes(x = date, y = weight)) +
  geom_point(alpha = 0.4) +
  #geom_line(data = www, aes(x = date, y = rollmax), color = "grey50") +
  geom_line(data = www, aes(x = date, y = rollmean), color = "red") 
#geom_line(data = www, aes(x = date, y = rollmin), color = "grey50")
www |>
  group_by(year, week) |>
  summarize(max_weight = max(weight, na.rm = TRUE),
            mean_weight = mean(weight, na.rm = TRUE),
            min_weight = min(weight, na.rm = TRUE)) |>
  ungroup() #|>
#ggplot(aes(week, min_weight)) +
#geom_point()
