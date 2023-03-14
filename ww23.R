library(googlesheets4)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

www <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1PGO2ewReghbTrlLISGDMR9AwzjDVWxVlBjNzqaVHQXk/edit?usp=sharing")
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
