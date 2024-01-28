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
  mutate(date = lubridate::mdy_hm(date)) |>
  mutate(weight = as.numeric(str_extract(weight, "[0-9]+.[0-9]+")))

#View(withings_weight)
www <- bind_rows(ww_weight, withings_weight)
www <- filter(www, !(weight == 215.7 & date > "2023-03-20"))
www <- www |>
  arrange(desc(date)) |>
  mutate(rollmax = rollmax(weight, 17, fill = NA, na.rm = TRUE),
         rollmean = rollmean(weight, 17, fill = NA, na.rm = TRUE),
         rollmin = rollapply(weight, 17, min, fill = NA, na.rm = TRUE),
         month = month(date),
         week = week(date),
         year = year(date))
#View(www)
#ggplot(www, aes(x = date, y = weight)) +
#  geom_point(alpha = 0.4) +
  #geom_line(data = www, aes(x = date, y = rollmax), color = "grey50") +
#  geom_line(data = www, aes(x = date, y = rollmean), color = "red") +
#  geom_smooth(method = "loess")
#geom_line(data = www, aes(x = date, y = rollmin), color = "grey50")
www2 <- www |>
  group_by(year, week) |>
  summarize(max_weight = round(max(weight, na.rm = TRUE), 1),
            mean_weight = round(mean(weight, na.rm = TRUE), 1),
            min_weight = round(min(weight, na.rm = TRUE), 1)) |>
  ungroup()
www2 <- www2 |>
  mutate(max_weight_loss = round(c(NA,rollapply(max_weight, 2,
                                           function(x) x[2] - x[1]))
                                 , 1),
         mean_weight_loss = round(c(NA,rollapply(mean_weight, 2,
                                           function(x) x[2] - x[1]))
                                 , 1),
         min_weight_loss = round(c(NA,rollapply(min_weight, 2,
                                           function(x) x[2] - x[1]))
                                 , 1)) |>
  arrange(desc(year), desc(week))
#View(www)
#ggplot(aes(week, min_weight)) +
#geom_point()
