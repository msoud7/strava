dir <- getwd()
setwd(dir)

library(readr)
library(dplyr)
library(lubridate)
Sys.setlocale("LC_TIME", "nl_NL.UTF-8")

activities <- read_csv("strava_data/activities.csv")

df <- activities %>% 
  rename(
    activity_id = `Activiteits-ID`,
    activity_date = `Datum van activiteit`,
    activity_name = `Naam activiteit`,
    activity_type = `Activiteitstype`,
    description = `Beschrijving van activiteit`,
    elapsed_time = `Verstreken tijd...16`,
    moving_time = `Beweegtijd`,
    distance = `Afstand...18`,
    max_speed = `Max. snelheid`,
    avg_speed = `Gemiddelde snelheid`,
    total_ascent = `Totale stijging`,
    total_descent = `Totale daling`,
    min_elevation = `Kleinste hoogte`,
    max_elevation = `Grootste hoogte`,
    max_grade = `Max. stijgingspercentage`,
    avg_grade = `Gemiddeld stijgingspercentage`,
    max_cadence = `Max. cadans`,
    avg_cadence = `Gemiddelde cadans`,
    max_heart_rate = `Max. hartslag...31`,
    avg_heart_rate = `Gemiddelde hartslag`,
    max_power = `Maximaal wattage`,
    avg_power = `Gemiddeld wattage`,
    weighted_avg_power = `Gewogen gemiddeld vermogen`,
    calories = `Calorieën`,
    temperature = `Buitentemperatuur`,
    feels_like_temp = `Gevoelstemperatuur`,
    humidity = `Vochtigheid`,
    wind_speed = `Windsnelheid`,
    precipitation = `Neerslagintensiteit`,
    perceived_exertion = `Ervaren inspanning`,
    start_time = `Starttijd`,
    commute = `Woon-werkverkeer...51`,
    bike_used = `Fiets`,
    training_load = `Trainingsbelasting`,
    intensity = `Intensiteit`
  ) %>%
  mutate(
    activity_date = dmy_hms(activity_date),
    start_time = as.POSIXct(start_time),
    elapsed_time = as.numeric(elapsed_time),
    moving_time = as.numeric(moving_time),
    distance = as.numeric(distance),
    max_speed = as.numeric(max_speed),
    avg_speed = as.numeric(avg_speed),
    total_ascent = as.numeric(total_ascent),
    calories = as.numeric(calories),
    commute = as.logical(commute),
    year = year(activity_date),
    month = month(activity_date, label = TRUE),
    weekday = wday(activity_date, label = TRUE),
    hour = hour(start_time),
    speed_ratio = max_speed / avg_speed,
    elevation_gain_per_km = total_ascent / (distance / 1000),
    duration_hours = elapsed_time / 3600,
    pace_min_per_km = ifelse(activity_type == "Run", 
                             (moving_time / 60) / (distance / 1000), NA)
  ) %>%
  select(
    activity_id, activity_date, activity_name, activity_type, description,
    elapsed_time, moving_time, distance, max_speed, avg_speed,
    total_ascent, total_descent, min_elevation, max_elevation,
    max_grade, avg_grade, max_cadence, avg_cadence,
    max_heart_rate, avg_heart_rate, max_power, avg_power,
    weighted_avg_power, calories, temperature, feels_like_temp,
    humidity, wind_speed, precipitation, perceived_exertion,
    start_time, commute, bike_used, training_load, intensity,
    year, month, weekday, hour, speed_ratio, elevation_gain_per_km, duration_hours, pace_min_per_km
  )

write_csv(df, "strava_data/cleaned_subset.csv")