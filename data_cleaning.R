dir <- getwd()
setwd(dir)

library(readr)
library(dplyr)
library(lubridate)
library(janitor)

Sys.setlocale("LC_TIME", "nl_NL.UTF-8")

activities <- read.csv("strava_data/activities.csv")

#calculate NA's by column
colSums(is.na(activities))

df <- activities %>% 
  select(where(~ sum(!is.na(.)) > 0)) #calculate number of usable observations 

cat("Removed observations with 0 usable variables:", length(colnames(activities)) - length(colnames(df)))

colSums(!is.na(df))

# kolomnamen en hun gewenste nieuwe namen
column_map <- c(
  activity_id = "Activiteits.ID",
  activity_date = "Datum.van.activiteit",
  activity_name = "Naam.activiteit",
  activity_type = "Activiteitstype",
  elapsed_time = "Verstreken.tijd",
  comparable_effort = "Vergelijkbare.poging",
  moving_time = "Beweegtijd",
  distance = "Afstand.1",
  max_speed = "Max..snelheid",
  avg_speed = "Gemiddelde.snelheid",
  total_ascent = "Totale.stijging",
  total_descent = "Totale.daling",
  min_elevation = "Kleinste.hoogte",
  max_elevation = "Grootste.hoogte",
  max_grade = "Max..stijgingspercentage",
  avg_grade = "Gemiddeld.stijgingspercentage",
  max_cadence = "Max..cadans",
  avg_cadence = "Gemiddelde.cadans",
  max_heart_rate = "Max..hartslag.1",
  avg_heart_rate = "Gemiddelde.hartslag",
  avg_power = "Gemiddeld.wattage",
  calories = "Calorieën",
  total_work = "Totale.arbeid",
  perceived_exertion = "Ervaren.inspanning",
  weighted_avg_power = "Gewogen.gemiddeld.vermogen",
  grade_adjusted_distance = "Aan.stijgingspercentage.aangepaste.afstand",
  weather_time = "Tijd.weerbeeld",
  avg_speed_elapsed = "Gemiddelde.snelheid..op.basis.van.verstreken.tijd.",
  total_steps = "Totaal.aantal.stappen",
  pool_length = "Lengte.van.zwembad",
  intensity = "Intensiteit"
)

# selecteer alleen bestaande kolommen
existing_columns <- column_map[column_map %in% names(df)]
data_selected <- df %>% select(any_of(existing_columns))
# hernoem kolommen naar Engelse namen
names(data_selected) <- names(existing_columns)

# veilige mutate met checks
data_selected <- data_selected %>%
  mutate(
    activity_date = if ("activity_date" %in% names(.)) parse_date_time(activity_date, orders = "d b Y H:M:S", locale = "nl_NL.UTF-8") else NA,
    hour = if ("activity_date" %in% names(.)) hour(activity_date) else NA,
    weekday = if ("activity_date" %in% names(.)) wday(activity_date, label = TRUE, locale = "nl_NL.UTF-8") else NA,
    week = if ("activity_date" %in% names(.)) week(activity_date) else NA,
    month = if ("activity_date" %in% names(.)) month(activity_date, label = TRUE, locale = "nl_NL.UTF-8") else NA,
    year = if ("activity_date" %in% names(.)) year(activity_date) else NA,
    distance_km = if ("distance" %in% names(.)) distance / 1000 else NA,
    pace = if (all(c("activity_type", "moving_time", "distance") %in% names(.))) {
      if_else(activity_type == "Hardloopsessie", moving_time / 60 / (distance / 1000), NA_real_)
    } else NA_real_,
    speed = if (all(c("distance", "moving_time") %in% names(.))) {
      (distance / 1000) / (moving_time / 3600)
    } else NA_real_,
    elevation_difference = if (all(c("max_elevation", "min_elevation") %in% names(.))) {
      max_elevation - min_elevation
    } else NA_real_
  )

# schrijf veilig weg
write_csv(data_selected, "strava_data/cleaned_subset.csv")