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

#remove unnecessary observations with no statistical meaning
df <- df %>% 
  select(-c(Beschrijving.van.activiteit, Woon.werkverkeer, Privénotitie.activiteit, Uitrusting.voor.activiteit, Aantal.vermogensgegevens, Woon.werkverkeer.1, Uitrusting, Media, Bestandsnaam,
            Weersomstandigheden, Buitentemperatuur, Gevoelstemperatuur, Dauwpunt, Vochtigheid, Luchtdruk, Windsnelheid, Windstoot, Windrichting, Neerslagintensiteit, Tijd.zonsondergang, Tijd.zonsopgang, Maanstand, Uitrusting, Kans.op.neerslag, Type.neerslag, Bewolking, Zicht, UV.index,
            Verstreken.tijd.1, Afstand, Max..hartslag, Vergelijkbare.poging.1, Voorkeur.voor.ervaren.inspanning, Ervaren.vergelijkbare.poging, Van.upload, Gemeld, Afstand..onverharde.wegen., Trainingsbelasting, Totaalaantal.cycli, Gemiddelde.vergelijkbare.tempo.op.vlak.terrein
  )) %>% 
  rename(
    activity_id = Activiteits.ID,
    activity_date = Datum.van.activiteit,
    activity_name = Naam.activiteit,
    activity_type = Activiteitstype,
    elapsed_time = Verstreken.tijd,
    comparable_effort = Vergelijkbare.poging,
    moving_time = Beweegtijd,
    distance = Afstand.1,
    max_speed = Max..snelheid,
    avg_speed = Gemiddelde.snelheid,
    total_ascent = Totale.stijging,
    total_descent = Totale.daling,
    min_elevation = Kleinste.hoogte,
    max_elevation = Grootste.hoogte,
    max_grade = Max..stijgingspercentage,
    avg_grade = Gemiddeld.stijgingspercentage,
    max_cadence = Max..cadans,
    avg_cadence = Gemiddelde.cadans,
    max_heart_rate = Max..hartslag.1,
    avg_heart_rate = Gemiddelde.hartslag,
    avg_power = Gemiddeld.wattage,
    calories = Calorieën,
    total_work = Totale.arbeid,
    perceived_exertion = Ervaren.inspanning,
    weighted_avg_power = Gewogen.gemiddeld.vermogen,
    grade_adjusted_distance = Aan.stijgingspercentage.aangepaste.afstand,
    weather_time = Tijd.weerbeeld,
    avg_speed_elapsed = Gemiddelde.snelheid..op.basis.van.verstreken.tijd.,
    total_steps = Totaal.aantal.stappen,
    pool_length = Lengte.van.zwembad,
    intensity = Intensiteit
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, orders = "d b Y H:M:S", locale = "nl_NL.UTF-8"),
    hour = hour(activity_date),
    weekday = wday(activity_date, label = TRUE, locale = "nl_NL.UTF-8"),
    week = week(activity_date),
    month = month(activity_date, label = TRUE, locale = "nl_NL.UTF-8"),
    year = year(activity_date),
    distance_km = distance / 1000, #distance in km
    pace = if_else(activity_type == "Hardloopsessie",                    # only for running m/s
                   moving_time / 60 / distance_km, 
                   NA_real_),
    speed = distance_km / (moving_time / 3600), #km/h
    elevation_difference = max_elevation - min_elevation,
  )

  

write_csv(df, "strava_data/cleaned_subset.csv")