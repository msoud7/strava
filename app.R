# Enhanced Strava Analytics Shiny App with Joke Stats - FIXED VERSION
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(GGally)
library(shinycssloaders)
library(htmltools)
library(readr)
library(tidyr)
library(janitor)

# Set locale for Dutch date parsing
Sys.setlocale("LC_TIME", "nl_NL.UTF-8")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Strava Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("table")),
      menuItem("Training Load", tabName = "training", icon = icon("chart-bar")),
      menuItem("Running Performance", tabName = "run_performance", icon = icon("person-running")),
      menuItem("Biking Performance", tabName = "bike_performance", icon = icon("bicycle")),
      menuItem("Swimming Performance", tabName = "swim_performance", icon = icon("fish")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Advanced Analysis", tabName = "advanced", icon = icon("calculator")),
      menuItem("Funny Stats", tabName = "joke_stats", icon = icon("laugh-beam"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .box-header {
          border-radius: 8px 8px 0 0;
        }
        .joke-stat-card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 20px;
          border-radius: 12px;
          text-align: center;
          margin: 10px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        .joke-stat-number {
          font-size: 2.5em;
          font-weight: bold;
          display: block;
        }
        .joke-stat-label {
          font-size: 1.1em;
          margin-top: 5px;
        }
        .trophy-cabinet {
          background: linear-gradient(45deg, #f093fb 0%, #f5576c 100%);
          padding: 15px;
          border-radius: 12px;
          margin: 10px 0;
        }
        .badge-item {
          background: rgba(255,255,255,0.2);
          padding: 10px;
          margin: 5px;
          border-radius: 8px;
          display: inline-block;
        }
        .motivational-quote {
          background: linear-gradient(45deg, #43e97b 0%, #38f9d7 100%);
          padding: 20px;
          border-radius: 12px;
          text-align: center;
          font-style: italic;
          font-size: 1.2em;
          color: #2c3e50;
          margin: 15px 0;
        }
        .burn-off-meter {
          background: linear-gradient(90deg, #ff9a9e 0%, #fecfef 100%);
          padding: 20px;
          border-radius: 12px;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      # Data Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Upload Strava Data", status = "primary", solidHeader = TRUE, width = 12,
                  fileInput("file", "Choose CSV File (activities.csv from Strava export)",
                            accept = c(".csv")),
                  helpText("Upload your Strava activities.csv file from your data export.")
                )
              ),
              
              fluidRow(
                valueBoxOutput("total_distance"),
                valueBoxOutput("total_activities"),
                valueBoxOutput("avg_duration")
              ),
              
              fluidRow(
                box(
                  title = "Data Summary", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(DT::dataTableOutput("summary_table"))
                ),
                box(
                  title = "Activity Types", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("activity_types_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Raw Data Preview", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("data_preview"))
                )
              )
      ),
      
      # Training Load Tab
      tabItem(tabName = "training",
              fluidRow(
                box(
                  title = "Filters", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("year_filter", "Select Year:",
                              choices = NULL,
                              selected = NULL),
                  selectInput("activity_filter", "Select Activity Type:",
                              choices = NULL,
                              selected = NULL,
                              multiple = TRUE)
                ),
                box(
                  title = "Monthly Distance", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(plotlyOutput("monthly_distance_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Weekly Activity Pattern", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("weekday_pattern"))
                ),
                box(
                  title = "Activity Heatmap", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("activity_heatmap"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Monthly Elapsed Time", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("monthly_time_plot"))
                ),
                box(
                  title = "Duration Distribution", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("duration_distribution"))
                )
              )
      ),
      
      # Running Performance Tab
      tabItem(tabName = "run_performance",
              fluidRow(
                valueBoxOutput("running_total_distance"),
                valueBoxOutput("running_avg_pace"),
                valueBoxOutput("running_best_pace")
              ),
              
              fluidRow(
                box(
                  title = "Pace vs Heart Rate", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("pace_hr_plot"))
                ),
                box(
                  title = "Heart Rate Zones Distribution", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("hr_zones_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Pace Distribution", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("pace_distribution"))
                ),
                box(
                  title = "Distance vs Pace", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("distance_pace_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Running Pace Improvement", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("running_pace_trends"))
                ),
                box(
                  title = "Monthly Running Volume", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("monthly_running_volume"))
                )
              )
      ),
      
      # Biking Performance Tab
      tabItem(tabName = "bike_performance",
              fluidRow(
                valueBoxOutput("biking_total_distance"),
                valueBoxOutput("biking_avg_speed"),
                valueBoxOutput("biking_total_elevation")
              ),
              
              fluidRow(
                box(
                  title = "Speed vs Power", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("speed_power_plot"))
                ),
                box(
                  title = "Elevation Profile", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("elevation_profile_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Power Distribution", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("power_distribution"))
                ),
                box(
                  title = "Speed vs Distance", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("speed_distance_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Biking Performance Trends", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("biking_speed_trends"))
                ),
                box(
                  title = "Monthly Biking Volume", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("monthly_biking_volume"))
                )
              )
      ),
      
      # Swimming Performance Tab
      tabItem(tabName = "swim_performance",
              fluidRow(
                valueBoxOutput("swimming_total_distance"),
                valueBoxOutput("swimming_avg_pace"),
                valueBoxOutput("swimming_total_time")
              ),
              
              fluidRow(
                box(
                  title = "Swimming Stroke Analysis", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("swimming_stroke_plot"))
                ),
                box(
                  title = "Pool vs Open Water", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("pool_openwater_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Swimming Pace Distribution", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("swimming_pace_distribution"))
                ),
                box(
                  title = "Distance per Session", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("swimming_distance_session"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Swimming Progress Over Time", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("swimming_progress"))
                ),
                box(
                  title = "Monthly Swimming Volume", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("monthly_swimming_volume"))
                )
              )
      ),
      
      # Trends Tab
      tabItem(tabName = "trends",
              fluidRow(
                box(
                  title = "Cumulative Distance", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("cumulative_distance_plot"))
                ),
                box(
                  title = "Distance Over Time", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("distance_trends_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Performance Improvement", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("performance_trends_plot"))
                ),
                box(
                  title = "Activity Frequency by Month/Year", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("activity_frequency_plot"))
                )
              )
      ),
      
      # Advanced Analysis Tab
      tabItem(tabName = "advanced",
              fluidRow(
                box(
                  title = "Correlation Matrix", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(plotOutput("correlation_plot", height = "600px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Custom Analysis", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("x_var", "X Variable:",
                              choices = NULL),
                  selectInput("y_var", "Y Variable:",
                              choices = NULL),
                  selectInput("color_var", "Color by:",
                              choices = NULL),
                  checkboxInput("add_trend", "Add trend line", FALSE)
                ),
                box(
                  title = "Custom Plot", status = "info", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("custom_plot"))
                )
              )
      ),
      
      # 🍔 JOKE STATS TAB 🍔
      tabItem(tabName = "joke_stats",
              # Motivational Quote Section
              fluidRow(
                div(class = "motivational-quote",
                    style = "width: 95%; margin: 0 auto;",
                    uiOutput("motivational_quote")
                )
              ),
              
              # Main Joke Stats Grid
              fluidRow(
                box(
                  title = "🍔 Food Burned Off", status = "warning", solidHeader = TRUE, width = 6,
                  div(
                    fluidRow(
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("big_macs", inline = TRUE)),
                                    div(class = "joke-stat-label", "🍔 Big Macs Burned"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("beers", inline = TRUE)),
                                    div(class = "joke-stat-label", "🍺 Beers Worked Off")))
                    ),
                    fluidRow(
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("pizza_slices", inline = TRUE)),
                                    div(class = "joke-stat-label", "🍕 Pizza Slices"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("chip_bags", inline = TRUE)),
                                    div(class = "joke-stat-label", "🥔 Bags of Chips")))
                    ),
                    fluidRow(
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("ice_cream", inline = TRUE)),
                                    div(class = "joke-stat-label", "🍦 Ice Cream Cones"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("chocolate_bars", inline = TRUE)),
                                    div(class = "joke-stat-label", "🍫 Chocolate Bars")))
                    )
                  )
                ),
                
                box(
                  title = "🏃‍♂️ Distance Achievements", status = "success", solidHeader = TRUE, width = 6,
                  div(
                    fluidRow(
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("football_fields", inline = TRUE)),
                                    div(class = "joke-stat-label", "⚽ Football Fields"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("marathons", inline = TRUE)),
                                    div(class = "joke-stat-label", "🏃‍♂️ Marathons Complete")))
                    ),
                    fluidRow(
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("moon_progress", inline = TRUE)),
                                    div(class = "joke-stat-label", "🌙 % to the Moon"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("netflix_hours", inline = TRUE)),
                                    div(class = "joke-stat-label", "📺 Netflix Hours Avoided")))
                    )
                  )
                )
              ),
              
              # Elevation and Milestones
              fluidRow(
                box(
                  title = "🏔️ Elevation Achievements", status = "info", solidHeader = TRUE, width = 6,
                  div(
                    fluidRow(
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("burj_khalifas", inline = TRUE)),
                                    div(class = "joke-stat-label", "🏗️ Burj Khalifas Climbed"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("eiffel_towers", inline = TRUE)),
                                    div(class = "joke-stat-label", "🗼 Eiffel Towers Climbed"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("elevator_rides", inline = TRUE)),
                                    div(class = "joke-stat-label", "🛗 Elevator Rides Saved"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("stairs_climbed", inline = TRUE)),
                                    div(class = "joke-stat-label", "🪜 Flights of Stairs Climbed")))
                    )
                  )
                ),
                
                box(
                  title = "🎯 Personal Bests & Records", status = "primary", solidHeader = TRUE, width = 6,
                  div(
                    fluidRow(
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("best_week_bigmacs", inline = TRUE)),
                                    div(class = "joke-stat-label", "🏆 Best Week: Big Macs"))),
                      column(6, div(class = "joke-stat-card",
                                    span(class = "joke-stat-number", textOutput("longest_activity", inline = TRUE)),
                                    div(class = "joke-stat-label", "⏱️ Longest Activity (hrs)")))
                    ),
                    fluidRow(
                      column(12, div(class = "joke-stat-card",
                                     span(class = "joke-stat-number", textOutput("most_active_month", inline = TRUE)),
                                     div(class = "joke-stat-label", "📅 Most Active Month")))
                    )
                  )
                )
              ),
              
              # Burn-off Meter and Trophy Cabinet
              fluidRow(
                box(
                  title = "🔥 Calorie Burn-off Meter", status = "warning", solidHeader = TRUE, width = 6,
                  div(class = "burn-off-meter",
                      h4("This Week's Calorie Debt:", style = "text-align: center; color: #2c3e50;"),
                      withSpinner(plotlyOutput("burnoff_meter", height = "200px")),
                      p(textOutput("burnoff_text"), style = "text-align: center; font-weight: bold; margin-top: 10px;")
                  )
                ),
                
                box(
                  title = "🏆 Trophy Cabinet", status = "success", solidHeader = TRUE, width = 6,
                  div(class = "trophy-cabinet",
                      h4("🏅 Your Achievements", style = "color: white; text-align: center;"),
                      uiOutput("trophy_cabinet")
                  )
                )
              ),
              
              # Fun Leaderboards
              fluidRow(
                box(
                  title = "📊 Monthly Big Mac Leaderboard", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("monthly_bigmac_chart"))
                ),
                box(
                  title = "🎮 Achievement Progress", status = "primary", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("achievement_progress"))
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive data loading with Dutch locale support
  data <- reactive({
    req(input$file)
    
    tryCatch({
      # Read the CSV file
      activities <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      # Data preprocessing following your original script
      df <- activities %>% 
        select(where(~ sum(!is.na(.)) > 0)) %>%
        select(-c(any_of(c("Beschrijving.van.activiteit", "Woon.werkverkeer", "Privénotitie.activiteit", 
                           "Uitrusting.voor.activiteit", "Aantal.vermogensgegevens", "Woon.werkverkeer.1", 
                           "Uitrusting", "Media", "Bestandsnaam", "Weersomstandigheden", "Buitentemperatuur", 
                           "Gevoelstemperatuur", "Dauwpunt", "Vochtigheid", "Luchtdruk", "Windsnelheid", 
                           "Windstoot", "Windrichting", "Neerslagintensiteit", "Tijd.zonsondergang", 
                           "Tijd.zonsopgang", "Maanstand", "Kans.op.neerslag", "Type.neerslag", 
                           "Bewolking", "Zicht", "UV.index", "Verstreken.tijd.1", "Afstand", "Max..hartslag", 
                           "Vergelijkbare.poging.1", "Voorkeur.voor.ervaren.inspanning", 
                           "Ervaren.vergelijkbare.poging", "Van.upload", "Gemeld", "Afstand..onverharde.wegen.", 
                           "Trainingsbelasting", "Totaalaantal.cycli", "Gemiddelde.vergelijkbare.tempo.op.vlak.terrein")))) %>%
        rename(
          activity_id = any_of("Activiteits.ID"),
          activity_date = any_of("Datum.van.activiteit"),
          activity_name = any_of("Naam.activiteit"),
          activity_type = any_of("Activiteitstype"),
          elapsed_time = any_of("Verstreken.tijd"),
          comparable_effort = any_of("Vergelijkbare.poging"),
          moving_time = any_of("Beweegtijd"),
          distance = any_of("Afstand.1"),
          max_speed = any_of("Max..snelheid"),
          avg_speed = any_of("Gemiddelde.snelheid"),
          total_ascent = any_of("Totale.stijging"),
          total_descent = any_of("Totale.daling"),
          min_elevation = any_of("Kleinste.hoogte"),
          max_elevation = any_of("Grootste.hoogte"),
          max_grade = any_of("Max..stijgingspercentage"),
          avg_grade = any_of("Gemiddeld.stijgingspercentage"),
          max_cadence = any_of("Max..cadans"),
          avg_cadence = any_of("Gemiddelde.cadans"),
          max_heart_rate = any_of("Max..hartslag.1"),
          avg_heart_rate = any_of("Gemiddelde.hartslag"),
          avg_power = any_of("Gemiddeld.wattage"),
          calories = any_of("Calorieën"),
          total_work = any_of("Totale.arbeid"),
          perceived_exertion = any_of("Ervaren.inspanning"),
          weighted_avg_power = any_of("Gewogen.gemiddeld.vermogen"),
          grade_adjusted_distance = any_of("Aan.stijgingspercentage.aangepaste.afstand"),
          weather_time = any_of("Tijd.weerbeeld"),
          avg_speed_elapsed = any_of("Gemiddelde.snelheid..op.basis.van.verstreken.tijd."),
          total_steps = any_of("Totaal.aantal.stappen"),
          pool_length = any_of("Lengte.van.zwembad"),
          intensity = any_of("Intensiteit")
        ) %>%
        mutate(
          activity_date = parse_date_time(activity_date, orders = c("d b Y H:M:S", "dmy HMS", "ymd HMS"), locale = "nl_NL.UTF-8"),
          hour = hour(activity_date),
          weekday = wday(activity_date, label = TRUE),
          week = week(activity_date),
          month = month(activity_date, label = TRUE),
          year = year(activity_date),
          distance_km = ifelse(!is.na(distance), distance / 1000, NA),
          pace = if_else(activity_type == "Hardloopsessie" & !is.na(moving_time) & !is.na(distance_km) & distance_km > 0,                    
                         moving_time / 60 / distance_km, 
                         NA_real_),
          speed = ifelse(!is.na(distance_km) & !is.na(moving_time) & moving_time > 0, 
                         distance_km / (moving_time / 3600), NA),
          elevation_difference = ifelse(!is.na(max_elevation) & !is.na(min_elevation), 
                                        max_elevation - min_elevation, NA),
          calories = ifelse(is.na(calories), 0, calories),
          avg_speed = (distance/1000) / (moving_time/3600),
          avg_pace_sec = (moving_time / distance) * 100 / 60
        )
      return(df)
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Joke Stats Calculations
  joke_stats <- reactive({
    req(data())
    df <- data()
    
    # Conversion factors
    conversions <- list(
      big_mac = 550,        # calories
      beer = 150,           # calories  
      pizza_slice = 285,    # calories
      chips_bag = 152,      # calories
      ice_cream_cone = 137, # calories
      chocolate_bar = 235,  # calories
      netflix_hour = 68,    # calories burned per hour watching
      football_field = 0.1097, # km
      marathon_distance = 42.195, # km
      burj_khalifa_height = 828, # meters
      eiffel_tower_height = 330, #meters
      elevator_floor = 5,   # meters per floor
      moon_distance = 384400, # km
      stairs_per_flight = 3 # meters per flight of stairs
    )
    
    # Calculate totals
    total_calories <- sum(df$calories, na.rm = TRUE)
    total_distance <- sum(df$distance_km, na.rm = TRUE)
    total_elevation <- sum(df$total_ascent, na.rm = TRUE)
    
    # Calculate joke stats
    list(
      big_macs = floor(total_calories / conversions$big_mac),
      beers = floor(total_calories / conversions$beer),
      pizza_slices = floor(total_calories / conversions$pizza_slice),
      chip_bags = floor(total_calories / conversions$chips_bag),
      ice_cream_cones = floor(total_calories / conversions$ice_cream_cone),
      chocolate_bars = floor(total_calories / conversions$chocolate_bar),
      netflix_hours = floor(total_calories / conversions$netflix_hour),
      football_fields = floor(total_distance / conversions$football_field),
      marathons = round(total_distance / conversions$marathon_distance, 2),
      moon_progress = round((total_distance / conversions$moon_distance) * 100, 4),
      burj_khalifas = round(total_elevation / conversions$burj_khalifa_height, 2),
      eiffel_towers = round(total_elevation / conversions$eiffel_tower_height, 2),
      elevator_rides = floor(total_elevation / conversions$elevator_floor),
      stairs_climbed = floor(total_elevation / conversions$stairs_per_flight),
      total_calories = total_calories,
      total_distance = total_distance,
      total_elevation = total_elevation
    )
  })
  
  # Update filter choices
  observe({
    req(data())
    df <- data()
    
    if("year" %in% names(df) && !all(is.na(df$year))) {
      updateSelectInput(session, "year_filter",
                        choices = sort(unique(df$year), decreasing = TRUE),
                        selected = max(df$year, na.rm = TRUE))
    }
    
    if("activity_type" %in% names(df)) {
      updateSelectInput(session, "activity_filter",
                        choices = unique(df$activity_type),
                        selected = unique(df$activity_type))
    }
    
    # Update custom analysis variables
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    all_vars <- c("None", names(df))
    
    updateSelectInput(session, "x_var", choices = numeric_vars)
    updateSelectInput(session, "y_var", choices = numeric_vars)
    updateSelectInput(session, "color_var", choices = all_vars, selected = "None")
  })
  
  # Value boxes
  output$total_distance <- renderValueBox({
    req(data())
    df <- data()
    total_dist <- round(sum(df$distance_km, na.rm = TRUE), 1)
    
    valueBox(
      value = paste(total_dist, "km"),
      subtitle = "Total Distance",
      icon = icon("road"),
      color = "blue"
    )
  })
  
  # Continuing from where the code left off...
  
  output$total_activities <- renderValueBox({
    req(data())
    valueBox(
      value = nrow(data()),
      subtitle = "Total Activities",
      icon = icon("chart-bar"),
      color = "green"
    )
  })
  
  output$avg_duration <- renderValueBox({
    req(data())
    df <- data()
    
    avg_seconds <- mean(df$moving_time, na.rm = TRUE)
    mins <- floor(avg_seconds / 60)
    secs <- round(avg_seconds %% 60)
    
    duration_formatted <- sprintf("%d:%02d", mins, secs)
    
    valueBox(
      value = paste(duration_formatted, "min"),
      subtitle = "Average Duration",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  # Data tables
  output$summary_table <- DT::renderDataTable({
    req(data())
    df <- data()
    
    summary_stats <- df %>%
      filter(!is.na(activity_type)) %>%
      group_by(activity_type) %>%
      summarise(
        Count = n(),
        `Total Distance (km)` = round(sum(distance_km, na.rm = TRUE), 1),
        `Avg Distance (km)` = round(mean(distance_km, na.rm = TRUE), 1),
        `Total Time (hrs)` = round(sum(moving_time / 3600, na.rm = TRUE), 1),
        `Avg Time (min)` = round(mean(moving_time / 60, na.rm = TRUE), 1),
        `Total Calories` = sum(calories, na.rm = TRUE),
        .groups = 'drop'
      )
    
    DT::datatable(summary_stats, 
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })
  
  output$data_preview <- DT::renderDataTable({
    req(data())
    df <- data()
    
    preview_cols <- c("activity_date", "activity_name", "activity_type", 
                      "distance_km", "moving_time", "calories", "avg_speed")
    
    preview_data <- df[, intersect(preview_cols, names(df)), drop = FALSE]
    
    DT::datatable(preview_data, 
                  options = list(scrollX = TRUE, pageLength = 15),
                  rownames = FALSE)
  })
  
  # Activity types plot
  output$activity_types_plot <- renderPlotly({
    req(data())
    df <- data()
    
    type_counts <- df %>%
      filter(!is.na(activity_type)) %>%
      count(activity_type, sort = TRUE)
    
    p <- ggplot(type_counts, aes(x = reorder(activity_type, n), y = n, fill = activity_type)) +
      geom_col(alpha = 0.8, show.legend = FALSE) +
      coord_flip() +
      labs(title = "Activity Types Distribution",
           x = "Activity Type", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Training Load Plots
  filtered_data <- reactive({
    req(data())
    df <- data()
    
    if (!is.null(input$year_filter) && input$year_filter != "") {
      df <- df %>% filter(year == input$year_filter)
    }
    
    if (!is.null(input$activity_filter) && length(input$activity_filter) > 0) {
      df <- df %>% filter(activity_type %in% input$activity_filter)
    }
    
    return(df)
  })
  
  output$monthly_distance_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    monthly_data <- df %>%
      filter(!is.na(distance_km) & !is.na(month)) %>%
      group_by(year, month) %>%
      summarise(total_distance = sum(distance_km, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(date = as.Date(paste(year, month, "01"), format = "%Y %b %d"))
    
    p <- ggplot(monthly_data, aes(x = date, y = total_distance)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(title = "Monthly Distance",
           x = "Month", y = "Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$weekday_pattern <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    weekday_data <- df %>%
      filter(!is.na(weekday)) %>%
      count(weekday) %>%
      mutate(weekday = factor(weekday,
                              levels = c("ma", "di", "wo", "do", "vr", "za", "zo"),
                              labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
    
    p <- ggplot(weekday_data, aes(x = weekday, y = n)) +
      geom_col(fill = "coral", alpha = 0.7) +
      labs(title = "Activity Pattern by Weekday",
           x = "Day of Week", y = "Number of Activities") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$activity_heatmap <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    if (!"hour" %in% names(df) || !"weekday" %in% names(df)) {
      return(NULL)
    }
    
    heatmap_data <- df %>%
      filter(!is.na(hour) & !is.na(weekday)) %>%
      count(weekday, hour) %>%
      mutate(weekday = factor(weekday, levels = c("ma", "di", "wo", "do", "vr", "za", "zo"),
                              labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
    
    p <- ggplot(heatmap_data, aes(x = hour, y = weekday, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Activity Heatmap",
           x = "Hour of Day", y = "Day of Week", fill = "Activities") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$monthly_time_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    monthly_time <- df %>%
      filter(!is.na(moving_time) & !is.na(month)) %>%
      group_by(year, month) %>%
      summarise(total_time = sum(moving_time / 3600, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(date = as.Date(paste(year, month, "01"), format = "%Y %b %d"))
    
    p <- ggplot(monthly_time, aes(x = date, y = total_time)) +
      geom_area(fill = "lightgreen", alpha = 0.5) +
      geom_line(color = "darkgreen", size = 1) +
      labs(title = "Monthly Training Time",
           x = "Month", y = "Time (hours)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$duration_distribution <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    if (!"moving_time" %in% names(df)) return(NULL)
    
    duration_data <- df %>%
      filter(!is.na(moving_time) & moving_time > 0) %>%
      mutate(duration_minutes = moving_time / 60)
    
    p <- ggplot(duration_data, aes(x = duration_minutes)) +
      geom_histogram(bins = 30, fill = "purple", alpha = 0.7) +
      labs(title = "Activity Duration Distribution",
           x = "Duration (minutes)", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Running Performance Value Boxes
  output$running_total_distance <- renderValueBox({
    req(data())
    df <- data()
    running_data <- df %>% filter(activity_type == "Hardloopsessie")
    total_dist <- round(sum(running_data$distance_km, na.rm = TRUE), 1)
    
    valueBox(
      value = paste(total_dist, "km"),
      subtitle = "Total Running Distance",
      icon = icon("person-running"),
      color = "red"
    )
  })
  
  output$running_avg_pace <- renderValueBox({
    req(data())
    df <- data()
    running_data <- df %>% filter(activity_type == "Hardloopsessie" & !is.na(pace))
    avg_pace_sec <- mean(running_data$pace, na.rm = TRUE) * 60
    mins <- floor(avg_pace_sec / 60)
    secs <- round(avg_pace_sec %% 60)
    formatted_pace <- sprintf("%d:%02d min/km", mins, secs)
    
    valueBox(
      value = formatted_pace,
      subtitle = "Average Pace",
      icon = icon("stopwatch"),
      color = "orange"
    )
  })
  
  
  output$running_best_pace <- renderValueBox({
    req(data())
    df <- data()
    running_data <- df %>% filter(activity_type == "Hardloopsessie" & !is.na(pace))
    best_pace_sec <- min(running_data$pace, na.rm = TRUE) * 60
    mins <- floor(best_pace_sec / 60)
    secs <- round(best_pace_sec %% 60)
    formatted_pace <- sprintf("%d:%02d min/km", mins, secs)
    
    valueBox(
      value = formatted_pace,
      subtitle = "Best Pace",
      icon = icon("trophy"),
      color = "purple"
    )
  })
  
  # Running Performance Plots
  output$pace_hr_plot <- renderPlotly({
    req(data())
    df <- data()
    
    running_data <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(pace) & !is.na(avg_heart_rate), pace < 6.5)
    
    if (nrow(running_data) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(running_data, aes(x = pace, y = avg_heart_rate)) +
      geom_point(alpha = 0.6, color = "red") +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Pace vs Heart Rate",
           x = "Pace (min/km)", y = "Average Heart Rate (bpm)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$hr_zones_plot <- renderPlotly({
    req(data())
    df <- data()
    
    running_data <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(avg_heart_rate)) %>%
      mutate(hr_zone = case_when(
        avg_heart_rate < 120 ~ "Zone 1 (Recovery)",
        avg_heart_rate < 140 ~ "Zone 2 (Aerobic)",
        avg_heart_rate < 160 ~ "Zone 3 (Threshold)",
        avg_heart_rate < 180 ~ "Zone 4 (VO2 Max)",
        TRUE ~ "Zone 5 (Anaerobic)"
      ))
    
    if (nrow(running_data) == 0) {
      return(plotly_empty())
    }
    
    zone_counts <- running_data %>% count(hr_zone)
    
    p <- ggplot(zone_counts, aes(x = hr_zone, y = n, fill = hr_zone)) +
      geom_col(alpha = 0.7) +
      labs(title = "Heart Rate Zones Distribution",
           x = "Heart Rate Zone", y = "Number of Activities") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p)
  })
  
  output$pace_distribution <- renderPlotly({
    req(data())
    df <- data()
    
    pace_data <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(pace) & pace > 0 & pace < 10)
    
    if (nrow(pace_data) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(pace_data, aes(x = pace)) +
      geom_histogram(bins = 20, fill = "skyblue", alpha = 0.7) +
      labs(title = "Running Pace Distribution",
           x = "Pace (min/km)", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$distance_pace_plot <- renderPlotly({
    req(data())
    df <- data()
    
    running_data <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(pace) & !is.na(distance_km) & 
               pace > 0 & pace < 10 & distance_km > 0)
    
    if (nrow(running_data) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(running_data, aes(x = distance_km, y = pace)) +
      geom_point(alpha = 0.6, color = "darkgreen") +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      labs(title = "Distance vs Pace",
           x = "Distance (km)", y = "Pace (min/km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$running_pace_trends <- renderPlotly({
    req(data())
    df <- data()
    
    running_trends <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(pace) & !is.na(activity_date) & 
               pace > 0 & pace < 10) %>%
      arrange(activity_date) %>%
      mutate(rolling_pace = zoo::rollmean(pace, k = 10, fill = NA, align = "right"))
    
    if (nrow(running_trends) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(running_trends, aes(x = activity_date)) +
      geom_point(aes(y = pace), alpha = 0.3, color = "gray") +
      geom_line(aes(y = rolling_pace), color = "blue", size = 1) +
      labs(title = "Running Pace Improvement Over Time",
           x = "Date", y = "Pace (min/km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$monthly_running_volume <- renderPlotly({
    req(data())
    df <- data()
    
    monthly_running <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(distance_km) & !is.na(month)) %>%
      group_by(year, month) %>%
      summarise(total_distance = sum(distance_km, na.rm = TRUE),
                activity_count = n(),
                .groups = 'drop') %>%
      mutate(date = as.Date(paste(year, month, "01"), format = "%Y %b %d"))
    
    if (nrow(monthly_running) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(monthly_running, aes(x = date, y = total_distance)) +
      geom_col(fill = "darkred", alpha = 0.7) +
      labs(title = "Monthly Running Volume",
           x = "Month", y = "Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Biking Performance Value Boxes
  output$biking_total_distance <- renderValueBox({
    req(data())
    df <- data()
    biking_data <- df %>% filter(activity_type == "Fietsrit")
    total_dist <- round(sum(biking_data$distance_km, na.rm = TRUE), 1)
    
    valueBox(
      value = paste(total_dist, "km"),
      subtitle = "Total Biking Distance",
      icon = icon("bicycle"),
      color = "blue"
    )
  })
  
  output$biking_avg_speed <- renderValueBox({
    req(data())
    df <- data()
    biking_data <- df %>% filter(activity_type == "Fietsrit" & !is.na(moving_time) & !is.na(distance))
    avg_spd <- round(mean((biking_data$distance/1000) / (biking_data$moving_time/3600), na.rm = TRUE), 1)
    
    valueBox(
      value = paste(avg_spd, "km/h"),
      subtitle = "Average Speed",
      icon = icon("tachometer-alt"),
      color = "green"
    )
  })
  
  output$biking_total_elevation <- renderValueBox({
    req(data())
    df <- data()
    biking_data <- df %>% filter(activity_type == "Fietsrit")
    total_elev <- round(sum(biking_data$total_ascent, na.rm = TRUE), 0)
    
    valueBox(
      value = paste(total_elev, "m"),
      subtitle = "Total Elevation Gain",
      icon = icon("mountain"),
      color = "orange"
    )
  })
  
  # Biking Performance Plots
  output$speed_power_plot <- renderPlotly({
    req(data())
    df <- data()
    
    biking_data <- df %>%
      filter(activity_type == "Fietsrit" & !is.na(avg_speed) & !is.na(avg_power))
    
    if (nrow(biking_data) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(biking_data, aes(x = avg_speed, y = avg_power)) +
      geom_point(alpha = 0.6, color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Speed vs Power",
           x = "Average Speed (km/h)", y = "Average Power (W)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$elevation_profile_plot <- renderPlotly({
    req(data())
    df <- data()
    
    elevation_data <- df %>%
      filter(activity_type == "Fietsrit" & !is.na(total_ascent)) %>%
      mutate(elevation_category = case_when(
        total_ascent < 100 ~ "Flat (0-100m)",
        total_ascent < 300 ~ "Rolling (100-300m)",
        total_ascent < 600 ~ "Hilly (300-600m)",
        total_ascent < 1000 ~ "Mountainous (600-1000m)",
        TRUE ~ "Very Mountainous (>1000m)"
      ))
    
    if (nrow(elevation_data) == 0) {
      return(plotly_empty())
    }
    
    elev_counts <- elevation_data %>% count(elevation_category)
    
    p <- ggplot(elev_counts, aes(x = elevation_category, y = n, fill = elevation_category)) +
      geom_col(alpha = 0.7) +
      labs(title = "Elevation Profile Distribution",
           x = "Elevation Category", y = "Number of Rides") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
    
    ggplotly(p)
  })
  
  output$power_distribution <- renderPlotly({
    req(data())
    df <- data()
    
    power_data <- df %>%
      filter(activity_type == "Fietsrit" & !is.na(avg_power) & avg_power > 0)
    
    if (nrow(power_data) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(power_data, aes(x = avg_power)) +
      geom_histogram(bins = 20, fill = "orange", alpha = 0.7) +
      labs(title = "Power Distribution",
           x = "Average Power (W)", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$speed_distance_plot <- renderPlotly({
    req(data())
    df <- data()
    
    biking_data <- df %>%
      filter(activity_type == "Fietsrit" & !is.na(avg_speed) & !is.na(distance_km) & 
               avg_speed > 0 & distance_km > 0)
    
    if (nrow(biking_data) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(biking_data, aes(x = distance_km, y = avg_speed)) +
      geom_point(alpha = 0.6, color = "darkblue") +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      labs(title = "Distance vs Speed",
           x = "Distance (km)", y = "Average Speed (km/h)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$biking_speed_trends <- renderPlotly({
    req(data())
    df <- data()
    
    biking_trends <- df %>%
      filter(activity_type == "Fietsrit" & !is.na(avg_speed) & !is.na(activity_date) & 
               avg_speed > 0) %>%
      arrange(activity_date) %>%
      mutate(rolling_speed = zoo::rollmean(avg_speed, k = 10, fill = NA, align = "right"))
    
    if (nrow(biking_trends) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(biking_trends, aes(x = activity_date)) +
      geom_point(aes(y = avg_speed), alpha = 0.3, color = "gray") +
      geom_line(aes(y = rolling_speed), color = "blue", size = 1) +
      labs(title = "Biking Speed Trends Over Time",
           x = "Date", y = "Average Speed (km/h)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$monthly_biking_volume <- renderPlotly({
    req(data())
    df <- data()
    
    monthly_biking <- df %>%
      filter(activity_type == "Fietsrit" & !is.na(distance_km) & !is.na(month)) %>%
      group_by(year, month) %>%
      summarise(total_distance = sum(distance_km, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(date = as.Date(paste(year, month, "01"), format = "%Y %b %d"))
    
    if (nrow(monthly_biking) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(monthly_biking, aes(x = date, y = total_distance)) +
      geom_col(fill = "darkblue", alpha = 0.7) +
      labs(title = "Monthly Biking Volume",
           x = "Month", y = "Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Swimming Performance Value Boxes
  output$swimming_total_distance <- renderValueBox({
    req(data())
    df <- data()
    swimming_data <- df %>% filter(activity_type == "Zwemmen")
    total_dist <- round(sum(swimming_data$distance / 1000, na.rm = TRUE), 2)
    
    valueBox(
      value = paste(total_dist, "km"),
      subtitle = "Total Swimming Distance",
      icon = icon("swimmer"),
      color = "aqua"
    )
  })
  
  output$swimming_avg_pace <- renderValueBox({
    req(data())
    df <- data()
    swimming_data <- df %>%
      filter(activity_type == "Zwemmen", !is.na(moving_time), !is.na(distance), distance > 0)
    
    if (nrow(swimming_data) == 0) {
      return(valueBox("–", "Average Pace", icon = icon("stopwatch"), color = "teal"))
    }
    
    total_time <- sum(swimming_data$moving_time, na.rm = TRUE)
    total_distance <- sum(swimming_data$distance, na.rm = TRUE)
    pace_per_100m <- (total_time / total_distance) * 100  # in seconds per 100m
    
    mins <- floor(pace_per_100m / 60)
    secs <- round(pace_per_100m %% 60)
    formatted_pace <- sprintf("%d:%02d min/100m", mins, secs)
    
    valueBox(
      value = formatted_pace,
      subtitle = "Average Pace",
      icon = icon("stopwatch"),
      color = "teal"
    )
  })
  
  output$swimming_total_time <- renderValueBox({
    req(data())
    df <- data()
    swimming_data <- df %>% filter(activity_type == "Zwemmen")
    total_secs <- sum(swimming_data$moving_time, na.rm = TRUE)
    
    hrs <- floor(total_secs / 3600)
    mins <- floor((total_secs %% 3600) / 60)
    secs <- round(total_secs %% 60)
    
    formatted_time <- sprintf("%02d:%02d:%02d", hrs, mins, secs)
    
    valueBox(
      value = formatted_time,
      subtitle = "Total Swimming Time",
      icon = icon("clock"),
      color = "navy"
    )
  })
  
  
  # Swimming Performance Plots (simplified due to typical data limitations)
  output$swimming_stroke_plot <- renderPlotly({
    # This would require stroke data which is not typically in basic Strava exports
    return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "Swimming Stroke Analysis",
                    xaxis = list(title = "Stroke data not available in basic export"),
                    yaxis = list(title = "")))
  })
  
  output$pool_openwater_plot <- renderPlotly({
    req(data())
    df <- data()
    
    swimming_data <- df %>%
      filter(activity_type == "Zwemmen") %>%
      mutate(swim_type = ifelse(!is.na(pool_length), "Pool", "Open Water"))
    
    if (nrow(swimming_data) == 0) {
      return(plotly_empty())
    }
    
    swim_counts <- swimming_data %>% count(swim_type)
    
    p <- ggplot(swim_counts, aes(x = swim_type, y = n, fill = swim_type)) +
      geom_col(alpha = 0.7) +
      labs(title = "Pool vs Open Water Swimming",
           x = "Swimming Type", y = "Number of Sessions") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$swimming_pace_distribution <- renderPlotly({
    req(data())
    df <- data()
    
    pace_data <- df %>%
      filter(activity_type == "Zwemmen", !is.na(distance), !is.na(moving_time), distance > 0)
    
    if (nrow(pace_data) == 0) {
      return(plotly_empty())
    }
    
    pace_data <- pace_data %>%
      mutate(pace_per_100m = (moving_time / distance) * 100 / 60)  # min per 100m
    
    p <- ggplot(pace_data, aes(x = pace_per_100m)) +
      geom_histogram(bins = 15, fill = "cyan", alpha = 0.7) +
      labs(title = "Swimming Pace Distribution",
           x = "Pace (min/100m)", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$swimming_distance_session <- renderPlotly({
    req(data())
    df <- data()
    
    distance_data <- df %>%
      filter(activity_type == "Zwemmen" & !is.na(distance_km) & distance_km > 0)
    
    if (nrow(distance_data) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(distance_data, aes(x = seq_along(distance_km), y = distance_km * 1000)) +
      geom_col(fill = "darkturquoise", alpha = 0.7) +
      labs(title = "Distance per Swimming Session",
           x = "Session", y = "Distance (m)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$swimming_progress <- renderPlotly({
    req(data())
    df <- data()
    
    swimming_progress <- df %>%
      filter(activity_type == "Zwemmen" & !is.na(activity_date) & !is.na(distance_km)) %>%
      arrange(activity_date) %>%
      mutate(cumulative_distance = cumsum(distance_km))
    
    if (nrow(swimming_progress) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(swimming_progress, aes(x = activity_date, y = cumulative_distance)) +
      geom_line(color = "darkblue", size = 1) +
      labs(title = "Swimming Progress Over Time",
           x = "Date", y = "Cumulative Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$monthly_swimming_volume <- renderPlotly({
    req(data())
    df <- data()
    
    monthly_swimming <- df %>%
      filter(activity_type == "Zwemmen" & !is.na(distance_km) & !is.na(month)) %>%
      group_by(year, month) %>%
      summarise(total_distance = sum(distance_km, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(date = as.Date(paste(year, month, "01"), format = "%Y %b %d"))
    
    if (nrow(monthly_swimming) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(monthly_swimming, aes(x = date, y = total_distance)) +
      geom_col(fill = "navy", alpha = 0.7) +
      labs(title = "Monthly Swimming Volume",
           x = "Month", y = "Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Trends Plots
  output$cumulative_distance_plot <- renderPlotly({
    req(data())
    df <- data()
    
    cumulative_data <- df %>%
      filter(!is.na(distance_km) & !is.na(activity_date)) %>%
      arrange(activity_date) %>%
      mutate(cumulative_distance = cumsum(distance_km))
    
    p <- ggplot(cumulative_data, aes(x = activity_date, y = cumulative_distance)) +
      geom_line(color = "darkgreen", size = 1) +
      labs(title = "Cumulative Distance Over Time",
           x = "Date", y = "Cumulative Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$distance_trends_plot <- renderPlotly({
    req(data())
    df <- data()
    
    monthly_trends <- df %>%
      filter(!is.na(distance_km) & !is.na(month)) %>%
      group_by(year, month, activity_type) %>%
      summarise(total_distance = sum(distance_km, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(date = as.Date(paste(year, month, "01"), format = "%Y %b %d"))
    
    p <- ggplot(monthly_trends, aes(x = date, y = total_distance, color = activity_type)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "Distance Trends by Activity Type",
           x = "Date", y = "Distance (km)", color = "Activity Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$performance_trends_plot <- renderPlotly({
    req(data())
    df <- data()
    
    performance_data <- df %>%
      filter(!is.na(activity_date) & !is.na(avg_speed)) %>%
      arrange(activity_date) %>%
      mutate(rolling_speed = zoo::rollmean(avg_speed, k = 30, fill = NA, align = "right"))
    
    p <- ggplot(performance_data, aes(x = activity_date)) +
      geom_point(aes(y = avg_speed), alpha = 0.3, color = "gray") +
      geom_line(aes(y = rolling_speed), color = "red", size = 1) +
      labs(title = "Performance Trends (30-day Rolling Average Speed)",
           x = "Date", y = "Speed (km/h)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$activity_frequency_plot <- renderPlotly({
    req(data())
    df <- data()
    
    frequency_data <- df %>%
      filter(!is.na(month)) %>%
      group_by(year, month) %>%
      summarise(activity_count = n(),
                .groups = 'drop') %>%
      mutate(date = as.Date(paste(year, month, "01"), format = "%Y %b %d"))
    
    p <- ggplot(frequency_data, aes(x = date, y = activity_count)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      labs(title = "Activity Frequency by Month",
           x = "Month", y = "Number of Activities") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Advanced Analysis
  output$correlation_plot <- renderPlot({
    req(data())
    df <- data()
    
    numeric_cols <- df %>%
      select_if(is.numeric) %>%
      select(-any_of(c("activity_id", "year", "month", "week", "hour"))) %>%
      select(where(~ sum(!is.na(.)) > 10))
    
    if (ncol(numeric_cols) < 2) {
      plot.new()
      text(0.5, 0.5, "Not enough numeric variables for correlation analysis", cex = 1.2)
      return()
    }
    
    cor_matrix <- cor(numeric_cols, use = "complete.obs")
    
    corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                       order = "hclust", tl.cex = 0.8, tl.col = "black")
  })
  
  output$custom_plot <- renderPlotly({
    req(data(), input$x_var, input$y_var)
    df <- data()
    
    if (input$x_var == input$y_var) {
      return(plotly_empty())
    }
    
    plot_data <- df %>%
      filter(!is.na(.data[[input$x_var]]) & !is.na(.data[[input$y_var]]))
    
    if (nrow(plot_data) == 0) {
      return(plotly_empty())
    }
    
    # Create base plot
    if (input$color_var != "None" && input$color_var %in% names(df)) {
      p <- ggplot(plot_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], 
                                 color = .data[[input$color_var]]))
    } else {
      p <- ggplot(plot_data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]]))
    }
    
    p <- p + geom_point(alpha = 0.6)
    
    if (input$add_trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    p <- p + labs(x = input$x_var, y = input$y_var) + theme_minimal()
    
    ggplotly(p)
  })
  
  # ========== JOKE STATS OUTPUTS ==========
  
  # Individual joke stats outputs
  output$big_macs <- renderText({
    req(joke_stats())
    formatC(joke_stats()$big_macs, format = "d", big.mark = ",")
  })
  
  output$beers <- renderText({
    req(joke_stats())
    formatC(joke_stats()$beers, format = "d", big.mark = ",")
  })
  
  output$pizza_slices <- renderText({
    req(joke_stats())
    formatC(joke_stats()$pizza_slices, format = "d", big.mark = ",")
  })
  
  output$chip_bags <- renderText({
    req(joke_stats())
    formatC(joke_stats()$chip_bags, format = "d", big.mark = ",")
  })
  
  output$ice_cream <- renderText({
    req(joke_stats())
    formatC(joke_stats()$ice_cream_cones, format = "d", big.mark = ",")
  })
  
  output$chocolate_bars <- renderText({
    req(joke_stats())
    formatC(joke_stats()$chocolate_bars, format = "d", big.mark = ",")
  })
  
  output$football_fields <- renderText({
    req(joke_stats())
    formatC(joke_stats()$football_fields, format = "d", big.mark = ",")
  })
  
  output$marathons <- renderText({
    req(joke_stats())
    paste0(formatC(joke_stats()$marathons, format = "f", digits = 1))
  })
  
  output$moon_progress <- renderText({
    req(joke_stats())
    paste0(formatC(joke_stats()$moon_progress, format = "f", digits = 4), "%")
  })
  
  output$netflix_hours <- renderText({
    req(joke_stats())
    formatC(joke_stats()$netflix_hours, format = "d", big.mark = ",")
  })
  
  output$burj_khalifas <- renderText({
    req(joke_stats())
    formatC(joke_stats()$burj_khalifas, format = "f", digits = 1)
  })
  
  output$eiffel_towers <- renderText({
    req(joke_stats())
    formatC(joke_stats()$eiffel_towers, format = "f", digits = 1)
  })
  
  output$elevator_rides <- renderText({
    req(joke_stats())
    formatC(joke_stats()$elevator_rides, format = "d", big.mark = ",")
  })
  
  output$stairs_climbed <- renderText({
    req(joke_stats())
    formatC(joke_stats()$stairs_climbed, format = "d", big.mark = ",")
  })
  
  # Personal bests and records
  output$best_week_bigmacs <- renderText({
    req(data())
    df <- data()
    
    weekly_bigmacs <- df %>%
      filter(!is.na(calories) & !is.na(week)) %>%
      group_by(year, week) %>%
      summarise(week_calories = sum(calories, na.rm = TRUE), .groups = 'drop') %>%
      mutate(bigmacs = floor(week_calories / 550))
    
    if (nrow(weekly_bigmacs) > 0) {
      max_bigmacs <- max(weekly_bigmacs$bigmacs, na.rm = TRUE)
      formatC(max_bigmacs, format = "d", big.mark = ",")
    } else {
      "0"
    }
  })
  
  output$longest_activity <- renderText({
    req(data())
    df <- data()
    
    if ("moving_time" %in% names(df)) {
      longest <- max(df$moving_time / 3600, na.rm = TRUE)
      if (is.finite(longest)) {
        formatC(longest, format = "f", digits = 1)
      } else {
        "0"
      }
    } else {
      "N/A"
    }
  })
  
  output$most_active_month <- renderText({
    req(data())
    df <- data()
    
    if ("month" %in% names(df)) {
      most_active <- df %>%
        filter(!is.na(month)) %>%
        count(month, sort = TRUE) %>%
        slice(1) %>%
        pull(month)
      
      if (length(most_active) > 0) {
        as.character(most_active)
      } else {
        "N/A"
      }
    } else {
      "N/A"
    }
  })
  
  # Motivational quotes
  motivational_quotes <- c(
    "💪 You're not just burning calories, you're forging your legend!",
    "🔥 Every step forward is a step away from yesterday's excuses!",
    "🚀 Your body achieves what your mind believes!",
    "⚡ Pain is weakness leaving the body!",
    "🏆 Champions train when others rest!",
    "💥 The only bad workout is the one you didn't do!",
    "🌟 Your potential is endless, your dedication shows it!",
    "🎯 Progress, not perfection - every activity counts!",
    "🔋 Powered by determination, fueled by passion!",
    "🎪 Making healthy choices one Big Mac at a time!"
  )
  
  output$motivational_quote <- renderUI({
    req(data())
    quote_index <- (nrow(data()) %% length(motivational_quotes)) + 1
    h3(motivational_quotes[quote_index], style = "text-align: center; margin: 0;")
  })
  
  # Burn-off meter
  output$burnoff_meter <- renderPlotly({
    req(data())
    df <- data()
    
    # Get current week calories
    current_week <- df %>%
      filter(!is.na(calories) & !is.na(activity_date)) %>%
      filter(activity_date >= (max(activity_date, na.rm = TRUE) - days(7))) %>%
      summarise(week_calories = sum(calories, na.rm = TRUE)) %>%
      pull(week_calories)
    
    if (length(current_week) == 0) current_week <- 0
    
    # Calculate how much junk food this "pays for"
    weekly_target <- 2000  # Target weekly calorie burn
    progress <- min(current_week / weekly_target, 1) * 100
    
    # Create gauge chart
    fig <- plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = progress,
      title = list(text = "Weekly Calorie Goal"),
      gauge = list(
        axis = list(range = c(0, 100)),
        bar = list(color = "darkgreen"),
        steps = list(
          list(range = c(0, 50), color = "lightgray"),
          list(range = c(50, 100), color = "lightgreen")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 100
        )
      )
    ) %>%
      layout(height = 200, margin = list(l = 20, r = 20, t = 20, b = 20))
    
    fig
  })
  
  output$burnoff_text <- renderText({
    req(data())
    df <- data()
    
    current_week <- df %>%
      filter(!is.na(calories) & !is.na(activity_date)) %>%
      filter(activity_date >= (max(activity_date, na.rm = TRUE) - days(7))) %>%
      summarise(week_calories = sum(calories, na.rm = TRUE)) %>%
      pull(week_calories)
    
    if (length(current_week) == 0) current_week <- 0
    
    bigmacs_this_week <- floor(current_week / 550)
    
    paste0("This week you've earned ", bigmacs_this_week, " Big Macs! 🍔")
  })
  
  # Trophy cabinet
  output$trophy_cabinet <- renderUI({
    req(data(), joke_stats())
    df <- data()
    stats <- joke_stats()
    
    achievements <- list()
    
    # Check various achievements
    if (stats$marathons >= 1) {
      achievements <- append(achievements, "🏃‍♂️ Marathon Finisher")
    }
    if (stats$big_macs >= 100) {
      achievements <- append(achievements, "🍔 Big Mac Hunter")
    }
    if (stats$burj_khalifas >= 1) {
      achievements <- append(achievements, "🏗️ Skyscraper Climber")
    }
    if (stats$moon_progress >= 0.01) {
      achievements <- append(achievements, "🚀 Space Explorer")
    }
    if (nrow(df) >= 100) {
      achievements <- append(achievements, "💪 Fitness Enthusiast")
    }
    if (stats$beers >= 1000) {
      achievements <- append(achievements, "🍺 Brewery Defeater")
    }
    
    if (length(achievements) == 0) {
      achievements <- list("🌟 Getting Started")
    }
    
    # Create achievement badges
    badges <- map(achievements, ~ {
      div(class = "badge-item", 
          style = "color: white; font-weight: bold;",
          .)
    })
    
    tagList(badges)
  })
  
  # Monthly Big Mac chart
  output$monthly_bigmac_chart <- renderPlotly({
    req(data())
    df <- data()
    
    monthly_bigmacs <- df %>%
      filter(!is.na(calories) & !is.na(month)) %>%
      group_by(year, month) %>%
      summarise(month_calories = sum(calories, na.rm = TRUE), .groups = 'drop') %>%
      mutate(
        bigmacs = floor(month_calories / 550),
        date = as.Date(paste(year, month, "01"), format = "%Y %b %d")
      ) %>%
      arrange(date)
    
    if (nrow(monthly_bigmacs) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(monthly_bigmacs, aes(x = date, y = bigmacs)) +
      geom_col(fill = "#ff6b6b", alpha = 0.8) +
      geom_text(aes(label = paste0(bigmacs, " 🍔")), 
                vjust = -0.5, size = 3, color = "darkred") +
      labs(title = "Monthly Big Macs Earned",
           x = "Month", y = "Big Macs") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Achievement progress
  output$achievement_progress <- renderPlotly({
    req(joke_stats())
    stats <- joke_stats()
    
    # Define achievement targets
    targets <- data.frame(
      achievement = c("Big Macs", "Marathons", "Burj Khalifas", "Moon Progress"),
      current = c(stats$big_macs, stats$marathons, stats$burj_khalifas, stats$moon_progress),
      target = c(1000, 10, 5, 1),
      unit = c("", "", "", "%")
    ) %>%
      mutate(
        progress = pmin(current / target * 100, 100),
        label = paste0(round(current, 2), "/", target, " ", unit)
      )
    
    p <- ggplot(targets, aes(x = achievement, y = progress, fill = achievement)) +
      geom_col(alpha = 0.7) +
      geom_text(aes(label = paste0(round(progress, 1), "%")), 
                vjust = -0.5, fontface = "bold") +
      ylim(0, 110) +
      labs(title = "Achievement Progress",
           x = "", y = "Progress (%)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)