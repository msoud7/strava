# Enhanced Strava Analytics Shiny App with Joke Stats
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
      menuItem("Performance", tabName = "performance", icon = icon("heartbeat")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Advanced Analysis", tabName = "advanced", icon = icon("calculator")),
      menuItem("🍔 Joke Stats", tabName = "joke_stats", icon = icon("laugh-beam"))
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
      
      # Performance Tab
      tabItem(tabName = "performance",
              fluidRow(
                box(
                  title = "Heart Rate vs Distance", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("hr_distance_plot"))
                ),
                box(
                  title = "Heart Rate Zones", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("hr_zones_plot"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Pace vs Heart Rate (Running)", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("pace_hr_plot"))
                ),
                box(
                  title = "Pace Distribution", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("pace_distribution"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Training Stress Over Time", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(plotlyOutput("training_stress_plot"))
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
                  title = "Pace Improvement", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("pace_trends_plot"))
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
          calories = ifelse(is.na(calories), 0, calories)
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
  
  output$total_activities <- renderValueBox({
    req(data())
    valueBox(
      value = nrow(data()),
      subtitle = "Total Activities",
      icon = icon("running"),
      color = "green"
    )
  })
  
  output$avg_duration <- renderValueBox({
    req(data())
    df <- data()
    avg_dur <- round(mean(df$elapsed_time, na.rm = TRUE) / 60, 1)
    
    valueBox(
      value = paste(avg_dur, "min"),
      subtitle = "Avg Duration",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  # JOKE STATS OUTPUTS
  
  # Motivational quotes
  motivational_quotes <- c(
    "🏃‍♂️ Run like your WiFi depends on it!",
    "🍕 You've burned enough calories for pizza tonight!",
    "🍺 Cheers! You've earned those beers!",
    "🚀 You're literally out of this world! (Well, 0.0001% to the moon)",
    "🏔️ You've climbed higher than most people's expectations!",
    "📺 Netflix binge guilt = officially canceled!",
    "🍔 Big Mac calories? What Big Mac calories?",
    "🛗 Taking the elevator? That's for quitters!",
    "🏃‍♀️ Sweat is just your fat crying tears of joy!",
    "🍫 Chocolate bars beware - you've been mathematically defeated!"
  )
  
  output$motivational_quote <- renderUI({
    set.seed(as.numeric(Sys.Date()))
    sample(motivational_quotes, 1)
  })
  
  # Individual joke stat outputs
  output$big_macs <- renderText({
    req(joke_stats())
    format(joke_stats()$big_macs, big.mark = ",")
  })
  
  output$beers <- renderText({
    req(joke_stats())
    format(joke_stats()$beers, big.mark = ",")
  })
  
  output$pizza_slices <- renderText({
    req(joke_stats())
    format(joke_stats()$pizza_slices, big.mark = ",")
  })
  
  output$chip_bags <- renderText({
    req(joke_stats())
    format(joke_stats()$chip_bags, big.mark = ",")
  })
  
  output$ice_cream <- renderText({
    req(joke_stats())
    format(joke_stats()$ice_cream_cones, big.mark = ",")
  })
  
  output$chocolate_bars <- renderText({
    req(joke_stats())
    format(joke_stats()$chocolate_bars, big.mark = ",")
  })
  
  output$football_fields <- renderText({
    req(joke_stats())
    format(joke_stats()$football_fields, big.mark = ",")
  })
  
  output$marathons <- renderText({
    req(joke_stats())
    joke_stats()$marathons
  })
  
  output$moon_progress <- renderText({
    req(joke_stats())
    paste0(joke_stats()$moon_progress, "%")
  })
  
  output$netflix_hours <- renderText({
    req(joke_stats())
    format(joke_stats()$netflix_hours, big.mark = ",")
  })
  
  output$burj_khalifas <- renderText({
    req(joke_stats())
    joke_stats()$burj_khalifas
  })
  
  output$eiffel_towers <- renderText({
    req(joke_stats())
    joke_stats()$eiffel_towers
  })
  
  output$elevator_rides <- renderText({
    req(joke_stats())
    format(joke_stats()$elevator_rides, big.mark = ",")
  })
  
  output$stairs_climbed <- renderText({
    req(joke_stats())
    format(joke_stats()$stairs_climbed, big.mark = ",")
  })
  
  # Personal bests (continuing from the cut-off point)
  output$best_week_bigmacs <- renderText({
    req(data())
    df <- data()
    if(nrow(df) > 0 && "calories" %in% names(df) && "activity_date" %in% names(df)) {
      weekly_calories <- df %>%
        mutate(week_year = paste(year(activity_date), week(activity_date), sep = "-")) %>%
        group_by(week_year) %>%
        summarise(total_calories = sum(calories, na.rm = TRUE), .groups = 'drop')
      
      max_week_calories <- max(weekly_calories$total_calories, na.rm = TRUE)
      max_week_bigmacs <- floor(max_week_calories / 550)
      format(max_week_bigmacs, big.mark = ",")
    } else {
      "0"
    }
  })
  
  output$longest_activity <- renderText({
    req(data())
    df <- data()
    if(nrow(df) > 0 && "elapsed_time" %in% names(df)) {
      max_time <- max(df$elapsed_time, na.rm = TRUE)
      round(max_time / 3600, 1)  # Convert to hours
    } else {
      "0"
    }
  })
  
  output$most_active_month <- renderText({
    req(data())
    df <- data()
    if(nrow(df) > 0 && "activity_date" %in% names(df)) {
      month_counts <- df %>%
        mutate(month_year = format(activity_date, "%B %Y")) %>%
        count(month_year, sort = TRUE)
      
      if(nrow(month_counts) > 0) {
        month_counts$month_year[1]
      } else {
        "Unknown"
      }
    } else {
      "Unknown"
    }
  })
  
  # Burn-off meter
  output$burnoff_meter <- renderPlotly({
    req(data())
    df <- data()
    
    # Calculate this week's calories
    current_week <- df %>%
      filter(activity_date >= floor_date(Sys.Date(), "week")) %>%
      summarise(weekly_calories = sum(calories, na.rm = TRUE)) %>%
      pull(weekly_calories)
    
    # Create gauge chart
    fig <- plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = current_week,
      domain = list(x = c(0, 1), y = c(0, 1)),
      title = list(text = "Weekly Calories Burned"),
      delta = list(reference = 2000),  # Reference to average weekly goal
      gauge = list(
        axis = list(range = list(NULL, 3000)),
        bar = list(color = "#ff6b6b"),
        steps = list(
          list(range = c(0, 1000), color = "#ffe0e0"),
          list(range = c(1000, 2000), color = "#ffb3b3"),
          list(range = c(2000, 3000), color = "#ff8080")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 2000
        )
      )
    )
    
    fig %>% layout(margin = list(l=20,r=30))
  })
  
  output$burnoff_text <- renderText({
    req(data())
    df <- data()
    
    current_week <- df %>%
      filter(activity_date >= floor_date(Sys.Date(), "week")) %>%
      summarise(weekly_calories = sum(calories, na.rm = TRUE)) %>%
      pull(weekly_calories)
    
    big_macs_this_week <- floor(current_week / 550)
    paste("That's", big_macs_this_week, "Big Macs this week! 🍔")
  })
  
  # Trophy cabinet
  output$trophy_cabinet <- renderUI({
    req(joke_stats())
    stats <- joke_stats()
    
    achievements <- list()
    
    if(stats$big_macs >= 100) achievements <- c(achievements, "🏆 Century Big Mac Burner")
    if(stats$marathons >= 1) achievements <- c(achievements, "🏃‍♂️ Marathon Finisher")
    if(stats$burj_khalifas >= 1) achievements <- c(achievements, "🏗️ Skyscraper Climber")
    if(stats$eiffel_towers >= 1) achievements <- c(achievements, "🗼 Get Eiffel Towered")
    if(stats$moon_progress >= 0.001) achievements <- c(achievements, "🚀 Space Cadet")
    if(stats$netflix_hours >= 100) achievements <- c(achievements, "📺 Netflix Avoider")
    if(stats$beers >= 500) achievements <- c(achievements, "🍺 Brewery Defeater")
    if(stats$football_fields >= 1000) achievements <- c(achievements, "⚽ Field General")
    if(stats$stairs_climbed >= 10000) achievements <- c(achievements, "🪜 Stair Master")
    
    if(length(achievements) == 0) {
      achievements <- c("🥉 Getting Started!")
    }
    
    # Create achievement badges
    badges <- lapply(achievements, function(achievement) {
      div(class = "badge-item", achievement)
    })
    
    do.call(tagList, badges)
  })
  
  # Monthly Big Mac chart
  output$monthly_bigmac_chart <- renderPlotly({
    req(data())
    df <- data()
    
    monthly_data <- df %>%
      mutate(month_year = format(activity_date, "%Y-%m")) %>%
      group_by(month_year) %>%
      summarise(
        total_calories = sum(calories, na.rm = TRUE),
        big_macs = floor(sum(calories, na.rm = TRUE) / 550),
        .groups = 'drop'
      ) %>%
      arrange(month_year) %>%
      tail(12)  # Last 12 months
    
    p <- ggplot(monthly_data, aes(x = month_year, y = big_macs)) +
      geom_col(fill = "#ff6b6b", alpha = 0.8) +
      geom_text(aes(label = big_macs), vjust = -0.5, size = 3) +
      labs(title = "Monthly Big Mac Burn-off",
           x = "Month", y = "Big Macs Burned") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Achievement progress chart
  output$achievement_progress <- renderPlotly({
    req(joke_stats())
    stats <- joke_stats()
    
    # Define achievement targets and current progress
    achievements <- data.frame(
      Achievement = c("Century Big Mac", "10 Marathons", "Space Progress", "Burj Khalifa", "1000 Beers"),
      Target = c(100, 10, 0.01, 1, 1000),
      Current = c(stats$big_macs, stats$marathons, stats$moon_progress/100, stats$burj_khalifas, stats$beers),
      stringsAsFactors = FALSE
    )
    
    achievements$Progress <- pmin(achievements$Current / achievements$Target * 100, 100)
    achievements$Achievement <- factor(achievements$Achievement, levels = achievements$Achievement)
    
    p <- ggplot(achievements, aes(x = Achievement, y = Progress)) +
      geom_col(fill = "#4ecdc4", alpha = 0.8) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "gold", size = 1) +
      labs(title = "Achievement Progress", x = "", y = "Progress (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 120)
    
    ggplotly(p)
  })
  
  # Data summary table
  output$summary_table <- DT::renderDataTable({
    req(data())
    df <- data()
    
    summary_data <- df %>%
      group_by(activity_type) %>%
      summarise(
        Count = n(),
        `Total Distance (km)` = round(sum(distance_km, na.rm = TRUE), 1),
        `Avg Distance (km)` = round(mean(distance_km, na.rm = TRUE), 1),
        `Total Time (hrs)` = round(sum(elapsed_time, na.rm = TRUE) / 3600, 1),
        `Total Calories` = round(sum(calories, na.rm = TRUE), 0),
        .groups = 'drop'
      ) %>%
      arrange(desc(Count))
    
    DT::datatable(summary_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Activity types plot
  output$activity_types_plot <- renderPlotly({
    req(data())
    df <- data()
    
    activity_counts <- df %>%
      count(activity_type, sort = TRUE) %>%
      head(10)
    
    p <- ggplot(activity_counts, aes(x = reorder(activity_type, n), y = n)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      coord_flip() +
      labs(title = "Activity Types", x = "Activity Type", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Data preview table
  output$data_preview <- DT::renderDataTable({
    req(data())
    DT::datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Filtered data for training tab
  filtered_data <- reactive({
    req(data())
    df <- data()
    
    if(!is.null(input$year_filter) && input$year_filter != "") {
      df <- df %>% filter(year == input$year_filter)
    }
    
    if(!is.null(input$activity_filter) && length(input$activity_filter) > 0) {
      df <- df %>% filter(activity_type %in% input$activity_filter)
    }
    
    return(df)
  })
  
  # Monthly distance plot
  output$monthly_distance_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    monthly_data <- df %>%
      group_by(year, month) %>%
      summarise(total_distance = sum(distance_km, na.rm = TRUE), .groups = 'drop') %>%
      mutate(month_year = as.Date(paste(year, as.numeric(month), "01", sep = "-")))
    
    p <- ggplot(monthly_data, aes(x = month_year, y = total_distance)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(title = "Monthly Distance", x = "Month", y = "Distance (km)") +
      theme_minimal() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months")
    
    ggplotly(p)
  })
  
  # Weekly activity pattern
  output$weekday_pattern <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    weekday_data <- df %>%
      count(weekday) %>%
      mutate(weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    
    p <- ggplot(weekday_data, aes(x = weekday, y = n)) +
      geom_col(fill = "coral", alpha = 0.8) +
      labs(title = "Activities by Day of Week", x = "Day", y = "Number of Activities") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Activity heatmap
  output$activity_heatmap <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    heatmap_data <- df %>%
      count(weekday, hour) %>%
      mutate(weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
    
    p <- ggplot(heatmap_data, aes(x = hour, y = weekday, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Activities") +
      labs(title = "Activity Heatmap", x = "Hour of Day", y = "Day of Week") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Monthly elapsed time plot
  output$monthly_time_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    monthly_time <- df %>%
      group_by(year, month) %>%
      summarise(total_time = sum(elapsed_time, na.rm = TRUE) / 3600, .groups = 'drop') %>%
      mutate(month_year = as.Date(paste(year, as.numeric(month), "01", sep = "-")))
    
    p <- ggplot(monthly_time, aes(x = month_year, y = total_time)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_point(color = "darkgreen", size = 2) +
      labs(title = "Monthly Training Time", x = "Month", y = "Time (hours)") +
      theme_minimal() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months")
    
    ggplotly(p)
  })
  
  # Duration distribution
  output$duration_distribution <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = elapsed_time / 60)) +
      geom_histogram(bins = 30, fill = "purple", alpha = 0.7) +
      labs(title = "Activity Duration Distribution", x = "Duration (minutes)", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Heart rate vs distance plot
  output$hr_distance_plot <- renderPlotly({
    req(data())
    df <- data()
    
    hr_data <- df %>%
      filter(!is.na(avg_heart_rate) & !is.na(distance_km) & distance_km > 0)
    
    if(nrow(hr_data) > 0) {
      p <- ggplot(hr_data, aes(x = distance_km, y = avg_heart_rate, color = activity_type)) +
        geom_point(alpha = 0.6) +
        labs(title = "Heart Rate vs Distance", x = "Distance (km)", y = "Average Heart Rate") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + labs(title = "No heart rate data available"))
    }
  })
  
  # Heart rate zones plot
  output$hr_zones_plot <- renderPlotly({
    req(data())
    df <- data()
    
    hr_data <- df %>%
      filter(!is.na(avg_heart_rate)) %>%
      mutate(
        hr_zone = case_when(
          avg_heart_rate < 120 ~ "Zone 1 (Recovery)",
          avg_heart_rate < 140 ~ "Zone 2 (Aerobic)",
          avg_heart_rate < 160 ~ "Zone 3 (Threshold)",
          avg_heart_rate < 180 ~ "Zone 4 (VO2 Max)",
          TRUE ~ "Zone 5 (Neuromuscular)"
        )
      )
    
    if(nrow(hr_data) > 0) {
      zone_counts <- hr_data %>% count(hr_zone)
      
      p <- ggplot(zone_counts, aes(x = hr_zone, y = n)) +
        geom_col(fill = "red", alpha = 0.7) +
        labs(title = "Heart Rate Zones", x = "Zone", y = "Number of Activities") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + labs(title = "No heart rate data available"))
    }
  })
  
  # Pace vs heart rate plot
  output$pace_hr_plot <- renderPlotly({
    req(data())
    df <- data()
    
    running_data <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(pace) & !is.na(avg_heart_rate) & pace > 0 & pace < 10)
    
    if(nrow(running_data) > 0) {
      p <- ggplot(running_data, aes(x = avg_heart_rate, y = pace)) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(title = "Pace vs Heart Rate (Running)", x = "Average Heart Rate", y = "Pace (min/km)") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + labs(title = "No running pace data available"))
    }
  })
  
  # Pace distribution
  output$pace_distribution <- renderPlotly({
    req(data())
    df <- data()
    
    pace_data <- df %>%
      filter(!is.na(pace) & pace > 0 & pace < 10)
    
    if(nrow(pace_data) > 0) {
      p <- ggplot(pace_data, aes(x = pace)) +
        geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
        labs(title = "Pace Distribution", x = "Pace (min/km)", y = "Frequency") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + labs(title = "No pace data available"))
    }
  })
  
  # Training stress over time
  output$training_stress_plot <- renderPlotly({
    req(data())
    df <- data()
    
    # Calculate training stress as a combination of duration and intensity
    stress_data <- df %>%
      filter(!is.na(elapsed_time) & !is.na(avg_heart_rate)) %>%
      mutate(
        training_stress = (elapsed_time / 3600) * (avg_heart_rate / 100),
        week_year = paste(year(activity_date), week(activity_date), sep = "-")
      ) %>%
      group_by(week_year, activity_date = floor_date(activity_date, "week")) %>%
      summarise(weekly_stress = sum(training_stress, na.rm = TRUE), .groups = 'drop') %>%
      arrange(activity_date)
    
    if(nrow(stress_data) > 0) {
      p <- ggplot(stress_data, aes(x = activity_date, y = weekly_stress)) +
        geom_line(color = "darkred", size = 1) +
        geom_point(color = "darkred", size = 2) +
        labs(title = "Weekly Training Stress", x = "Date", y = "Training Stress Score") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + labs(title = "No training stress data available"))
    }
  })
  
  # Cumulative distance plot
  output$cumulative_distance_plot <- renderPlotly({
    req(data())
    df <- data()
    
    cumulative_data <- df %>%
      arrange(activity_date) %>%
      mutate(cumulative_distance = cumsum(replace_na(distance_km, 0)))
    
    p <- ggplot(cumulative_data, aes(x = activity_date, y = cumulative_distance)) +
      geom_line(color = "darkblue", size = 1) +
      labs(title = "Cumulative Distance Over Time", x = "Date", y = "Cumulative Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distance trends plot
  output$distance_trends_plot <- renderPlotly({
    req(data())
    df <- data()
    
    p <- ggplot(df, aes(x = activity_date, y = distance_km)) +
      geom_point(alpha = 0.5, color = "steelblue") +
      geom_smooth(method = "loess", se = FALSE, color = "red") +
      labs(title = "Distance Trends Over Time", x = "Date", y = "Distance (km)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Pace trends plot
  output$pace_trends_plot <- renderPlotly({
    req(data())
    df <- data()
    
    pace_data <- df %>%
      filter(activity_type == "Hardloopsessie" & !is.na(pace) & pace > 0 & pace < 10)
    
    if(nrow(pace_data) > 0) {
      p <- ggplot(pace_data, aes(x = activity_date, y = pace)) +
        geom_point(alpha = 0.5, color = "orange") +
        geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
        labs(title = "Pace Improvement Over Time", x = "Date", y = "Pace (min/km)") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + labs(title = "No running pace data available"))
    }
  })
  
  # Activity frequency plot
  output$activity_frequency_plot <- renderPlotly({
    req(data())
    df <- data()
    
    frequency_data <- df %>%
      count(year, month) %>%
      mutate(month_year = as.Date(paste(year, as.numeric(month), "01", sep = "-")))
    
    p <- ggplot(frequency_data, aes(x = month_year, y = n)) +
      geom_col(fill = "purple", alpha = 0.7) +
      labs(title = "Activity Frequency by Month", x = "Month", y = "Number of Activities") +
      theme_minimal() +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months")
    
    ggplotly(p)
  })
  
  # Correlation plot
  output$correlation_plot <- renderPlot({
    req(data())
    df <- data()
    
    numeric_cols <- df %>%
      select_if(is.numeric) %>%
      select(distance_km, elapsed_time, avg_heart_rate, calories, total_ascent, avg_speed) %>%
      select_if(~sum(!is.na(.)) > 10)  # Only include columns with sufficient data
    
    if(ncol(numeric_cols) > 1) {
      GGally::ggpairs(numeric_cols, 
                      lower = list(continuous = "smooth_loess", combo = "box_no_facet"),
                      diag = list(continuous = "barDiag", discrete = "barDiag"),
                      upper = list(continuous = "cor", combo = "box_no_facet"))
    } else {
      ggplot() + labs(title = "Insufficient numeric data for correlation analysis")
    }
  })
  
  # Custom plot
  output$custom_plot <- renderPlotly({
    req(data(), input$x_var, input$y_var)
    df <- data()
    
    # Filter out rows where both x and y variables are not NA
    plot_data <- df %>%
      filter(!is.na(.data[[input$x_var]]) & !is.na(.data[[input$y_var]]))
    
    if(nrow(plot_data) > 0) {
      p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var))
      
      if(input$color_var != "None" && input$color_var %in% names(df)) {
        p <- p + aes_string(color = input$color_var)
      }
      
      p <- p + geom_point(alpha = 0.6)
      
      if(input$add_trend) {
        p <- p + geom_smooth(method = "lm", se = FALSE)
      }
      
      p <- p + theme_minimal() +
        labs(title = paste("Custom Plot:", input$x_var, "vs", input$y_var))
      
      ggplotly(p)
    } else {
      ggplotly(ggplot() + labs(title = "No data available for selected variables"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)