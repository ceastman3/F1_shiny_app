library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(leaflet)


# Reads in data
drivers = read.csv("./data/drivers.csv")
status = read.csv("./data/status.csv")
races = read.csv("./data/races.csv")
seasons = read.csv("./data/seasons.csv")
constructors = read.csv("./data/constructors.csv")
circuits = read.csv("./data/circuits.csv")
results = read.csv("./data/results.csv")

drivers = drivers |>
  mutate(full_name = paste(forename, surname)) |>
  select(-url)

races = races |>
  select("raceId", "year", "round", "circuitId", "name", "date")

races = left_join(races, circuits, by="circuitId") |>
  rename(
    race_name = name.x,
    circuit_name = name.y
  ) |>
  select(-alt, -url)

seasons <- seasons[order(seasons$year), ]
row.names(seasons) <- NULL

results = left_join(results, drivers, by="driverId") |>
  rename( driver_num = number.y ) |>
  select(-number.x)

circuits = circuits |>
  select(-url)

constructors = constructors |>
  select(constructorId, constructorRef, constr_name=name, constr_nationality=nationality)

results = left_join(results, constructors, by="constructorId")

results = left_join(results, status, by="statusId")
results = left_join(results, races, by="raceId")





ui = fluidPage(
  titlePanel(
    # img(src="f1_logo.png"),
    h1("Formula 1 Season Statistics (1950-2023)", style={'color:white; 
                                              background-color:#FF1E00;
                                              border-radius:5px;
                                              padding:5px'})
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select Season", unique(seasons$year), selected = "2023"),
      verbatimTextOutput("summary")
    ),
    mainPanel(
      h3("Total Points Per Driver", style = "color:white; background-color:#FF1E00; border-radius:5px; padding:5px"),
      plotOutput("season_standings"),
    )
  ),
  fluidRow(
    column(4,
     h3("Race Schedule", style = "color:white; background-color:#FF1E00; border-radius:5px; padding:5px"),
     plotOutput("timelinePlot")
    ),
    column(8,
     h3("Circuit Locations", style = "color:white; background-color:#FF1E00; border-radius:5px; padding:5px"),
     leafletOutput("map")
    )
  ),
  fluidRow(
    style={'background-color:#FF1E00;
            margin-top:10px;
            padding:5px;'},
    column(3,
           h3("Race Results", style = "color:white; background-color:#FF1E00; border-radius:5px; padding:5px")
           ),
     column(9,
            selectizeInput("race", "Select a Race", NULL, multiple = FALSE)
            )
  ),
  fluidRow(
    style={'margin:5px;'},
     dataTableOutput("position_table")
  )
)

server = function(input, output, session) {
  
  filteredRaces = reactive({
    req(input$season)
    subset(races, year == input$season)$race_name
  })
  
  seasonResults = reactive({
    req(input$season)
    results |>
      filter(year==input$season)
  })
  
  seasonRaces = reactive({
    req(input$season)
    races |>
      filter(year==input$season)
  })
  
  observeEvent(input$season, {
    updateSelectInput(session, "race", choices = filteredRaces())
  })
  
  output$map = renderLeaflet({
    leaflet(data=seasonRaces()) |>
      addTiles() |>
      setView(lng = -30, lat = 30, zoom = 2) |>
      addCircleMarkers(radius = 5,
                       color = "#FF1E00",
                       popup=~paste("<b>Race:</b>", race_name,
                                    "<br><b>Circuit:</b>", circuit_name, 
                                    "<br><b>City:</b>", location, 
                                    "<br><b>Country:</b>", country,
                                    "<br><b>Date:</b>", date))
  })
  
  output$season_standings = renderPlot({
    season_results = seasonResults() |>
      group_by(driverId, full_name, constr_name) |>
      summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")
    
    ggplot(season_results, aes(y=reorder(full_name, +total_points), x=total_points, fill = constr_name)) +
      geom_bar(stat="identity") +
      geom_text(aes(label = total_points), hjust = -0.2, size = 3) +
      labs(x = "Total Points", 
           y = "Driver", 
           title = "Total Points per Driver",
           fill = "Constructor") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8),
            legend.position = "right")
  })
  
  output$position_table = renderDataTable({
    seasonResults() %>%
      filter(race_name == input$race) |>
      select(Position=positionText,
             Forname=forename,
             Surname=surname,
             Number=driver_num,
             Nationality=nationality,
             Constructor=constr_name,
             Time=time,
             Points=points,
             Status=status)
  })
  
  output$timelinePlot <- renderPlot({
    time_races = seasonRaces() |>
      select(date, race_name)
    
    ggplot(time_races, aes(x = 1, y = date, label = race_name)) +
      geom_segment(aes(x = 1, xend = 1, y = min(date), yend = max(date)), color = "grey", alpha=0.8) +
      geom_point(color = "red", size = 2) +
      geom_text(nudge_x = 0, vjust = 0.5, hjust=-0.1) +
      geom_text(aes(label = as.character(date)), nudge_y = -0.05, hjust = 1.1, color = "black") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      xlim(0.5, 2) 
  })
  
  output$summary = renderText({
    res = seasonResults() |>
      group_by(driverId, full_name, constr_name) |>
      summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")
    
    
    drvr = res[res$total_points == max(res$total_points),]
    paste(input$season, "Season Champion:",
          "\n\nDriver:", drvr$full_name,
          "\nConstructor:", drvr$constr_name,
          "\nTotal points:", drvr$total_points)
    # paste(input$season, "Season Champion:")
    # paste("Driver", drvr$full_name)
  })
}

shinyApp(ui, server)