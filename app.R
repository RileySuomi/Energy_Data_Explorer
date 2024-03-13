#
# Created by Riley Suomi.
# Data Science
#

if (!require(plotly)) install.packages('plotly')
if (!require(shiny)) install.packages('shiny')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(rnaturalearth)) install.packages('rnaturalearth')
if (!require(rnaturalearthdata)) install.packages('rnaturalearthdata')

library(shiny)
library(tidyverse)
library(plotly)
library("rnaturalearth")
library(rnaturalearthdata)

# data being used

energy_data = read_csv("owid-energy-data.csv")
consumption_data = read_csv("consumption-co2-per-capita-vs-gdppc.csv")

# the world data 

world_countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name != "Antarctica")

# the base layout of world map

my_worldmap_theme <- function() {
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )
}

my_cutpoints <- c(0, 1000, 3000, 10000, 30000, 100000, 110000)

# ----------
# Map for the world map of energy usage
# ----------

energy_map <- function(year_slide) {
  
  map_data <- energy_data |>
    filter(year == year_slide)
  
  world_energy21 <- world_countries |>
    left_join(map_data, by = c("iso_a3" = "iso_code")) |>
    mutate(energy_usage = cut(energy_per_capita, breaks = my_cutpoints)) |>
    select(admin, year, energy_usage, energy_per_capita)
  
  m <- ggplot(world_energy21, aes(text = paste(admin, "<br>", year))) +
    geom_sf(aes(fill = energy_per_capita), color = "black") +
    scale_fill_gradient2(low = "#f3ebc6", mid = "#b9493a", high = "#ab0a00", 
                         midpoint = 55000, 
                         na.value = "grey", 
                         limits = c(0, 120000),
                         breaks = c(0, 1000, 3000, 10000, 30000, 100000),
                         labels = c("0", "1000", "3000", "10000", "30000", "100000")) +
    theme(legend.position = "bottom") +
    my_worldmap_theme()
  
  ggplotly(m) |>
    style(hoveron = "fill")
  
}

#--------------
# Energy Consumption scatterplot
#-------------

# make variables easier
cons_data_new <- consumption_data |>
  mutate(gdp_per_capita = GDP_per_capita_PPP) |>
  select((-GDP_per_capita_PPP)) |>
  mutate(consumption_base_emissions = Per_capita_consumption_based) |>
  select((-Per_capita_consumption_based))

# want to do the year 2021

cons_data_21 <- cons_data_new |>
  filter(Year == 2021)

# only got continent data for 2015 so we want to take that
cons_data_15 <- cons_data_new |>
  filter(Year == 2015 & (!is.na(Continent)))

cons_continent <- cons_data_15 |>
  left_join(cons_data_21, by = c("Entity" = "Entity"))

# want to have a tab for the whole or maybe just a desired continent
wanted_conts <- function(mycontinent) {
  if (mycontinent == "World") {
    cons_continent <- cons_continent |>
      filter(Continent != "Antarctica")
  }
  else{
    cons_continent <- cons_continent |>
      filter(Continent != "Antarctica", Continent == mycontinent)
  }
}


# making variables easier names 
cons_continent <- cons_continent |>
  mutate(GDP_per_capita = log10(gdp_per_capita.y)) |>
  mutate(consumption_based_emissions_per_capita = consumption_base_emissions.y) |>
  mutate(Continent = Continent.x) |>
  mutate(Population = Population_countries.y)

# hover effect labels
cons_continent <- cons_continent |>
  mutate(text = paste(Entity, "<br>", 
                      "GDP per capita: ", GDP_per_capita, "<br>", 
                      "Consumption-based emissions per capita: ", consumption_based_emissions_per_capita, "<br>", 
                      "Population: ", Population))


consumption_map <- function(mycontinent) {
  
  # want to have a tab for the whole world or maybe just a desired continent
  wanted_conts <- function(mycontinent) {
    if (mycontinent == "World") {
      cons_continent <- cons_continent |>
        filter(Continent != "Antarctica")
    }
    else{
      cons_continent <- cons_continent |>
        filter(Continent != "Antarctica", Continent == mycontinent)
    }
  }
  
  cons_continent <- wanted_conts(mycontinent)
  
  scatterplot <- ggplot(cons_continent) +
    geom_point(aes(x = GDP_per_capita, 
                   y = consumption_based_emissions_per_capita, 
                   color = Continent, 
                   size = Population,
                   text=text)) +
    scale_x_continuous(breaks = c(3, 3.301, 4, 4.301, 4.699, 5),
                       labels = c("$2000", "$5000", "$10000", "$20000", "$50000", "$100000")) +
    labs(x = "GDP per capita", 
         y = "Consumption-based emissions per capita"
    )
  ggplotly(scatterplot, tooltip = "text")
}


ui <- fluidPage(

    # A look at energy consumption around the world
    titlePanel("World Energy"),

    # Sidebar with a slider input for number of bins 
    
    tabsetPanel(id = "tab",
                tabPanel(
                  title = "World map of Energy Usage", value = "energymap",
                  sliderInput("year_slide", "Select a year:", 
                                min = 2015,
                                max = 2021,
                                value = 2021
                              )
                ), 
                tabPanel(
                  title = "Energy Consumption Scatterplot", value = "consumptiongraph",
                  selectInput("mycont", "Select a part of the World:", 
                              choices = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America", "World"),
                              selected = "World"
                    
                  )
                )
          ),

      # Show a plot of the generated distribution
      mainPanel(
        width = "100%", 
        h3(textOutput("TitleText")),
        h5(textOutput("SubtitleText")),
        plotlyOutput("energies"),
        h5("Original data sources and maps are from: ", 
           tags$a(href = "https://ourworldindata.org", "Our World in Data"),
           " : ",
           tags$a(href = "https://ourworldindata.org/explorers/energy?time=2021&facet=none&country=~TTO&hideControls=false&Total+or+Breakdown=Total&Energy+or+Electricity=Primary+energy&Metric=Per+capita+consumption", 
                  "Energy Data Explorer"),
           "and",
           tags$a(href= "https://ourworldindata.org/grapher/consumption-co2-per-capita-vs-gdppc?time=1990..latest", "CO2 and Emissions")
           
        )
      )
)

# Define server logic required
server <- function(input, output) {
   
  # need to add title and subtitle displays
  output$TitleText <- renderText ({
    if (input$tab == "energymap"){ 
      paste("Energy use per Person, ", input$year_slide)
    }
    else if (input$tab == "consumptiongraph") { 
      paste("Consumption-based CO2 emissions per capita vs. GDP per capita")
    }
  })
  
  output$SubtitleText <- renderText ({
    if (input$tab == "energymap"){ 
      "Measured in kilowatt-hours per person. Here, energy refers to primary energy (includes energy that the end user needs, in the form of electricity, transport and heating, plus inefficiencies and energy that is lost when raw resources are transformed into a usable form.) 
      using the substitution method."
    }
    else if (input$tab == "consumptiongraph") { 
      paste("Consumption-based emissions are measured in tonnes per person. They are territorial emissions minus emissions embedded in exports, plus
        emissions embedded in imports. GDP per capita is adjusted for price differences between countries (PPP) and over time (inflation).") 
            
    }
  })
    
  output$energies <- renderPlotly({
    if (input$tab == "energymap"){ 
      energy_map(input$year_slide) 
      }
    else if (input$tab == "consumptiongraph") { 
      consumption_map(input$mycont) 
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
