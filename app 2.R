#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Suicide Indicators App

# Load necessary libraries for the app
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(bslib)
library(plotly)

# Read the dataset from the specified location and pre-processing
raw_data <- read.csv("data/master.csv")

dataset <- raw_data %>%
  filter(suicides_no != 0 & population != 0)

# read in the world country spatial data frame
world_spdf <- readOGR(dsn = path.expand(paste0("data/maps/")),
                      layer = "TM_WORLD_BORDERS-0-3",
                      verbose = FALSE)

# Define the user interface (UI) for the shiny app
# Create the first tab of the app for country-wide comparison

ui <- navbarPage(
  "Suicide Identification Dashboard",
  id = "navs",
  tabPanel(
    "Trigger Warning",
    titlePanel("Trigger Warning!!"),
    mainPanel(
      br(),
      strong(
        "This application includes content that some may find sensitive. The analysis contains details on suicides, which may be unsettling to some users. We kindly request that you proceed to the following tabs only if you feel comfortable doing so. Thank you for your understanding."
      ),
      br(),
      br(),
      strong(
        "Please proceed to the next tabs only if this won't trigger you. Thank you"
      ),
    )
  ),
  tabPanel(
    "Country Wide Comparison",
    titlePanel("Country Wide Comparison"),
    sidebarLayout(
      sidebarPanel(
        htmlOutput("message"),
        selectInput(
          "country1",
          "Country 1:",
          choices = sort(unique(dataset$country)),
          selected = "Canada"
        ),
        selectInput(
          "country2",
          "Country 2:",
          choices = sort(unique(dataset$country)),
          selected = "United States"
        ),
        sliderInput(
          "year_range",
          "Select Year Range:",
          min = 1987,
          max = 2017,
          sep = "",
          value = c(1999, 2005)
        ),
        radioButtons(
          "sex",
          "Select Gender:",
          choices = list(
            "Both" = "both",
            "Female" = "female",
            "Male" = "male"
          ),
          selected = "both"
        ),
      ),
      mainPanel(tabsetPanel(
        id = "tabs",
        tabPanel(
          "Gender and Age wise compaarison",
          splitLayout(plotOutput("stacked_bars"),
                      plotOutput("grouped_bars"))
        ),
        tabPanel("Suicide and GDP trends",
                 plotOutput("line_plot"))
      ))
    )
  ),
  
  # Create the second tab of the app for displaying the suicide rate by country on a map
  tabPanel(
    "Suicide Rate by Country",
    titlePanel("Suicide Rate by Country"),
    sidebarLayout(sidebarPanel(
      htmlOutput("message1"),
      sliderInput(
        "suicide_map_year",
        "Select Year:",
        min = 1987,
        max = 2017,
        value = 2007,
        sep = "",
        animate = FALSE
        # animationOptions(interval = 300, loop = TRUE) # runs to slow with updating
      )
    ),
    mainPanel(
      leafletOutput("suicide_map", height = "75vh")
    ))
  ),
)

# Define the server function
server <- function(input, output, session) {
  # Create reactive data for selected range of years, countries of interest
  subset_data <- reactive({
    filtered_data <- subset(
      dataset,
      year >= input$year_range[1] &
        year <= input$year_range[2] &
        country %in% c(input$country1, input$country2) &
        sex == input$sex
    )})
  output$message <- renderUI({
    HTML(paste("<div style='text-align:center;'>Welcome to the Suicide Identification Dashboard!</div>", "",
               "<div style='text-align:center;'>The visualizations on this panel allow users to compare suicide rates and socio-economic factors between two selected countries. These graphs are designed to help users gain insights and understanding of the complex factors that contribute to suicide rates such as gender, age, countries GDP and to identify potential areas for intervention and prevention.</div>", "", "", sep="<br/>"
    ))})
  output$message1 <- renderUI({
    HTML(paste("<div style='text-align:center;'>The animated maps displayed in this tab provides an intuitive way for users to explore the trends and patterns of suicide rates and GDP over time and across different regions of the world. This can help users to identify potential correlations and relationships between the two factors in terms of spatial arrangments.</div>", "", "", sep="<br/>"
    ))})
  
  
  
  # Create reactive data for selected range of years and countries
  # Calculate the proportion of suicides to the total population
  output$stacked_bars <- renderPlot({
    calc_data <-
      dataset |> mutate(sex_new = ifelse(sex == "male" |
                                           sex == "female", "both", sex)) |>
      group_by(year, country, sex, sex_new) |>
      summarise(
        suicides = sum(suicides_no),
        population = sum(population),
        suicides_100k_pop_recal = sum(suicides_no) / sum(population)
      ) |>
      ungroup()
    
    data <-
      calc_data |>
      filter(country == input$country1 |
               country == input$country2) |>
      filter(year >= input$year_range[1] &
               year <= input$year_range[2]) |>
      filter(sex_new == input$sex | sex == input$sex)
    
    # Create a stacked bar plot for the suicide rate by gender for both countries
    ggplot(data,
           aes(
             x = as.Date(paste0(year, "-01-01")),
             y = suicides_100k_pop_recal,
             fill = sex,
           )) +
      geom_bar(stat = "identity", position = "stack") +
      labs(
        title = sprintf(
          "
                                       Suicide Rate in %s and %s between
                                                               %s and %s by Gender",
          input$country1,
          input$country2,
          input$year_range[1],
          input$year_range[2]
        ),
        x = "Year",
        y = "Proportion of Suicides per 100k Population"
      ) +
      theme_classic() +
      scale_fill_manual(
        values = if (input$sex == "male")
          "#56B4E9"
          else if (input$sex == "female")
            "#FF9999"
            else
              c("#FF9999", "#56B4E9"),
        name = "Gender:"
      ) +
      facet_grid(cols = vars(country)) +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 year") +
      scale_y_continuous(
        labels = scales::number_format(big.mark = ",", decimal.mark = "."),
        limits = c(0, max(data$suicides_100k_pop_recal))
      )
  })
  
  output$grouped_bars <- renderPlot({
    # data wrangling
    bar_data <- dataset |>
      filter(country == input$country1 |
               country == input$country2) |>
      filter(year >= input$year_range[1] &
               year <= input$year_range[2]) |>
      mutate(age = as.factor(age)) |> 
      group_by(country, age) |>
      summarise(
        suicides = sum(suicides_no, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        suicides_pop = sum(suicides_no, na.rm = TRUE)* 1000000  / sum(population, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        rank = case_when(
          # Used for axis order.
          age == "5-14 years" ~ 1,
          age == "15-24 years" ~ 2,
          age == "25-34 years" ~ 3,
          age == "35-54 years" ~ 4,
          age == "55-74 years" ~ 5,
          age == "75+ years" ~ 6
        )
      )
    
    country_1 <- bar_data |> filter(country == input$country1) |> complete(country,age, fill = list(suicides_pop = 0))
    country_2 <- bar_data |> filter(country == input$country2) |> complete(country,age, fill = list(suicides_pop = 0))
    
    
    # Dumbbell plot
    ggplot(bar_data) + geom_segment(data = country_1,
                                    aes(x=suicides_pop, y=age, yend = age,xend=country_2$suicides_pop),
                                    size = 4.5, #Note that I sized the segment to fit the points
                                    alpha = .5) +
      geom_point(aes(x = suicides_pop, y = age, color = country), size = 4, show.legend = TRUE) +
      labs(
        title = sprintf(
          "                         Suicide Counts Per 1 million in %s and %s between
                                                %s and %s by Age Group",
          input$country1,
          input$country2,
          input$year_range[1],
          input$year_range[2]
        ),
        x = "Age group",
        y = "Suicides counts"
      ) +
      theme_classic() +
      scale_fill_manual(values = c("#D55E00", "#0072B2"), name = "Country:") +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  
  output$line_plot <- renderPlot({
    #  wrangle the data for countries
    country_data <- dataset |>
      group_by(year, country) |>
      summarise(
        population = sum(population),
        Suicides = sum(suicides_no) / (population) * 1000000,
        GDP = sum(gdp_per_capita....) / n()
      ) |>
      ungroup() |>
      filter(country == input$country1 |
               country == input$country2) |>
      filter(year >= input$year_range[1] &
               year <= input$year_range[2]) |>
      select(year, country, Suicides, GDP)
    
    # label country data for faceting
    dc1 <- country_data |>
      select(year, country, Suicides) |>
      mutate(Measure = 'Suicides') |>
      rename("Value" = "Suicides")
    
    dc2 <- country_data |>
      select(year, country, GDP) |>
      mutate(Measure = 'GDP') |>
      rename("Value" = "GDP")
    
    #  wrangle the data for world
    world_data <- dataset |>
      group_by(year) |>
      summarise(
        population = sum(population),
        country = "World Average",
        Suicides = sum(suicides_no) / (population) * 1000000,
        GDP = sum(gdp_per_capita....) / n()
      ) |>
      ungroup() |>
      filter(year >= input$year_range[1] &
               year <= input$year_range[2]) |>
      select(year, country, Suicides, GDP)
    
    #  label the world data for faceting
    dw1 <- world_data |>
      select(year, country, Suicides) |>
      mutate(Measure = 'Suicides') |>
      rename("Value" = "Suicides")
    
    dw2 <- world_data |>
      select(year, country, GDP) |>
      mutate(Measure = 'GDP') |>
      rename("Value" = "GDP")
    
    # finalise dataframe after wrangling
    df <- rbind(dw1,
                dw2,
                dc1,
                dc2)
    
    # plot to render
    ggplot(df, aes(x = year,
                   y = Value,
                   color = country)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = c("#D55E00",
                                             "#0072B2",
                                             "#E7B800"),
                                             name = "Region") +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        x = "Year",
        title = sprintf(
          "Suicide per million and GDP in %s and %s between
           %s and %s",
          input$country1,
          input$country2,
          input$year_range[1],
          input$year_range[2]
        )
      ) +
      facet_wrap( ~ Measure,
                  scales = "free")
    
  })
  
  # Suicide rate geographic world map
  output$suicide_map <- renderLeaflet({
    # get the input year for reference throughout
    selected_year <- input$suicide_map_year
    
    # filter the primary dataset to that year and summarize
    year_data <- dataset |>
      filter(year == selected_year | is.na(year)) |>
      group_by(country) |>
      summarise(
        year = selected_year,
        suicides_100k_pop = mean(suicides_100k_pop),
        gdp_per_capita = mean(gdp_per_capita....),
        population = sum(population)
      )
    
    # use a copy of polygon object to not over write
    year_spdf <- world_spdf
    
    # combine subset suicide data with polygon data
    year_spdf@data <- merge(
      x = world_spdf@data,
      y = year_data,
      by.x = "NAME",
      by.y = "country",
      all.x = TRUE
    ) |>
      select(FIPS, ISO2, ISO3, UN, everything())
    
    # order of the data must match the original polygon order otherwise the
    # polygons don't line up on the map/labels
    reordered_idx <-
      match(world_spdf@data$NAME, year_spdf@data$NAME)
    year_spdf@data <- year_spdf@data[reordered_idx,]
    
    # Create a color palette with handmade bins.
    mybins <- c(0, 10, 20, 30, 40, 50, Inf)
    mypalette <-
      colorBin(
        palette = "YlOrBr",
        domain = year_spdf@data$suicides_100k_pop,
        na.color = "transparent",
        bins = mybins
      )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Country: ",
      year_spdf@data$NAME,
      "<br/>",
      "Suicides per 100k: ",
      round(year_spdf@data$suicides_100k_pop, 2),
      "<br/>",
      "GDP per capita: $",
      round(year_spdf@data$gdp_per_capita, 0),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Final Map
    m <- leaflet(year_spdf) %>%
      addTiles() %>%
      setView(lat = 10,
              lng = 0,
              zoom = 2) %>%
      addPolygons(
        fillColor = ~ mypalette(suicides_100k_pop),
        stroke = TRUE,
        fillOpacity = 0.9,
        color = "white",
        weight = 0.3,
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values = ~ population,
        opacity = 0.9,
        title = "Suicides Per 100k",
        position = "topright"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)