library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)

# Load your data
accident_data <- read.csv("accident.csv")

# Make months in calendar order
month_levels <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")
accident_data$MONTHNAME <- factor(accident_data$MONTHNAME, levels = month_levels)

# Define regional states
regional_states <- c("South Dakota", "North Dakota", "Minnesota", "Iowa", "Nebraska", "Wyoming", "Montana")

# UI
ui <- navbarPage("Accidents",
                 tabPanel("Map View",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("month", "Select Month",
                                          choices = c("All", month_levels), selected = "All"),
                              selectInput("harm", "Select Harmful Event",
                                          choices = c("All", sort(unique(accident_data$HARM_EVNAME))), selected = "All"),
                              sliderInput("fatal_range", "Minimum Fatalities",
                                          min = 0,
                                          max = max(accident_data$FATALS, na.rm = TRUE),
                                          value = 0,
                                          step = 1)
                            ),
                            mainPanel(
                              leafletOutput("accidentMap", height = 600)
                            )
                          )
                 ),
                 tabPanel("Fatalities by State",
                          plotOutput("fatalChart", height = 500)
                 )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- accident_data
    
    if (input$month != "All") {
      data <- data %>% filter(MONTHNAME == input$month)
    }
    
    if (input$harm != "All") {
      data <- data %>% filter(HARM_EVNAME == input$harm)
    }
    
    data <- data %>% filter(FATALS >= input$fatal_range)
    
    data
  })
  
  # Leaflet map
  output$accidentMap <- renderLeaflet({
    data <- filtered_data()
    
    leaflet(data) %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%  # Center on US
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~LONGITUD, lat = ~LATITUDE,
        radius = 4,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.6,
        popup = ~paste("State:", STATENAME, "<br>",
                       "County:", COUNTYNAME, "<br>",
                       "Fatalities:", FATALS)
      )
  })
  
  # Bar chart of fatalities by regional state
  output$fatalChart <- renderPlot({
    data <- filtered_data() %>%
      filter(STATENAME %in% regional_states) %>%
      group_by(STATENAME) %>%
      summarise(Fatalities = sum(FATALS, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = reorder(STATENAME, -Fatalities), y = Fatalities)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Fatalities in SD and Neighboring States",
                         if (input$month != "All") paste("in", input$month) else "",
                         if (input$harm != "All") paste("-", input$harm) else ""),
           x = "State", y = "Fatalities") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)