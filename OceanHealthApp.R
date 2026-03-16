library(shiny)
library(ggplot2)

# Fake dample dataset
ocean_health <- data.frame(
  Location = c("Great Barrier Reef", "Caribbean", "Red Sea", "Solomon Sea", "Hawaii", "Philippine Sea"),
  
  ## % of coral bleached
  Coral_Bleaching = c(30, 45, 20, 50, 25, 40),
  
  # pH level
  Ocean_pH = c(8.1, 8.0, 8.2, 7.9, 8.05, 8.0)
)

# User interface
ui <- fluidPage(
  
  # Add custom CSS Theme
  tags$head(
    tags$style(HTML("
    
      /* Sidebar for Data Control */
      .well { background-color: #C6D7E6; border-radius: 8px; padding: 20px; }
      
      /* Navbar styling */
      .nav-tabs > li > a { background-color: #4682B4; color: white; }
      .nav-tabs > li > a:hover { background-color: #333d87; color: #fff; }
      .nav-tabs > li[class=active] > a { background-color: #B0C4DE; color: black; font-weight: bold; }
      
      /* Title styling */
      .title { color: #4682B4; font-weight: bold; font-size: 28px; margin-bottom: 20px; }
      
      /* Table styling */
      table { border-collapse: collapse; width: 100%; }
      table, th, td { border: 1px solid #4682B4; padding: 8px; }
      th { background-color: #B0C4DE; color: black; }

      /* Add gap between navbar and charts */
      .tab-content { margin-top: 20px; }

      /* Bold axis labels and titles for charts */
      .ggplot-x-label, .ggplot-y-label, .ggplot-title { font-weight: bold; }
    "))
  ),
  
  # Title
  titlePanel(div("Ocean Health Dashboard", class = "title")),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "locations", "Select Locations:", choices = ocean_health$Location, selected = ocean_health$Location
      ),
      sliderInput(
        "bleachRange",
        "Coral Bleaching Percentage (%):", min = 0, max = 100, value = c(0, 100)
      )
    ),
    
    mainPanel(
      
      # Create navbar items
      tabsetPanel(
        tabPanel("Bleaching vs pH", plotOutput("scatterPlot")),
        tabPanel("Bleaching Bar Chart", plotOutput("barPlot")),
        tabPanel("Data Table", tableOutput("dataTable"))
      )
    )
  )
)

# Create the server
server <- function(input, output, session) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    subset(
      ocean_health,
      Location %in% input$locations &
        Coral_Bleaching >= input$bleachRange[1] &
        Coral_Bleaching <= input$bleachRange[2]
    )
  })
  
  # Scatter plot of Coral Bleaching vs Ocean pH
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Ocean_pH, y = Coral_Bleaching, color = Location)) +
      geom_point(size = 4) +
      geom_text(aes(label = Location), vjust = -1, hjust = 0.5) +
      labs(
        x = "Ocean pH",
        y = "Coral Bleaching (%)",
        title = "Coral Bleaching vs Ocean pH"
      ) +
      theme_minimal() +
      ylim(0, 100) +
      theme(
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 16)
      ) +
      
      
    # Custom colors for Scatter Plot
    scale_color_manual(values = c(
      "Great Barrier Reef" = "darkblue",
      "Caribbean" = "#B0C4DE",
      "Red Sea" = "blue",
      "Solomon Sea" = "#00BFFF80",
      "Hawaii" = "lightblue",
      "Philippine Sea" = "#4682B4"
    ))
  })
  
  # Bar chart of Coral Bleaching by Location
  output$barPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Location, y = Coral_Bleaching, fill = Location)) +
      geom_bar(stat = "identity") +
      labs(
        x = "Location", y = "Coral Bleaching (%)", title = "Coral Bleaching by Location"
      ) +
      theme_minimal() +
      theme( axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", size = 16)
      ) +
  
    # Custom colors for bar chart
    scale_fill_manual(values = c(
      "Great Barrier Reef" = "darkblue",
      "Caribbean" = "#B0C4DE",
      "Red Sea" = "blue",
      "Solomon Sea" = "#00BFFF80",
      "Hawaii" = "lightblue",
      "Philippine Sea" = "#4682B4"
    ))
  })
  
  # Show data table
  output$dataTable <- renderTable({
    filtered_data()
  })
}

# Run the app via the server
shinyApp(ui = ui, server = server)
