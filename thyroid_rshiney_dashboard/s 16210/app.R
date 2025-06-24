library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(leaflet)
library(tidyr)
library(shinydashboard)
library(plotly)
library(ggcorrplot) 

# Load dataset
data <- read.csv("thyroid_cancer_risk_data_preprocessed.csv")

# Check if data is loaded properly
if (nrow(data) == 0) {
  stop("Error: The dataset is empty or not loaded correctly!")
}

# Ensure column names are correct
print(names(data))

# Summarize data: Count Malignant & Benign per country
country_summary <- data %>%
  group_by(Country, Diagnosis) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Diagnosis, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(Total = Malignant + Benign,  
         Malignant_Prop = ifelse(Total > 0, Malignant / Total, 0),  
         Benign_Prop = ifelse(Total > 0, Benign / Total, 0))  

# Define country coordinates
country_coords <- tibble(
  Country = c("Brazil", "China", "Germany", "India", "Japan", "Nigeria", "Russia", "South Korea", "UK", "USA"),
  lat = c(-14.2350, 35.8617, 51.1657, 20.5937, 36.2048, 9.0820, 61.5240, 35.9078, 51.5074, 37.7749),
  lon = c(-51.9253, 104.1954, 10.4515, 78.9629, 138.2529, 8.6753, 105.3188, 127.7669, -0.1278, -122.4194)
)

# Merge coordinates with summarized data
map_data <- left_join(country_summary, country_coords, by = "Country")


# UI with shinydashboard
#title
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div("Thyroid Cancer Risk", 
                     style = "float: center; font-size: 18px; font-weight: bold; color: white;")
      
  ),
  
#sidebar 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset Overview", tabName = "Overview", icon = icon("table")),
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("chart-bar"))
    )
  ),

#include plots and visualizations to the body
  dashboardBody(
    
   
    tabItems(
      tabItem(tabName = "Overview",  # Ensure correct tab name
              fluidRow(
                
                valueBoxOutput("num_obs", width = 6),
                valueBoxOutput("num_vars", width = 6),valueBox("Response Variable: Diagnosis", 
                                                               "Categories: Benign (Non-cancerous) & Malignant (Cancerous)", 
                                                               icon = icon("notes-medical"),width=6, color = "red"),
                
                # New Value Box for Dataset Purpose
                valueBox("Dataset Purpose", 
                         "Used for identify most influential factors for thyroid cancers", 
                         icon = icon("bullseye"), width=6,color = "orange")
              ),
              ##add dataset
              DTOutput("data_table",width = '100%')
      ),
      
      tabItem(tabName = "Dashboard",
              fluidRow(
                ##add help section
              infoBox("Help", 
                      "Hover over the graphs to see detailed data points. Click on legends to toggle categories.", 
                      icon = icon("info-circle"), 
                      color = "light-blue", 
                      fill = TRUE,
                      width = 12)),
              
              ##add plots for univariate and bivariate analysis
              
              fluidRow(
                box(title = "Diagnosis Distribution", status = "primary", solidHeader = FALSE, width = 4,
                    plotlyOutput("diagnosis_pie",height = '350px',)
                ),
                box(title = "Risk Level Distribution", status = "primary", solidHeader = FALSE, width = 4,
                    plotlyOutput("risk_pie",height = '350px',)
                ),
                box(title = "Thyroid Cancer Diagnosis by Country", status = "primary", solidHeader = FALSE, width = 4,
                    leafletOutput("map",height = '315px',),
                    actionButton("reset_map", "Reset Map", icon = icon("undo")))),
              
              ##add buttons 
              
              fluidRow(
                column(6, selectInput("selected_var", "Select Variable:", choices = names(data), selected = names(data)[4])),  
                column(6, radioButtons("response_var", "Compare each variable with:", 
                                       choices = c("Thyroid_Cancer_Risk", "Diagnosis"), selected = "Diagnosis", inline = TRUE))
              ),
              fluidRow(
                
                column(6, plotlyOutput("uni_plot", height = "450px", width = "100%")),
                column(6, plotlyOutput("bi_plot", height = "450px", width = "100%"))
              ),
              
              fluidRow(
                column(8, 
                       h4("Adjust Sliders to Update the Plot", align = "center")  # Title above the box
                )),
              
              fluidRow(
                box(status = "primary", solidHeader = FALSE, width = 8,
                    
                  ##add sliders
                  fluidRow(
                    column(4, 
                           sliderInput("t3", "Select T3 Level:", min = min(data$T3_Level), max = max(data$T3_Level), value = range(data$T3_Level)),
                           sliderInput("t4", "Select T4 Level:", min = min(data$T4_Level), max = max(data$T4_Level), value = range(data$T4_Level)),
                           sliderInput("tsh", "Select TSH Level:", min = min(data$TSH_Level), max = max(data$TSH_Level), value = range(data$TSH_Level)),
                           sliderInput("age", "Select Age:", min = min(data$Age), max = max(data$Age), value = range(data$Age))
                    ),
                    column(4, 
                           radioButtons("plot_type", "Select Plot Type:", 
                                        choices = c("Boxplot", "Density Plot"), 
                                        selected = "Density Plot", 
                                        inline = TRUE),
                           ##add the map
                           plotlyOutput("num_plot", height = "400px",width = '500px')
                    )
                  )
                ),
                ##add the heatmap
                column(4, plotOutput("corHeatmap", height = "480px"))
                
               
               
                 
                )
              
              
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive function to filter data based on user input for T3, T4, TSH levels, and Age
  filtered_data <- reactive({
    df <- data %>%
      filter(T3_Level >= input$t3[1] & T3_Level <= input$t3[2],
             T4_Level >= input$t4[1] & T4_Level <= input$t4[2],
             TSH_Level >= input$tsh[1] & TSH_Level <= input$tsh[2],
             Age >= input$age[1] & Age <= input$age[2])
    df
  })
  
  # Render Data Table with search and pagination enabled
  output$data_table <- renderDT({
    
    datatable(data, options = list(pageLength = 10, searchHighlight = TRUE,scrollX = TRUE))
      
    
  })
  
  # Display number of observations in a value box
  output$num_obs <- renderValueBox({
    valueBox(
      value = nrow(data),
      subtitle = "Number of Observations",
      icon = icon("database"),
      color = "blue"
    )  
  })
  
  # Display number of variables in a value box
  output$num_vars <- renderValueBox({
    valueBox(
      value = ncol(data),
      subtitle = "Number of Variables",
      icon = icon("table"),
      color = "green"
    ) 
  })
  
  # Generate pie chart for Diagnosis distribution
  output$diagnosis_pie <- renderPlotly({
    diagnosis_counts <-data %>%
      count(Diagnosis)
    
    plot_ly(diagnosis_counts, labels = ~Diagnosis, values = ~n, type = "pie",
            marker = list(colors = c("#1f77b4", "#ff7f0e"))) 
      
  })
  
  # Generate pie chart for Thyroid Cancer Risk distribution
  output$risk_pie <- renderPlotly({
    risk_counts <- data %>%
      count(Thyroid_Cancer_Risk)
    
    plot_ly(risk_counts, labels = ~Thyroid_Cancer_Risk, values = ~n, type = "pie",
            marker = list(colors = c( "#d62728","#2ca02c", "#9467bd")))
      
  })

  # Univariate visualization - Boxplot for numeric variables and Bar chart for categorical variables
  output$uni_plot <- renderPlotly({
    selected_var <- input$selected_var
    
    if (is.numeric(data[[selected_var]])) {
      # Generate a boxplot for numeric variables
      p <- ggplot(data, aes_string(y = selected_var)) +
        geom_boxplot(fill = 'cyan', width = 1) +  
        theme_minimal() +
        labs(title = paste("Boxplot of", selected_var),  # Title for boxplot
             y = selected_var,
             x = NULL)  
    } else {
      # Generate a bar chart for categorical variables
      p <- ggplot(data, aes_string(x = selected_var, fill = selected_var)) +
        geom_bar() +
        theme_minimal() +
        labs(title = paste("Bar Chart of", selected_var),  # Title for bar chart
             x = selected_var,
             y = "Count")
    }
    
    ggplotly(p) # Convert to interactive Plotly plot
  })
  
  
  # Bivariate visualization - Boxplot for numeric vs categorical, bar chart for categorical vs categorical
  output$bi_plot <- renderPlotly({
    selected_var <- input$selected_var
    response_var <- input$response_var
    
    if (is.numeric(data[[selected_var]])) {
      # Generate a boxplot for numeric variables grouped by categorical response variable
      p <- ggplot(data, aes_string(x = response_var, y = selected_var, fill = response_var)) +
        geom_boxplot() +
        theme_minimal() +
        ggtitle(paste("Boxplot of", selected_var, "by", response_var))
      
    } else {
      # Generate a proportional bar chart for categorical vs categorical
      prop_data <- data %>%
        group_by(across(all_of(selected_var)), across(all_of(response_var))) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(across(all_of(selected_var))) %>%  
        mutate(prop = count / sum(count))
      
      p <- ggplot(prop_data, aes_string(x = selected_var, y = "prop", fill = response_var)) +
        geom_col(position = "dodge") +  
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  
        theme_minimal() +
        ggtitle(paste("Multiple Bar Chart of", selected_var, "by", response_var)) +
        xlab(selected_var) + ylab("Proportion")
    }
    
    ggplotly(p)  # Convert ggplot to an interactive Plotly graph
  })
  
  # Numeric variable visualization - Boxplot or Density Plot
  output$num_plot <- renderPlotly({
    req(filtered_data())  # Ensure filtered data is available
    plot_data <- filtered_data()  
    plot_type <- input$plot_type  
    response_var <- input$response_var 
    
    
    if (plot_type == "Boxplot") {
      # Generate boxplot for Age distribution by response variable
      p <- ggplot(plot_data, aes_string(x = response_var, y = "Age", fill = response_var)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = paste("Age Distribution by", response_var))
    } else {
      # Generate density plot for Age distribution by response variable
      p <- ggplot(plot_data, aes_string(x = "Age", fill = response_var)) +
        geom_density(alpha = 0.3, size = 1) + 
        theme_minimal() +
        labs(title = paste("Density Plot of Age by", response_var))
    }
    
    ggplotly(p)  # Convert ggplot to an interactive Plotly graph
  })
  
  # Correlation Heatmap for numerical variables
  output$corHeatmap <- renderPlot({
    # Select only numerical columns
    numeric_data <- data %>%
      select_if(is.numeric)
    
    # Compute correlation matrix
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Generate heatmap using ggcorrplot
    ggcorrplot(cor_matrix, method = "square", 
               lab = TRUE, 
               colors = c("#6D9EC1", "white", "red"),
               legend.title = "Correlation") +
      ggtitle("Correlation Heatmap of Numerical Variables") +
      theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
    })
  
  
  # Default map settings
  default_lat <- 20  
  default_lng <- 0   
  default_zoom <- 2  
  
  # Render Leaflet Map for country-level cancer risk visualization
  output$map <- renderLeaflet({
    validate(
      need(nrow(map_data) > 0, "No valid country data found! Check dataset.")
    )
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        radius = ~ifelse(Total > 0, Malignant_Prop * 20, 3),  
        color = "red",
        fillColor = "red",
        fillOpacity = 0.5,
        stroke = TRUE,
        label = ~paste0(Country, ' Malignant: ', Malignant, " (", round(Malignant_Prop * 100, 1), "%)"),
        group = "Malignant"
      ) %>%
      addCircleMarkers(
        ~lon, ~lat,
        radius = ~ifelse(Total > 0, Benign_Prop * 10, 3),  
        color = "blue",
        fillColor = "blue",
        fillOpacity = 0.5,
        stroke = TRUE,
        label = ~paste0(Country, ' Benign: ', Benign, " (", round(Benign_Prop * 100, 1), "%)"),
        group = "Benign"
      ) %>%
      addLayersControl(
        baseGroups = c("Malignant", "Benign"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  # Reset the map when the button is clicked
  observeEvent(input$reset_map, {
    leafletProxy("map") %>%
      setView(lng = default_lng, lat = default_lat, zoom = default_zoom)
  })
  
  
  
  
} 

# Run app
shinyApp(ui, server)
