# Load required packages
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(viridis)
library(rsconnect)
library(plotly)
library(gganimate)
library(transformr)
library(gifski)


#######################################
# DATA CREATION
#############################################################
#1. load solar data
#setwd

solar_build <- read_rds("solar_build.rds")
solar_build_cama <- read_rds("build_solar_cama.rds")
zip <- read_rds("zipcode_solar.rds")
ward <- read_rds("ward_solar.rds")
ward_time <- read_rds("ward_solar_time.rds")
zip_time <- read_rds("zipcode_solar_time.rds")


#### vb style -----------------
VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p( msg , style = style )
}

############################################################

# basic cleaning and variable generation

#date
solar_build$Date_Approved <- as.Date(solar_build$`Date Approved`, format = "%d-%m-%Y")

solar_df <- as.data.frame(solar_build)

earliest_date <- min(solar_build$Date_Approved, na.rm = TRUE)
start_date <- as.Date("01-01-2018", format = "%d-%m-%Y")
last_date <- max(solar_build$Date_Approved, na.rm = TRUE)


sun_hours <- 4.2 #https://unboundsolar.com/solar-information/sun-hours-us-map
#themes - https://rstudio.github.io/shinythemes/


##########################

#Load CAMA-solar data
#solar_cama <- read_csv("cama_solar.csv")

# Calculate the 0.98 quantile for a specific column, e.g., 'Capacity (MW)'
#quantile_cut <- quantile(solar_cama$`Capacity (MW)`, 0.99, na.rm = TRUE)

# Subset the dataframe to include only rows within this quantile
#solar_cama <- filter(solar_cama, `Capacity (MW)` <= quantile_cut) %>% 
#  filter(!is.na(ROOF_D) & !is.na(STYLE_D))


#solar_cama <- solar_cama %>% select(`Unit ID`, AYB, `Capacity (MW)`, ROOF_D, STYLE_D) %>% filter(!is.na(`Capacity (MW)`))

#####################
# target values
target <- 665
current <- 201
remaining <- target - current

# PLotly 1 - pieplot
dtgt <- data.frame(
  category = c("Current", "Remaining"),
  value = c(current, remaining)
)
plot <- plot_ly(dtgt, labels = ~category, values = ~value, type = 'pie', hole = 0.5) %>%
  layout(showlegend = TRUE, 
         annotations = list(
           list(text = 'Progress', x = 0.5, y = 0.5, font = list(size = 20), showarrow = FALSE)
         ),
         paper_bgcolor = 'rgba(0,0,0,0)',  # Transparent background
         plot_bgcolor = 'rgba(0,0,0,0)',   # Transparent plot background
         height = 300,  # Manually set height
         width = 300,   # Manually set width
         margin = list(l = 0, r = 0, b = 0, t = 0)  # Remove margins
         )


####################


#################
#zip and ward line animations

# Subset zip_solar_time
zip_time <- zip_time %>%
  group_by(NAME) %>%
  filter(sum(cumulative_solar_adopted_count) >= 15) %>%
  ungroup()


ward_time$month <- as.numeric(ward_time$month)
zip_time$month <- as.numeric(zip_time$month)

###########################################################################
# 1 - Define UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
  # Dashboard Header
  div(class = "dashboard-header",
      h2("Rooftop Solar Capacity in Washington DC")
  ),
  #titlePanel("Rooftop solar capacity in Washington DC"),
  
  # Include CSS in the head section
  tags$head(
    
    tags$style(HTML("
    
        .plot-column {
            width: 300px;  /* Adjust width if necessary */

        border: 1px solid #ddd;
        padding: 10px;
        margin-right: 10px;
      }

  .progress-box {
    border: 2px solid 'rgba(0,0,0,0)';

    background-color: 'rgba(0,0,0,0)'; /* Set the background color */
    color: #98FB98;
    width: 280px;  /* Adjust width if necessary */
    height: 100px; /* Adjust height if necessary */
left: 680px; /* Increase left padding to move text right */

    margin: 10px auto; /* Center align */
    padding: 10px;
    position: absolute; /* Changed from absolute to relative */
    display: flex; /* Changed from flex to block if flex is not necessary */
    bottom: 200px; /* Spacing from the bottom */

       justify-content: center;
    align-items: center;

  }
  
  .progress-text {
    bottom: 10px; /* Spacing from the bottom */
    left: 0px; /* Increase left padding to move text right */

    font-size: 24px;
    margin: 0;
    width: 100%; /* Full width */
    color: white; /* Text color */
  }
          /* Additional CSS for styling */
      .dashboard-header {
        color: #FFFFFF;
        background-color: #A9A9A9;
        padding: 10px 15px;
        border-radius: 0;
        border-bottom: 1px solid #e5e5e5;
      }

      .info-box {
  background-color: #ADFF2F; /* Green background with some transparency */
  color: #000000;
      width: 300px;
       height: 180px;
        border: 2px solid black;
        padding: 20px;
        bottom:0px;
        animation: fadeInGrow 0.5s;
        font-size: 25px; /* Larger text size */
  font-family: 'Tahoma, Gadget, sans-serif'; /* Bold and impactful font */

      }
      
            .plotly-container {
        border: 2px solid white;
        margin-top: 10px; /* Adjust this value to move plotly up */
        margin_bottom: 0px; /* Spacing from the bottom */
        height = 80,  # Manually set height
         width = 100,   # Manually set width

            }
      
      @keyframes fadeInGrow {
  from {opacity: 0; transform: scale(0.5);}
  to {opacity: 1; transform: scale(1);}
}"))
  ),
  
  # Your name as an h2 header
  h3("(PPOL-5202 2023)"),
  
  mainPanel(
    tabsetPanel(
      # Map View Tab
      tabPanel("Map View", 

               # Main Content
               # Main Content
               div(class = "container-fluid",
                   div(class = "row",
                       div(class = "col-md-8",
                           # Map and Controls Solar panel data source: Monthly Update of Solar Generator Certification, October 2023
                           h3("Registered Solar Panels in DC Buildings"),
                           p("To get started, select a spatial level and click on a ward/zipcode for details.",
                             "Building-level visualizations are on the third tab.",
                             a("Github repo", href = "https://github.com/hkumar747/ppol5202_final_dc_solar/tree/main", target = "_blank")
                           ),
                           selectInput("spatialLevel", "Choose Spatial Level:", choices = c("Ward" = "ward", "Zipcode" = "zipcode")),
                           leafletOutput("dynamic_map1")
                       ),

                       div(class = "col-md-4",
                           # Donut Chart Box
                           div(class = "plotly-container",
                               plotlyOutput("donutPlot") # The plot will be rendered here
                           )
                       ),
                       div(class = "col-md-4",
                           # Information Box
                           uiOutput("infoBox")
                       ),
                           # Progress Box
                           div(class = "progress-box",
                               div(class = "progress-text",
                                   strong("Target: ", target,"MW"),
                                   HTML("<br>"), # Line break
                                   strong("Current: ", current,"MW")
                               
                           )
                       )
                   )
               )
      
  ),
      # Estimating Power Production Tab
      tabPanel("Chart view", 
               h3("Compare progress"),
               HTML("<br>"), # Line break
                HTML("<br>"), # Line break
               # Dropdown to select 'ward' or 'zip'
               selectInput("LocationType", "Select Location Type", choices = c("Ward", "Zip")),
               strong("Total buildings with solar"),
               HTML("<br>"), # Optional Line break for spacing
               "Zipcodes and wards have added solar at unequal rates.",
               "Wards 4, 7 and 5 led in terms of cumulative rooftop panel installations, with more than 2,000 panels installed, and rate increasing in recent years.",
               "Zipcodes 20011, 20002, 20019 lead the pack among the zipcodes.",
               HTML("<br>"), # Optional Line break for spacing
               HTML("<br>"), # Optional Line break for spacing
               # Placing two line plots on top of each other
               shinycssloaders::withSpinner(plotlyOutput("lineplot_solar")),
               HTML("<br>"), # Optional Line break for spacing
               HTML("<br>"), # Optional Line break for spacing
               HTML("<br>"), # Optional Line break for spacing
               HTML("<br>"), # Optional Line break for spacing
               
               strong("Total Capacity"),
               HTML("<br>"), # Optional Line break for spacing
               HTML("<br>"), # Optional Line break for spacing
               
               shinycssloaders::withSpinner(plotlyOutput("lineplot_cap"))
      ),
      
      # Building Characteristics Tab
      tabPanel("Building View", 
               # Adding a sidebar
               sidebarLayout(
                 sidebarPanel(
                   checkboxInput("CAMA", "Subset to buildings with CAMA", value = TRUE),
                   # Sliders for filtering
                   sliderInput("capacityFilter", "Capacity (MW):",
                               min = 0, max = 100, value = c(0, 100)),
                   sliderInput("AYBFilter", "Year Built:",
                               min = 1900, max = 2023, value = c(1900, 2023)),
                   selectInput("category", "Choose building attribute:",
                               choices = c("Roof Type" = "ROOF_D",
                                           "Type" = "RESIDENTIAL_TYPE",
                                           "Building Style" = "STYLE_D"))
                 ),
                 mainPanel(
                   h3("Newer buildings, (slightly) bigger capacities."),
                   "Filtering on the basis of age and capacity, solar capacity has grown among newer buildings in small amounts. ",
                   "Because of merging issues, plot only shows data for 4,582 buildings which were exactly merged with unique addresses.",
                   "Average capacity of panels seems to has increased over the years, from under 0.01 MW to 0.015 MW in 2023.",
                   "Click on an active building polygon for details.",
                   HTML("<br>"), # Optional Line break for spacing
                   HTML("<br>"), # Optional Line break for spacing
                   
                   leafletOutput("buildingMap"),
                   h3("What kind of building is more likely to have solar panels?"),
                   
                   "Mixed use properties seem to have the highest capacities, as do metal roofs.",
                   
                   HTML("<br>"), # Optional Line break for spacing
                   
                   HTML("<br>"),
                   "Average capacity of buildings by roof type and structure type.",
  
                   withSpinner(plotOutput("barplot"))
                 )
               )
      )
    )
  )
)

####################################################################################

# 2 - Define server logic
server <- function(input, output, session) {
  
  # Load data
  ward_data <- readRDS("ward_solar.rds")
  zip_data <- readRDS("zipcode_solar.rds")
  solar_build <- readRDS("solar_build.rds")
  solar_build_cama <- read_rds("build_solar_cama.rds")
  
  # Reactive expression to choose the correct data based on the dropdown input
  map_data <- reactive({
    switch(input$spatialLevel,
           "ward" = ward_data,
           "zipcode" = zip_data)
          # "building" = solar_build)
  })
  
  # Render the Leaflet map
  output$dynamic_map1 <- renderLeaflet({
    data <- map_data()
    
    if (input$spatialLevel %in% c("ward", "zipcode")) {
      pal <- colorNumeric(palette = "YlOrRd", domain = data$total_capacity_mw)
      
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          layerId = ~NAME,
          fillColor = ~pal(total_capacity_mw),
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          popup = ~paste("Zip/Ward:", NAME,
                         "<br>Total Capacity MW:", total_capacity_mw,
                         "<br>Percentage Solar:", perc_solar),
          highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE)
        )

    } else {
      pal <- colorNumeric(palette = "YlOrRd", domain = data$`Capacity (MW)`)
      
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(`Capacity (MW)`),
          fillOpacity = 0.7,
          color = "#1a1d62",
          weight = 1,
          popup = ~paste("Address:", `Facility Address`, "<br>Capacity MW:", `Capacity (MW)`),
          highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE)
        ) %>%
        setView(lng=-77.0162, lat=38.9528,
          zoom = 10
        )
    }
  })
  # Reactive value to store the clicked ward or zip
  clicked_area <- reactiveVal(NULL)
  
  # observe event - text box if clicked
  # Observe event for map click
  observeEvent(input$dynamic_map1_shape_click, {
    clicked_name <- input$dynamic_map1_shape_click$id
    
    # Filter data for the selected ward
    selected_data <- filter(map_data(), NAME == clicked_name)
    
    # Variables for the UI box
    reactive_name <- selected_data$NAME
    total_capacity_reactive <- sum(selected_data$total_capacity_mw, na.rm = TRUE)
    perc_solar_reactive <- selected_data$perc_solar
    sum_solar <- selected_data$solar_adopted_count
    
    # Update the information box
    output$infoBox <- renderUI({
      tags$div(
        class = "info-box",
        HTML(paste(
          "<strong>Unit:</strong> ", reactive_name, "<br>",
          "<strong>Capacity:</strong> ", total_capacity_reactive, " MW<br>",
          "<strong>Number:</strong> ", sum_solar, " <br>",
          
          "<strong>Percentage:</strong> ", perc_solar_reactive, "%"
        ))
      )
    })
  })
  
  # CSS for the info box
  output$css <- renderText({
    "
    .info-box {
      border: 2px solid green;
      padding: 20px;
      margin-top: 20px;
      animation: fadeIn 0.5s;
    }
    @keyframes fadeIn {
      from {opacity: 0;}
      to {opacity: 1;}
    }
    "
  })
  #donut plot
  output$donutPlot <- renderPlotly({
    plot  # The plot created earlier
  })
  #######################
  # PLOT 2 - LINEPLOT
  ###########################  
  
  # Reactive expression to calculate the cumulative sum of 'Capacity (MW)' for filtered data
  line_data <- reactive({
    # Filter and arrange data
    solar_df %>%
      arrange(Date_Approved) %>%
      group_by(Date_Approved) %>%
      summarise(Daily_Capacity = sum(`Capacity (MW)`)) %>%
      mutate(cum_Cap = cumsum(Daily_Capacity))
  })
  
  # Add a reactive input for CapFactor
  reactiveCapFactor <- reactive(input$CapFactor)

  # Add a reactive input for CapFactor
  reactiveSunHour <- reactive(input$SunHour)
  
  ##output2 - lineplot
  output$lineplot_1 <- renderPlot({
    req(line_data())  # Ensure 'filtered_data' is available before proceeding
    
    cum_data <- line_data() %>%
      mutate(cum_Cap_Adjusted = 0.001*cum_Cap * reactiveSunHour()*365* reactiveCapFactor())
    
    
    ggplot(cum_data, aes(x = Date_Approved, y = cum_Cap_Adjusted)) +
      geom_line(color="red", size=1.5) +
      labs(x = "Date", y = "Electricity generated (1000 MWh") +
      expand_limits(y = 100) +  # Set the upper limit of y to the maximum value
      theme_minimal()
  })
  

  
    # Reactive expression for lineplot based on Location Type (ward or zip)
    line_data_location <- reactive({
      if (input$LocationType == "Ward") {
        ward_time 
        } else if (input$LocationType == "Zip") {
        zip_time 
        }
    })
    
    #plotly2 - lineplot
    # Output for cumulative_solar_adopted_count line plot using plotly
    output$lineplot_solar <- renderPlotly({
      # Ensure data is available
      req(line_data_location())
      
      # Generate the plot
      lineplot_count <- ggplot(line_data_location(), 
                               aes(x = month, y = cumulative_solar_adopted_count, color = NAME)) +
        geom_line() +
        geom_point()
      
      # Convert to plotly and set layout
      ggplotly(lineplot_count) %>%
        layout(   
          title = list(
          text = "Cumulative Solar Adopted Count Over Time",
          font = list(color = 'white')  # White text for the title
        ),
               showlegend = FALSE,  # Turn off the legend
               xaxis = list(
                 title = list(
                   text = "Month",
                   font = list(color = 'white')  # White text for x-axis label
                 ),       gridcolor = 'white',  # White gridlines for x-axis
                 zerolinecolor = 'white'  # White zero line for x-axis
               ),
               yaxis = list(
                 title = list(
                   text = "NO. of buildings",
                   font = list(color = 'white')
                   ),# White text for y-axis label
                   gridcolor = 'white',  # White gridlines for y-axis
                 zerolinecolor = 'white'  # White zero line for y-axis
               ),               showlegend = TRUE,
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)',
               height = 500,
               width = 700,
               margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    })    
    ##second lineplot
    output$lineplot_cap <- renderPlotly({
      # Ensure data is available
      req(line_data_location())
      
      # Generate the plot
      lineplot_count <- ggplot(line_data_location(), 
                               aes(x = month, y = cumulative_total_capacity_mw, color = NAME)) +
        geom_line() +
        geom_point()
      
      # Convert to plotly and set layout
      ggplotly(lineplot_count) %>%
        layout(title =   list(
          text = "Cumulative Solar Adopted Count Over Time",
          font = list(color = 'white')  # White text for the title
        ),
        showlegend = FALSE,  # Turn off the legend
               
               xaxis = list(
                 title = "Month",
                 gridcolor = 'white',  # White gridlines for x-axis
                 zerolinecolor = 'white'  # White zero line for x-axis
               ),
               yaxis = list(
                 title = "Cumulative Solar Adopted Count",
                 gridcolor = 'white',  # White gridlines for y-axis
                 zerolinecolor = 'white'),  # White zero line for y-axis
                 
               paper_bgcolor = 'rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)',
               height = 500,
               width = 700,
               margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    })  
  #######################
  # PLOT 3 - Barplot
  ###########################  

  # Reactive expression to determine which dataset to use based on checkbox
  reactive_building_data <- reactive({
    if (input$CAMA) {
      readRDS("build_solar_cama.rds")  # Make sure this file path is correct
    } else {
      readRDS("solar_build.rds")       # Make sure this file path is correct
    }

  })
    # Reactive expression for filtering data based on capacity and year built
    reactive_filtered_data <- reactive({
      df <- reactive_building_data()
      df <- df[df$`Capacity (MW)` >= input$capacityFilter[1] & df$`Capacity (MW)` <= input$capacityFilter[2], ]
      df <- df[df$AYB >= input$AYBFilter[1] & df$AYB <= input$AYBFilter[2], ]
      df
    })
  # Render Leaflet map for building data
  output$buildingMap <- renderLeaflet({
    # Initial map rendering with the default data
    renderMapWithData(reactive_filtered_data())
  })
  
  # Observe changes in the CAMA checkbox and update the map accordingly
  observeEvent(input$CAMA, {
    # Using leafletProxy to update the map reactively
    leafletProxy("buildingMap", session) %>% {
      renderMapWithData(reactive_filtered_data())
    }
  })
  
  # Function to render map with given data
  renderMapWithData <- function(data) {
    pal <- colorNumeric(palette = "YlOrRd", domain = data$`Capacity (MW)`)
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(`Capacity (MW)`),
        fillOpacity = 0.7,
        color = "#1a1d62",
        weight = 1,
        popup = ~if (input$CAMA) {
          paste("Address:", `Facility Address`,
                "<br>Rooms:", `ROOMS`,
                "<br>Number:", `Number`,
                "<br>Capacity MW:", `Capacity (MW)`)
        } else {
          paste("Address:", `Facility Address`,
                "<br>Capacity MW:", `Capacity (MW)`)
        },
        highlightOptions = highlightOptions(weight = 2, color = "white", bringToFront = TRUE)
      ) %>%
      setView(lng = -77.0162, lat = 38.9528, zoom = 13)
  }
  # Reactive expression for selected category
  selected_category_data <- reactive({
    category <- input$category
    solar_build_cama %>%
      group_by(!!sym(category)) %>%
      summarise(Average_Capacity = mean(`Capacity (MW)`, na.rm = TRUE)) %>%
      arrange(desc(Average_Capacity)) # Optional: Arrange in descending order of average capacity
  })
  
  # Render the horizontal bar plot
  output$barplot <- renderPlot({
    req(selected_category_data())  # Ensure data is available
    
    ggplot(selected_category_data(), aes_string(y = input$category, x = "Average_Capacity", fill = "Average_Capacity")) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c(option = "inferno", direction = -1) + # Inferno color scale
      #coord_flip() + # Flip the coordinates for horizontal bars
      labs(y = input$category, x = "Average Capacity (MW)") +
      theme_minimal() + theme(
        axis.text.x = element_text(size = 12), # Adjust size for x tick labels
        axis.text.y = element_text(size = 12), # Adjust size for y tick labels
        axis.title.x = element_text(size = 14), # Adjust size for x axis label
        axis.title.y = element_text(size = 14) )
  })
  
  
}

##############################################################

# Run the application locally 
shinyApp(ui = ui, server = server)



#rsconnect::deployApp()
# 

