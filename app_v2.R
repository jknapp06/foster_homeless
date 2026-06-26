library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(leaflegend)
library(sf)
library(janitor)
library(openxlsx)
library(htmltools)

# Load data files
homeless <- read_csv("data/homeless.csv") %>% 
  clean_names()

district_geo <- read_sf("data/California_School_District_Areas_2022-23.geojson") %>% 
  clean_names() %>%
  filter(county_name == "Solano") %>% 
  rename(cds = cds_code) %>% 
  mutate(center_point = st_centroid(geometry),
         lng = st_coordinates(center_point)[,1],
         lat = st_coordinates(center_point)[,2])

school_geo <- read_sf("data/California_Schools_2022-23.geojson") %>% 
  clean_names() %>%
  filter(county_name == "Solano") %>% 
  mutate(school_color = case_match(
    school_type,
    "K-12" ~ "#00BFFF",
    "Juvenile Court" ~ "#483D8B",
    "Special Education" ~ "#DB7093",
    "County Community" ~ "#BA55D3",
    "Community Day" ~ "#DA70D6",
    "Continuation" ~ "#6A5ACD",
    "High" ~ "#7B68EE",
    "Middle" ~ "#1E90FF",
    "Elementary" ~ "#87CEFA",
    "Alternative Schools of Choice" ~ "#EE82EE",
    .default = "#808080"  # Default color for any missing school type
  )) %>%
  rename(cds = cds_code)

resources_plottable <- read.xlsx("data/resources_with_addresses.xlsx")

# Clean and prepare homeless data
homeless_total <- homeless %>% 
  filter(county_name == "Solano",
         academic_year == "2023-24",
         dass == "All" | aggregate_level == "S",
         charter_school == "All" | aggregate_level == "S",
         reporting_group == "Total") %>% 
  mutate(across(c(temporarily_doubled_up, temporary_shelters, 
                  hotels_motels, temporarily_unsheltered, 
                  missing_unknown),
                ~ifelse(is.character(.), parse_number(.), .)))

# Join school data with homeless data
# MODIFIED: The join may fail if cases don't match exactly
school_homelessness <- school_geo %>%
  left_join(homeless_total, by = join_by(
    s_code == school_code,
    county_name == county_name, 
    district_name == district_name,
    school_name == school_name
  )) %>%
  # MODIFIED: Fill in missing data with zeroes instead of filtering out
  mutate(
    homeless_student_enrollment = ifelse(is.na(homeless_student_enrollment), 0, homeless_student_enrollment),
    enroll_total = ifelse(is.na(enroll_total), 0, enroll_total),
    percent_homeless = ifelse(homeless_student_enrollment > 0 & enroll_total > 0,
                              round(homeless_student_enrollment / enroll_total * 100, 1),
                              0)
  )

# Get district summary data
district_summary <- homeless_total %>%
  filter(aggregate_level == "D" | aggregate_level == "C") %>%
  rename(
    total_enrollment = cumulative_enrollment,
    homeless_count = homeless_student_enrollment,
    doubled_up = temporarily_doubled_up,
    shelters = temporary_shelters,
    hotels = hotels_motels,
    unsheltered = temporarily_unsheltered,
    unknown = missing_unknown
  ) %>% 
  mutate(percent_homeless = round(homeless_count / total_enrollment * 100, 1))

# Categorize resources
resource_categories <- c(
  "Emergency Services" = "Emergency.Services",
  "Family Resource Center" = "Family.Resource.Center",
  "Emergency Shelter" = "Emergency.Shelter",
  "Medical Facility" = "Medical.Facility",
  "Housing Assistance" = "Housing.Assistance",
  "Low Income Housing" = "Low.Income.Housing.Resources",
  "Police Station" = "Police.Station",
  "Fire Department" = "Fire.Department",
  "Safe Surrender Site" = "Safe.surrender.site"
)
# UI Definition
ui <- page_sidebar(
  title = "Solano County Resources",
  # theme = bs_theme(bootswatch = "cosmo"),
  fillable = TRUE,
  
  # Header with logo
  header = div(
    class = "d-flex align-items-center p-3 bg-light border-bottom",
    img(src = "SCOE_Logo.jpg", height = 80, width = 155, class = "me-3"),
    div(
      h3("Youth Experiencing Housing Insecurity in Solano County", class = "m-0"),
      p("Interactive Map of Schools & Community Resources", class = "mb-0 text-muted")
    )
  ),
  
  # Sidebar controls
  sidebar = sidebar(
    h4("Map Controls"),
    
    # School filters
    selectInput("district_filter", 
                "Filter by District",
                choices = c("All Districts", unique(school_homelessness$district_name)),
                selected = "All Districts"),
    
    selectInput("school_filter",
                "Filter by School Type",
                choices = c("All Types", unique(school_homelessness$school_type)),
                selected = "All Types"),
    
    hr(),
    
    # Resource filters
    h4("Community Resources"),
    checkboxInput("show_resources", "Show Resources", value = TRUE),
    
    conditionalPanel(
      condition = "input.show_resources",
      checkboxGroupInput("resource_type", "Resource Type",
                         choices = names(resource_categories),
                         selected = names(resource_categories)
      )
    ),
    
    hr(),
    
    # Info about selected item
    conditionalPanel(
      condition = "input.selected_item",
      h4("Selected Item Details"),
      htmlOutput("selected_details"),
      actionButton("clear_selection", "Clear Selection", class = "btn-sm btn-outline-secondary mt-2")
    ),
    
    # Attribution
    div(
      class = "mt-auto pt-3 small text-muted",
      "Data sources: California Department of Education",
      br(),
      "Prepared by SCOE"
    )
  ),
  
  # Main content area with cards
  div(
    style = "display: flex; flex-direction: column; gap: 0.5rem; padding: 0.5rem;",
    
    # Map card
    card(
      full_screen = TRUE,
      style = "height: 450px; margin-bottom: 0.5rem;",
      card_header("Solano County Map"),
      leafletOutput("homelessness_map", height = "calc(100% - 56px)")
    ),
    
    # District summary and housing breakdown
    div(
      style = "display: flex; gap: 0.5rem; margin-bottom: 0.5rem;",
      
      # District statistics card
      card(
        style = "flex: 8; height: 350px;",
        card_header("District Summary"),
        DTOutput("district_table", height = "calc(100% - 56px)")
      ),
      
      # Housing breakdown card
      card(
        style = "flex: 4; height: 350px;",
        card_header("Housing Situation Breakdown"),
        plotOutput("housing_plot", height = "calc(100% - 56px)")
      )
    ),
    
    # Community resources card
    card(
      style = "height: 400px;",
      card_header("Community Resources"),
      DTOutput("resources_table", height = "calc(100% - 56px)")
    )
  ),
  
  # Hidden input for selected item
  tags$input(id = "selected_item", type = "hidden", value = "")
)
# Server logic
server <- function(input, output, session) {
  
  # Reactive filtered datasets
  filtered_schools <- reactive({
    schools <- school_homelessness
    
    if (input$district_filter != "All Districts") {
      schools <- schools %>% filter(district_name == input$district_filter)
    }
    
    if (input$school_filter != "All Types") {
      schools <- schools %>% filter(school_type == input$school_filter)
    }
    
    return(schools)
  })
  
  filtered_resources <- reactive({
    if (!input$show_resources) {
      return(NULL)
    }
    
    resources <- resources_plottable
    
    # Filter by selected resource types
    if (!is.null(input$resource_type) && length(input$resource_type) > 0) {
      selected_columns <- resource_categories[input$resource_type]
      
      # Filter resources that have at least one of the selected types
      resources <- resources %>%
        filter(rowSums(across(all_of(selected_columns), ~ifelse(is.na(.), 0, .))) > 0)
    }
    
    return(resources)
  })
  
  output$homelessness_map <- renderLeaflet({
    # Create the base leaflet map
    leaflet() %>%
      # Add multiple base map providers
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      # Set initial view centered on Solano County
      setView(lng = -122.0, lat = 38.3, zoom = 10) %>%
      # Add layers control for both base maps and overlay groups
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite", "CartoDB"),
        overlayGroups = c("District Boundaries", "Schools", "Resources"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomright") %>%
      addEasyButton(easyButton(
        icon = "fa-home", 
        title = "Reset View",
        onClick = JS("function(btn, map){ map.setView([38.3, -122.0], 10); }")
      ))
  })
  
  # Selected item tracking
  selected_item_data <- reactiveVal(NULL)
  
  # Observe map clicks
  observeEvent(input$homelessness_map_marker_click, {
    click <- input$homelessness_map_marker_click
    if (!is.null(click)) {
      updateTextInput(session, "selected_item", value = click$id)
      
      # Find data for the selected item
      if (startsWith(click$id, "school_")) {
        school_id <- sub("school_", "", click$id)
        school_data <- school_homelessness %>% filter(school_name == school_id)
        selected_item_data(list(type = "school", data = school_data))
      } else if (startsWith(click$id, "resource_")) {
        resource_id <- as.numeric(sub("resource_", "", click$id))
        resource_data <- resources_plottable[resource_id, ]
        selected_item_data(list(type = "resource", data = resource_data))
      }
    }
  })
  
  # Clear selection
  observeEvent(input$clear_selection, {
    updateTextInput(session, "selected_item", value = "")
    selected_item_data(NULL)
  })
  
  # Use observe to reactively update map layers when filters change
  observe({
    # Get the current map
    leafletProxy("homelessness_map") %>%
      clearGroup("District Boundaries") %>%
      clearGroup("Schools") %>%
      clearGroup("Resources")
    
    # Filter district boundaries based on selection
    filtered_districts <- district_geo
    if (!is.null(input$district_filter) && input$district_filter != "All Districts") {
      filtered_districts <- district_geo %>%
        filter(district_name == input$district_filter)
    }
    
    # Add the filtered district boundaries
    leafletProxy("homelessness_map") %>%
      addPolygons(
        data = filtered_districts,
        fillColor = "navy",
        fillOpacity = 0.2,
        weight = 2,
        color = "blue",
        opacity = 0.7,
        group = "District Boundaries",
        label = ~district_name,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "yellow",
          opacity = 1,
          bringToFront = TRUE
        )
      )
    
    # Add schools with filtered data
    schools_to_map <- filtered_schools()
    
    if (nrow(schools_to_map) > 0) {
      # Create labels for schools
      school_labels <- sprintf(
        "<strong>%s</strong><br/>District: %s<br/>School Type: %s<br/>Total Enrollment: %d<br/>Homeless Students: %d<br/>Percent Homeless: %.1f%%",
        schools_to_map$school_name,
        schools_to_map$district_name,
        schools_to_map$school_type,
        schools_to_map$enroll_total,
        schools_to_map$homeless_student_enrollment,
        schools_to_map$percent_homeless
      ) %>% lapply(HTML)
      
      # Define simplified school type to marker color mapping
      # Group schools as requested
      school_type_to_color <- list(
        "Elementary" = "lightblue",   # Elementary gets lightblue
        "K-12" = "lightblue",         # K-12 same as Elementary
        "High" = "darkblue",          # High School gets darkblue
        "Continuation" = "darkblue",  # Continuation same as High School
        "Middle" = "cadetblue",       # Middle School gets its own color
        "County Community" = "purple",  # Community schools get purple
        "Community Day" = "purple"     # Community Day same as Community
        # All others will default to gray
      )
      
      # Add school markers with custom icons based on school type
      for (i in 1:nrow(schools_to_map)) {
        school <- schools_to_map[i, ]
        
        # Map the school type to a marker color
        # If it doesn't exist in our mapping, default to gray
        marker_color <- school_type_to_color[[school$school_type]]
        if (is.null(marker_color)) marker_color <- "gray"
        
        # Create school icon with the appropriate color
        school_icon <- makeAwesomeIcon(
          icon = "graduation-cap",
          markerColor = marker_color,
          iconColor = "white",
          library = "fa"
        )
        
        # Create popup content
        popup_content <- paste0(
          "<h5>", school$school_name, "</h5>",
          "<strong>District:</strong> ", school$district_name, "<br>",
          "<strong>School Type:</strong> ", school$school_type, "<br>",
          "<strong>Total Enrollment:</strong> ", school$enroll_total, "<br>",
          "<strong>Homeless Students:</strong> ", school$homeless_student_enrollment, "<br>",
          "<strong>Percent Homeless:</strong> ", school$percent_homeless, "%<br>",
          "<hr>",
          "<strong>Housing Situations:</strong><br>",
          "Doubled-up: ", school$temporarily_doubled_up, "<br>",
          "Shelters: ", school$temporary_shelters, "<br>",
          "Hotels/Motels: ", school$hotels_motels, "<br>",
          "Unsheltered: ", school$temporarily_unsheltered, "<br>",
          "Unknown: ", school$missing_unknown
        )
        
        leafletProxy("homelessness_map") %>%
          addAwesomeMarkers(
            lng = school$longitude,
            lat = school$latitude,
            icon = school_icon,
            group = "Schools",
            layerId = paste0("school_", school$school_name),
            label = school_labels[[i]],
            popup = popup_content
          )
      }
      
      # Clear any existing legends and add the new one
      leafletProxy("homelessness_map") %>%
        clearControls() %>%
        addLegend(
          position = "bottomleft",
          title = "School Types",
          colors = c("lightblue", "cadetblue", "darkblue", "purple", "gray"),
          labels = c("Elementary/K-12", "Middle", "High/Continuation", "Community Schools", "Other"),
          opacity = 0.8
        )
    }
    
    # Add resources
    if (input$show_resources) {
      resources_to_map <- filtered_resources()
      
      # Further filter resources by district
      if (!is.null(input$district_filter) && input$district_filter != "All Districts") {
        # Create a mapping for special cases
        district_to_city <- list(
          "Fairfield-Suisun Unified" = c("Fairfield", "Suisun City", "Suisun"),
          "Vallejo City Unified" = c("Vallejo")
          # Add other special mappings as needed
        )
        
        # Get cities that correspond to the selected district
        if (input$district_filter %in% names(district_to_city)) {
          cities <- district_to_city[[input$district_filter]]
        } else {
          # Extract city name from district name (assumes format like "Benicia Unified")
          # This works for cases where the city name is the first word in the district name
          city_name <- strsplit(input$district_filter, " ")[[1]][1]
          cities <- c(city_name)
        }
        
        # Filter resources based on address containing the city name
        # Create a regex pattern to match any of the cities in the address field
        city_pattern <- paste(cities, collapse = "|")
        
        # Filter resources to only those with matching cities in the address
        resources_to_map <- resources_to_map %>%
          filter(grepl(city_pattern, Address, ignore.case = TRUE))
      }
      
      if (!is.null(resources_to_map) && nrow(resources_to_map) > 0) {
        # Define resource icons function
        getResourceIcon <- function(resource) {
          resource_icons <- list(
            "Emergency.Services" = makeAwesomeIcon(icon = "ambulance", markerColor = "red", library = "fa"),
            "Family.Resource.Center" = makeAwesomeIcon(icon = "home", markerColor = "green", library = "fa"),
            "Emergency.Shelter" = makeAwesomeIcon(icon = "bed", markerColor = "orange", library = "fa"),
            "Medical.Facility" = makeAwesomeIcon(icon = "hospital", markerColor = "cadetblue", library = "fa"),
            "Housing.Assistance" = makeAwesomeIcon(icon = "house-user", markerColor = "purple", library = "fa"),
            "Low.Income.Housing.Resources" = makeAwesomeIcon(icon = "building", markerColor = "darkpurple", library = "fa"),
            "Other" = makeAwesomeIcon(icon = "info", markerColor = "darkpurple", library = "fa")
          )
          
          # Check each category column
          for (category_name in names(resource_categories)) {
            category_col <- resource_categories[category_name]
            if (!is.null(resource[[category_col]]) && resource[[category_col]] == 1) {
              return(resource_icons[[category_col]])
            }
          }
          # Return default icon if no category matches
          return(resource_icons[["Other"]])
        }
        
        # Add each resource as a marker
        for (i in 1:nrow(resources_to_map)) {
          resource <- resources_to_map[i, ]
          
          # Determine which icon to use based on resource type
          icon_to_use <- getResourceIcon(resource)
          
          # Create HTML content for popup
          popup_content <- paste0(
            "<h5>", resource$Name, "</h5>",
            "<strong>Address:</strong> ", resource$Address, "<br>",
            "<strong>Phone:</strong> ", resource$Phone.Number, "<br>",
            ifelse(!is.null(resource$Website) && resource$Website != "N/A" && resource$Website != "",
                   paste0("<strong>Website:</strong> <a href='", resource$Website, "' target='_blank'>", 
                          ifelse(nchar(resource$Website) > 30, paste0(substr(resource$Website, 1, 30), "..."), resource$Website), 
                          "</a><br>"), ""),
            ifelse(!is.null(resource$Description) && resource$Description != "N/A" && resource$Description != "",
                   paste0("<strong>Description:</strong> ", resource$Description, "<br>"), "")
          )
          
          leafletProxy("homelessness_map") %>%
            addAwesomeMarkers(
              lng = resource$longitude,
              lat = resource$latitude,
              icon = icon_to_use,
              group = "Resources",
              layerId = paste0("resource_", i),
              popup = popup_content,
              label = resource$Name
            )
        }
      }
    } else {
      # Hide resources
      leafletProxy("homelessness_map") %>% 
        clearGroup("Resources")
    }
  })
  # District summary table
  output$district_table <- renderDT({
    datatable(
      district_summary %>%
        select(
          District = district_name,
          `Total Enrollment` = total_enrollment,
          `Homeless Students` = homeless_count,
          `% Homeless` = percent_homeless
        ) %>%
        arrange(desc(`% Homeless`)),
      options = list(
        pageLength = 5,
        dom = 't',
        scrollY = "250px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      selection = 'single',
      class = 'cell-border stripe'
    )
  })
  
  # Housing situation breakdown plot
  output$housing_plot <- renderPlot({
    # If a district is selected, filter to that district
    housing_data <- if (input$district_filter != "All Districts") {
      district_summary %>%
        filter(district_name == input$district_filter)
    } else {
      # Otherwise use total county data
      district_summary %>%
        filter(aggregate_level == "C")
    }
    
    # Convert to long format for plotting
    housing_long <- housing_data %>%
      pivot_longer(
        cols = c("doubled_up", "shelters", "hotels", "unsheltered", "unknown"),
        names_to = "housing_type",
        values_to = "count"
      ) %>%
      mutate(
        # Improve labels for display
        housing_type = case_match(
          housing_type,
          "doubled_up" ~ "Doubled-up",
          "shelters" ~ "Shelters",
          "hotels" ~ "Hotels/Motels",
          "unsheltered" ~ "Unsheltered",
          "unknown" ~ "Unknown"
        ),
        # Calculate percentages
        percentage = count / sum(count) * 100,
        # Create label with count and percentage
        label = sprintf("%d\n(%.1f%%)", count, percentage)
      )
    
    # Set color palette
    housing_colors <- c(
      "Doubled-up" = "#4e79a7",
      "Shelters" = "#f28e2c",
      "Hotels/Motels" = "#e15759",
      "Unsheltered" = "#76b7b2",
      "Unknown" = "#9c755f"
    )
    
    # Create the plot
    ggplot(housing_long, aes(x = housing_type, y = count, fill = housing_type)) +
      geom_bar(stat = "identity", width = 1, show.legend = F) +
      geom_text(
        aes(label = label),
        color = "white",
        fontface = "bold"
      ) +
      scale_fill_manual(values = housing_colors) +
      scale_y_continuous(labels = scales::label_comma()) +
      theme_minimal() +
      labs(
        title = paste("Housing Situations in", unique(housing_long$district_name)),
        x = NULL
      ) +
      theme(
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
  })
  
  # Resources table output
  output$resources_table <- renderDT({
    # Get the filtered resources
    resources_data <- filtered_resources()
    
    if (is.null(resources_data) || nrow(resources_data) == 0) {
      return(datatable(
        data.frame(Message = "No resources match the current filters or resources are hidden."),
        options = list(dom = 't'),
        rownames = FALSE,
        selection = 'none',
        class = 'cell-border stripe'
      ))
    }
    
    # Prepare data for display
    resources_display <- resources_data %>%
      mutate(
        # Create a Type column that shows the resource type(s)
        Type = case_when(
          Emergency.Services == 1 ~ "Emergency Services",
          Family.Resource.Center == 1 ~ "Family Resource Center",
          Emergency.Shelter == 1 ~ "Emergency Shelter",
          Medical.Facility == 1 ~ "Medical Facility",
          Housing.Assistance == 1 ~ "Housing Assistance",
          Low.Income.Housing.Resources == 1 ~ "Low Income Housing",
          TRUE ~ "Other"
        )
      ) %>%
      select(
        Name,
        Type,
        Address,
        Phone.Number
      ) %>%
      arrange(Name)
    
    # Create the datatable
    datatable(
      resources_display,
      options = list(
        pageLength = 10,
        dom = 'tip',
        scrollY = "300px",
        scrollCollapse = TRUE
      ),
      rownames = FALSE,
      selection = 'single',
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Type',
        backgroundColor = styleEqual(
          c("Emergency Services", "Family Resource Center", "Emergency Shelter", 
            "Medical Facility", "Housing Assistance", "Low Income Housing", "Other"),
          c("#f8d7da", "#d1e7dd", "#fff3cd", "#cfe2ff", "#e2d9f3", "#f8f9fa", "#e9ecef")
        )
      )
  })
  
  # Add observer for clicking resource in table
  observeEvent(input$resources_table_rows_selected, {
    if (!is.null(input$resources_table_rows_selected)) {
      selected_row <- input$resources_table_rows_selected
      
      # Get all resources that match our filters
      resources_data <- filtered_resources()
      
      if (!is.null(resources_data) && nrow(resources_data) > 0) {
        # Get the selected resource
        selected_resource <- resources_data[selected_row, ]
        
        # Update selected item
        updateTextInput(session, "selected_item", value = paste0("resource_", selected_row))
        selected_item_data(list(type = "resource", data = selected_resource))
        
        # Center map on the selected resource
        leafletProxy("homelessness_map") %>%
          setView(lng = selected_resource$longitude, lat = selected_resource$latitude, zoom = 14)
      }
    }
  })
  
  # Selected item details
  output$selected_details <- renderUI({
    selected <- selected_item_data()
    
    if (is.null(selected)) {
      return(NULL)
    }
    
    if (selected$type == "school") {
      school <- selected$data
      
      div(
        h5(school$school_name),
        p(strong("District:"), school$district_name),
        p(strong("School Type:"), school$school_type),
        p(strong("Total Enrollment:"), school$enroll_total),
        p(strong("Homeless Students:"), school$homeless_student_enrollment, 
          paste0("(", school$percent_homeless, "%)")),
        hr(),
        p(strong("Housing Situations:")),
        p("Doubled-up:", school$temporarily_doubled_up),
        p("Shelters:", school$temporary_shelters),
        p("Hotels/Motels:", school$hotels_motels),
        p("Unsheltered:", school$temporarily_unsheltered),
        p("Unknown:", school$missing_unknown)
      )
    } else if (selected$type == "resource") {
      resource <- selected$data
      
      # Determine resource types
      resource_types <- c()
      for (category_name in names(resource_categories)) {
        category_col <- resource_categories[category_name]
        if (!is.null(resource[[category_col]]) && resource[[category_col]] == 1) {
          resource_types <- c(resource_types, category_name)
        }
      }
      
      div(
        h5(resource$Name),
        p(strong("Type:"), paste(resource_types, collapse = ", ")),
        p(strong("Address:"), resource$Address),
        p(strong("Phone:"), resource$Phone),
        if (!is.null(resource$Website) && resource$Website != "N/A" && resource$Website != "") {
          p(strong("Website:"), 
            a(href = resource$Website, target = "_blank", 
              ifelse(nchar(resource$Website) > 30, 
                     paste0(substr(resource$Website, 1, 30), "..."), 
                     resource$Website)))
        },
        if (!is.null(resource$Email) && resource$Email != "N/A" && resource$Email != "") {
          p(strong("Email:"), resource$Email)
        },
        if (!is.null(resource$Services) && resource$Services != "N/A" && resource$Services != "") {
          p(strong("Services:"), resource$Services)
        }
      )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
