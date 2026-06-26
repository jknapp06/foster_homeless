# this version added extra views, but they don't work

library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(sf)
library(janitor)
library(openxlsx)

# Load data files
homeless <- read_csv("data/homeless.csv") %>%
  clean_names()

district_geo <- read_sf("data/California_School_District_Areas_2022-23.geojson") %>%
  clean_names() %>%
  filter(county_name == "Solano") %>%
  rename(cds = cds_code) %>%
  mutate(
    center_point = st_centroid(geometry),
    lng = st_coordinates(center_point)[,1],
    lat = st_coordinates(center_point)[,2]
  )

school_geo <- read_sf("data/California_Schools_2022-23.geojson") %>%
  clean_names() %>%
  filter(county_name == "Solano") %>%
  mutate(
    school_color = case_match(
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
      .default = "#808080"
    )
  ) %>%
  rename(cds = cds_code)

# Initialize missing columns with default values
expected_columns <- c(
  "homeless_student_enrollment", "percent_homeless", "temporarily_doubled_up",
  "temporary_shelters", "hotels_motels", "temporarily_unsheltered", "missing_unknown"
)

for (col in expected_columns) {
  if (!(col %in% colnames(school_geo))) {
    school_geo[[col]] <- NA
  }
}

resources_plottable <- read.xlsx("data/resources_with_addresses.xlsx") %>% 
  clean_names()
  # %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Clean and prepare homeless data
homeless_total <- homeless %>%
  filter(
    county_name == "Solano",
    academic_year == "2023-24",
    dass == "All" | aggregate_level == "S",
    charter_school == "All" | aggregate_level == "S",
    reporting_group == "Total"
  ) %>%
  mutate(
    across(
      c(temporarily_doubled_up, temporary_shelters,
        hotels_motels, temporarily_unsheltered,
        missing_unknown),
      ~ifelse(is.character(.), parse_number(.), .)
    )
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
ui <- page_navbar(
  title = tags$div(
    style = "display: flex; align-items: center;",
    tags$img(src = "SCOE_Logo.jpg", height = 40, width = 75, style = "margin-right: 10px;"),
    "Solano County Resources"
  ),
  theme = bs_theme(bootswatch = "cosmo"),
  
  # Main page with map and filters
  nav_panel(
    title = "Map",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("district_filter", "Filter by District",
                    choices = c("All Districts", unique(school_geo$district_name))),
        selectInput("school_filter", "Filter by School Type",
                    choices = c("All Types", unique(school_geo$school_type))),
        checkboxInput("show_resources", "Show Resources", value = TRUE),
        conditionalPanel(
          condition = "input.show_resources",
          checkboxGroupInput("resource_type", "Resource Type",
                             choices = names(resource_categories),
                             selected = names(resource_categories))
        ),
        p("Data source: CA Dept. Ed and Children's Network of Solano County")
      ),
      leafletOutput("homelessness_map", height = "600px")
    )
  ),
  
  # Resources tab
  nav_panel(
    title = "Resources",
    layout_column_wrap(
      width = 1/4,
      card(
        card_header("Filter Resources"),
        sliderInput("distance_threshold", "Distance Threshold (miles)",
                    min = 5, max = 30, value = 10, step = 5, ticks = FALSE),
        checkboxGroupInput("resource_type_filter", "Resource Type",
                           choices = names(resource_categories),
                           selected = names(resource_categories))
      ),
      card(
        card_header("Community Resources"),
        card_body(
          DTOutput("resources_table")
        )
      )
    )
  ),
  
  # Schools tab
  nav_panel(
    title = "Schools",
    layout_sidebar(
      sidebar = sidebar(
        card(
          card_header("Filter Schools"),
          selectInput("district_filter_schools", "Filter by District",
                      choices = c("All Districts", unique(school_geo$district_name))),
          selectInput("school_filter_schools", "Filter by School", choices = NULL),
          selectInput("resource_type_schools", "Resource Type",
                      choices = c("All Types", names(resource_categories))),
          sliderInput("distance_threshold_schools", "Distance Threshold (miles)",
                      min = 5, max = 30, value = 10, step = 5, ticks = FALSE)
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Schools"),
          DTOutput("schools_table")
        ),
        card(
          card_header("Schools Without Resources"),
          DTOutput("schools_without_resources_table")
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Housing Situations"),
          plotOutput("housing_plot")
        ),
        card(
          card_header("District Summary"),
          DTOutput("district_table")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive filtered datasets
  filtered_schools <- reactive({
    schools <- school_geo
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
    if (!is.null(input$resource_type) && length(input$resource_type) > 0) {
      selected_columns <- resource_categories[input$resource_type]
      resources <- resources %>%
        filter(rowSums(across(all_of(selected_columns), ~ifelse(is.na(.), 0, .))) > 0)
    }
    print("Resources Filtered.")
    return(resources)
  })
  
  # Selected item tracking
  selected_item_data <- reactiveVal(NULL)
  
  # Initialize the map
  output$homelessness_map <- renderLeaflet({
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
  
  # Observe changes and update the map using leafletProxy
  observe({
    schools <- filtered_schools()
    resources <- filtered_resources()
    
    leafletProxy("homelessness_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls()
    
    # Add district boundaries
    leafletProxy("homelessness_map") %>%
      addPolygons(
        data = district_geo,
        fillColor = "#3388ff",
        weight = 1,
        opacity = 1,
        color = "#666",
        fillOpacity = 0.2,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.5,
          bringToFront = TRUE
        ),
        label = ~district_name,
        layerId = ~district_name,
        group = "District Boundaries"
      )
    
    # Add school markers
    if (nrow(schools) > 0) {
      school_labels <- sprintf(
        "<strong>%s</strong><br/>District: %s<br/>School Type: %s<br/>Total Enrollment: %d",
        schools$school_name,
        schools$district_name,
        schools$school_type,
        schools$enroll_total
      ) %>% lapply(HTML)
      
      school_type_to_color <- list(
        "Elementary" = "lightblue",
        "K-12" = "lightblue",
        "High" = "darkblue",
        "Continuation" = "darkblue",
        "Middle" = "cadetblue",
        "County Community" = "purple",
        "Community Day" = "purple"
      )
      
      for (i in 1:nrow(schools)) {
        school <- schools[i, ]
        if (is.na(school$longitude) || is.na(school$latitude)) next
        
        marker_color <- school_type_to_color[[school$school_type]]
        if (is.null(marker_color)) marker_color <- "gray"
        
        school_icon <- makeAwesomeIcon(
          icon = "graduation-cap",
          markerColor = marker_color,
          iconColor = "white",
          library = "fa"
        )
        
        popup_content <- paste0(
          "<h5>", school$school_name, "</h5>",
          "<strong>District:</strong> ", school$district_name, "<br>",
          "<strong>School Type:</strong> ", school$school_type, "<br>",
          "<strong>Total Enrollment:</strong> ", school$enroll_total, "<br>"
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
    if (!is.null(resources) && nrow(resources) > 0) {
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
        
        for (category_name in names(resource_categories)) {
          category_col <- resource_categories[category_name]
          if (!is.null(resource[[category_col]]) && resource[[category_col]] == 1) {
            return(resource_icons[[category_col]])
          }
        }
        return(resource_icons[["Other"]])
      }
      
      for (i in 1:nrow(resources)) {
        resource <- resources[i, ]
        if (is.na(resource$longitude) || is.na(resource$latitude)) next
        
        icon_to_use <- getResourceIcon(resource)
        
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
    } else {
      leafletProxy("homelessness_map") %>%
        clearGroup("Resources")
    }
  })
  
  # Selected item details
  output$selected_details <- renderUI({
    item <- selected_item_data()
    if (is.null(item)) return(NULL)
    
    if (item$type == "school") {
      school <- item$data
      HTML(paste(
        "<h4>", school$school_name, "</h4>",
        "<p><strong>School Type:</strong> ", school$school_type, "</p>",
        "<p><strong>District:</strong> ", school$district_name, "</p>",
        "<p><strong>Address:</strong> ", school$street, ", ", school$city, ", ", school$state, " ", school$zip, "</p>"
      ))
    } else if (item$type == "resource") {
      resource <- item$data
      resource_types <- names(resource_categories)[unlist(lapply(resource_categories, function(x) {
        if(is.na(resource[[x]])) return(FALSE)
        return(resource[[x]] == 1)
      }))]
      
      HTML(paste(
        "<h4>", resource$Name, "</h4>",
        "<p><strong>Resource Type:</strong> ", paste(resource_types, collapse = ", "), "</p>",
        "<p><strong>Address:</strong> ", resource$Address, "</p>",
        "<p><strong>Phone:</strong> ", resource$Phone, "</p>",
        "<p><strong>Website:</strong> <a href='", resource$Website, "' target='_blank'>", resource$Website, "</a></p>"
      ))
    }
  })
  
  # Update school filter based on district selection in Schools tab
  observe({
    if (input$district_filter_schools == "All Districts") {
      school_choices <- c("All Schools", unique(school_geo$school_name))
    } else {
      filtered <- school_geo %>% filter(district_name == input$district_filter_schools)
      school_choices <- c("All Schools", unique(filtered$school_name))
    }
    updateSelectInput(session, "school_filter_schools", choices = school_choices)
  })
  
  # Schools tab data table
  output$schools_table <- renderDT({
    schools_data <- school_geo %>%
      st_drop_geometry() %>%
      select(school_name, school_type, district_name, city)
    
    if (input$district_filter_schools != "All Districts") {
      schools_data <- schools_data %>% filter(district_name == input$district_filter_schools)
    }
    
    if (!is.null(input$school_filter_schools) && input$school_filter_schools != "All Schools") {
      schools_data <- schools_data %>% filter(school_name == input$school_filter_schools)
    }
    
    datatable(
      schools_data,
      options = list(
        pageLength = 10,
        dom = 'tip',
        scrollY = "400px"
      ),
      rownames = FALSE
    )
  })
  
  # Schools without resources table
  output$schools_without_resources_table <- renderDT({
    distance <- input$distance_threshold_schools
    resource_type <- ifelse(input$resource_type_schools == "All Types", "any", input$resource_type_schools)
    
    # Get the appropriate precomputed data
    file_name <- paste0("without_", gsub(" ", "_", tolower(resource_type)), "_within_", distance, "_miles.csv")
    
    if (file_name %in% names(precomputed_schools_without_resources)) {
      schools_without <- precomputed_schools_without_resources[[file_name]]
      
      if (input$district_filter_schools != "All Districts") {
        schools_without <- schools_without %>% filter(district_name == input$district_filter_schools)
      }
      
      if ((!is.null(input$school_filter_schools) && input$school_filter_schools != "All Schools")) {
        schools_without <- schools_without %>% filter(school_name == input$school_filter_schools)
      }
      
      datatable(
        schools_without,
        options = list(
          pageLength = 10,
          dom = 'tip',
          scrollY = "400px"
        ),
        rownames = FALSE
      )
    } else {
      # Fallback if precomputed data not available
      datatable(
        data.frame(message = paste("No data available for", resource_type, "within", distance, "miles")),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # Resources table
  output$resources_table <- renderDT({
    distance <- input$distance_threshold
    resource_types <- input$resource_type_filter
    
    if (length(resource_types) == 0) {
      return(datatable(
        data.frame(message = "Please select at least one resource type"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Filter resources based on selected types
    selected_columns <- resource_categories[resource_types]
    
    resources_data <- resources_plottable %>%
      st_drop_geometry() %>%
      filter(rowSums(across(all_of(selected_columns), ~ifelse(is.na(.), 0, .))) > 0) %>%
      select(Name, Type, Address, City, Phone, Website)
    
    datatable(
      resources_data,
      options = list(
        pageLength = 15,
        dom = 'tip',
        scrollY = "400px"
      ),
      rownames = FALSE
    )
  })
  
  output$district_table <- renderDT({
    district_data <- district_summary %>%
      select(district_name, total_enrollment, homeless_count, percent_homeless,
             doubled_up, shelters, hotels, unsheltered, unknown)
    
    if (input$district_filter_schools != "All Districts") {
      district_data <- district_data %>% filter(district_name == input$district_filter_schools)
    }
    
    datatable(
      district_data,
      options = list(
        dom = 'tip',
        scrollX = TRUE,
        pageLength = 5
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'percent_homeless',
        background = styleColorBar(c(0, max(district_data$percent_homeless, na.rm = TRUE)), '#b7d8ff'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Housing situation plot
  output$housing_plot <- renderPlot({
    # Filter data based on selected district
    plot_data <- district_summary
    
    if (input$district_filter_schools != "All Districts") {
      plot_data <- plot_data %>% filter(district_name == input$district_filter_schools)
    }
    
    # Skip plotting if no data
    if (nrow(plot_data) == 0) {
      return(NULL)
    }
    
    # Prepare data for plotting
    plot_data <- plot_data %>%
      select(district_name, doubled_up, shelters, hotels, unsheltered, unknown) %>%
      pivot_longer(cols = c(doubled_up, shelters, hotels, unsheltered, unknown),
                   names_to = "housing_situation", values_to = "count") %>%
      mutate(
        housing_situation = factor(housing_situation,
                                   levels = c("doubled_up", "shelters", "hotels", "unsheltered", "unknown"),
                                   labels = c("Doubled Up", "Shelters", "Hotels/Motels", "Unsheltered", "Unknown"))
      )
    
    # Create plot
    ggplot(plot_data, aes(x = housing_situation, y = count, fill = housing_situation)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Homeless Student Housing Situations",
        x = "Housing Situation",
        y = "Number of Students"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
