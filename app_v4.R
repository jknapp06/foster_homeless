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

school_geo <- read_sf("data/California_Schools_2023-24.geojson") %>%
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

resources_plottable <- read.xlsx("data/resources_with_addresses.xlsx") %>%
  clean_names()

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

# Define broader resource categories
resource_categories <- list(
  "Housing Assistance" = c(
    "housing_assistance", "transitional_housing", "rental_assistance_program",
    "emergency_shelter", "low_income_housing_resources"
  ),
  "Food Assistance" = c(
    "food_assistance", "food_pantries", "family_farms", "farmers_markets"
  ),
  "Family Support Services" = c(
    "family_support_services", "family_counseling", "clothing_and_diaper_assistance",
    "domestic_violence_support", "baby_formula_resources", "adults_with_disabilities",
    "children_with_disabilities", "victim_assistance_programs", "family_resource_center"
  ),
  "Emergency Services" = c(
    "emergency_services", "safe_surrender_site", "police_station", "fire_department"
  ),
  "Healthcare Services" = c(
    "medical_facility", "healthcare_services", "community_clinic",
    "free_or_low_cost_medical_services", "substance_abuse_treatment", "senior_services",
    "mental_health_resource", "maternity_resources_and_support"
  ),
  "Community Activities" = c(
    "community_activities", "museum", "recreation_and_leisure", "public_library"
  ),
  "Education and Literacy" = c(
    "education_and_literacy", "tutoring_services", "college"
  )
)

# Load precomputed resources within distances
distance_thresholds <- c(5, 10, 15, 20, 25, 30)
precomputed_resources <- list()

for (threshold in distance_thresholds) {
  for (type in names(resource_categories)) {
    # Replace spaces with underscores for the file names
    file_name <- paste0("data/within_", threshold, "_miles_", type, ".csv")
    if (file.exists(file_name)) {
      precomputed_resources[[paste0("within_", threshold, "_miles_", type)]] <- read_csv(file_name)
    }
  }
}

# Load precomputed schools without nearby resources
precomputed_schools_without_resources <- list()

for (threshold in distance_thresholds) {
  # For each resource type
  for (type in names(resource_categories)) {
    file_name <- paste0("data/without_", type, "_within_", threshold, "_miles.csv")
    if (file.exists(file_name)) {
      precomputed_schools_without_resources[[paste0("without_", type, "_within_", threshold, "_miles")]] <- read_csv(file_name)
    }
  }
  
  # For any resource
  file_name <- paste0("data/without_any_resource_within_", threshold, "_miles.csv")
  if (file.exists(file_name)) {
    precomputed_schools_without_resources[[paste0("without_any_resource_within_", threshold, "_miles")]] <- read_csv(file_name)
  }
}

# Function to get schools without resources
get_schools_without_resources <- function(resource_type, distance) {
  if (resource_type == "All Types" || resource_type == "all") {
    file_key <- paste0("without_any_resource_within_", distance, "_miles")
  } else {
    file_key <- paste0("without_", resource_type, "_within_", distance, "_miles")
  }
  
  if (file_key %in% names(precomputed_schools_without_resources)) {
    return(precomputed_schools_without_resources[[file_key]])
  } else {
    return(data.frame(school_name = character(0), district_name = character(0)))
  }
}

# Function to get resources near a school - adapted for your actual data structure
get_resources_near_school <- function(school_name, resource_category, distance) {
  # For debugging
  print(paste("Getting resources for school:", school_name))
  print(paste("Resource category:", resource_category))
  print(paste("Distance threshold:", distance, "miles"))
  
  if (is.null(school_name) || school_name == "") {
    print("No school selected")
    return(data.frame())
  }
  
  # Looking for a specific category
  file_key <- paste0("within_", distance, "_miles_", resource_category)
  print(paste("Looking for specific category file:", file_key))
  
  if (file_key %in% names(precomputed_resources)) {
    resources_data <- precomputed_resources[[file_key]]
    print(paste("Found precomputed file with", nrow(resources_data), "total records"))
    
    # First, we need to get the school_id for the selected school
    school_id <- NULL
    if ("school_id" %in% names(resources_data)) {
      # Get unique school IDs from the data
      unique_schools <- unique(resources_data$school_id)
      print(paste("Found", length(unique_schools), "unique schools in the data"))
      
      # If we have a schools dataframe with name to ID mapping, use it
      if (exists("schools") && "school_id" %in% names(schools) && "school_name" %in% names(schools)) {
        school_row <- schools %>% filter(school_name == school_name)
        if (nrow(school_row) > 0) {
          school_id <- school_row$school_id[1]
          print(paste("Found school_id", school_id, "for school", school_name))
        }
      } else {
        # If we don't have a school mapping, just use the school name as an approximation
        print("No school ID mapping found, using school name instead")
        school_id <- school_name
      }
      
      # If we found a school ID, filter resources
      if (!is.null(school_id)) {
        school_resources <- resources_data %>% 
          filter(school_id == school_id)
        
        print(paste("Found", nrow(school_resources), "resources for school ID", school_id))
      } else {
        print("Could not determine school ID, returning all resources")
        school_resources <- resources_data
      }
    } else {
      # If there's no school_id column, return all resources
      print("No school_id column found in data, returning all resources")
      school_resources <- resources_data
    }
    
    if (nrow(school_resources) > 0) {
      # Add category information
      school_resources$category <- resource_category
      
      # Get the resource details
      resource_details <- get_resource_details(school_resources)
      
      return(resource_details)
    } else {
      print("No resources found for this school in the specified category")
      return(data.frame())
    }
  } else {
    print(paste("File not found:", file_key))
    return(data.frame())
  }
}

# Helper function to retrieve resource details based on resource_id
get_resource_details <- function(resources_data) {
  # Check if we have a resources master list
  if (exists("resources_master") && "resource_id" %in% names(resources_master)) {
    print("Looking up details in resources master list")
    
    # Join the resources_data with the master list to get details
    if ("resource_id" %in% names(resources_data)) {
      detailed_resources <- resources_data %>%
        left_join(resources_master, by = "resource_id")
      
      print(paste("Retrieved details for", nrow(detailed_resources), "resources"))
      return(detailed_resources)
    }
  }
  
  # If we don't have a master list or resource_id column, just return the original data
  print("No resource master list available, returning original data")
  return(resources_data)
}

# Function to get schools near a resource
get_schools_near_resource <- function(resource_name, distance) {
  # Since we don't have a direct precomputed file for this,
  # we'll use the distances_df to filter
  if (exists("distances_df")) {
    nearby_schools <- distances_df %>%
      filter(resource_name == !!resource_name, 
             distance_miles <= distance) %>%
      left_join(school_geo %>% st_drop_geometry() %>% 
                  select(school_name, district_name, school_type, city),
                by = "school_name")
    return(nearby_schools)
  }
  return(data.frame())
}

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
        checkboxInput("show_resources", "Show Resources", value = TRUE),
        conditionalPanel(
          condition = "input.show_resources",
          checkboxGroupInput("resource_type", "Resource Type",
                             choices = names(resource_categories),
                             selected = names(resource_categories))
        ),
        p("Data source:"),
        p("CA Dept. Ed"),
        a("Children's Network of Solano County", href = "https://www.childnet.org/")
      ),
      leafletOutput("homelessness_map", height = "600px")
    )
  ),
  
  # Resources tab
  nav_panel(
    title = "Resources",
    page_sidebar(
      sidebar = sidebar(
        card(
          selectInput("district_filter_resources", "School District:",
                      choices = c("All Districts", unique(school_geo$district_name))),
          selectInput("school_filter_resources", "School:",
                      choices = c("Select a district first")),
          checkboxGroupInput("resource_types_filter", "Resource Types:",
                             choices = names(resource_categories),
                             selected = names(resource_categories)),
          sliderInput("distance_threshold", "Show resources within (miles):",
                      min = 5, max = 30, value = 5, step = 5, 
                      ticks = FALSE),
          actionButton("find_resources", "Find Resources", 
                       class = "btn-primary btn-block")
        )
      ),
      
      # Main content area with resources near selected school
      layout_columns(
        card(
          card_header(textOutput("selected_school_header")),
          p("Resources available near this school:"),
          
          # # Debug message
          # verbatimTextOutput("debug_output"),
          
          # Resources table with category column
          h4("All Resources"),
          DTOutput("all_resources_table"),
          
          # Resource details card that appears when a resource is clicked
          uiOutput("resource_details_card")
        )
      )
    )
  ),
  
  # Analysis tab
  nav_panel(
    title = "Analysis",
    page_sidebar(
      sidebar = sidebar(
        card(
          card_header("Resource Gap Analysis"),
          selectInput("gap_resource_type", "Resource Type:",
                      choices = c("All Types", names(resource_categories))),
          sliderInput("gap_distance", "Distance Threshold (miles):",
                      min = 5, max = 30, value = 15, step = 5, 
                      ticks = FALSE),
          actionButton("analyze_gaps", "Find Schools Without Resources", 
                       class = "btn-primary btn-block")
        )
      ),
      
      layout_columns(
        card(
          card_header("Schools Without Nearby Resources"),
          p("The following schools do not have the selected resource type within the specified distance:"),
          DTOutput("schools_without_resources_table")
        ),
        card(
          card_header("Resources by School Type"),
          plotOutput("resources_by_school_type")
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
 
# Map tab ------------------------- 
  # Reactive filtered datasets
  filtered_schools <- reactive({
    school_geo
  })
  
  filtered_resources <- reactive({
    if (!input$show_resources) {
      return(NULL)
    }
    resources <- resources_plottable
    if (!is.null(input$resource_type) && length(input$resource_type) > 0) {
      selected_columns <- unlist(resource_categories[input$resource_type])
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
        "%s",
        schools$school_name
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
          "<strong>Address:</strong> ", school$street, ", ", school$city, ", ", school$state, " ", school$zip, "<br>",
          "<strong>Total Enrollment:</strong> ", school$enroll_total, "<br>",
          "<strong>Homeless Enrollment:</strong> ", school$ho_mcount, "<br>",
          "<strong>Homeless Percent:</strong> ", school$ho_mpct, "%<br>",
          "<br>",
          ifelse(school$charter == "Y", "<strong style='border: 1px solid black; padding: 3px;'>Charter</strong> ", ""),
          ifelse(!is.null(school$title_i_status) && school$title_i_status == "Y", "<strong style='border: 1px solid black; padding: 3px;'>Title I</strong>", "")
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
          "Housing Assistance" = makeAwesomeIcon(icon = "home", markerColor = "green", library = "fa"),
          "Food Assistance" = makeAwesomeIcon(icon = "cutlery", markerColor = "orange", library = "fa"),
          "Family Support Services" = makeAwesomeIcon(icon = "users", markerColor = "blue", library = "fa"),
          "Emergency Services" = makeAwesomeIcon(icon = "ambulance", markerColor = "red", library = "fa"),
          "Healthcare Services" = makeAwesomeIcon(icon = "medkit", markerColor = "cadetblue", library = "fa"),
          "Community Activities" = makeAwesomeIcon(icon = "calendar", markerColor = "purple", library = "fa"),
          "Education and Literacy" = makeAwesomeIcon(icon = "book", markerColor = "darkgreen", library = "fa"),
          "Other" = makeAwesomeIcon(icon = "info", markerColor = "darkpurple", library = "fa")
        )
        
        for (category_name in names(resource_categories)) {
          category_cols <- resource_categories[[category_name]]
          if (any(!is.na(resource[category_cols]) & resource[category_cols] == 1)) {
            return(resource_icons[[category_name]])
          }
        }
        return(resource_icons[["Other"]])
      }
      
      for (i in 1:nrow(resources)) {
        resource <- resources[i, ]
        if (is.na(resource$longitude) || is.na(resource$latitude)) next
        
        icon_to_use <- getResourceIcon(resource)
        
        # Determine resource categories
        resource_types <- names(resource_categories)[sapply(resource_categories, function(x) {
          any(!is.na(resource[x]) & resource[x] == 1)
        })]
        
        popup_content <- paste0(
          "<h5>", resource$name, "</h5>",
          "<strong>Address:</strong> ", resource$address, "<br>",
          ifelse(!is.na(resource$phone_number) && resource$phone_number != "N/A" && resource$phone_number != "",
                 paste0("<strong>Phone:</strong> ", resource$phone_number, "<br>"), ""),
          ifelse(!is.na(resource$website) && resource$website != "N/A" && resource$website != "",
                 paste0("<strong>Website:</strong> <a href='", resource$website, "' target='_blank'>",
                        ifelse(nchar(resource$website) > 30, paste0(substr(resource$website, 1, 30), "..."), resource$website),
                        "</a><br>"), ""),
          ifelse(!is.na(resource$description) && resource$description != "N/A" && resource$description != "",
                 paste0("<strong>Description:</strong> ", resource$description, "<br>"), ""),
          "<br>",
          paste(sapply(resource_types, function(rt) paste0("<span style='border: 1px solid black; padding: 3px; margin-right: 3px;'>", rt, "</span>")), collapse = " ")
        )
        
        
        leafletProxy("homelessness_map") %>%
          addAwesomeMarkers(
            lng = resource$longitude,
            lat = resource$latitude,
            icon = icon_to_use,
            group = "Resources",
            layerId = paste0("resource_", i),
            popup = popup_content,
            label = resource$name
          )
      }
    } else {
      leafletProxy("homelessness_map") %>%
        clearGroup("Resources")
    }
  })
  
# Resource tab ---------------------
  
  # Update school dropdown based on district selection in Resources tab
  observe({
    if (input$district_filter_resources == "All Districts") {
      school_choices <- sort(unique(school_geo$school_name))
    } else {
      filtered <- school_geo %>% 
        filter(district_name == input$district_filter_resources)
      school_choices <- sort(unique(filtered$school_name))
    }
    updateSelectInput(session, "school_filter_resources", 
                      choices = school_choices,
                      selected = school_choices[1])
  })
  
  # Create dynamic tabs for resource categories
  observe({
    # Only create tabs once
    if (length(input$resource_types_filter) > 0 && !exists("tabs_created") || !tabs_created) {
      # Remove existing tabs except the first one (All Resources)
      removeTab("resource_categories_tabs", target = "All Resources", session = session)
      
      # Add All Resources tab
      appendTab("resource_categories_tabs",
                tabPanel("All Resources", DTOutput("all_resources_table")),
                select = TRUE,
                session = session)
      
      # Add a tab for each resource category
      for (category in names(resource_categories)) {
        appendTab("resource_categories_tabs",
                  tabPanel(category, DTOutput(paste0("resources_table_", gsub(" ", "_", category)))),
                  session = session)
      }
      
      # Set flag to indicate tabs have been created
      tabs_created <- TRUE
    }
  })
  
  nearby_resources <- reactive({
    req(input$school_filter_resources, input$find_resources)
    req(input$distance_threshold)
    
    # Combine all selected resource types
    all_nearby_resources <- data.frame()
    
    for (category in input$resource_types_filter) {
      resources <- get_resources_near_school(
        input$school_filter_resources,
        category,
        input$distance_threshold
      )
      
      if(nrow(resources) > 0) {
        # Ensure category is included
        if(!"category" %in% names(resources)) {
          resources$category <- category
        }
        all_nearby_resources <- bind_rows(all_nearby_resources, resources)
      }
    }
    
    # Check if we found any resources
    if(nrow(all_nearby_resources) > 0) {
      print("Columns in combined resources data:")
      print(names(all_nearby_resources))
      
      # Ensure we have a clean dataset to display
      all_nearby_resources <- all_nearby_resources %>% distinct()
      
      # If we have a distance column, sort by it
      if("distance_miles" %in% names(all_nearby_resources)) {
        all_nearby_resources <- all_nearby_resources %>% arrange(distance_miles)
      }
      
      print(paste("Final dataset has", nrow(all_nearby_resources), "rows"))
    } else {
      # Create an empty dataframe with minimal structure
      all_nearby_resources <- data.frame(
        category = character(0),
        message = character(0)
      )
      print("No resources found for the selected criteria")
    }
    
    return(all_nearby_resources)
  })
  
  # Update the resources table renderer to work with your data structure
  output$all_resources_table <- renderDT({
    resources_to_display <- nearby_resources()
    
    if(nrow(resources_to_display) > 0) {
      # For now, just display all columns that we have
      display_data <- resources_to_display
      
      # Prepare data for display - select commonly useful columns if they exist
      useful_columns <- intersect(
        c("resource_id", "distance_miles", "category", 
          "resource_name", "organization_name", "address", "phone", "website"),
        names(display_data)
      )
      
      # If we found some useful columns, use those; otherwise use all columns
      if(length(useful_columns) > 0) {
        display_data <- display_data %>% select(all_of(useful_columns))
      }
      
      # Format the datatable
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          dom = 'tip',
          scrollX = TRUE,
          scrollY = "300px"
        ),
        selection = "single",
        rownames = FALSE
      ) %>%
        formatRound(columns = intersect("distance_miles", names(display_data)), 2)
    } else {
      # Show a message when no resources are found
      datatable(
        data.frame(
          message = paste("No resources found within", 
                          input$distance_threshold, 
                          "miles of this school")
        ),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  # Selected school header
  output$selected_school_header <- renderText({
    req(input$school_filter_resources)
    
    school_info <- school_geo %>%
      filter(school_name == input$school_filter_resources) %>%
      st_drop_geometry()
    
    if(nrow(school_info) > 0) {
      paste0(school_info$school_name, " (", school_info$district_name, ")")
    } else {
      "Please select a school"
    }
  })
  
  # Create tables for each resource category
  lapply(names(resource_categories), function(category) {
    category_id <- gsub(" ", "_", category)
    
    output[[paste0("resources_table_", category_id)]] <- renderDT({
      req(input$school_filter_resources, input$find_resources)
      
      resources <- get_resources_near_school(
        input$school_filter_resources, 
        category, 
        input$distance_threshold
      )
      
      if(nrow(resources) > 0) {
        resources <- resources %>%
          select(name, distance_miles, address, city) %>%
          distinct() %>%
          arrange(distance_miles)
        
        datatable(
          resources,
          options = list(
            pageLength = 10,
            dom = 'tip',
            scrollY = "300px"
          ),
          selection = "single",
          rownames = FALSE
        ) %>%
          formatRound('distance_miles', 2)
      } else {
        data.frame(message = paste("No", tolower(category), "resources found within", 
                                   input$distance_threshold, 
                                   "miles of this school"))
      }
    })
  })
  
  # Track selected resource
  selected_resource <- reactiveVal(NULL)
  
  # Observe resource selection in the "All Resources" table
  observeEvent(input$all_resources_table_rows_selected, {
    req(input$all_resources_table_rows_selected)
    
    all_resources <- get_resources_combined()
    if(nrow(all_resources) > 0 && length(input$all_resources_table_rows_selected) > 0) {
      selected_row <- input$all_resources_table_rows_selected
      resource_name <- all_resources$name[selected_row]
      
      # Get full resource details
      resource_details <- resources_plottable %>%
        filter(name == resource_name) %>%
        st_drop_geometry()
      
      if(nrow(resource_details) > 0) {
        selected_resource(resource_details)
      }
    }
  })
  
  # Also observe selections in category-specific tables
  lapply(names(resource_categories), function(category) {
    category_id <- gsub(" ", "_", category)
    table_id <- paste0("resources_table_", category_id, "_rows_selected")
    
    observeEvent(input[[table_id]], {
      req(input[[table_id]])
      
      resources <- get_resources_near_school(
        input$school_filter_resources, 
        category, 
        input$distance_threshold
      )
      
      if(nrow(resources) > 0 && length(input[[table_id]]) > 0) {
        selected_row <- input[[table_id]]
        resource_name <- resources$name[selected_row]
        
        # Get full resource details
        resource_details <- resources_plottable %>%
          filter(name == resource_name) %>%
          st_drop_geometry()
        
        if(nrow(resource_details) > 0) {
          selected_resource(resource_details)
        }
      }
    })
  })
  
  # Helper function to get combined resources for all selected categories
  get_resources_combined <- reactive({
    req(input$school_filter_resources)
    
    all_resources <- data.frame()
    
    for (category in input$resource_types_filter) {
      resources <- get_resources_near_school(
        input$school_filter_resources, 
        category, 
        input$distance_threshold
      )
      
      if(nrow(resources) > 0) {
        resources$category <- category
        all_resources <- bind_rows(all_resources, resources)
      }
    }
    
    if(nrow(all_resources) > 0) {
      all_resources %>%
        select(name, category, distance_miles, address, city) %>%
        distinct() %>%
        arrange(distance_miles)
    } else {
      data.frame()
    }
  })
  
  # Resource details card UI (continued)
  output$resource_details_card <- renderUI({
    resource <- selected_resource()
    
    if(is.null(resource)) {
      return(NULL)
    }
    
    # Determine which resource categories this resource belongs to
    resource_types <- names(resource_categories)[sapply(resource_categories, function(cols) {
      any(!is.na(resource[cols]) & resource[cols] == 1)
    })]
    
    card(
      full_screen = TRUE,
      card_header(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h3(resource$name),
          actionButton("close_resource_details", "×", 
                       style = "background: none; border: none; font-size: 24px;")
        )
      ),
      
      tags$div(
        class = "p-3",
        
        # Resource categories as tags/badges
        tags$div(
          class = "mb-3",
          lapply(resource_types, function(type) {
            tags$span(
              class = "badge bg-primary me-1",
              type
            )
          })
        ),
        
        # Address with map button
        tags$div(
          class = "mb-2",
          tags$strong("Address: "), 
          span(resource$address), 
          " ", 
          span(resource$city), 
          " ", 
          span(resource$state), 
          " ", 
          span(resource$zip_code),
          actionButton(
            "show_on_map", 
            label = tags$i(class = "fa fa-map"), 
            class = "btn btn-sm btn-outline-primary ms-2"
          )
        ),
        
        # Phone
        if(!is.na(resource$phone_number) && resource$phone_number != "") {
          tags$div(
            class = "mb-2",
            tags$strong("Phone: "), 
            span(resource$phone_number)
          )
        },
        
        # Website
        if(!is.na(resource$website) && resource$website != "") {
          tags$div(
            class = "mb-2",
            tags$strong("Website: "), 
            tags$a(href = resource$website, target = "_blank", resource$website)
          )
        },
        
        # Description
        if(!is.na(resource$description) && resource$description != "") {
          tags$div(
            class = "mb-2",
            tags$strong("Description: "), 
            p(resource$description)
          )
        },
        
        # Hours
        if(!is.na(resource$hours) && resource$hours != "") {
          tags$div(
            class = "mb-2",
            tags$strong("Hours: "), 
            p(resource$hours)
          )
        },
        
        # Distance from school
        tags$div(
          class = "mb-2",
          tags$strong("Distance from school: "), 
          span(format(round(as.numeric(get_distance_to_school(resource$name)), 2), nsmall = 2)),
          " miles"
        ),
        
        # Print button
        div(
          class = "mt-4 text-center",
          actionButton("print_resource", "Print Resource Information", 
                       class = "btn btn-success", 
                       icon = icon("print"))
        )
      )
    )
  })
  
  # Close resource details
  observeEvent(input$close_resource_details, {
    selected_resource(NULL)
  })
  
  # Get distance between selected school and resource
  get_distance_to_school <- function(resource_name) {
    req(input$school_filter_resources)
    
    # Check if we have the distance in our precomputed data
    distance_data <- distances_df %>%
      filter(school_name == input$school_filter_resources, 
             resource_name == !!resource_name)
    
    if(nrow(distance_data) > 0) {
      return(distance_data$distance_miles[1])
    } else {
      # If not found in precomputed data, calculate it now
      school_point <- school_geo %>%
        filter(school_name == input$school_filter_resources) %>%
        st_centroid()
      
      resource_point <- resources_plottable %>%
        filter(name == resource_name)
      
      if(nrow(school_point) > 0 && nrow(resource_point) > 0) {
        # Calculate distance in meters and convert to miles
        distance_m <- st_distance(school_point, resource_point)[1]
        return(as.numeric(distance_m) * 0.000621371) # Convert meters to miles
      }
      
      return(NA)
    }
  }
  
  # Show resource on map when button is clicked
  observeEvent(input$show_on_map, {
    req(selected_resource())
    
    resource <- selected_resource()
    school_data <- school_geo %>%
      filter(school_name == input$school_filter_resources)
    
    # Create a special map modal with the resource and school locations
    showModal(modalDialog(
      title = paste("Map: ", resource$name),
      size = "l",
      
      leafletOutput("resource_map", height = "400px"),
      
      footer = tagList(
        modalButton("Close")
      )
    ))
    
    # Create the map
    output$resource_map <- renderLeaflet({
      resource_point <- resources_plottable %>%
        filter(name == resource$name)
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # Add the school
        addCircleMarkers(
          data = school_data,
          lng = ~lng, lat = ~lat,
          radius = 8,
          color = "blue",
          fillOpacity = 0.8,
          popup = ~paste0("<b>School:</b> ", school_name)
        ) %>%
        # Add the resource
        addCircleMarkers(
          data = resource_point,
          radius = 8,
          color = "red",
          fillOpacity = 0.8,
          popup = ~paste0("<b>Resource:</b> ", name, "<br><b>Address:</b> ", address)
        ) %>%
        # Add a line connecting them
        addPolylines(
          lng = c(school_data$lng, st_coordinates(resource_point)[1]),
          lat = c(school_data$lat, st_coordinates(resource_point)[2]),
          color = "purple",
          dashArray = "5,5",
          weight = 2
        ) %>%
        # Set bounds to fit both points
        fitBounds(
          min(school_data$lng, st_coordinates(resource_point)[1]),
          min(school_data$lat, st_coordinates(resource_point)[2]),
          max(school_data$lng, st_coordinates(resource_point)[1]), 
          max(school_data$lat, st_coordinates(resource_point)[2])
        )
    })
  })
  
  # Print resource information
  observeEvent(input$print_resource, {
    req(selected_resource())
    
    resource <- selected_resource()
    school_name <- input$school_filter_resources
    
    # Create content for printing
    print_content <- paste0(
      "<html><head>",
      "<style>body{font-family:Arial,sans-serif;margin:20px;line-height:1.4}",
      "h2{color:#2c3e50} .section{margin-bottom:15px} ",
      ".label{font-weight:bold;min-width:100px;display:inline-block}",
      "</style></head><body>",
      "<h2>", resource$name, "</h2>",
      "<div class='section'><span class='label'>School:</span>", school_name, "</div>",
      "<div class='section'><span class='label'>Address:</span>", 
      resource$address, ", ", resource$city, ", ", resource$state, " ", resource$zip_code, "</div>"
    )
    
    if(!is.na(resource$phone_number) && resource$phone_number != "") {
      print_content <- paste0(
        print_content, 
        "<div class='section'><span class='label'>Phone:</span>", resource$phone_number, "</div>"
      )
    }
    
    if(!is.na(resource$website) && resource$website != "") {
      print_content <- paste0(
        print_content, 
        "<div class='section'><span class='label'>Website:</span>", resource$website, "</div>"
      )
    }
    
    if(!is.na(resource$description) && resource$description != "") {
      print_content <- paste0(
        print_content, 
        "<div class='section'><span class='label'>Description:</span><p>", resource$description, "</p></div>"
      )
    }
    
    if(!is.na(resource$hours) && resource$hours != "") {
      print_content <- paste0(
        print_content, 
        "<div class='section'><span class='label'>Hours:</span>", resource$hours, "</div>"
      )
    }
    
    distance <- get_distance_to_school(resource$name)
    print_content <- paste0(
      print_content, 
      "<div class='section'><span class='label'>Distance:</span>", 
      format(round(as.numeric(distance), 2), nsmall = 2), " miles from ", school_name, "</div>",
      "<div><small>Printed on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</small></div>",
      "</body></html>"
    )
    
    # Send to browser for printing
    showModal(modalDialog(
      title = "Print Resource Information",
      size = "l",
      
      tags$div(
        tags$p("Press the button below to print or save this resource information:"),
        tags$iframe(srcdoc = print_content, style = "width:100%;height:400px;border:1px solid #ddd"),
        tags$script(HTML(
          'function printResourceInfo() {
          var content = document.querySelector("iframe").contentWindow;
          content.focus();
          content.print();
          return false;
        }'
        ))
      ),
      
      footer = tagList(
        actionButton("trigger_print", "Print", onclick = "printResourceInfo(); return false;"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })
  
# School tab -------------------------------
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
    selected_columns <- unlist(resource_categories[resource_types])
    
    resources_data <- resources_plottable %>%
      st_drop_geometry() %>%
      filter(rowSums(across(all_of(selected_columns), ~ifelse(is.na(.), 0, .))) > 0) %>%
      select(name, address, city, phone_number, website)
    
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

# App -----------------------------
# Run the application
shinyApp(ui = ui, server = server)
