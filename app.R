library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(sf)
library(janitor)
library(openxlsx)
library(shinydashboard)
library(fontawesome)
library(duckdb)
library(DBI)

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

school_geo <- read_sf("data/DistrictAreas2526.gpkg") %>%
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

# Connect to DuckDB
con <- dbConnect(duckdb(), dbdir = "data/resource_db.duckdb", read_only = TRUE)

# Load data from DuckDB
schools_df <- dbGetQuery(con, "SELECT * FROM schools")
resources_df <- dbGetQuery(con, "SELECT * FROM resources")
distances_df <- dbGetQuery(con, "SELECT * FROM distances")
resources_with_distances <- dbGetQuery(con, "SELECT * FROM resources_with_distances")

# Define broader resource categories
resource_categories <- list(
  "Housing Assistance" = c(
    "housing_assistance", "transitional_housing", "rental_assistance_program",
    "emergency_shelter", "low_income_housing_resources", "emergency_shelter_2"
  ),
  "Food Assistance" = c(
    "food_assistance", "food_pantries", "family_farms", "farmers_markets"
  ),
  "Family Support Services" = c(
    "family_support_services", "family_counseling", "clothing_and_diaper_assistance",
    "domestic_violence_support", "baby_formula_resources", "adults_with_disabilities",
    "children_with_disabilities", "victim_assistance_programs", "family_resource_center",
    "youth_programs", "senior_services", "military_veteran_resources"
  ),
  "Emergency Services" = c(
    "emergency_services", "safe_surrender_site", "police_station", "fire_department"
  ),
  "Healthcare Services" = c(
    "medical_facility", "healthcare_services", "community_clinic",
    "free_or_low_cost_medical_services", "substance_abuse_treatment", 
    "mental_health_resource", "maternity_resources_and_support"
  ),
  "Community Activities" = c(
    "community_activities", "museum", "recreation_and_leisure", "public_library"
  ),
  "Education and Literacy" = c(
    "education_and_literacy", "tutoring_services", "college", "school_district_office"
  ),
  "Employment and Financial Assistance" = c(
    "employment_services", "job_training_programs", "financial_assistance", "taxes"
  ),
  "Legal and Immigration" = c(
    "legal_aid", "immigration_citizenship" 
  ),
  "Transportation" = c(
    "transportation_services"
  )
)



# UI --------------------
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
        selectInput("district_filter_resources", "School District:",
                    choices = c("All Districts", unique(schools_df$district_name))),
        selectInput("school_filter_resources", "School:",
                    choices = c("Select a district first")),
        sliderInput("distance_threshold", "Show resources within (miles):",
                    min = 5, max = 30, value = 10, step = 5,
                    ticks = FALSE),
        checkboxGroupInput("resource_category_filter", "Resource Categories:",
                           choices = names(resource_categories),
                           selected = names(resource_categories)),
        actionButton("print_selected", "Print Selected Resources",
                     class = "btn-primary btn-block")
      ),
      
      # Main content area with resources near selected school
      layout_columns(
        DTOutput("all_resources_table")
      )
    )
  ),
  
  # # Gap Analysis tab
  # nav_panel(
  #   title = "Gap Analysis",
  #   page_sidebar(
  #     sidebar = sidebar(
  #       selectInput("district_filter_gap", "School District:",
  #                   choices = c("All Districts", unique(schools_df$district_name))),
  #       selectInput("school_filter_gap", "School:",
  #                   choices = c("Select a district first"))
  #     ),
  #     
  #     # Main content area with closest resources for each category
  #     DTOutput("closest_resources_table"),  # Top table
  #     DTOutput("resources_in_category_table")  # Bottom table
  #   )
  # ),
  # 
  # 
  # # Schools tab
  # nav_panel(
  #   title = "Schools",
  #   layout_sidebar(
  #     sidebar = sidebar(
  #       selectInput("district_filter_schools", "Filter by District",
  #                   choices = c("All Districts", unique(school_geo$district_name))),
  #       selectInput("school_filter_schools", "Filter by School", choices = NULL)
  #     ),
  #     layout_column_wrap(
  #       width = 1/2,
  #       infoBox(
  #         value = textOutput("school_name"),
  #         title = "School Name"
  #       ),
  #       infoBox(
  #         value = textOutput("district_name"),
  #         title = "District"
  #       ),
  #       infoBox(
  #         value = textOutput("charter"),
  #         title = "Charter"
  #       ),
  #       infoBox(
  #         value = textOutput("title_i_status"),
  #         title = "Title I"
  #       ),
  #       infoBox(
  #         value = textOutput("total_enrollment"),
  #         title = "Total Enrollment"
  #       ),
  #       infoBox(
  #         value = textOutput("homeless_count"),
  #         title = "Homeless Enrollment"
  #       )
  #     ),
  #     layout_column_wrap(
  #       width = 1/2,
  #       plotOutput("housing_plot"),
  #       plotOutput("programs_plot")
  #     )
  #   )
  # )
)

# Server ----------------
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
          "Healthcare Services" = makeAwesomeIcon(icon = "medkit", markerColor = "darkred", library = "fa"),
          "Community Activities" = makeAwesomeIcon(icon = "calendar", markerColor = "purple", library = "fa"),
          "Education and Literacy" = makeAwesomeIcon(icon = "book", markerColor = "darkgreen", library = "fa"),
          "Transportation" = makeAwesomeIcon(icon = "bus", markerColor = "lightblue", library = "fa"),
          "Legal and Immigration" = makeAwesomeIcon(icon = "balance-scale", markerColor = "pink", library = "fa"), 
          "Employment and Financial Assistance" = makeAwesomeIcon(icon = "money-bill", markerColor = "lightgreen", library = "fa"),
          "Other" = makeAwesomeIcon(icon = "info", markerColor = "beige", library = "fa")  # Lavender
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
      school_choices <- sort(unique(schools_df$school_name))
    } else {
      filtered <- schools_df %>%
        filter(district_name == input$district_filter_resources)
      school_choices <- sort(unique(filtered$school_name))
    }
    updateSelectInput(session, "school_filter_resources",
                      choices = school_choices,
                      selected = school_choices[1])
  })
  
  # Reactive data for resources table
  nearby_resources <- reactive({
    req(input$school_filter_resources)
    req(input$distance_threshold)
    
    # Query DuckDB for resources within the selected distance
    query <- paste0("
    SELECT DISTINCT r.name, r.address, r.phone_number, r.website, r.description, d.distance_miles, rc.category
    FROM resources r
    JOIN distances d ON r.name = d.resource_id
    JOIN schools s ON d.school_id = s.school_name
    JOIN resource_type_mappings rtm ON r.name = rtm.resource_id
    JOIN resource_categories rc ON rtm.resource_type = rc.resource_type
    WHERE s.school_name = '", input$school_filter_resources, "'
    AND d.distance_miles <= ", input$distance_threshold, "
    ORDER BY d.distance_miles ASC
  ")
    
    resources_data <- dbGetQuery(con, query)
    
    # Filter resources based on selected categories
    if (!is.null(input$resource_category_filter) && length(input$resource_category_filter) > 0) {
      resources_data <- resources_data %>%
        filter(category %in% input$resource_category_filter)
    }
    
    return(resources_data)
  })
  
  # Resources table
  output$all_resources_table <- renderDT({
    resources_to_display <- nearby_resources()
    
    if(nrow(resources_to_display) > 0) {
      display_data <- resources_to_display %>%
        select(name, address, phone_number, website, description, distance_miles) %>%
        distinct()  # Ensure no duplicates in the final display
      
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          dom = 'tip',
          scrollX = TRUE,
          scrollY = "75vh",  # Set scrollY to a percentage of the viewport height
          scrollCollapse = TRUE,  # Allow the table to collapse if there are fewer entries
          paging = FALSE,  # Disable pagination to allow scrolling through all data
          info = FALSE  # Hide the "Showing X of Y entries" info
        ),
        selection = "multiple",  # Allow multiple row selection
        rownames = FALSE
      ) %>%
        formatRound(columns = "distance_miles", 2)
    } else {
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
  
  # Observer to handle print action
  observeEvent(input$print_selected, {
    selected_rows <- input$all_resources_table_rows_selected
    if (!is.null(selected_rows) && length(selected_rows) > 0) {
      resources_to_print <- nearby_resources()
      selected_resources <- resources_to_print[selected_rows, ]
      
      # Create a printable list
      print_content <- paste0(
        "<html><head>",
        "<style>body{font-family:Arial,sans-serif;margin:20px;line-height:1.4}",
        "h2{color:#2c3e50} .section{margin-bottom:15px} ",
        ".label{font-weight:bold;min-width:100px;display:inline-block}",
        "</style></head><body>",
        "<h2>Selected Resources</h2>"
      )
      
      for (i in 1:nrow(selected_resources)) {
        resource <- selected_resources[i, ]
        print_content <- paste0(
          print_content,
          "<div class='section'><span class='label'>Name:</span>", resource$name, "</div>",
          "<div class='section'><span class='label'>Address:</span>", resource$address, "</div>",
          "<div class='section'><span class='label'>Phone:</span>", resource$phone_number, "</div>",
          "<div class='section'><span class='label'>Website:</span>", resource$website, "</div>",
          "<div class='section'><span class='label'>Description:</span>", resource$description, "</div>",
          "<div class='section'><span class='label'>Distance:</span>", resource$distance_miles, " miles</div>",
          "<hr>"
        )
      }
      
      print_content <- paste0(
        print_content,
        "<div><small>Printed on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</small></div>",
        "</body></html>"
      )
      
      # Display the printable content in a modal dialog
      showModal(modalDialog(
        title = "Print Selected Resources",
        size = "l",
        
        tags$div(
          tags$p("Press the button below to print or save the selected resources:"),
          tags$iframe(srcdoc = print_content, style = "width:100%;height:400px;border:1px solid #ddd"),
          tags$script(HTML(
            'function printSelectedResources() {
            var content = document.querySelector("iframe").contentWindow;
            content.focus();
            content.print();
            return false;
          }'
          ))
        ),
        
        footer = tagList(
          actionButton("trigger_print", "Print", onclick = "printSelectedResources(); return false;"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    } else {
      showNotification("No resources selected for printing.", type = "message")
    }
  })
  
  
  # # Gap Analysis tab ---------------------
  # 
  # # Update school dropdown based on district selection in Gap Analysis tab
  # observe({
  #   if (input$district_filter_gap == "All Districts") {
  #     school_choices <- sort(unique(schools_df$school_name))
  #   } else {
  #     filtered <- schools_df %>%
  #       filter(district_name == input$district_filter_gap)
  #     school_choices <- sort(unique(filtered$school_name))
  #   }
  #   updateSelectInput(session, "school_filter_gap",
  #                     choices = school_choices,
  #                     selected = school_choices[1])
  # })
  # 
  # # Reactive data for closest resources table
  # closest_resources <- reactive({
  #   req(input$school_filter_gap)
  #   
  #   # Query DuckDB for the closest resources for each category
  #   closest_resources_data <- data.frame()
  #   
  #   for (category in names(resource_categories)) {
  #     query <- paste0("
  #     SELECT rc.category, MIN(d.distance_miles) as min_distance
  #     FROM resources r
  #     JOIN distances d ON r.name = d.resource_id
  #     JOIN schools s ON d.school_id = s.school_name
  #     JOIN resource_type_mappings rtm ON r.name = rtm.resource_id
  #     JOIN resource_categories rc ON rtm.resource_type = rc.resource_type
  #     WHERE s.school_name = '", input$school_filter_gap, "'
  #     AND rc.category = '", category, "'
  #   ")
  #     
  #     category_data <- dbGetQuery(con, query)
  #     closest_resources_data <- bind_rows(closest_resources_data, category_data)
  #   }
  #   
  #   # Sort the data by distance in descending order
  #   closest_resources_data <- closest_resources_data %>%
  #     arrange(desc(min_distance))
  #   
  #   return(closest_resources_data)
  # })
  # 
  # # Closest resources table
  # output$closest_resources_table <- renderDT({
  #   resources_to_display <- closest_resources()
  #   
  #   if(nrow(resources_to_display) > 0) {
  #     display_data <- resources_to_display %>%
  #       select(category, min_distance) %>%
  #       rename(Distance = min_distance)
  #     
  #     datatable(
  #       display_data,
  #       options = list(
  #         pageLength = 10,
  #         dom = 'tip',
  #         scrollX = TRUE,
  #         scrollY = "200px"
  #       ),
  #       selection = "single",  # Allow single row selection for category click
  #       rownames = FALSE
  #     ) %>%
  #       formatRound(columns = "Distance", 2)
  #   } else {
  #     datatable(
  #       data.frame(
  #         message = "No closest resources found for the selected school"
  #       ),
  #       options = list(dom = 't'),
  #       rownames = FALSE
  #     )
  #   }
  # })
  # 
  # # Reactive data for resources in selected category
  # resources_in_category <- reactive({
  #   req(input$closest_resources_table_rows_selected)
  #   
  #   selected_row <- input$closest_resources_table_rows_selected
  #   if (!is.null(selected_row) && length(selected_row) > 0) {
  #     selected_category <- closest_resources()$category[selected_row]
  #     
  #     query <- paste0("
  #     SELECT r.name, r.address, r.phone_number, r.website, r.description, d.distance_miles
  #     FROM resources r
  #     JOIN distances d ON r.name = d.resource_id
  #     JOIN schools s ON d.school_id = s.school_name
  #     JOIN resource_type_mappings rtm ON r.name = rtm.resource_id
  #     JOIN resource_categories rc ON rtm.resource_type = rc.resource_type
  #     WHERE s.school_name = '", input$school_filter_gap, "'
  #     AND rc.category = '", selected_category, "'
  #     ORDER BY d.distance_miles ASC
  #   ")
  #     
  #     resources_data <- dbGetQuery(con, query)
  #     return(resources_data)
  #   } else {
  #     return(data.frame())
  #   }
  # })
  # 
  # # Resources in category table
  # output$resources_in_category_table <- renderDT({
  #   resources_to_display <- resources_in_category()
  #   
  #   if(nrow(resources_to_display) > 0) {
  #     display_data <- resources_to_display %>%
  #       select(name, address, phone_number, website, description, distance_miles)
  #     
  #     datatable(
  #       display_data,
  #       options = list(
  #         pageLength = 10,
  #         dom = 'tip',
  #         scrollX = TRUE,
  #         scrollY = "400px"
  #       ),
  #       rownames = FALSE
  #     ) %>%
  #       formatRound(columns = "distance_miles", 2)
  #   } else {
  #     datatable(
  #       data.frame(
  #         message = "Select a category to view resources"
  #       ),
  #       options = list(dom = 't'),
  #       rownames = FALSE
  #     )
  #   }
  # })
  # 
  # # School tab -------------------------------
  # 
  # # Update school filter based on district selection in Schools tab
  # observe({
  #   if (input$district_filter_schools == "All Districts") {
  #     school_choices <- c("All Schools", unique(schools_df$school_name))
  #   } else {
  #     filtered <- schools_df %>%
  #       filter(district_name == input$district_filter_schools)
  #     school_choices <- c("All Schools", unique(filtered$school_name))
  #   }
  #   updateSelectInput(session, "school_filter_schools", choices = school_choices)
  # })
  # 
  # # Reactive data for school information
  # school_info <- reactive({
  #   if (input$district_filter_schools == "All Districts" && is.null(input$school_filter_schools)) {
  #     district_data <- district_summary %>%
  #       filter(district_name == input$district_filter_schools)
  #     return(district_data)
  #   } else if (!is.null(input$school_filter_schools) && input$school_filter_schools != "All Schools") {
  #     school_data <- schools_df %>%
  #       filter(school_name == input$school_filter_schools)
  #     return(school_data)
  #   } else {
  #     return(NULL)
  #   }
  # })
  # 
  # # Value boxes for school information
  # output$school_name <- renderText({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       return(info$school_name[1])
  #     } else {
  #       return("N/A")
  #     }
  #   } else {
  #     return("N/A")
  #   }
  # })
  # 
  # output$district_name <- renderText({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       return(info$district_name[1])
  #     } else {
  #       return("N/A")
  #     }
  #   } else {
  #     return("N/A")
  #   }
  # })
  # 
  # output$charter <- renderText({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       return(ifelse(info$charter[1] == "Y", "Yes", "No"))
  #     } else {
  #       return("N/A")
  #     }
  #   } else {
  #     return("N/A")
  #   }
  # })
  # 
  # output$title_i_status <- renderText({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       return(ifelse(info$title_i_status[1] == "Y", "Yes", "No"))
  #     } else {
  #       return("N/A")
  #     }
  #   } else {
  #     return("N/A")
  #   }
  # })
  # 
  # output$total_enrollment <- renderText({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       return(info$enroll_total[1])
  #     } else {
  #       return("N/A")
  #     }
  #   } else {
  #     return("N/A")
  #   }
  # })
  # 
  # output$homeless_count <- renderText({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       return(info$ho_mcount[1])
  #     } else {
  #       return("N/A")
  #     }
  #   } else {
  #     return("N/A")
  #   }
  # })
  # 
  # # Housing situation plot
  # output$housing_plot <- renderPlot({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       plot_data <- info %>%
  #         select(doubled_up, shelters, hotels, unsheltered, unknown) %>%
  #         pivot_longer(cols = everything(), names_to = "housing_situation", values_to = "count") %>%
  #         mutate(
  #           housing_situation = factor(housing_situation,
  #                                      levels = c("doubled_up", "shelters", "hotels", "unsheltered", "unknown"),
  #                                      labels = c("Doubled Up", "Shelters", "Hotels/Motels", "Unsheltered", "Unknown"))
  #         )
  #       
  #       ggplot(plot_data, aes(x = housing_situation, y = count, fill = housing_situation)) +
  #         geom_bar(stat = "identity", position = "dodge") +
  #         scale_fill_brewer(palette = "Set2") +
  #         labs(
  #           title = "Homeless Student Housing Situations",
  #           x = "Housing Situation",
  #           y = "Number of Students"
  #         ) +
  #         theme_minimal() +
  #         theme(
  #           legend.position = "none",
  #           axis.text.x = element_text(angle = 45, hjust = 1),
  #           plot.title = element_text(hjust = 0.5, size = 14)
  #         )
  #     }
  #   }
  # })
  # 
  # # Programs plot
  # output$programs_plot <- renderPlot({
  #   info <- school_info()
  #   if (!is.null(info)) {
  #     if (nrow(info) > 0) {
  #       plot_data <- info %>%
  #         select(a_acount, a_icount, a_scount, f_icount, h_icount, p_icount, w_hcount, m_rcount, n_rcount, e_lcount, fo_scount, mi_gcount, sed_count, sw_dcount, frp_mcount) %>%
  #         pivot_longer(cols = everything(), names_to = "program", values_to = "count") %>%
  #         mutate(
  #           program = factor(program,
  #                            levels = c("a_acount", "a_icount", "a_scount", "f_icount", "h_icount", "p_icount", "w_hcount", "m_rcount", "n_rcount", "e_lcount", "fo_scount", "mi_gcount", "sed_count", "sw_dcount", "frp_mcount"),
  #                            labels = c("Asian", "American Indian", "African American", "Filipino", "Hispanic", "Pacific Islander", "White", "Migrant", "Non-English", "Economic Disadvantage", "Foster", "Migrant", "Special Education", "Students with Disabilities", "Free/Reduced Meals"))
  #         )
  #       
  #       ggplot(plot_data, aes(x = program, y = count, fill = program)) +
  #         geom_bar(stat = "identity", position = "dodge") +
  #         scale_fill_brewer(palette = "Set3") +
  #         labs(
  #           title = "Homeless Student Programs",
  #           x = "Program",
  #           y = "Number of Students"
  #         ) +
  #         theme_minimal() +
  #         theme(
  #           legend.position = "none",
  #           axis.text.x = element_text(angle = 45, hjust = 1),
  #           plot.title = element_text(hjust = 0.5, size = 14)
  #         )
  #     }
  #   }
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
