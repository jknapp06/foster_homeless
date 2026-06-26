library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(sf)
library(pins)
library(here)
library(fontawesome)

# Connect to local app board
board <- board_folder(here("data", "pins"))

# Load pre-processed data
homeless_total      <- pin_read(board, "homeless_total")
district_geo        <- pin_read(board, "district_geo")
school_geo          <- pin_read(board, "school_geo")
resources_plottable <- pin_read(board, "resources_plottable")

# Get district summary data
district_summary <- homeless_total |>
  filter(aggregate_level %in% c("D", "C")) |>
  rename(
    total_enrollment = cumulative_enrollment,
    homeless_count = homeless_student_enrollment,
    doubled_up = temporarily_doubled_up,
    shelters = temporary_shelters,
    hotels = hotels_motels,
    unsheltered = temporarily_unsheltered,
    unknown = missing_unknown
  ) |>
  mutate(percent_homeless = round(homeless_count / total_enrollment * 100, 1))

# Define broader resource categories
resource_categories <- list(
  "Housing Assistance" = c("housing_assistance", "transitional_housing", "rental_assistance_program", "emergency_shelter", "low_income_housing_resources", "emergency_shelter_2"),
  "Food Assistance" = c("food_assistance", "food_pantries", "family_farms", "farmers_markets"),
  "Family Support Services" = c("family_support_services", "family_counseling", "clothing_and_diaper_assistance", "domestic_violence_support", "baby_formula_resources", "adults_with_disabilities", "children_with_disabilities", "victim_assistance_programs", "family_resource_center", "youth_programs", "senior_services", "military_veteran_resources"),
  "Emergency Services" = c("emergency_services", "safe_surrender_site", "police_station", "fire_department"),
  "Healthcare Services" = c("medical_facility", "healthcare_services", "community_clinic", "free_or_low_cost_medical_services", "substance_abuse_treatment", "mental_health_resource", "maternity_resources_and_support"),
  "Community Activities" = c("community_activities", "museum", "recreation_and_leisure", "public_library"),
  "Education and Literacy" = c("education_and_literacy", "tutoring_services", "college", "school_district_office"),
  "Employment and Financial Assistance" = c("employment_services", "job_training_programs", "financial_assistance", "taxes"),
  "Legal and Immigration" = c("legal_aid", "immigration_citizenship"),
  "Transportation" = c("transportation_services")
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
                    choices = c("All Districts", sort(unique(school_geo$district_name)))),
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
      layout_columns(
        DTOutput("all_resources_table")
      )
    )
  )
)

# Server ----------------
server <- function(input, output, session) {
  
  # Map tab -------------------------
  filtered_schools <- reactive({
    school_geo
  })
  
  filtered_resources <- reactive({
    req(input$show_resources)
    resources <- resources_plottable
    
    if (!is.null(input$resource_type) && length(input$resource_type) > 0) {
      selected_columns <- unlist(resource_categories[input$resource_type])
      resources <- resources |>
        filter(rowSums(across(all_of(selected_columns), ~ifelse(is.na(.), 0, .))) > 0)
    }
    return(resources)
  })
  
  # Initialize map
  output$homelessness_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
      setView(lng = -122.0, lat = 38.3, zoom = 10) |>
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite", "CartoDB"),
        overlayGroups = c("District Boundaries", "Schools", "Resources"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(position = "bottomright") |>
      addEasyButton(easyButton(
        icon = "fa-home",
        title = "Reset View",
        onClick = JS("function(btn, map){ map.setView([38.3, -122.0], 10); }")
      ))
  })
  
  observe({
    schools <- filtered_schools()
    resources <- filtered_resources()
    
    proxy <- leafletProxy("homelessness_map") |>
      clearMarkers() |>
      clearShapes() |>
      clearControls()
    
    # District boundaries
    proxy |> addPolygons(
      data = district_geo,
      fillColor = "#3388ff", weight = 1, opacity = 1,
      color = "#666", fillOpacity = 0.2,
      highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.5, bringToFront = TRUE),
      label = ~district_name, layerId = ~district_name,
      group = "District Boundaries"
    )
    
    # Schools
    if (nrow(schools) > 0) {
      school_labels <- sprintf("%s", schools$school_name) |> lapply(HTML)
      
for (i in seq_len(nrow(schools))) {
        school <- schools[i, ]
        if (is.na(school$longitude) || is.na(school$latitude)) next
        
        school_icon <- makeAwesomeIcon(
          icon = "graduation-cap",
          markerColor = school$school_color, 
          iconColor = "white",
          library = "fa"
        )
        
        popup_content <- paste0(
          "<h5>", school$school_name, "</h5>",
          "<strong>District:</strong> ", school$district_name, "<br>",
          "<strong>Total Enrollment:</strong> ", school$enroll_total, "<br>",
          "<strong>Homeless Enrollment:</strong> ", school$ho_mcount, "<br>",
          "<strong>Homeless Percent:</strong> ", school$ho_mpct, "%<br>"
        )
        
        proxy |> addAwesomeMarkers(
          lng = school$longitude, lat = school$latitude,
          icon = school_icon, group = "Schools",
          layerId = paste0("school_", school$school_name),
          label = school_labels[[i]], popup = popup_content
        )
      }
      
        proxy |>
          addLegend(
            position = "bottomleft",
            title = "Instruction Level",
            colors = c("lightblue", "cadetblue", "darkblue", "purple", "gray"),
            labels = c("Elementary", "Middle", "High", "K-12 / Combo", "Other"),
            opacity = 0.8
          )
    }
    
    # Resources
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
          "Other" = makeAwesomeIcon(icon = "info", markerColor = "beige", library = "fa")
        )
        
        for (category_name in names(resource_categories)) {
          category_cols <- resource_categories[[category_name]]
          if (any(!is.na(resource[category_cols]) & resource[category_cols] == 1)) {
            return(resource_icons[[category_name]])
          }
        }
        return(resource_icons[["Other"]])
      }
      
      for (i in seq_len(nrow(resources))) {
        resource <- resources[i, ]
        if (is.na(resource$longitude) || is.na(resource$latitude)) next
        
        icon_to_use <- getResourceIcon(resource)
        
        popup_content <- paste0(
          "<h5>", resource$name, "</h5>",
          "<strong>Address:</strong> ", resource$address, "<br>"
        )
        
        proxy |> addAwesomeMarkers(
          lng = resource$longitude, lat = resource$latitude,
          icon = icon_to_use, group = "Resources",
          layerId = paste0("resource_", i),
          popup = popup_content, label = resource$name
        )
      }
    } else {
      proxy |> clearGroup("Resources")
    }
  })
  
# Resource tab ---------------------
  observe({
    school_choices <- if (input$district_filter_resources == "All Districts") {
      sort(unique(school_geo$school_name))
    } else {
      sort(unique(school_geo$school_name[school_geo$district_name == input$district_filter_resources]))
    }
    updateSelectInput(session, "school_filter_resources", choices = school_choices, selected = school_choices[1])
  })
  
  nearby_resources <- reactive({
    req(input$school_filter_resources, input$distance_threshold)
    
    # 1. Isolate the target school as an sf object
    target_school <- school_geo |> 
      filter(school_name == input$school_filter_resources) |> 
      slice(1) # Safety catch for duplicates
    
    req(nrow(target_school) > 0)
    
    # 2. Convert resources to sf object (if not already) to calculate distances
    res_sf <- resources_plottable |>
      filter(!is.na(longitude), !is.na(latitude)) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    
    # 3. Calculate distance matrix (returns meters), convert to miles
    distances_meters <- st_distance(res_sf, target_school)
    res_sf$distance_miles <- as.numeric(distances_meters) * 0.000621371
    
    # 4. Filter by distance threshold and drop spatial geometry for the data table
    resources_data <- res_sf |>
      filter(distance_miles <= input$distance_threshold) |>
      st_drop_geometry()
    
    # 5. Filter by selected categories using your indicator columns
    if (!is.null(input$resource_category_filter) && length(input$resource_category_filter) > 0) {
      selected_columns <- unlist(resource_categories[input$resource_category_filter])
      resources_data <- resources_data |>
        filter(rowSums(across(any_of(selected_columns), ~ifelse(is.na(.), 0, .))) > 0)
    }
    
    # Return formatted dataframe
    resources_data |>
      arrange(distance_miles)
  })
  
  output$all_resources_table <- renderDT({
    resources_to_display <- nearby_resources()
    
    if(nrow(resources_to_display) > 0) {
      display_data <- resources_to_display |>
        select(name, address, phone_number, website, description, distance_miles) |>
        distinct()
      
      datatable(
        display_data,
        options = list(pageLength = 10, dom = 'tip', scrollX = TRUE, scrollY = "75vh", scrollCollapse = TRUE, paging = FALSE, info = FALSE),
        selection = "multiple", rownames = FALSE
      ) |> formatRound(columns = "distance_miles", 2)
    } else {
      datatable(
        data.frame(message = paste("No resources found within", input$distance_threshold, "miles.")),
        options = list(dom = 't'), rownames = FALSE
      )
    }
  })
  
  observeEvent(input$print_selected, {
    selected_rows <- input$all_resources_table_rows_selected
    req(length(selected_rows) > 0)
    
    selected_resources <- nearby_resources()[selected_rows, ]
    
    print_content <- paste0(
      "<html><head><style>body{font-family:Arial,sans-serif;margin:20px;line-height:1.4} h2{color:#2c3e50} .section{margin-bottom:15px} .label{font-weight:bold;min-width:100px;display:inline-block}</style></head><body><h2>Selected Resources</h2>"
    )
    
    for (i in seq_len(nrow(selected_resources))) {
      r <- selected_resources[i, ]
      print_content <- paste0(
        print_content,
        "<div class='section'><span class='label'>Name:</span>", r$name, "</div>",
        "<div class='section'><span class='label'>Distance:</span>", r$distance_miles, " miles</div><hr>"
      )
    }
    print_content <- paste0(print_content, "</body></html>")
    
    showModal(modalDialog(
      title = "Print Selected Resources", size = "l",
      tags$div(
        tags$iframe(srcdoc = print_content, style = "width:100%;height:400px;border:1px solid #ddd"),
        tags$script(HTML('function printSelectedResources() { var content = document.querySelector("iframe").contentWindow; content.focus(); content.print(); return false; }'))
      ),
      footer = tagList(
        actionButton("trigger_print", "Print", onclick = "printSelectedResources(); return false;"),
        modalButton("Cancel")
      ), easyClose = TRUE
    ))
  })
}

shinyApp(ui = ui, server = server)