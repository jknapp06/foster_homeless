#

library(shiny)
library(tidyverse)
library(DT)
library(wesanderson)
library(flexdashboard)
library(png)
library(ggtext)
library(ggdist)
library(systemfonts)

register_font(
  name = "weepeople",
  plain = "www/weepeople.ttf"
)

wee_letters <- c("Q","b","V", "h")

dashboard <- read_csv("data/ca_dashboard.csv")
enrollment <-  read_csv("data/enrollment.csv")
homeless <- read_csv("data/homeless.csv") %>% 
  filter(charter_school == "All" | aggregate_level == "S", # schools (charter == Yes | No)
         dass == "All" | aggregate_level == "S") %>%       # or All schools at higher aggregate levels
  mutate(percent_homeless = homeless_student_enrollment/cumulative_enrollment * 100,
         across(ends_with("(percent)"), parse_number))

#  ,
# wee_people = map(homeless_student_enrollment, 
#                  ~ paste("<span style='font-family:weepeople;inline-size: 650px;overflow-wrap: break-word'>", 
#                          paste(sample(wee_letters, 
#                                        .x, replace = T),
#                                collapse = "", sep = " "),
#                          "</span>")),
# icon = "b")

dashboard <- 
  dashboard %>% 
  mutate(statuslevel = as_factor(statuslevel),
         reportingyear = fct_relevel(as_factor(reportingyear), "2019"),
         indicator = fct_relevel(as_factor(indicator), 
                                 "ELA", "Math", "ELPI", 
                                 "absenteeism", "graduation", 
                                 "suspension", "college/career"),
         studentgroup = as_factor(studentgroup),
         student_group_long = fct_relevel(as_factor(student_group_long),
                                          "All students",
                                          "Black/African American",
                                          "American Indian or Alaska Native",
                                          "Asian",
                                          "Filipino",
                                          "Hispanic",
                                          "Pacific Islander",
                                          "White",
                                          "Multiple Races/Two or more",
                                          "Socioeconomically Disadvantaged",
                                          "Homeless Youth",
                                          "Students with Disabilities",
                                          "Foster Youth",
                                          "English Learner",
                                          "English Learners Only",
                                          "RFEPs Only",
                                          "English Only",
                                          "Smarter Balanced Assessment",
                                          "CA Alternative Assessment"
         ),
         priority_eligible = as_factor(priority_eligible),
         indicator_eligible = as_factor(indicator_eligible),
         charter_flag = if_else(is.na(charter_flag), "N", charter_flag),
         color = as_factor(color)
         
  )

homeless_to_orange <- read_csv("data/homeless_to_orange.csv")

years <- 
  dashboard %>% 
  select(reportingyear) %>% 
  distinct()

counties <- 
  homeless %>% 
  select(county_name) %>% 
  distinct()

districts <- 
  homeless %>%
  select(county_name, district_name) %>% 
  distinct()

schools <- 
  homeless %>% 
  select(county_name, district_name, school_name) %>% 
  distinct()

categories_for_districts <- 
  homeless %>%
  filter(aggregate_level == "D") %>% 
  select(reporting_group) %>% 
  distinct()

# charters <- 
#   dashboard %>% 
#   filter(charter_flag == "Y") %>% 
#   select(cds, countyname, schoolname) %>% 
#   distinct()

# leas <- 
#   bind_rows(
#     select(districts, cds, countyname, leaname = "districtname"),
#     select(charters, cds, countyname, leaname = "schoolname")
#   ) %>% 
#   drop_na()

student_groups <-
  dashboard %>% 
  select(studentgroup, student_group_long) %>% 
  distinct()

latest_school_year = "2022-23"

status_colors <- c(
  "lightgrey", "tomato", "orange",
  "yellow", "mediumseagreen", "dodgerblue", "darkslategrey"
)

ui <- navbarPage(
  
  theme = bslib::bs_theme(bootswatch = "zephyr"),
  
  header = fluidRow(
    includeCSS("www/weepeople.css"),
    column(4, img(height = 130, width = 248, src = "SCOE_Logo.jpg")),
    column(8, fluidRow(h4("Internal use only.", align = "center"))),
  ),
  
  # Application title
  titlePanel("Foster Students and Students Experiencing Homelessness"),
  
  tabPanel("Homelessness in Districts",
           
           # District Panel ---------------------
           sidebarLayout(
             sidebarPanel(
               selectInput("county",
                           label = "Select County",
                           choices = counties$county_name,
                           selected = "Solano"),
               selectInput("district",
                           label = "Select District",
                           choices = NULL,
                           selected = NULL),
               selectInput("reporting_group",
                           label = "Select Reporting Category",
                           choices = categories_for_districts$reporting_group,
                           selected = "Race/Ethnicity"),
               p("Categories are only reported at the district level.
                  School counts reflect the total number of students only.")
             ),
             
             mainPanel(
               htmlOutput("homelessDistrictTitle"),
               htmlOutput("homeless_group_icons",
                          container = wellPanel),
               
               # # Bar chart
               # plotOutput("homeless_groups"),
               
             )
           ),
           hr(),
           # ),
           # 
           # tabPanel("Homelessness in Schools",
           
           # School panel -------------------
           sidebarLayout(
             sidebarPanel(
               selectInput("school",
                           label = "Select School",
                           choices = NULL,
                           selected = NULL),
             ),
             mainPanel(
               htmlOutput("homelessSchoolTitle"),
               wellPanel(fluidRow(column(5, htmlOutput("total_homeless")),
                                  column(7, htmlOutput("homeless_icons"))),
               ),
               # htmlOutput("student_housing_status"),
               # Gauges ---------
               # fluidRow(column(2, tags$h5("Percent temporarily doubled up"),
               #                 div(gaugeOutput("doubled_gauge"),
               #                     style = "vertical-align:bottom")),
               #          column(2, tags$h5("Percent temporarily in shelters"),
               #                div(gaugeOutput("shelters_gauge"),
               #                    style = "vertical-align:bottom")),
               #          column(2, tags$h5("Percent in hotels/motels"),br(),
               #                div(gaugeOutput("hotel_motels_gauge"),
               #                    style = "vertical-align:bottom")),
               #          column(2, tags$h5("Percent temporarily unsheltered"),
               #                 gaugeOutput("unsheltered_gauge")),
               #          column(2, tags$h5("Percent missing/unknown"),br(),
               #                 gaugeOutput("unknown_gauge"))),
               
               # Homelessness over time ------------
               plotOutput("homeless_plot")
             )
           )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # selected options ----------------
  selected_county <- reactive(
    input$county
  )
  
  district_options <- reactive(
    districts %>%
      filter(county_name == selected_county())
  )
  
  county_office <- reactive(
    district_options() %>% 
      filter(str_detect(district_name, "Office")) %>% 
      pull(district_name) %>% 
      nth(1)
  )
  
  observeEvent(district_options(),{
    updateSelectInput(inputId = "district", 
                      choices = district_options()$district_name,
                      selected = county_office())
  })
  
  
  selected_district <- reactive(
    input$district
  )
  
  school_options <- reactive(
    schools %>% 
      filter(county_name == selected_county(),
             district_name == selected_district()) %>% 
      distinct()
  )
  
  observeEvent(school_options(),{
    updateSelectInput(inputId = "school", 
                      choices = school_options()$school_name,
                      selected = school_options()$school_name[1])
  })
  
  selected_school <- reactive(
    input$school
  )
  
  # selected_cds <- reactive(
  #   schools %>% 
  #     filter(districtname == selected_district(),
  #            schoolname == selected_school()) %>%
  #     distinct() %>% 
  #     pull(cds) %>% 
  #     nth(1)
  # )
  
  # dashboard_filtered <- reactive(
  #   dashboard %>%
  #     filter(countyname == selected_county(),
  #            districtname == selected_district(),
  #            schoolname == selected_school())
  # )
  
  # Filter homeless data ---------------
  
  homeless_in_district <- reactive(
    homeless %>%
      filter(county_name == selected_county(),
             district_name == selected_district())
  )
  
  homeless_in_school <- reactive(
    homeless_in_district() %>% 
      filter(school_name == selected_school())
  )
  
  # Foster plot --------------------------
  # output$foster_homeless_plot <- renderPlot({
  #     
  #   dashboard_filtered() %>% 
  #     filter(student_group_long == "Homeless Youth" |
  #              student_group_long == "Foster Youth",
  #            indicator == "suspension") %>% 
  #     ggplot() +
  #     geom_line(mapping = aes(x = reportingyear,
  #                             y = currdenom,
  #                             group = student_group_long,
  #                             color = student_group_long),
  #               linewidth = 2) +
  #     theme_minimal()
  #     
  # })
  
  # Homeless counts ---------
  
  output$homelessDistrictTitle <- renderUI(
    h3(paste("Students Experiencing Homelessness in", selected_district()))
  )
  
  selected_group <- reactive(
    input$reporting_group
  )
  
  selected_subgroups <- reactive(
    
    homeless_in_district() %>% 
      filter(reporting_group == selected_group(),
             academic_year == latest_school_year,
             aggregate_level == "D")
  )
  
  output$homeless_group_icons <- renderUI({
    
    homeless_icon_rows <- ""
    
    for(i in 1:length(selected_subgroups()$homeless_student_enrollment)){
      
      if(!is.null(selected_subgroups()$homeless_student_enrollment[i]) &
         !is.na(selected_subgroups()$homeless_student_enrollment[i]) &
         selected_subgroups()$homeless_student_enrollment[i] > 0){
        
        homeless_icon_rows <- HTML(paste0(homeless_icon_rows,
                                          "<div class='row'> <div class='col-3'>", selected_subgroups()$reporting_long[i], "</div>",
                                          "<div class='col-1'>", selected_subgroups()$homeless_student_enrollment[i], "</div>",
                                          "<div class='col-8'>", span(paste(sample(wee_letters, 
                                                                                   selected_subgroups()$homeless_student_enrollment[i],
                                                                                   replace = T), 
                                                                            collapse = "", sep = ""),
                                                                      style = "font-family:weepeople;
                                              color:slategray;
                                              font-size: 20pt;
                                              inline-size:600px;
                                              overflow-wrap: break-word"), "</div>",
                                          "</div>"
        )
        )
      }
    }
    
    homeless_icon_rows
    
  })
  
  
  # output$homeless_groups <- renderPlot({
  # 
  #   homeless_in_district() %>%
  #     filter(reporting_group == selected_group(),
  #            academic_year == latest_school_year) %>%
  #     ggplot() +
  #     geom_col(mapping = aes(y = reporting_long,
  #                            x = homeless_student_enrollment,
  #                            fill = reporting_long)) +
  #     geom_richtext(mapping = aes(y = reporting_long,
  #                                 x = 0,
  #                                 label = wee_people),
  #                   hjust = 0,
  #                   size = 10) +
  #     labs(title = paste("Students in", selected_district(),
  #                        "Experiencing Homelessness by", selected_group()),
  #          y = NULL,
  #          x = NULL,
  #          fill = NULL) +
  #     theme_minimal() +
  #     theme(
  #       text = element_text(size = 12),
  #       title = element_text(size = 16)
  #     )
  #   
  
  
  # output$homeless_groups <- renderPlot({
  # 
  #   homeless_in_district() %>%
  #     filter(reporting_group == selected_group(),
  #            academic_year == latest_school_year) %>%
  #     ggplot() +
  #     # geom_col(mapping = aes(y = reporting_long,
  #     #                        x = homeless_student_enrollment,
  #     #                        shape = icon)) +
  #     geom_dots(mapping = aes(y = reporting_long,
  #                             x = homeless_student_enrollment,
  #                             group = reporting_long,
  #                             shape = icon),
  #               family = "weepeople") +
  #     labs(title = paste("Students in", selected_district(),
  #                        "Experiencing Homelessness by", selected_group()),
  #          y = NULL,
  #          x = NULL,
  #          fill = NULL) +
  #     theme_minimal() +
  #     theme(
  #       text = element_text(size = 12),
  #       title = element_text(size = 16)
  #     )
  # 
  # })
  
  # School panel ---------------------
  
  output$homelessSchoolTitle <- renderUI(
    h3(paste("Students Experiencing Homelessness in", 
             selected_district(),
             selected_school()))
  )
  
  homeless_in_school_this_year <- reactive(
    homeless_in_district() %>% 
      filter(school_name == selected_school(),
             reporting_category == "TA",
             academic_year == latest_school_year) %>% 
      pull(homeless_student_enrollment) %>% 
      nth(1)
  )
  
  output$total_homeless <- renderUI(
    p(h5(paste("Total number of students who experienced housing insecurity in",
               selected_school(), latest_school_year)),
      div(h3(homeless_in_school_this_year(), style="text-align:center"),
      ))
  )
  
  output$homeless_icons <- renderUI(
    h3(paste(sample(wee_letters, homeless_in_school_this_year(), replace = T), 
             collapse = "", sep = ""),
       style = "font-family:weepeople;inline-size: 650px;overflow-wrap: break-word;color:slategray")
  )
  
  output$student_housing_status <- renderUI({
    
    living_situations <- 
      homeless_in_school() %>% 
      select(temporarily_doubled_up,
             temporary_shelters,
             `hotels/motels`,
             temporarily_unsheltered,
             `missing/unknown`)
    
    housing_status_html <- ""
    
    for(situation in living_situations){
      
      if(!is.na(living_situations[situation]) &
         !is.null(living_situations[situation])){
        housing_status_html = HTML(paste0(housing_status_html,
                                          "<div class='row'> <div class='col-3'>", names(living_situations[situation]), "</div>",
                                          "<div class='col-1'>", living_situations[situation], "</div>",
                                          "<div class='col-8'>", span(paste(sample(wee_letters, 
                                                                                   living_situations[situation],
                                                                                   replace = T), 
                                                                            collapse = "", sep = ""),
                                                                      style = "font-family:weepeople;
                                                color:slategray;
                                                font-size: 20pt;
                                                inline-size:600px;
                                                overflow-wrap: break-word"), "</div>",
                                          "</div>"
        )
        )
      }
    }
    
    housing_status_html
  })
  
  
  # Homeless gauges ----------------------------
  
  # output$doubled_gauge <- renderGauge({
  #   homeless_in_district() %>% 
  #     filter(reporting_category == "TA") %>% 
  #     pull(`temporarily_doubled_up_(percent)`) %>% 
  #     nth(1) %>% 
  #     gauge(min = 0,
  #           max = 100,
  #           symbol = "%",
  #           gaugeSectors(warning = c(0, 100)))
  # })
  # 
  # output$shelters_gauge <- renderGauge({
  #   homeless_in_district() %>% 
  #     filter(reporting_category == "TA") %>% 
  #     pull(`temporary_shelters_(percent)`) %>% 
  #     nth(1) %>% 
  #     gauge(min = 0,
  #           max = 100,
  #           symbol = "%",
  #           gaugeSectors(warning = c(0, 100)))
  # })
  # 
  # output$hotel_motels_gauge <- renderGauge({
  #   homeless_in_district() %>% 
  #     filter(reporting_category == "TA") %>% 
  #     pull(`hotels/motels_(percent)`) %>% 
  #     nth(1) %>% 
  #     gauge(min = 0,
  #           max = 100,
  #           symbol = "%",
  #           gaugeSectors(warning = c(0, 100)))
  # })
  # 
  # output$unsheltered_gauge <- renderGauge({
  #   homeless_in_district() %>% 
  #     filter(reporting_category == "TA") %>% 
  #     pull(`temporarily_unsheltered_(percent)`) %>% 
  #     nth(1) %>% 
  #     gauge(min = 0,
  #           max = 100,
  #           symbol = "%",
  #           gaugeSectors(warning = c(0, 100)))
  # })
  # 
  # output$unknown_gauge <- renderGauge({
  #   homeless_in_district() %>% 
  #     filter(reporting_category == "TA") %>% 
  #     pull(`missing/unknown_(percent)`) %>% 
  #     nth(1) %>% 
  #     gauge(min = 0,
  #           max = 100,
  #           symbol = "%",
  #           gaugeSectors(warning = c(0, 100)))
  # })
  
  
  # Homeless plots --------------------------
  output$homeless_plot <- renderPlot({
    
    homeless_in_school() %>% 
      filter(reporting_category == "TA") %>% 
      ggplot() +
      geom_line(mapping = aes(x = as_factor(academic_year), 
                              y = homeless_student_enrollment,
                              group = school_name),
                linewidth = 2,
                color = "skyblue") +
      expand_limits(y = 0) +
      labs(title = paste("Students Experiencing Homelessness in\n", 
                         selected_district(),
                         selected_school(),
                         "\nsince", homeless_in_school()$academic_year[1]),
           x = "Academic Year",
           y = "Number of Students") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        title = element_text(size = 16),
        axis.text = element_text(size = 12)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
