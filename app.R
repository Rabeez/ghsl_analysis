library(shiny)
library(tidyverse)
library(sf)
library(basemapR)

prepare_data <- function(uc, fau) {
  data <- fau %>% 
    mutate(type = "Functional Urban Area") %>% 
    rename(country = Cntry_ISO,
           name = eFUA_name,
           area = FUA_area) %>%  
    select(country, name, type, area) %>% 
    bind_rows(
      uc %>% 
        mutate(type = "Urban Center") %>% 
        rename(country = CTR_MN_ISO,
               name = UC_NM_MN,
               area = AREA) %>% 
        select(country, name, type, area)
    ) %>%
    arrange(country, name, type) %>% 
    group_by(country, name, type) %>% 
    slice(1) %>% # only keep one city for a given same in a country (india should not have 2 jalalabads)
    ungroup() %>% 
    group_by(country, name) %>% 
    mutate(
      ratio = area / lag(area)
    ) %>% 
    fill(ratio, .direction = "up") %>% 
    mutate(
      title = str_c(country, name, sep = "-")
    ) %>% 
    select(country, name, title, type, area, ratio) %>% 
    filter(!(name %in% c("UNNAMED", "N/A")))
  
  correct_cities <- as_tibble(data) %>% 
    count(title) %>% 
    filter(n > 1) %>% 
    pull(title)
  
  data %>% 
    filter(title %in% correct_cities)
}

make_comparison_plot <- function(p_data, names) {
  make_map_plot <- function(d) {
    title = d %>% head(1) %>% pull(title)
    pct = scales::percent(d %>% head(1) %>% pull(ratio), accuracy = 0.01)
    
    bbox <- expand_bbox(st_bbox(d), X = 1, Y = 1)
    
    d %>% 
      ggplot(aes(fill = type), environment = environment()) + 
      base_map(bbox, increase_zoom = 2, basemap = "google-hybrid") +
      geom_sf(alpha = 0.5, color = NA) +
      # guides(fill = "none") +
      labs(title = glue::glue("{title} | {pct}")) +
      ggthemes::theme_map()
  }
  
  data <- p_data %>% 
    filter(title %in% names)
  remaining_titles <- names[!(names %in% unique(data$title))]
  
  message(glue::glue("Following cities not found: {remaining_titles}"))
  
  data <- data %>% 
    group_by(country, name) %>% 
    nest() %>% 
    mutate(
      # this adds the grouping columns (`country`, `name`) into the nested dataframes
      data = lapply(data, function(d) d %>% mutate(country = country, name = name)),
      plot = lapply(data, make_map_plot)
    )
  
  patchwork::wrap_plots(
    data$plot,
    ncol = case_when(
      length(names) == 1 ~ 1,
      length(names) == 2 ~ 2,
      length(names) == 3 ~ 2,
      TRUE ~ 3
    )
  ) +
    patchwork::plot_layout(guides = 'collect') + 
    patchwork::plot_annotation(
      # title = "Urban Sprawl",
      subtitle = "Data from the Global Human Settlement Layer (GHSL)\n",
      caption = "map data © 2020 Google"
    )
}


ui <- fluidPage(
  style = "height: 600px",
  
  titlePanel("Urban Sprawl"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      selectizeInput(
        "chosen_cities", 
        label = "Cities:",
        choices = NULL,
        multiple = TRUE
      )
    ),
    
    mainPanel(
      width = 9,
      style = "padding: 0; height: 100%; align-content: left",
      plotOutput("main_plot") %>% 
        shinycssloaders::withSpinner(type = 6)
    )
    
  )
)

server <- function(input, output, session) {
  
  df_uc <- read_sf("data/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")
  df_fua <- read_sf("data/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg") %>% 
    st_transform("WGS84")
  p_data <- prepare_data(df_uc, df_fua)
  
  updateSelectizeInput(
    session,
    "chosen_cities",
    choices = unique(p_data$title),
    selected = c("PAK-Karachi"),
    server = TRUE
  )
  
  output$main_plot <- renderPlot({
    make_comparison_plot(p_data, input$chosen_cities)
  })
  
}

shinyApp(ui, server)
