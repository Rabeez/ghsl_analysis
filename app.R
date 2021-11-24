library(shiny)
library(tidyverse)
library(sf)
library(basemapR)


## Setup app theme ----
my_theme <- bslib::bs_theme(
  bootswatch = "litera"
)
thematic::thematic_shiny(
  font = "auto"
)


## Utility functions ----
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
      ggplot(aes(fill = type)) + 
      base_map(bbox, increase_zoom = 2, basemap = "google-hybrid") +
      geom_sf(alpha = 0.5, color = NA) +
      scale_fill_manual(values = c("#1b9e77", "#e7298a")) + 
      # guides(fill = "none") +
      labs(title = glue::glue("{title} | Coverage: {pct}")) +
      ggthemes::theme_map()
  }
  
  if (missing(names) | is.null(names) | (length(names) == 0)) {
    # render blank when cities selection is empty
    blank_plot <- ggplot(tibble(text = "Please select a city")) + 
      geom_text(aes(x = 0,y = 0,label = text)) + 
      theme_void()
    return(ggplot())
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
      length(names) == 4 ~ 2,
      TRUE ~ 3
    )
  ) +
    patchwork::plot_layout(guides = 'collect') + 
    patchwork::plot_annotation(
      title = "Urban Sprawl",
      subtitle = "Data from the Global Human Settlement Layer (GHSL)\n",
      caption = "map data © 2020 Google"
    )
}


## UI ----
ui <- fluidPage(
  theme = my_theme,
  
  # titlePanel("Urban Sprawl"),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      # This gets the broser viewport dimensions on resize and passes them to shiny as `dimension` variable
      # https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
      tags$head(tags$script('
                            var dimension = [0, 0];
                            $(document).on("shiny:connected", function(e) {
                                dimension[0] = window.innerWidth;
                                dimension[1] = window.innerHeight;
                                Shiny.onInputChange("dimension", dimension);
                            });
                            $(window).resize(function(e) {
                                dimension[0] = window.innerWidth;
                                dimension[1] = window.innerHeight;
                                Shiny.onInputChange("dimension", dimension);
                            });
                        ')),
      
      selectizeInput(
        "chosen_cities", 
        label = "Cities:",
        choices = NULL,
        multiple = TRUE
      ),
      
      div(
        class = "alert alert-secondary",
        withMathJax(),
        "\\(\\text{Coverage} = \\frac{Area(\\text{Urban Center})}{Area(\\text{Functional Urban Area})}\\)"
      )
    ),
    
    mainPanel(
      width = 9,
      style = "padding: 0; height: 100%",
      plotOutput("main_plot") %>% 
        shinycssloaders::withSpinner(type = 6)
    )
    
  )
)


## Server ----
server <- function(input, output, session) {
  
  df_uc <- read_sf("data/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")
  df_fua <- read_sf("data/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg") %>% 
    st_transform("WGS84")
  p_data <- prepare_data(df_uc, df_fua)
  
  updateSelectizeInput(
    session,
    "chosen_cities",
    choices = unique(p_data$title),
    selected = c("PAK-Karachi", "GBR-London", "JPN-Tokyo"),
    server = TRUE
  )
  
  output$main_plot <- renderPlot({
    make_comparison_plot(p_data, input$chosen_cities)
  }, height = reactive({0.95 * input$dimension[2]}))
  
}


## Execution ----
shinyApp(ui, server)
