library(tidyverse)
library(sf)
library(basemapR)


# global human settlement layer
# https://ghsl.jrc.ec.europa.eu/datasets.php


# urban centers

df <- read_sf("data/GHS_STAT_UCDB2015MT_GLOBE_R2019A/GHS_STAT_UCDB2015MT_GLOBE_R2019A_V1_2.gpkg")

pak <- df %>%
  filter(CTR_MN_NM == "Pakistan") 
bbox <- expand_bbox(st_bbox(pak), X = 1000, Y = 1000)


pak %>% filter(UC_NM_MN == "Lahore") %>% ggplot() + geom_sf()

pak %>% 
  ggplot() +
  base_map(bbox, increase_zoom = 2, basemap = "dark") + 
  geom_sf(aes(fill = P15), color = NA) + 
  scale_fill_gradient2(low = "blue", mid = "pink", high = "red", midpoint = log10(1000000), 
                       trans = "log10", label = scales::label_comma()) + 
  ggthemes::theme_map()


pak %>% 
  mutate(
    plot_label = fct_other(UC_NM_MN, 
                           keep = c("Lahore", "Karachi", "Islamabad", "Quetta", "Peshawar"), 
                           other_level = "")
  ) %>% 
  ggplot(aes(x = AREA, y = P15)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  ggrepel::geom_text_repel(aes(label = plot_label)) +
  scale_x_log10(label = scales::comma_format()) + 
  scale_y_log10(label = scales::comma_format())
  


# functional urban areas

sf::sf_use_s2(FALSE)
df_fua <- read_sf("data/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg") %>% 
  st_transform("WGS84")

pak_fua <- df_fua %>%
  filter(Cntry_name == "Pakistan") 
# pak_fua$geom <- pak_fua$geom %>%
#   s2::s2_rebuild() %>%
#   sf::st_as_sfc()
bbox_fua <- expand_bbox(st_bbox(pak_fua), X = 1000, Y = 1000)

pak_fua %>% 
  ggplot() +
  base_map(bbox_fua, increase_zoom = 2, basemap = "dark") + 
  geom_sf(aes(fill = FUA_p_2015), color = NA) + 
  scale_fill_gradient2(low = "blue", mid = "pink", high = "red", midpoint = log10(1000000), 
                       trans = "log10", label = scales::label_comma()) + 
  ggthemes::theme_map()


pak_fua %>% 
  mutate(
    plot_label = fct_other(eFUA_name, 
                           keep = c("Lahore", "Karachi", "Islamabad", "Quetta", "Peshawar"), 
                           other_level = "")
  ) %>% 
  ggplot(aes(x = FUA_area, y = FUA_p_2015)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth() + 
  ggrepel::geom_text_repel(aes(label = plot_label)) +
  scale_x_log10(label = scales::comma_format()) + 
  scale_y_log10(label = scales::comma_format())


# urban area coverage


a <- df_fua %>% 
  filter(eFUA_name == "Lahore") %>% 
  mutate(type = "Functional Urban Area") %>% 
  rename(country = Cntry_name,
         name = eFUA_name,
         area = FUA_area) %>%  
  select(country, name, type, area)

b <- df %>% 
  filter(UC_NM_MN == "Lahore") %>% 
  mutate(type = "Urban Center") %>% 
  rename(country = CTR_MN_NM,
         name = UC_NM_MN,
         area = AREA) %>% 
  select(country, name, type, area)


prepare_data <- function(uc, fau) {
  fau %>% 
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
    select(country, name, title, type, area, ratio)
}

p_data <- prepare_data(df, df_fua) %>% 
  filter(!(name %in% c("UNNAMED", "N/A")))

p_data %>% 
  select(country, name, ratio) %>% 
  distinct() %>% 
  ggplot(aes(x = ratio)) + 
  geom_histogram() + 
  scale_x_log10(label = scales::label_percent())

compare_types <- function(p_data, names) {
  make_map_plot <- function(d) {
    title = d %>% head(1) %>% pull(title)
    pct = scales::percent(d %>% head(1) %>% pull(ratio), accuracy = 0.01)
    
    bbox <- expand_bbox(st_bbox(d), X = 1, Y = 1)
    
    d %>% 
      ggplot(aes(fill = type)) + 
      base_map(bbox, increase_zoom = 2, basemap = "google-hybrid") +
      geom_sf(alpha = 0.5, color = NA) +
      # guides(fill = "none") +
      labs(title = glue::glue("{title} | {pct}")) +
      ggthemes::theme_map()
  }
  
  # dynamic filtering
  # looks for value in title column first
  # then looks for remaining values in the name column
  # this allows me to specify country name optionally in case same named city is in multiple countries
  a <- p_data %>% 
    filter(title %in% names)
  found_titles <- unique(a$title)
  remaining_names <- names[!(names %in% found_titles)]
  b <- p_data %>% 
    filter(name %in% remaining_names)
  found_names <- unique(b$name)
  
  data <- bind_rows(a, b)
  message(glue::glue("Following cities not found: {names[!(names %in% c(found_titles, found_names))]}"))
  
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
      title = "Urban Sprawl",
      subtitle = "Data from the Global Human Settlement Layer (GHSL)\n",
      caption = "map data © 2020 Google"
    )
}

compare_types(p_data, c("Lahore"))
compare_types(p_data, c("Lahore", "Karachi", "Quetta", "Peshawar", "Rawalpindi [Islamabad]"))

compare_types(
  p_data, 
  p_data %>% 
    filter(ratio < 0.03) %>% 
    pull(name) %>% 
    unique() %>% 
    sample(6)
)
compare_types(p_data, c("Manama", "SGP-Singapore"))
compare_types(p_data, c("GBR-Cambridge"))

compare_types(p_data, c("Kabul", "Kandahar", "AFG-Jalalabad", "Gilgit"))
compare_types(p_data, c("GBR-London", "Madrid"))
compare_types(p_data, c("Lisbon", "Tokyo", "Seoul"))
compare_types(p_data, c("GBR-London", "Cairo"))
compare_types(p_data, c("Lisbon", "New York"))
compare_types(p_data, c("Chennai", "Bengaluru", "Kolkata", "Delhi [New Delhi]"))
compare_types(p_data, c("Osaka [Kyoto]", "Istanbul", "Los Angeles"))
compare_types(p_data, c("BGD-Dhaka", "Jakarta"))
