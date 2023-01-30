# Load packages -----
pacman::p_load(
  install = F,    # Do (not) install packages 
  update = F,     # Do (not) update  package if possible
  char = c( 
    "tidyverse",
    "here", 
    "patchwork",  
    "janitor",
    "sf",
    "sugarbag",
    "ggthemes"))

# Load election results data ---
if (!dir.exists(here::here("data"))) dir.create(here::here("data"))

df_2022_raw <- here::here("data", "circ_results_2022_raw.xlsx")
df_2022_tidy <- here::here("data", "circ_results_2022_tidy.csv")

if (!file.exists(df_2022_tidy)) {
  # Download the data
  download.file(
    url = "https://www.data.gouv.fr/fr/datasets/r/b7dc46bb-a43c-44ca-9683-d2ac54f52977",
    destfile = df_2022_raw)
  
  # Unzip data and save the csv
  df_2022 <-
    df_2022_raw %>%
    readxl::read_xlsx()
  
  # Rename variables
  df_2022 <- 
    df_2022 %>% 
    janitor::clean_names()  %>%
    rename(
      id_circ = code_de_la_circonscription,
      id_dept = code_du_departement,
      nom_dept = libelle_du_departement)
  
  # Select only the candidates who won
  df_2022 <- 
    df_2022 %>% 
    mutate(
      party_2022 = case_when(
        sieges == "Elu" ~ nuance,
        x37    == "Elu" ~ x33,
        x46    == "Elu" ~ x42
      ))
  
  # Move the df around
  df_2022 <-
    df_2022 %>%
    relocate(
      id_circ,
      id_dept,
      nom_dept) %>% 
    select(
      id_circ,
      id_dept,
      nom_dept,
      party_2022)
  
  write_csv(
    x = df_2022,
    file = df_2022_tidy)
}

df_2022 <- read_csv(df_2022_tidy, 
                    show_col_types = F)

# Load shapefile ---
shapefile_dir <- here::here("data", "shapefile")
if (!dir.exists(shapefile_dir)) dir.create(shapefile_dir)
shapefile_dir_empty <- length(list.files(shapefile_dir)) == 0L

if (shapefile_dir_empty) {
  shapefile_zip <- here::here(shapefile_dir, "shapefile.zip")
  download.file("https://www.data.gouv.fr/fr/datasets/r/91fd8357-c753-43d8-b722-760305614c92",
                shapefile_zip)
  unzip(shapefile_zip,
        exdir = shapefile_dir)
  file.remove(shapefile_zip)
}

df_geom <- sf::st_read(shapefile_dir)

# Rename variables
df_geom <- 
  df_geom %>% 
  rename(
    id_dept = code_dpt,
    id_circ = num_circ,
    id_regn = code_reg,
    nom_dept = nom_dpt, 
    nom_regn = nom_reg) %>% 
  select(-ID, -nom_dept) 

# Add leading zeroes and convert to character
df_geom <-
  df_geom %>%
  mutate(
    id_dept = as.character(str_pad(id_dept, 2, pad = "0")),
    id_circ = as.character(str_pad(id_circ, 2, pad = "0")),
    id_regn = str_replace_all(id_regn, "-", "0"),
    id_regn = as.character(str_pad(id_regn, 2, pad = "0")),
    nom_regn = if_else(nom_regn == "-", "Overseas Collectivity", nom_regn),
    nom_regn = str_to_title(nom_regn))


# Add election data
df_geom <- 
  df_geom %>% 
  left_join(
    y = df_2022,
    by = c("id_dept", "id_circ"))

df_france <- df_geom %>% 
  filter(as.numeric(id_regn) > 10)

# Create map of mainland France
gg_map_france <- 
  df_france %>%
  ggplot(aes(fill = party_2022)) + 
  geom_sf(alpha = 0.1) +
  sugarbag::geom_sugarbag(aes(geometry = geometry)) +
  theme_void() 

gg_map_france



# Manual sugarbag ----

df_france <- df_france %>% 
  unite(sf_id, id_dept, id_circ, remove = F)


centroids <- create_centroids(df_france, "sf_id")
grid <- create_grid(centroids,
                    hex_size = 0.3, 
                    buffer_dist = 1.2)

french_focal_points <- tibble::tribble(
  ~name, ~longitude, ~latitude,
  "Paris",   2.326563,  48.82518,
  "Marseille",     5.3698,   43.2965,
  "Lyon",     4.8357,    45.764,
  "Toulouse",     1.4442,   43.6047,
  "Nice",      7.262,   43.7102
)

hex_allocated <- allocate(
  centroids = centroids,
  hex_grid = grid,
  sf_id = "sf_id",
  hex_size = 0.3, # same size used in create_grid
  hex_filter = 3,
  focal_points = french_focal_points,
  width = 30, 
  verbose = TRUE
)

hexagons <- fortify_hexagon(data = hex_allocated, sf_id = "sf_id", hex_size = 0.3) %>% 
  separate(sf_id, c("id_dept", "id_circ"), remove = FALSE) %>% 
  left_join(df_2022, by = c("id_dept", "id_circ"))

ggplot() +
  geom_sf(data=df_france, 
            colour = "white",
            fill = "grey90") +
  geom_polygon(data = hexagons, 
               aes(x=long, lat, 
                   group = sf_id,
                   fill = party_2022), 
               colour="white", size=0.1) +
  scale_fill_viridis_d() +
  theme_map() +
  theme(legend.position = "none", aspect.ratio = 0.8) #+ 
  #coord_map(projection="mercator")
  #coord_map(projection="rectangular", lat0=10)

#plotly::ggplotly()

# Check grid
ggplot() +
  geom_sf(data=df_france, 
          colour = "white",
          fill = "grey90") +
  geom_point(data = grid, 
               aes(x=hex_long, hex_lat), alpha=0.5, size=0.1) +
  scale_fill_viridis_d() +
  theme_map() +
  theme(legend.position = "none", aspect.ratio = 1)
