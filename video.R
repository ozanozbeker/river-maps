# 1 | Libraries ----
install.packages(
  "remotes"
)

remotes::install_version(
  "rayshader",
  version = "0.35.7",
  repos = "http://cran.us.r-project.org"
)

remotes::install_version(
  "rayimage",
  version = "0.9.1",
  repos = "http://cran.us.r-project.org"
)

remotes::install_version(
  "rayvertex",
  version = "0.7.9",
  repos = "http://cran.us.r-project.org"
)

remotes::install_version(
  "rayrender",
  version = "0.29.6",
  repos = "http://cran.us.r-project.org"
)

library(tidyverse)
library(fs)
library(terra)
library(sf)
library(elevatr)
library(geodata)
library(rayshader)

state = "West Virginia"

# 2 | State Borders ----
state_sf = gadm(country = "USA", level = 1, path = "data") |>
  st_as_sf() |>
  filter(NAME_1 == state)

plot(st_geometry(state_sf))

# 3 | Download Rivers ----
url_rivers = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
file_rivers = str_c("data/", path_file(url_rivers))

download.file(
  url = url_rivers,
  destfile = file_rivers,
  mode = "wb"
)

unzip(file_rivers, exdir = "data")

dir_info("data") |>
  filter(path |> str_ends(".zip")) |>
  pull("path") |>
  file_delete()

# 4 | Load Rivers ----
filename = dir_info("data", recurse = 1) |>
  filter(
    path |> str_ends("na.shp"),
    type == "file") |>
  pull(path)

bbox_wkt = str_c(
  "POLYGON((",
  st_bbox(state_sf)[["xmin"]], " ",st_bbox(state_sf)[["ymin"]], ",",
  st_bbox(state_sf)[["xmin"]], " ",st_bbox(state_sf)[["ymax"]], ",",
  st_bbox(state_sf)[["xmax"]], " ",st_bbox(state_sf)[["ymax"]], ",",
  st_bbox(state_sf)[["xmax"]], " ",st_bbox(state_sf)[["ymin"]], ",",
  st_bbox(state_sf)[["xmin"]], " ",st_bbox(state_sf)[["ymin"]],
  "))"
)

state_river = filename |>
  st_read(wkt_filter = bbox_wkt) |>
  st_intersection(state_sf)

plot(st_geometry(state_river))

# 5 | River Width ----
# https://epsg.io/21781 | This covers all of North America
crs_na = "+proj=utm +zone=17 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

state_river_width = state_river |>
  as_tibble() |>
  mutate(
    width = case_when( # in pixels
      # ORD_FLOW == 1 ~ 20,
      # ORD_FLOW == 2 ~ 18,
      ORD_FLOW == 3 ~ 12,
      ORD_FLOW == 4 ~ 10,
      ORD_FLOW == 5 ~  8,
      ORD_FLOW == 6 ~  6,
      ORD_FLOW == 7 ~  4,
      .default = 0)) |>
  st_as_sf() |>
  st_transform(crs = crs_na)

# 6 | DEM ----
dem = get_elev_raster(
  locations = state_sf,
  z = 9,
  clip = "locations"
)

dem_state = dem |>
  rast() |>
  project(crs_na)

dem_matrix = raster_to_matrix(dem_state)

# 7 | Render Scene ----
dem_matrix |>
  height_shade(
    texture = colorRampPalette(c("#003f5c","#00202e"))(128)) |>
  add_overlay(
    generate_line_overlay(
      geometry = state_river_width,
      extent = dem_state,
      heightmap = dem_matrix,
      color = "#ffa600",
      linewidth = state_river_width$width,
      data_column_width = "width"),
    alphalayer = 1) |>
  plot_3d(
    dem_matrix,
    zscale = 20,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(200, 200, 2160, 2160),
    zoom = 0.7,
    phi = 85,
    theta = 0
  )

# 8 | Render Object ----
file_name = "west-virginia-3d-elevation-rivers.png"

url_hdri = "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
file_hdri = str_c("data/", path_file(url_hdri))

download.file(
  url = url_hdri,
  destfile = file_hdri,
  mode = "wb"
)

render_highquality(
  filename = file_name,
  samples = 256,
  preview = TRUE,
  light = FALSE,
  environment_light = file_hdri,
  interactive = FALSE,
  progress = TRUE,
  width = 2160,
  height = 2160,
  parallel = TRUE,
  lightdirection = 225
)
