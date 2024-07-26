# 1 | Libraries ----
# remotes::install_version(
#   "rayshader",
#   version = "0.35.7",
#   repos = "http://cran.us.r-project.org"
# )
#
# remotes::install_version(
#   "rayimage",
#   version = "0.9.1",
#   repos = "http://cran.us.r-project.org"
# )
#
# remotes::install_version(
#   "rayvertex",
#   version = "0.7.9",
#   repos = "http://cran.us.r-project.org"
# )
#
# remotes::install_version(
#   "rayrender",
#   version = "0.29.6",
#   repos = "http://cran.us.r-project.org"
# )

devtools::install_github("tylermorganwall/rayshader")
devtools::install_github("tylermorganwall/rayimage")
devtools::install_github("tylermorganwall/rayvertex")
devtools::install_github("tylermorganwall/rayrender")

library(tidyverse)
library(fs)
library(terra)
library(sf)
library(elevatr)
library(geodata)
library(rayshader)

state = "West Virginia"

# 2 | Download Files
url_rivers = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip"
file_rivers = str_c("data/", path_file(url_rivers))

url_hdri = "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
file_hdri = str_c("data/", path_file(url_hdri))

if (FALSE) {
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

  download.file(
    url = url_hdri,
    destfile = file_hdri,
    mode = "wb"
  )
}

# 3 | State Borders ----
state_sf = gadm(country = "USA", level = 1, path = "data") |>
  st_as_sf() |>
  filter(NAME_1 == state)

plot(st_geometry(state_sf))

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
      ORD_FLOW == 3 ~ 16,
      ORD_FLOW == 4 ~ 14,
      ORD_FLOW == 5 ~ 12,
      ORD_FLOW == 6 ~ 10,
      ORD_FLOW == 7 ~  8,
      .default = 0)) |>
  st_as_sf() |>
  st_transform(crs = crs_na)

rm(state_river)

# 6 | DEM ----
dem = get_elev_raster(
  locations = state_sf,
  z = 10,
  clip = "locations"
)

dem_state = dem |>
  rast() |>
  project(crs_na)

dem_matrix = raster_to_matrix(dem_state)

# 7 | Render Scene ----
dem_matrix |>
  height_shade(
    texture = colorRampPalette(c("#003f5C","#00121A"))(128)) |>
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
    zscale = 7,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(200, 200, 800, 800),
    zoom = 0.55,
    phi = 85,
    theta = 0)

rgl::snapshot3d(paste0("west-virginia-2d-rivers_v2.png"))

# 8 | Render Object ----
file_name = str_c(state, " 3D Elevation Map with Rivers.png")

render_highquality(
  filename = file_name,
  # samples = 256,
  # sample_method = "sobol",
  preview = TRUE,
  light = FALSE,
  environment_light = file_hdri,
  intensity_env = 1.5,
  interactive = FALSE,
  progress = TRUE,
  width = 3000,
  height = 2400
)
