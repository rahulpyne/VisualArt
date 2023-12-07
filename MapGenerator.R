library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)

# Need only for first setup of Rstudio
install.packages("remotes")
remotes::install_github("https://github.com/tylermorganwall/rayshader")
remotes::install_github("https://github.com/tylermorganwall/rayrender")

# load kontur data of entire country

data <- st_read("data/kontur_population_IN_20231101.gpkg")

# load states
#If US, one can use tigris directly
#st <- states()

state_sf = st_read(dsn="data/Rajasthan.gpkg")

# set wb_sf crs to crs of data

#florida <- st |> 
 # filter(NAME == "Florida") |> 
  #st_transform(crs = st_crs(data))

state_sf <- state_sf |>
  st_transform(crs = st_crs(data))

# check with map

state_sf |> 
  ggplot() +
  geom_sf()



# do intersection on data to limit kontur to West Bengal
st_state <- st_intersection(data, state_sf)

# define aspect ratio based on bounding box

bb <- st_bbox(st_state)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))


bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

# check by plotting points

state_sf |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- type.convert(width / height, as.is = TRUE)
}

# convert to raster so we can then convert to matrix

#test
#size <- 1000 

size <- 5000

state_rast <- st_rasterize(st_state, 
                             nx = floor(size * w_ratio),
                             ny = floor(size * h_ratio))

mat <- matrix(state_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette

c1 <- met.brewer("Paquin", direction = -1)
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 3)(256)
swatchplot(texture)

# plot that 3d thing!

rgl::close3d()

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100/5,
          solid = FALSE,
          shadowdepth = 0,
          theta = -115,
          phi = 40,
          zoom = .7)

render_camera(theta = -115, phi = 40, zoom = 0.7)

outfile <- "images/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = c(200,200, 210, 210),
    lightaltitude = c(10, 70, 10, 70),
    lightcolor = c(c1[3], "white", c1[8], "white"),
    lightintensity = c(750,50, 1000, 50),
    samples = 450,#100,
    width = 5000,#6000,
    height = 5000#6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


