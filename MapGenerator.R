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

wb_sf = st_read(dsn="data/WestBengal/WB.gpkg")

# set wb_sf crs to crs of data

#florida <- st |> 
 # filter(NAME == "Florida") |> 
  #st_transform(crs = st_crs(data))

wb_sf <- wb_sf |>
  st_transform(crs = st_crs(data))

# check with map

wb_sf |> 
  ggplot() +
  geom_sf()



# do intersection on data to limit kontur to West Bengal
st_wb <- st_intersection(data, wb_sf)

# define aspect ratio based on bounding box

bb <- st_bbox(st_wb)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))


bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

# check by plotting points

wb_sf |> 
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

wb_rast <- st_rasterize(st_wb, 
                             nx = floor(size * w_ratio),
                             ny = floor(size * h_ratio))

mat <- matrix(wb_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette

c1 <- met.brewer("Tam", direction = -1)
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

# plot that 3d thing!

rgl::close3d()

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100/5,
          solid = FALSE,
          shadowdepth = 0,
          theta = -60,
          phi = 60,
          zoom = .9)

render_camera(theta = -60, phi = 60, zoom = 0.9)

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
    lightdirection = c(-90,-90, -100, -100),
    lightaltitude = c(10, 70, 10, 70),
    lightcolor = c(c1[2], "white", c1[7], "white"),
    lightintensity = c(750,50, 1000, 50),
    samples = 450,#100,
    width = 6000,#6000,
    height = 6000#6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


