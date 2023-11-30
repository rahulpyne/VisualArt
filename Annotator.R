library(magick)
library(MetBrewer)
library(colorspace)
library(ggplot2)
library(glue)
library(stringr)

img <- image_read("images/final_plot.png")

colors <- met.brewer("Tam")
swatchplot(colors)

text_color <- darken(colors[7], .25)
swatchplot(text_color)

annot <- glue("This map shows population density of Florida. ",
              "Population estimates are bucketed into 400 meter (about 1/4 mile) ",
              "hexagons.") |> 
  str_wrap(45)

img |> 
  image_crop(gravity = "center",
             geometry = "5000x5000+0+150") |> 
  image_annotate(text = glue("{str_to_title('west bengal')}"),
                 gravity = "west",
                 location = "+400+600",
                 color = text_color,
                 size = 200,
                 weight = 700,
                 font = "Surveyor") |>
  image_annotate(text = glue("India"),
                 gravity = "west",
                 location = "+750+800",
                 color = text_color,
                 size = 150,
                 weight = 600,
                 font = "Surveyor") |> 
  image_annotate(glue("This map shows population density."),
                 gravity = "southwest",
                 location = "+50+300",
                 font = "Surveyor",
                 color = alpha(text_color, .7),
                 size = 100) |>
  image_annotate(glue("@Graphic by Rahul Pyne | ",
                      "Data: Kontur Population (Released 2023-11-01)"),
                 gravity = "southwest",
                 location = "+50+100",
                 font = "Surveyor",
                 color = alpha(text_color, .5),
                 size = 70) |> 
  image_write("images/titled_final_plot.png")
