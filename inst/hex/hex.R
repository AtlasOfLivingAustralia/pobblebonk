# script to build an example hex sticker

library(hexSticker)
library(ggplot2)
library(ggtext)
library(showtext)
library(glue)
# font_add_google("Lato", family = "lato")

dark <- "#99260e"
light <- "#F26649"

text_df <- data.frame(
  x = c(0.2, 0.22, 0.49),
  y = c(0.55, 0.45, 0.11), 
  label = c(
    glue("<span style='color:{dark}'>t</span><span style='color:{light}'>a</span><span style='color:{dark}'>w</span><span style='color:{light}'>ny</span>"),
    glue("<span style='color:{dark}'>d</span><span style='color:{light}'>ra</span><span style='color:{dark}'>g</span><span style='color:{light}'>on</span>"),
    glue("<span style='color:{dark}'>tdwg.org</span> <span style='color:{light}'>ala.org.au</span>")
  ),
  size = c(7, 7, 2.5),
  angle = c(0, 0, 30)
)
p <- ggplot() + 
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_richtext(data = text_df[1:2, ], 
                mapping = aes(x = x, 
                              y = y, 
                              label = label),
                hjust = 0,
                size = 7,
                fill = NA,
                label.color = NA,
                family = "lato") +
  geom_richtext(data = text_df[3, ], 
                mapping = aes(x = x, 
                              y = y, 
                              label = label),
                hjust = 0,
                angle = 30,
                size = 2.5,
                fill = NA,
                label.color = NA,
                family = "lato") +
  theme_void() +
  theme(legend.position = "none")

# export as sticker
sticker(
  subplot = p,
  s_x = 1,
  s_y = 1,
  s_width = 2.8,
  s_height = 2.5,
  package = "",
  p_color = "#ffffff",
  p_y = 1,
  p_family = "lato",
  p_size = 5,
  # border
  h_fill = "#ffffff",
  h_color = "#000000",
  filename = "man/figures/logo.png",
  # url
  url = "",
  u_family = "lato",
  u_color = dark,
  u_y = 0.1,
  u_size = 2.5
)
