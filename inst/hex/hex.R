# script to build an example hex sticker

library(hexSticker)
library(ggplot2)
library(ggtext)
library(showtext)
font_add_google("Lato", family = "lato")

text_df <- data.frame(
  x = c(0.2, 0.22),
  y = c(0.56, 0.44), 
  label = c("<span style='color:#000000'>t</span><span style='color:#F26649'>a</span><span style='color:#000000'>w</span><span style='color:#F26649'>ny</span>", 
            "<span style='color:#000000'>d</span><span style='color:#F26649'>ra</span><span style='color:#000000'>g</span><span style='color:#F26649'>on</span>")
)
p <- ggplot(text_df) + 
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_richtext(data = text_df, 
                mapping = aes(x = x, y = y, label = label),
                hjust = 0,
                size = 7, 
                fill = NA,
                label.color = NA,
                family = "lato") +
  theme_void()

# export as sticker
sticker(
  subplot = p,
  # s_x = 1,
  s_y = 1,
  s_width = 2,
  s_height = 2,
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
  url = "ala.org.au",
  u_family = "lato",
  u_color = "#F26649",
  u_y = 0.1,
  u_size = 2.5
)