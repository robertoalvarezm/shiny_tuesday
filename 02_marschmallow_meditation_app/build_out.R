# setup ----
library(tidyverse)
library(gganimate)
library(transformr)
library(showtext)

# loading google fonts ----
font_add_google("Amatic SC")
showtext_auto()

# create a dataset ----
data <- tibble(x = rep(0, 100),
               y = rep(0, 100)) %>% 
  mutate(size = c(rep(20, 10),  # hold
                  seq(20, 178, 4),  # inhale
                  rep(178, 10),  # hold
                  seq(178, 20, -4)),  # exhale
         stage = row_number(),  # frame of reference for animation
         text = c(rep("H O L D", 10),
                  rep("I N H A L E", 40),
                  rep("H O L D", 10),
                  rep("E X H A L E", 40)),
         text_size = c(rep(5, 10),  # hold
                       seq(5, 28.8, 0.6),  # inhale
                       rep(28.8, 10),  # hold
                       seq(28.8, 5, -0.6) # exhale
                       ),
         alpha = c(rep(0, 10),
                   seq(0, 0.975, 0.025),
                   rep(0.975, 10),
                   seq(0.975, 0, -0.025)))  # transparency

# background images
space <- png::readPNG("02_marschmallow_meditation_app/marshmallow.png")

breathe <- data %>% 
  ggplot(aes(x = x, y = y, label = text)) +
  ggpubr::background_image(space) +
  geom_point(size = data$size) +
  geom_point(aes(x = 0, y = 0),
             size = 178, color = "white", shape = 1, alpha = data$alpha) +
  geom_text(size = data$text_size - 2, color = "#ffffff", family = "Amatic SC") +
  xlim(-200, 200) + 
  ylim(-200, 200) +
  theme_void()
