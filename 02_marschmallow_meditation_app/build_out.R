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
  theme_void() +
  theme(legend.position = "none") +
  transition_reveal(stage) +
  enter_fade() +
  exit_shrink() +
  ease_aes('linear')

breathe

# create a box-breathing dataset ----
#' between hold (x2), inhale, and exhale we need to keep 100 rows 
data_02 <- tibble(x = rep(0, 100),
                  y = rep(0, 100)) %>% 
  #' need to change size to correspond with text
  mutate(size = c(rep(20, 25),  # hold
                  seq(20, 178, length.out = 25),  # inhale
                  rep(178, 25),  # hold
                  seq(178, 20, length.out = 25)),  # exhale
         stage = row_number(),  # frame of reference for animation
         #' change text variable to change length of each component
         text = c(rep("H O L D", 25),
                  rep("I N H A L E", 25),
                  rep("H O L D", 25),
                  rep("E X H A L E", 25)),
         #' ALSO have to change this one!
         text_size = c(rep(5, 25),  # hold
                       seq(5, 28.8, length.out = 25),  # inhale
                       rep(28.8, 25),  # hold
                       seq(28.8, 5, length.out = 25) # exhale
         ),
         # used for the outer circle
         alpha = c(rep(0, 10),
                   seq(0, 0.975, 0.025),
                   rep(0.975, 10),
                   seq(0.975, 0, -0.025)))  # transparency

data_02 %>% 
  count(text)

breathe_02 <- data_02 %>% 
  ggplot(aes(x = x, y = y, label = text)) +
  ggpubr::background_image(space) +
  geom_point(size = data_02$size) +
  geom_point(aes(x = 0, y = 0),
             size = 178, color = "white", shape = 1, alpha = data_02$alpha) +
  geom_text(size = data_02$text_size - 2, color = "#ffffff", family = "Amatic SC") +
  xlim(-200, 200) + 
  ylim(-200, 200) +
  theme_void() +
  theme(legend.position = "none") +
  transition_reveal(stage) +
  enter_fade() +
  exit_shrink() +
  ease_aes('linear')

animate(breathe_02, fps = 10)
