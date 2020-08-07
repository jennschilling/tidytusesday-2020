# Author : Jenn Schilling
# Title: #TidyTuesday Penguins
# Date: 7/28/2020
# Artwork by @allison_horst

#### Packages ####
library(tidyverse)
library(palmerpenguins)
library(rpart)
library(png)
library(cowplot)
library(ggtext)
library(magick)
library(extrafont)

#### Data ####

penguins.clean <- penguins %>% 
  filter(!is.na(flipper_length_mm)) # remove 2 NA

# Add accent
penguins.clean <- penguins.clean %>% 
  mutate(species = ifelse(species == "Adelie", "Adélie", 
                          ifelse(species == "Gentoo", "Gentoo", "Chinstrap")))


#### Analysis ####

# Make decision tree for identifying penguin species
penguin.tree <- rpart(species ~ ., penguins.clean)

prp(penguin.tree, 
    type = 5, 
    yesno = 2, 
    uniform = TRUE, 
    varlen = 0, 
    faclen = 0, 
    tweak = 1,
    prefix = "",
    suffix = "",
)
title("Penguin Species Tree Classification")

# Create data frame to plot decision tree

# Nodes - Flipper Length (mm), Bill Length (mm), Island (mm)
# Labels - <207, >= 207, <43, >=43, Dream/Torgersen, Biscoe
# Leaves - Adélie, Chinstrap, Chinstrap, Gentoo

nodes <- tribble(
  ~name, ~x, ~y,
  "Flipper Length (mm)", 4.5, 8,
  "Bill Length(mm)", 2, 5,
  "Island", 7, 5
)

leaves <- tribble(
  ~name, ~x, ~y,
  "Adélie", 1, 2,
  "Chinstrap", 3, 2,
  "Chinstrap", 6, 2,
  "Gentoo", 8, 2 
)

segments <- tribble(
  ~x, ~xend, ~y, ~yend,
  4.5, 2, 8, 5,
  4.5, 7, 8, 5,
  2, 1, 5, 2,
  2, 3, 5, 2,
  7, 6, 5, 2,
  7, 8, 5, 2
)

linelabels <- tribble(
  ~name, ~x, ~y,
  "< 207", 3, 7,
  ">= 207", 6, 7,
  "< 43", 1.25, 4,
  ">= 43", 2.75, 4,
  "Dream or\nTorgersen", 6, 4,
  "Biscoe", 7.75, 4
)

tree.plot <- ggplot() +
  geom_segment(data = segments, aes(x, y, xend = xend, yend = yend)) +
  geom_label(data = nodes, aes(x, y, label = name)) +
  geom_label(data = leaves, 
             aes(x, y, label = name, fill = name), 
             alpha = 1,
             color = "white",
             show.legend = FALSE) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) + 
  geom_text(data = linelabels, aes(x, y, label = name)) +
  xlim(1, 8) +
  ylim(1, 8) +
  labs(title = "Classification of Penguin Species",
       subtitle = "Based on flipper length, bill length, and island, penguins can be classified into species using a decision tree.") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    # Remove x-axis
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
  ) 


# Plot flipper length by penguin species
flipper.plot <- ggplot(penguins.clean) +
  geom_density(aes(x = flipper_length_mm, fill = species),
               alpha = 0.5, 
               color = NA,
               show.legend = FALSE) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Distribution of Flipper Lengths (mm)",
       subtitle = "Gentoo penguins have longer flippers.",
       x = "") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank()
  )

# plot bill length by penguin species
bill.plot <- ggplot(penguins.clean) +
  geom_density(aes(x = bill_length_mm, fill = species), 
               alpha = 0.5, 
               color = NA,
               show.legend = FALSE) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Distribution of Bill Lengths (mm)",
       subtitle = "Adélie penguins have shorter bills.",
       x = "") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank()
  ) 

# Plot locations 
loc.plot <- ggplot(penguins.clean, aes(x = island, fill = species)) +
  geom_bar(position = "dodge", alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) +
  labs(title = "Island Location",
       subtitle = "Gentoo penguins are only found on Biscoe. Chinstrap penguins are only found on Dream.",
       x = "",
       y = "Number of Penguins") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  ) 


# Image
penguins.img <- readPNG("2020-07-28\\penguins.png")


# Title & Subtitle
title <- ggplot() + 
  labs(title = "Classifying Palmer Penguins", 
       subtitle = "<b style='color:#d95f02'>Adélie</b>, 
       <b style='color:#7570b3'>Chinstrap</b>, and 
       <b style='color:#1b9e77'>Gentoo</b> penguins live on islands in Antarctica. 
       Can they be classified based on characteristics and island?") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    plot.title = element_text(size = 22),
    plot.subtitle = element_markdown(size = 18)) 

# Footer & Citation
footer <- ggplot() + 
  labs(title = "", 
       subtitle = "#TidyTuesday | 28 July 2020 | Jenn Schilling | jennschilling.me    ",
       caption = "Source: Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer Archipelago (Antarctica) 
penguin data. R package version 0.1.0. https://allisonhorst.github.io/palmerpenguins/
Artwork by @allison_horst") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    plot.subtitle = element_text(hjust = 1),
    plot.caption = element_text(hjust = 0)) 

#### Put it all together ####

plots <- plot_grid(
  flipper.plot,
  bill.plot,
  loc.plot,
  tree.plot
) 

plot_grid(title, plots, footer, ncol = 1, rel_heights = c(0.15, 1, 0.15)) +
  draw_image(penguins.img,  x = 1, y = 1, hjust = 1, vjust = 1, 
             width = 0.2, height = 0.2)


#### Create slideshow for Instagram ####

# Max. size for square post is 1080x1080px (highest resolution)
# Instagram square posts are 600x600px

# Get more fonts (package - extrafont)
font_import() # note - this will take awhile to run
loadfonts(device = "win")

# Title Slide
insta.title <- ggplot() + 
  geom_richtext(aes(x = 5, 
                    y = 8, 
                    label = "<b style='font-size:30px; font-family:Candara'>
                    #TidyTuesday <br> 28 July 2020 <br> Classifying Palmer Penguins</b>",
                    label.color = NA)) +
  
  geom_richtext(aes(x = 5, 
                    y = 6, 
                    label = "<b style='font-size:25px; font-family:Candara'>
                    <b style='color:#d95f02'>Adélie</b>, 
                    <b style='color:#7570b3'>Chinstrap</b>, and 
                    <b style='color:#1b9e77'>Gentoo</b> penguins<br> 
                    live on islands in Antarctica. <br>
                    Can they be classified based on<br> characteristics and island?</b>",
                    label.color = NA)) +
  xlim(0, 10) +
  ylim(0, 10) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    # Remove x-axis
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
  ) +   
  draw_image(penguins.img,  x = 1.5, y = -1, 
                 width = 7, height = 7)

ggsave("2020-07-28\\title.png", 
       insta.title, 
       width = 6, height = 6, dpi = 100)

# Need to add legends back into the plots since they will be shown on 
# individual slides instead of all together

flipper.plot <- ggplot(penguins.clean) +
  geom_density(aes(x = flipper_length_mm, fill = species),
               alpha = 0.5, 
               color = NA) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Distribution of Flipper Lengths (mm)",
       subtitle = "Gentoo penguins have longer flippers.",
       x = "") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# plot bill length by penguin species
bill.plot <- ggplot(penguins.clean) +
  geom_density(aes(x = bill_length_mm, fill = species), 
               alpha = 0.5, 
               color = NA) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Distribution of Bill Lengths (mm)",
       subtitle = "Adélie penguins have shorter bills.",
       x = "") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) 

# Plot locations 
loc.plot <- ggplot(penguins.clean, aes(x = island, fill = species)) +
  geom_bar(position = "dodge", alpha = 0.5) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) +
  labs(title = "Island Location",
       subtitle = "Gentoo penguins are only found on Biscoe. Chinstrap penguins are only found on Dream.",
       x = "",
       y = "Number of Penguins") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    legend.title = element_blank()
  ) 

# Flipper Slide
ggsave("2020-07-28\\flippersize.png", 
       flipper.plot, 
       width = 6, height = 6, dpi = 100)

# Bill Slide
ggsave("2020-07-28\\billsize.png", 
       bill.plot, 
       width = 6, height = 6, dpi = 100)

# Location Slide
ggsave("2020-07-28\\location.png", 
       loc.plot, 
       width = 6, height = 6, dpi = 100)

# Need to make subtitle two lines for instagram slide
tree.plot <- ggplot() +
  geom_segment(data = segments, aes(x, y, xend = xend, yend = yend)) +
  geom_label(data = nodes, aes(x, y, label = name)) +
  geom_label(data = leaves, 
             aes(x, y, label = name, fill = name), 
             alpha = 1,
             color = "white",
             show.legend = FALSE) +
  scale_fill_manual(values = c("#d95f02", "#7570b3", "#1b9e77")) + 
  geom_text(data = linelabels, aes(x, y, label = name)) +
  xlim(1, 8) +
  ylim(1, 8) +
  labs(title = "Classification of Penguin Species",
       subtitle = "Based on flipper length, bill length, and island, penguins 
can be classified into species using a decision tree.") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    # Remove x-axis
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
  ) 


# Tree Slide
ggsave("2020-07-28\\tree.png", 
       tree.plot, 
       width = 6, height = 6, dpi = 100)

# Ending Slide

insta.end <- ggplot() + 
  geom_richtext(aes(x = 5, 
                    y = 9, 
                    label = "<b style='font-size:30px; font-family:Candara'>
                    #TidyTuesday | 28 July 2020 
                    <br><br> Jenn Schilling | jennschilling.me",
                    label.color = NA)) +
  
  geom_richtext(aes(x = 5, 
                    y = 6, 
                    label = "<p style='font-size:20px; font-family:Candara'>
                    <b>Source:</b> Horst AM, Hill AP, Gorman KB (2020). <br>
                    palmerpenguins: Palmer Archipelago (Antarctica) penguin data.<br>
                    R package version 0.1.0. <br>
                    https://allisonhorst.github.io/palmerpenguins/<br>
                    Artwork by @allison_horst</p>",
                    label.color = NA)) +
  xlim(0, 10) +
  ylim(0, 10) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Remove y-axis
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    # Remove x-axis
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
  ) +   
  draw_image(penguins.img,  x = 2.5, y = 0, 
             width = 5, height = 5)

ggsave("2020-07-28\\end.png", 
       insta.end, 
       width = 6, height = 6, dpi = 100)
