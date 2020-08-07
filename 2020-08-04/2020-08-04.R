# Author : Jenn Schilling
# Title: #TidyTuesday European Energy
# Date: 8/4/2020


#### Libraries ####
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(extrafont)
library(cowplot)
library(ggrepel)
library(ggtext)

# Load  fonts
loadfonts(device = "win", quiet = TRUE)


#### Get the Data ####
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals


#### Explore the Data ####
skim(energy_types)

# Country Name is Missing for 8 rows
energy_types %>% filter(is.na(country_name))

# Fix UK Country Name
energy_types <- energy_types %>%
  mutate(country_name = ifelse(country == 'UK', 
                               'United Kingdom',
                               country_name))

skim(country_totals)

# Country Name is Missing for 8 rows
country_totals %>% filter(is.na(country_name))

# Fix UK Country Name
country_totals <- country_totals %>%
  mutate(country_name = ifelse(country == 'UK', 
                               'United Kingdom',
                               country_name))

# 2016 is missing one value
country_totals %>% filter(is.na(`2016`)) # Malta has Exports of NA in 2016

# Pumped hydro (level 2 measure) is a sub measure of hydro (level 1 measure), 
# if you include it in sum it's like double counting.
# Remove Level 2 from Energy Types
energy_types <- energy_types %>%
  filter(level != 'Level 2')

# Make Data Long
energy_types_long <- energy_types %>%
  pivot_longer(cols = `2016`:`2018`, names_to = 'year', values_to = 'gwh')

country_totals_long <- country_totals %>%
  pivot_longer(cols = `2016`:`2018`, names_to = 'year', values_to = 'gwh')


# Graph imports and exports by country and year
ggplot(country_totals_long %>% 
         filter(type == 'Exports' | type == 'Imports') %>%
         mutate(gwh = ifelse(type == 'Exports', -gwh, gwh)) %>%
         arrange(gwh)) +
  geom_col(aes(x = country, y = gwh, fill = type)) +
  facet_grid(rows = vars(year)) +
  theme_classic()

#### Analyze + Graph the Data ####

# Get list of types for country data
country_totals_long %>% 
  select(type) %>%
  unique(.)

# Total Net Production, Imports, Exports, Energy Absorbed by Pumping, Energy Supplied

# Get list of types for energy data
energy_types_long %>%
  select(type) %>%
  unique(.)

# Conventional Thermal, Nuclear, Hydro, Wind, Solar, Geothermal, Other

# I'm interested in renewable energy, so I'm going to go with energy_types data

# Renewable Sources: sun, wind, water, geothermal
# NonRenewable Sources: conventional thermal (fossil fuels), nuclear
# Leave out other

# Determine Totals
# Note - Total Net Production in county_totals is also the sum of all energy_types
energy_types_summary <- energy_types_long %>%
  select(-level) %>%
  mutate(type = ifelse(type == 'Conventional thermal',
                       'conventional_thermal', type),
         type = tolower(type)) %>%
  pivot_wider(names_from = type, values_from = gwh) %>%
  mutate(total = rowSums(select(.,c('conventional_thermal', 'nuclear', 
                                  'hydro', 'wind', 'solar', 
                                  'geothermal', 'other')))) %>%
  group_by(year) %>%
  mutate(rank_total = dense_rank(desc(total))) %>%
  ungroup() %>%
  pivot_longer(cols = conventional_thermal:other, 
               names_to = 'type', 
               values_to = 'gwh') %>%
  mutate(percent_total = gwh / total)

# Get the top 10 producers in each year
energy_types_top10 <- energy_types_summary %>%
  filter(rank_total <= 10)

# Look at results
energy_types_top10 %>% 
  select(year, country_name, rank_total) %>% 
  unique(.) %>%
  pivot_wider(names_from = year, values_from = rank_total) %>%
  arrange(`2018`)

rankings_top10 <- energy_types_top10 %>% 
  mutate(label = paste(country_name, 
                       paste(sprintf("%.0fK", total/1000), 
                             "GWh", sep = " "),
                       sep = "\n")) %>%
  select(year, country_name, rank_total, label) %>% 
  unique(.) %>%
  mutate(rank_label = ifelse(rank_total == 1, '  1st',
                      ifelse(rank_total == 2, ' 2nd',
                      ifelse(rank_total == 3, '  3rd',
                      ifelse(rank_total == 10, '10th',
                        paste0('  ', rank_total, 'th'))))))

# Make a cool line plot to show change in rankings over  the years
top10_plot <- ggplot(rankings_top10) +
  # Background line to trace rank
  geom_line(aes(x = year, y = rank_total, 
                group = rank_total),
            # show.legend = FALSE,
            size = 0.5,
            linetype = "dashed",
            color = "grey") +
  # Line for rank of each country
  geom_line(aes(x = year, y = rank_total, 
                group = country_name, color = country_name),
            show.legend = FALSE,
            size = 1) +
  # Points for rank of each country
  geom_point(aes(x = year, y = rank_total, 
                group = country_name, color = country_name),
            show.legend = FALSE,
            size = 2) +
  # End of line labels
  geom_text(data = filter(rankings_top10, year == 2018),
            aes(x = year, y = rank_total, 
                group = country_name, color = country_name,
                label = label), 
            hjust = -0.2,
            show.legend = FALSE,
            family = "Candara",
            size = 3.5) +
  # Beginning of line labels
  geom_text(data = filter(rankings_top10, year == 2016),
            aes(x = year, y = rank_total, 
                group = country_name,
                # label = ifelse(rank_total == 10, rank_total, 
                #                paste(" ", rank_total))
                label = rank_label), 
            hjust = 3,
            show.legend = FALSE,
            family = "Candara",
            size = 3.5)  +
  scale_y_reverse() +
  labs(x = "",
       y = "",
       title = "Country ranking by total energy production",
       subtitle = "Label: 2018 total energy production in gigawatt hours") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    # Customize Font
    text = element_text(family = "Candara", size = 12)#,
    # Shift title
    # plot.title = element_text(hjust = -0.05),
    # plot.subtitle = element_text(hjust = -0.055)
  )

top10_plot

# % Renewable energy by the Top 10 
energy_types_top10_renew <- energy_types_top10 %>%
  mutate(renewable = ifelse(type == 'solar' |
                              type == 'hydro' |
                              type == 'geothermal' |
                              type == 'wind', TRUE, FALSE)) %>%
  group_by(year, country_name, rank_total, total, renewable) %>%
  summarise(percent_renew = sum(percent_total)) %>%
  ungroup() %>%
  filter(renewable == TRUE) %>%
  select(-renewable) %>%
  mutate(year = as.numeric(year))

# Create color order to match line plot
country_order <- energy_types_top10 %>%
  filter(year == 2018) %>%
  mutate(country_order = rank_total) %>%
  select(country_name, country_order) %>%
  unique(.)

energy_types_top10_renew <- left_join(energy_types_top10_renew,
                                      country_order,
                                      by = 'country_name')

renew_plot <- ggplot(energy_types_top10_renew) +
  geom_col(aes(x = year, y = percent_renew,
               fill = country_name),
           show.legend = FALSE) +
  geom_text(aes(x = year, y = percent_renew, 
                label = paste(sprintf("%.0f", percent_renew*100), 
                              "", sep = "%")),
            vjust = -0.15,
            family = "Candara",
            size = 3) +
  facet_wrap(. ~ reorder(country_name, country_order), 
             nrow = 5,
             scales = "free") +
  scale_y_continuous(limits = c(0, 1.15), 
                     labels = scales::percent, 
                     n.breaks = 3) +
  scale_x_continuous(n.breaks = 3) +
  labs(
    x = "",
    y = "",
    title = "% of Total Energy Production that is Renewable"
  ) +
  theme(
    # Remove background
    panel.background = element_blank(),
    # Remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Format y-axis line
    #axis.line.y = element_line(color = "grey"),
    #axis.ticks.y = element_line(color = "grey"),
    # Remove y-axis line
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    # Remove x-axis line
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    # Customize facet label appearance
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # Customize Font
    #axis.text.y = element_text(family = "Candara", size = 8),
    axis.text.x = element_text(family = "Candara", size = 10),
    text = element_text(family = "Candara", size = 10),
    # Shift title
    plot.title = element_text(hjust = -0.1)
  )

renew_plot

# What about a different renewable energy plot
renew_plot2 <- ggplot(energy_types_top10_renew, 
       aes(x = year, y = percent_renew,
           label = country_name
           #label = paste(sprintf("%.0f", percent_renew*100), "", sep = "%")
           )) +
  geom_jitter(aes(color = country_name),
              #width = 0.10,
              show.legend = FALSE,
              size = 3,
              position = position_jitter(seed = 1)) +
  # geom_text_repel(position = position_jitter(seed = 1),
  #           vjust = -0.1,
  #           family = "Candara",
  #           size = 3) +
  geom_text_repel(position = position_jitter(seed = 1),
            vjust = -0.1,
            family = "Candara",
            size = 3) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent, 
                     n.breaks = 5) +
  scale_x_continuous(breaks = c(2016, 2017, 2018),
                     labels = c("2016", "2017", "2018")) +
  geom_vline(aes(xintercept = 2016.5), color = "grey") +
  geom_vline(aes(xintercept = 2017.5), color = "grey") +
  geom_hline(aes(yintercept = 0.5),
             color = "gray",
             linetype = "dashed") +
  labs(
    x = "",
    y = "",
    title = "Percent of total energy production that is renewable",
    subtitle = ""
  ) +
  theme(
    # Remove background
    panel.background = element_blank(),
    # Remove gridlines
    panel.grid.major.y = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    # Format y-axis line
    axis.line.y = element_blank(),
    axis.ticks.y = element_line(color = "grey"),
    # Remove x-axis line
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    # Customize facet label appearance
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    # Customize Font
    axis.text.y = element_text(family = "Candara", size = 12),
    axis.text.x = element_text(family = "Candara", size = 12),
    text = element_text(family = "Candara", size = 12)#,
    # Shift title
    # plot.title = element_text(hjust = -0.2)
  )

renew_plot2

#### Put Plots Together ####

plots <- plot_grid(top10_plot, renew_plot2)

# Title & Subtitle
title <- ggplot() + 
  labs(title = "European Energy Production 2016-2018",
       subtitle = "Analysis by top ten energy producers in Europe") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    text = element_text(family = "Candara", size = 14)) 

# Footer & Citation
footer <- ggplot() + 
  labs(title = "", 
       subtitle = "#TidyTuesday | 4 August 2020 | Designer: Jenn Schilling | Data: Eurostat") +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    plot.subtitle = element_text(hjust = 1),
    plot.caption = element_text(hjust = 0),
    text = element_text(family = "Candara", size = 10))

combo_plot <- plot_grid(title, plots, footer, ncol = 1, 
                        rel_heights = c(0.10, 1, 0.10))

ggsave("2020-08-04\\top10_european_energy.png", 
       combo_plot, 
       width = 11, height = 8, dpi = 100)                     


#### For Instagram ####

# Title Slide
insta_title <- ggplot() + 
  geom_richtext(aes(x = 5, 
                    y = 7, 
                    label = "<b style='font-size:30px; font-family:Candara'>
                    #TidyTuesday <br> 04 August 2020 <br> 
                    European Energy Production</b>",
                    label.color = NA)) +
  
  geom_richtext(aes(x = 5, 
                    y = 5, 
                    label = "<b style='font-size:25px; font-family:Candara'>
                    Investigating the top ten energy producers in<br> Europe 
                    and their production of renewable energy.</b>",
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
  ) 

ggsave("2020-08-04\\title.png", 
       insta_title, 
       width = 6, height = 6, dpi = 100)

# Ending Slide
insta_end <- ggplot() + 
  geom_richtext(aes(x = 5, 
                    y = 6, 
                    label = "<b style='font-size:30px; font-family:Candara'>
                    #TidyTuesday | 04 August 2020 
                    <br><br> Jenn Schilling | jennschilling.me",
                    label.color = NA)) +
  
  geom_richtext(aes(x = 5, 
                    y = 4, 
                    label = "<p style='font-size:20px; font-family:Candara'>
                    <b>Source:</b> Eurostat",
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
  )

ggsave("2020-08-04\\end.png", 
       insta_end, 
       width = 6, height = 6, dpi = 100)

ggsave("2020-08-04\\top10plot.png", 
       top10_plot, 
       width = 7, height = 7, dpi = 100)

ggsave("2020-08-04\\renewplot.png", 
       renew_plot2, 
       width = 7, height = 7, dpi = 100)


