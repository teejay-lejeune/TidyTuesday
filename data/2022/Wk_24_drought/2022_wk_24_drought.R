library(tidycensus)
library(tigris)
library(ggplot2)
library(dplyr)
library(zoo)
library(showtext)
library(gganimate)
library(transformr)
library(rcartocolor)
library(gifski)

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

names(drought_fips) <- tolower(names(drought_fips))

join_drought_fips <- drought_fips %>% 
  filter(state != 'AK', state != 'HI', state != 'PR') %>% 
  mutate(state_code = str_sub(fips, 1, 2), 
         county_code = str_sub(fips, 3),
         date = as.yearqtr(date)) %>%  
  inner_join(fips_codes, by = c("state", "state_code", "county_code")) %>% 
  select(-c(state_code, county_code)) %>% 
  group_by(fips, date, state, state_name, county) %>% 
  summarise(mean_dsci = round(mean(dsci)))

counties <- county_laea
counties <- counties %>% mutate(GEOID = recode(GEOID, "46113" = "46102"))

drought_fips_sf <- inner_join(counties, join_drought_fips, by = c("GEOID" = "fips"))

font_add_google("Zilla Slab", family = "zilla")
showtext_auto()

drought_map <- ggplot(drougt_fips_sf, aes(fill = mean_dsci)) +
  geom_sf() +
  scale_fill_carto_c(palette = "SunsetDark", name = "Drought Level",
                     limits = c(0, 500), breaks = c(seq(0,500, 500/4)),
                     labels = c("Dry", "Moderate", "Severe", 
                               "Extreme", "Exceptional")) +
  labs(title = "Drought Levels Across the USA", 
       subtitle = "{current_frame}",
       caption = "TJ LeJeune   |   TidyTuesday Week 24   |   Source: www.drought.gov") +
  theme( panel.grid = element_blank(),
         panel.background = element_blank(),
         plot.background = element_rect(colour = "honeydew", fill = "honeydew"),
         plot.title = element_text(size = 20),
         plot.subtitle = element_text(size = 12),
         plot.margin = margin(30,30,30,30),
         text = element_text(family = "zilla"),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         legend.background = element_blank(),
         legend.title = element_text(vjust = 5))+
  guides(fill = guide_colorbar(ticks = FALSE)) +
  transition_manual(date)

map_GIF <- animate(drought_map, height = 700, width = 700, fps = 30, duration = 25,
                   end_pause = 5, renderer = gifski_renderer())

anim_save("dought_map.gif", map_GIF)

post_tweet("My first submission to #tidytuesday. Animating droughts in the US since 2000.")

