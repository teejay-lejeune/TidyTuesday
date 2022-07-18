library(tidyverse)
library(lubridate)
library(ggalt)
library(viridis)
library(scales)
library(showtext)
library(ggrepel)


flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

names(flights) <- tolower(names(flights))
flights$flt_date <- date(flights$flt_date)

flights_per_month <- flights %>%
  mutate(month = ym(paste(flights$year, flights$month_num, sep = "-"))) %>% 
  group_by(month) %>% 
  summarise(tot_flights = sum(flt_tot_1))

ggplot(flights_per_month, aes(x = month, y = tot_flights)) +
  geom_line()

pop_airports <- flights %>% 
  group_by(apt_name) %>% 
  summarise(total_flights = sum(flt_tot_1)) %>% 
  arrange(desc(total_flights)) %>% 
  head(10)

flights_by_airport <- flights %>% 
  filter(apt_name %in% pop_airports$apt_name) %>% 
  mutate(month = ym(paste(year, month_num, sep = "-"))) %>% 
  group_by(apt_name, month) %>% 
  summarise(total_flights = sum(flt_tot_1)) %>% 
  ungroup() 

font_add("Frutiger", 'C:/Tidy Tuesday/Fonts/frutiger-fonts/frutiger fonts/Frutiger.ttf')
showtext.auto()

ggplot(flights_by_airport, aes(x = month, y = total_flights,
                               color = fct_reorder2(apt_name, month, total_flights))) +
  geom_xspline(spline_shape = 1, size = .75) +
  geom_vline(xintercept = as.numeric(date('2020-03-01')),
             size = 1,
             linetype = 2) +
  annotate("text", x = date("2020-04-01"), 
           y = 45000, 
           label = "Covid-19 Restrictions Enforced", 
           family = "Frutiger",
           size = 5,
           hjust = 0) +
  geom_label_repel(data = flights_by_airport %>% filter(month == "2022-05-01"), 
                   aes(label = apt_name, family = "Frutiger"),
                   xlim = c(date("2022-06-01"), NA),
                   segment.size = 1,
                   segment.curvature = -.1,
                   segment.ncp = 10) +
  coord_cartesian(clip = "off") +
  scale_color_viridis(discrete = TRUE) +
  scale_x_continuous(limits = c(as.numeric(date('2016-01-01')), as.numeric(date('2022-06-01'))),
                     breaks = c(seq(as.numeric(date('2016-01-01')), as.numeric(date('2022-01-01')), 365.33)),
                     labels = c(seq(2016, 2022, 1)),
                     expand = c(0,10)) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = .001)) +
  labs(title = "Top 10 European Airports",
       subtitle = "by monthly number of flights   |   2016-2022",
       x = "Year", 
       y = "Flights",
       caption = "TJ LeJeune   |   TidyTuesday Week 28   |   Source: https://ansperformance.eu/data/")+
  theme_classic() +
  theme(text = element_text(family = "Frutiger"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.margin = margin(30, 200, 30, 60),
        legend.position = "none") 


