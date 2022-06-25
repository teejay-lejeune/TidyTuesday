library(tidytuesdayR)
library(tidyverse)
remotes::install_github("vladmedenica/themedubois")
library(themedubois)
library(showtext)
library(scales)

tuesdata <- tidytuesdayR::tt_load(2020, week = 25)

blackpast <- tuesdata$blackpast
census <- tuesdata$census
slave_routes <- tuesdata$slave_routes
african_names <- tuesdata$african_names


slave_cumsum<-slave_routes %>% 
  select(year_arrival, n_slaves_arrived) %>% 
  filter(is.na(n_slaves_arrived) == FALSE) %>% 
  group_by(year_arrival) %>% 
  summarise(n_slaves_year  = sum(n_slaves_arrived)) %>% 
  ungroup() %>% 
  mutate(n_slaves_total = cumsum(n_slaves_year))

showtext_auto()

important_years <- c(1520, 1776, 1830, 1861)

ggplot(slave_cumsum) +
  geom_line(aes(x = year_arrival, y = n_slaves_total), size = 2) +
  geom_point(data = filter(slave_cumsum, year_arrival %in% important_years), 
             aes(x = year_arrival, y = n_slaves_total), color = "red", size = 4) +
  geom_segment(x = 1540, xend = 1523, 
               y = 750000, yend = 100000,
               lineend = "round", linejoin = "round", 
               arrow = arrow(length = unit(.25, "cm"))) +
  geom_segment(x = 1750, xend = 1773, 
               y = 2500000, yend = 2100000,
               lineend = "round", linejoin = "round", 
               arrow = arrow(length = unit(.25, "cm"))) +
  geom_rect(xmin = 1782, xmax = 1830,
            ymin = 2000000, ymax = 4500000,
            fill = "#ffd700", alpha = .005, color = "black") +
  geom_segment(x = 1855, xend = 1833, 
             y = 3800000, yend = 4300000,
             lineend = "round", linejoin = "round", 
             arrow = arrow(length = unit(.25, "cm"))) +
  geom_segment(x = 1837, xend = 1857, 
               y = 5500000, yend = 5100000,
               lineend = "round", linejoin = "round", 
               arrow = arrow(length = unit(.25, "cm"))) +
  annotate("text", x = 1550, y = 1200000, label = "Earliest recorded\n slave trade route", 
           family = "Russo One", size = 3) +
  annotate("text", x = 1750, y = 2800000, label = "USA gains it's\n independence", 
           family = "Russo One", size = 3) +
  annotate("text", x = 1850, y = 3500000, label = "Abolitionist\n movement\n starts",
           family = "Russo One", size = 3) +
  annotate("text", x = 1755, y = 4300000, label = "Rapid increase\n of slave trade", 
           family = "Russo One", size = 3) +
  annotate("text", x = 1810, y = 5500000, label = "Beginning of the\n American Civil War", 
           family = "Russo One", size = 3) +
  ggtitle("TOTAL NUMBER OF SLAVES TRADED.", "1514-1865")+
  labs(x = "YEAR", y = "NUMBER OF SLAVES") +
  theme_dubois() +
  theme(text = element_text(family = "Russo One")) +
  scale_y_continuous(limits = c(0, 5500000), breaks = seq(0, 5500000, 1000000),
                     labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_continuous(limits = c(1520, 1875), breaks = seq(1500, 1850, 50))
