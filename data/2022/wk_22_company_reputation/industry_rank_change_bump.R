library(tidytuesdayR)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-05-31')

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

industry_rank_2022 <- poll %>% 
  select(company, industry, rq_2022) %>%
  distinct() %>% 
  mutate(year = 2022) %>% 
  select(industry, year, rq_2022) %>% 
  group_by(industry, year) %>% 
  summarise(mean_rq = mean(rq_2022))

industry_rankings <- rbind (rank_by_industy, industry_rank_2022)

industry_rankings <- industry_rankings %>% 
  group_by(year) %>% 
  arrange(year, desc(mean_rq), industry) %>% 
  mutate(rank = row_number())


# Bump chart
industry_rankings %>% 
  ggplot(aes(x = as.numeric(year), y = rank, color = industry)) +
  geom_bump(size = 1.5, smooth = 10) +
  geom_point(size = 3) +
  geom_text(data = industry_rankings %>% filter(year == 2017),
            aes(x = year - 0.1, label = industry),
            size = 4, hjust = 1) +
  geom_text(data = industry_rankings %>% filter(year == 2022),
            aes(x = year + 0.1, label = industry),
            size = 4, hjust = 0) +
  scale_x_continuous(breaks = c(2017:2022), 
                     limits = c(2015.75,2023.25)) +
  scale_y_reverse(breaks = c(1:19)) +
  scale_color_manual(values = as.vector(alphabet(n = 19))) +
  theme_minimal_grid() +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Industry RQ Rankings", "2017 to 2022")

