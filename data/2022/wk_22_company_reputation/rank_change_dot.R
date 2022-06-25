library(tidytuesdayR)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-05-31')

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')


poll_company_rank <- poll %>% 
  select(!c(year:rq)) %>% 
  distinct() %>% 
  mutate(change = ifelse(is.na(poll_company_rank$change), 
                         101-poll_company_rank$`2022_rank`, 
                         poll_company_rank$change),
         color = change >= 0) %>% 
  arrange(desc(change)) %>% 
  filter(row_number() > max(row_number()) - 10 | row_number() <= 10)


# Cleveland Dot Plot, same data, different style
poll_company_rank %>% 
  ggplot(aes(x = change, y = reorder(company,change))) +
  geom_point(aes(color = color), size = 7) +
  geom_segment(aes(yend = reorder(company,change), xend = 0, color = color),
               size = 3) +
  geom_text(aes(label = change), size = 3.25) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none") +
  labs(x = "Rank Change", y = "Company") +
  ggtitle("Rank Change From 2021 to 2022", 
          "Top/Bottom 10 gainers and losers")
