library(tidyvers)
library(countrycode)
library(reactablefmtr)

technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')
labels <- technology %>% distinct(variable, label)

# Get country names using 'countrycode' package
technology <- technology %>% 
  filter(iso3c != "XCD") %>% 
  mutate(iso3c = recode(iso3c, "ROM" = "ROU"),
         country = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
         country = case_when(
           iso3c == "ANT" ~ "Netherlands Antilles",
           iso3c == "CSK" ~ "Czechoslovakia",
           iso3c == "XKX" ~ "Kosovo",
           TRUE           ~ country))

elec <- labels$variable[46:56]

electricity <- technology %>% 
  filter(variable %in% elec)

pct_elec <- electricity %>% 
  select(variable, year, value, country) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-elec_cons, -electric_gen_capacity) %>% 
  filter(elecprod > 100) %>% 
  mutate(pct_coal        = round(elec_coal/elecprod, 4),
         pct_gas         = round(elec_gas/elecprod, 4),
         pct_nuc         = round(elec_nuc/elecprod, 4),
         pct_oil         = round(elec_oil/elecprod, 4),
         pct_hydro       = round(elec_hydro/elecprod, 4),
         pct_solar       = round(elec_solar/elecprod, 4),
         pct_wind        = round(elec_wind/elecprod, 4),
         pct_renew_other = round(elec_renew_other/elecprod, 4),
         pct_renew       = pct_hydro + pct_solar + pct_wind + pct_renew_other)

pct_elec_20 <- pct_elec %>% 
  filter(year == 2020)

pct_elec_15 <- pct_elec %>% 
  filter(year == 2015) %>% 
  select(country, pct_renew)

elec_table <- left_join(pct_elec_20, 
                        pct_elec_15, 
                        by = "country",
                        suffix = c("_2020", "_2015")) %>% 
  mutate(across(starts_with("elec"), round),
         pct_change = (pct_renew_2020 - pct_renew_2015)*100) %>% 
  select(2,11:22)


#####################
#### BUILD TABLE ####
#####################

table <- elec_table %>% 
  mutate(pct_change_col = case_when(pct_change >= 0 ~ "darkgreen",
                                    TRUE ~ "red")) %>% 
  reactable(
            theme = fivethirtyeight(centered = TRUE),
            pagination = FALSE,
            highlight = TRUE,
            defaultSorted = "pct_renew_2020",
            defaultSortOrder = "desc",
            columnGroups = list(
              colGroup(name = "Non-Renewable Energy",
                       columns = c("pct_coal", "pct_gas", "pct_nuc", "pct_oil")),
              colGroup(name = "Renewable Energy",
                       columns = c("pct_hydro", "pct_solar", "pct_wind", "pct_renew_other"))
              ),
            columns = list(
              country = colDef(
                minWidth = 110,
                style = list(borderRight = "1px solid #777")
              ),
              elecprod = colDef(
                name = "Total Energy Output (TWh)",
                minWidth = 200,
                align = "left",
                style = list(borderRight = "1px solid #777"),
                cell = data_bars(., max_value = 1000, text_position = "center", 
                                 fill_color = viridis(5), bias = 6, round_edges = TRUE,
                                 force_outside = c(0, 200), box_shadow = TRUE),
                footer = "All countries with > 100 TWh of energy output"
              ),
              pct_coal = colDef(
                name = "Coal (% of Total)",
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10))
              ),
              pct_gas = colDef(
                name = "Gas",
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10))
              ),
              pct_nuc = colDef(
                name = "Nuclear",
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10))
              ),
              pct_oil = colDef(
                name = "Oil",
                format = colFormat(percent = TRUE),
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10)),
                style = list(borderRight = "1px dashed #777")
              ),
              pct_hydro = colDef(
                name = "Hydro",
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10))
              ),
              pct_solar = colDef(
                name = "Solar",
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10))
              ),
              pct_wind = colDef(
                name = "Wind",
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10))
              ),
              pct_renew_other = colDef(
                name = "Other Renewable",
                cell = icon_sets(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(10)),
                style = list(borderRight = "1px solid #777")
              ),
              pct_renew_2020 = colDef(
                name = "% Renewable in 2020",
                minWidth = 115,
                align = "left",
                cell = color_tiles(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(5),bias = 1.5, box_shadow = TRUE),
                style = list(borderRight = "1px dashed #777")
              ),
              pct_renew_2015 = colDef(
                name = "% Renewable in 2015",
                align = "left",
                minWidth = 110,
                cell = color_tiles(., number_fmt = label_percent(accuracy = 0.01), colors = viridis(5),bias = 1.5, box_shadow = TRUE),
                style = list(borderRight = "1px solid #777")
              ),
              pct_change = colDef(
                name = "% Change (2015 to 2020)",
                cell = pill_buttons(., number_fmt = function(value) paste0(sprintf("%+.2f", value), "%"), colors = "transparent", opacity = 0, bold_text = TRUE, text_color_ref = "pct_change_col")
              ),
              pct_change_col = colDef(show = FALSE)
              
            )
            
  ) %>%
  google_font(font_family = "Chakra Petch", font_weight = 300) %>% 
  add_title(title = "Which Country Uses The Most Renewable Energy?") %>% 
  add_subtitle(subtitle = "by % of total energy output") %>% 
  add_source(source = "TJ LeJeune   |   TidyTuesday Week 29   |   Source: data.nber.org")

  save_reactable_test(table, "tech_table.html")
  save_reactable_test(table, "tech_table_pic.png")

