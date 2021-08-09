library(tidyverse)
library(viridis)
library(leaflet)
library(rgdal)
library(formattable)


# Directory paths
data_dir <- file.path('.', 'data')


# ----------
# Prep data
# ----------

# Load data
load(file = file.path(data_dir, '224545_data.RData'))
orders_df <- orders_df_224545
lists_df <- lists_df_224545

# Load zip data
zip_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_to_state.csv')) %>% 
  mutate(pop_poc_pct = (pop_black + pop_hispanic + pop_amerindian) / pop_total * 100)
zip_shp <- readOGR(file.path(data_dir, 'cb_2018_us_zcta510_500k', 'cb_2018_us_zcta510_500k.shp'))


# ----------------------------
# Investigate order summaries
# ----------------------------

# Used to title Houston searches as Secondary
orders_df %>% 
  filter(zip_code == '773|770|774|775') %>% 
  select(date_start, order_title, market, zip_code) %>% 
  arrange(date_start, market) %>% 
  View()

# Secondary market varies a little, could be due to hs_grad_class searched too
orders_df %>% 
  select(market, date_start, county, zip_code) %>% 
  distinct() %>% 
  arrange(market, date_start) %>% 
  View()

orders_df %>%
  select(date_start, order_title, market, hs_grad_class, zip_code, sat_score_min, sat_score_max, psat_score_min, psat_score_max, gpa_high, gpa_low) %>%
  arrange(date_start, market, hs_grad_class) %>% 
  View()

# Cost per student went from $0.43 to $0.45 (starting Fall 2018) to $0.47 (starting Fall 2019)
orders_rate_df <- orders_df %>% select(order_num, date_start, order_cost, num_students) %>% mutate(rate = round(order_cost / num_students, 2)) %>% arrange(rate, date_start)
View(orders_rate_df)

orders_rate_df %>%
  ggplot(aes(x = as.character(order_num), y = num_students, fill = as.character(date_start))) + 
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_viridis(discrete = T) +
  labs(x = '') +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  facet_grid(~rate, scales = 'free_x', space = 'free_x')

orders_rate_df %>%
  ggplot(aes(x = as.character(order_num), y = order_cost, fill = as.character(date_start))) + 
  geom_bar(position = 'stack', stat = 'identity') +
  scale_fill_viridis(discrete = T) +
  labs(x = '') +
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  ) +
  facet_grid(~rate, scales = 'free_x', space = 'free_x')


# ------------------------
# Map student-level lists
# ------------------------

# Create var for race breaks
zip_data$race_brks_nonwhiteasian <- cut(zip_data$pop_poc_pct, 
                                        breaks = c(-1, 20, 40, 60, 80, 90, 101), 
                                        labels = c('0-19%', '20-39%', '40-59%', 
                                                   '60-79%', '80-89%', '90-100%'))

# Create var for income breaks
zip_data$inc_brks <- cut(zip_data$median_household_income, 
                         breaks = c(-1, 50000, 75000, 100000, 150000, 200000, 10000000), 
                         labels = c('<$50k', '$50k-74k', '$75k-99k', 
                                    '$100k-149k', '$150k-199k', '$200k+'))

# Create var for first 3 digits of zip code
zip_data$zip_code_3 <- zip_data$zip_code %>% str_sub(end = 3)

# Merge zip data to shapes
zip_shp <- merge(zip_shp, zip_data, by.x = 'ZCTA5CE10', by.y = 'zip_code', all.x = T)


# Create shared color scale functions
color_income <- colorFactor('YlGnBu', zip_shp$inc_brks)
color_race <- colorFactor('YlGnBu', zip_shp$race_brks_nonwhiteasian)
color_pop <- colorNumeric('YlGnBu', zip_shp$pop_total, n = 5)


# View different sets of zip codes purchased
zip_purchased <- str_split(unique(orders_df$zip_code) %>% na.omit() %>% as.character(), '\\|')

zip_shp_purchased <- subset(zip_shp, zip_code_3 %in% zip_purchased[[1]])

# Create popups
pop_zip <- paste0('<b>', zip_shp_purchased$zip_name, '</b><br>',
                  'Total Population: ', format(zip_shp_purchased$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)

income_zip <- paste0('<b>', zip_shp_purchased$zip_name, '</b><br>',
                     'Median Household Income: ', currency(zip_shp_purchased$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)

race_zip <- paste0('<b>', zip_shp_purchased$zip_name, '</b><br>',
                   '% Population of Color: ', sprintf('%.1f', zip_shp_purchased$pop_poc_pct)) %>% lapply(htmltools::HTML)

# Plot purchased zip codes
leaflet(data = zip_shp_purchased) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addMiniMap(tiles = providers$CartoDB.Positron,
             toggleDisplay = TRUE) %>% 
  
  addPolygons(stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'gray', group = 'Purchased') %>% 
  addPolygons(stroke = F, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop(pop_total), label = pop_zip, group = 'By Population') %>%
  addPolygons(stroke = F, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = income_zip, group = 'By Median Household Income') %>%
  addPolygons(stroke = F, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks_nonwhiteasian), label = race_zip, group = 'By Race/Ethnicity') %>% 
  
  # add legends
  addLegend(position = 'topright', pal = color_pop, values = ~pop_total,
            title = 'Population',
            className = 'info legend legend-pop',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(position = 'topright', pal = color_income, values = ~inc_brks,
            title = 'Median Household Income',
            className = 'info legend legend-income',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(position = 'topright', pal = color_race, values = ~race_brks_nonwhiteasian,
            title = 'Black, Latinx, and <br>Native American Population',
            className = 'info legend legend-race',
            na.label = 'NA',
            opacity = 1) %>%
  
  # add options
  addLayersControl(
    position = c('bottomleft'),
    baseGroups = c('Purchased', 'By Population', 'By Median Household Income', 'By Race/Ethnicity'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  htmlwidgets::onRender("
        function(el, x) {
            var myMap = this;
            $('.legend').css('display', 'none');
            
            myMap.on('baselayerchange', function(e) {
                $('.legend').css('display', 'none');
                switch(e.name) {
                    case 'By Population':
                        $('.legend-pop').css('display', 'inherit');
                        break;
                    case 'By Median Household Income':
                        $('.legend-income').css('display', 'inherit');
                        break;
                    case 'By Race/Ethnicity':
                        $('.legend-race').css('display', 'inherit');
                        break;
                }
                e.layer.bringToBack();
            });
    }")
