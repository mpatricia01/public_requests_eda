library(leaflet)
library(rgdal)
library(raster)
library(formattable)
library(tidyverse)
library(readxl)
library(lubridate)
library(htmlwidgets)


# Directory paths
data_dir <- file.path('.', 'data')


# ----------
# Prep data
# ----------

# Read in data
lists_df <- read_csv(file.path(data_dir, '145637_lists.csv'), col_types = cols(.default = 'c'))
orders_df <- read_csv(file.path(data_dir, '145637_orders.csv'), col_types = c('univ_id' = 'c', 'order_num' = 'c'))

# Checks
str_detect(lists_df$Source, '^(?:\\w+ \\d+, \\d{4} [SACT]{3} Search \\d+;?\\s*)+$') %>% table()
str_count(lists_df$Source, 'SAT|ACT') %>% sum()  # 465231 matches number of rows in lists_df_pivot

# https://cathblatter.rbind.io/blog/2020/03/16/using-pivot-longer-and-regex-for-data-wrangling/
lists_df_pivot <- lists_df %>% 
  pivot_longer(
    cols = starts_with(c('sat_', 'act_')),
    names_to = c('.value', 'test_num'),
    names_pattern = '(^\\w+)_(\\d+)'
  ) %>%
  select(-test_num) %>% 
  pivot_longer(
    cols = starts_with(c('sat_', 'act_')),
    names_to = c('test_type', '.value'),
    names_sep = '_',
    values_drop_na = T
  ) %>%
  rename(order_num = test, order_date = date) %>% 
  mutate(order_date = mdy(order_date))

# ACT orders
lists_df_act <- lists_df_pivot %>% filter(test_type == 'act')

# Missing order summary for most of lists_df_act (only have 3/22/19 & 6/27/19 order summary, which doesn't match any of the student list dates)
View(lists_df_act %>% select(order_num, order_date) %>% distinct())

# SAT orders
lists_df_sat <- lists_df_pivot %>% filter(test_type == 'sat')

# Missing order summary for 107713 of 415689 rows in lists_df_sat (15 distinct orders)
anti_join(lists_df_sat, orders_df, by = 'order_num') %>% nrow()
View(anti_join(lists_df_sat, orders_df, by = 'order_num') %>% select(order_num, order_date) %>% distinct())

# 3 order summaries w/ no lists entries, but these look like draft orders that weren't actually placed (i.e., 'Edit name')
View(anti_join(orders_df, lists_df_sat, by = 'order_num'))

# Explore remaining matched rows (rows may be duplicates if they belong to multiple orders)
merged_df_sat <- inner_join(lists_df_sat, orders_df, by = 'order_num')

# Categories of orders
remove_NA_cols <- function(data_df) {
  data_df[!sapply(data_df, function(x) all(is.na(x)))]
}

IL_orders <- orders_df %>%
  filter(order_num %in% c('327696', '327699', '327649', '327630', '327700', '327681',
                          '371076', '371072', '371132', '371093', '371077', '371079',
                          '383145', '383135', '374839', '383138', '383146', '383164',
                          '403307', '403329', '403217', '403330', '403288', '403308',
                          '456700', '456677', '456725', '456650', '456681', '456628',
                          '470081', '470138', '470119',
                          '500538', '500551', '500540', '500488', '500501', '500510',
                          '541075', '541047', '541076', '541012', '540968', '540967',
                          '567374', '567426', '567375', '567373', '567440', '567441'
                          )) %>% 
  remove_NA_cols() %>% 
  arrange(date_start, str_extract(race_ethnicity, '^.'), sat_score_min, psat_score_min)
# write_csv(IL_orders, '~/Downloads/IL_orders.csv')

OOS_orders <- orders_df %>% 
  filter(order_num %in% c('371665', '371662', '371629',
                          '386336', '386441', '386335',
                          '403333', '403314', '403340',
                          '456710', '456737',
                          '469731', '470123', '470283',
                          '483724', '483701', '483702',
                          '483751',
                          '500494', '500590',
                          '567377', '567376'
                          )) %>% 
  remove_NA_cols()

intl_orders <- orders_df %>% 
  filter(order_num %in% c('372044', '371669', '470250', '483721')) %>% 
  remove_NA_cols()

# Read in secondary data
zip_cbsa_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_code_cbsa.csv'))
zip_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_to_state.csv')) %>% 
  mutate(pop_poc_pct = (pop_black + pop_hispanic + pop_amerindian) / pop_total * 100) %>% 
  left_join(zip_cbsa_data, by = 'zip_code')
msa_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/msa_metadata.csv'), na = c('', 'NULL')) %>% 
  mutate(pop_poc_pct = pop_black_pct + pop_hispanic_pct + pop_amerindian_pct)
hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c')) %>% 
  mutate(pct_poc = pct_black + pct_hispanic + pct_amerindian)
univ_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/meta_university.csv'), col_types = c('univ_id' = 'c', 'fips_state_code' = 'c', 'fips_county_code' = 'c')) %>% 
  select(-X1)
ceeb_nces <- read_csv(file.path(data_dir, 'ceeb_nces_crosswalk.csv'))
cds_nces <- read_csv(file.path(data_dir, 'cds_nces_crosswalk.csv')) %>% 
  mutate(ncessch = str_c(NCESDist, NCESSchool)) %>% 
  select(ncessch, CDSCode)
df_sat_ca_20 <- read_excel(file.path(data_dir, 'sat20.xlsx'), na = c('N/A', '*'))
df_sat_ca_19 <- read_excel(file.path(data_dir, 'sat19.xlsx'), na = c('N/A', '*'), skip = 5)

# Load shape files: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
cbsa_shp <- readOGR(file.path(data_dir, 'cb_2018_us_cbsa_500k', 'cb_2018_us_cbsa_500k.shp'))
state_shp <- readOGR(file.path(data_dir, 'cb_2018_us_state_500k', 'cb_2018_us_state_500k.shp'))
zip_shp <- readOGR(file.path(data_dir, 'cb_2018_us_zcta510_500k', 'cb_2018_us_zcta510_500k.shp'))

save(IL_orders, OOS_orders, OOS_eng_orders, OOS_noneng_orders, intl_orders,
     lists_df_pivot, lists_df_sat, lists_df_act, df_sat_ca_20, df_sat_ca_19,
     file = file.path(data_dir, '145637_data.RData'))

# ----------
# IL orders
# ----------

# Usually filter for full IL state, except sometimes only these 12 MSA's within IL
IL_msa <- (IL_orders$cbsa_name %>% na.omit() %>% unique() %>% str_match_all('IL - ([^|]+)'))[[1]][, 2]
IL_msa_df <- msa_data %>% filter(cbsa_title %in% IL_msa)

IL_state <- '17'

# Create var for race breaks
msa_data$race_brks_nonwhiteasian <- cut(msa_data$pop_poc_pct, 
                                        breaks = c(-1, 20, 40, 60, 80, 90, 101), 
                                        labels = c('0-19%', '20-39%', '40-59%', 
                                                   '60-79%', '80-89%', '90-100%'))

hs_data$race_brks_nonwhiteasian <- cut(hs_data$pct_poc, 
                                       breaks = c(-1, 20, 40, 60, 80, 90, 101), 
                                       labels = c('0-19%', '20-39%', '40-59%', 
                                                  '60-79%', '80-89%', '90-100%'))

# Create var for income breaks
msa_data$inc_brks <- cut(msa_data$median_household_income, 
                         breaks = c(-1, 50000, 75000, 100000, 150000, 200000, 10000000), 
                         labels = c('<$50k', '$50k-74k', '$75k-99k', 
                                    '$100k-149k', '$150k-199k', '$200k+'))


cbsa_shp <- merge(cbsa_shp, msa_data, by.x = 'GEOID', by.y = 'cbsa_code', all.x = T)

IL_msa_shp <- subset(cbsa_shp, str_detect(cbsa_title, 'IL'))
IL_purchased_shp <- subset(cbsa_shp, GEOID %in% IL_msa_df$cbsa_code)
IL_state_shp <- subset(state_shp, GEOID == IL_state)

IL_hs <- hs_data %>% filter(state_code == 'IL')


# Create shared color scale functions
color_income <- colorFactor('YlGnBu', msa_data$inc_brks)
color_race <- colorFactor('YlGnBu', msa_data$race_brks_nonwhiteasian)
color_pop <- colorNumeric('YlGnBu', IL_msa_shp$pop_total, n = 5)

# Create popups
pop_msa <- paste0('<b>', IL_msa_shp$cbsa_title, '</b><br>',
                  'Total Population: ', format(IL_msa_shp$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)

income_msa <- paste0('<b>', IL_msa_shp$cbsa_title, '</b><br>',
                     'Median Household Income: ', currency(IL_msa_shp$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)

race_msa <- paste0('<b>', IL_msa_shp$cbsa_title, '</b><br>',
                   '% Population of Color: ', sprintf('%.1f', IL_msa_shp$pop_poc_pct)) %>% lapply(htmltools::HTML)


highlight_msa <- highlightOptions(color = 'black',
                                  bringToFront = F)

# Create map
map_IL <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  addMiniMap(tiles = providers$CartoDB.Positron,
             toggleDisplay = TRUE) %>%
  
  addPolygons(data = IL_state_shp, stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'gray', group = 'IL') %>% 
  addPolygons(data = raster::intersect(IL_state_shp, IL_msa_shp), weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop(pop_total), label = pop_msa, group = 'IL by Population', highlightOptions = highlight_msa) %>%
  addPolygons(data = raster::intersect(IL_state_shp, IL_msa_shp), weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = income_msa, group = 'IL by Median Household Income', highlightOptions = highlight_msa) %>%
  addPolygons(data = raster::intersect(IL_state_shp, IL_msa_shp), weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks_nonwhiteasian), label = race_msa, group = 'IL by Race/Ethnicity', highlightOptions = highlight_msa) %>%
  addPolylines(data = raster::intersect(IL_state_shp, IL_purchased_shp), weight = 1, color = 'black', fillOpacity = 0, group = 'Purchased MSA') %>% 
  
  # add markers
  addCircleMarkers(data = IL_hs, lng = ~longitude, lat = ~latitude, group = 'HS by Race/Ethnicity',
                   radius = 3, fill = TRUE, fillOpacity = 1, weight = 1, color = 'white', fillColor = ~color_race(race_brks_nonwhiteasian)) %>%

  # add legends
  addLegend(data = IL_msa_shp,
            position = 'topright', pal = color_pop, values = ~pop_total,
            title = 'Population',
            className = 'info legend legend-pop',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(data = cbsa_shp,
            position = 'topright', pal = color_income, values = ~inc_brks,
            title = 'Median Household Income',
            className = 'info legend legend-income',
            na.label = 'NA',
            opacity = 1) %>%

  addLegend(data = cbsa_shp,
            position = 'topright', pal = color_race, values = ~race_brks_nonwhiteasian,
            title = 'Black, Latinx, and <br>Native American Population',
            className = 'info legend legend-race',
            na.label = 'NA',
            opacity = 1) %>%

  # add options
  addLayersControl(
    position = c('bottomleft'),
    baseGroups = c('IL', 'IL by Population', 'IL by Median Household Income', 'IL by Race/Ethnicity'),
    overlayGroups = c('Purchased MSA', 'HS by Race/Ethnicity'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  hideGroup('HS by Race/Ethnicity') %>% 
  
  htmlwidgets::onRender("
        function(el, x) {
            var myMap = this;
            $('.legend').css('display', 'none');
            
            myMap.on('baselayerchange', function(e) {
                $('.legend').css('display', 'none');
                switch(e.name) {
                    case 'IL by Population':
                        $('.legend-pop').css('display', 'inherit');
                        break;
                    case 'IL by Median Household Income':
                        $('.legend-income').css('display', 'inherit');
                        break;
                    case 'IL by Race/Ethnicity':
                        $('.legend-race').css('display', 'inherit');
                        break;
                }
                e.layer.bringToBack();
            });
            
            myMap.on('overlayadd', function(e) {
                if (e.name === 'HS by Race/Ethnicity') {
                    $('.legend-race').css('display', 'inherit');
                }
            });
            
            myMap.on('overlayremove', function(e) {
                if (e.name === 'HS by Race/Ethnicity' && $('.leaflet-control-layers-base input[type=radio]:checked~span').text().trim() !== 'IL by Race/Ethnicity') {
                    $('.legend-race').css('display', 'none');
                }
            });
    }")

# saveWidget(map_IL, '~/Downloads/map_IL.html', background = 'transparent')

# ---------------
# Non-ENG orders
# ---------------

OOS_noneng_lists <- lists_df_sat %>% filter(order_num %in% c('483724', '470283', '371629', '456737', '386335', '403340'), Country == 'United States', ZipCode != '-', str_detect(State, '^[A-Z]{2}$')) %>% 
  select(-test_type, -order_num, -order_date) %>% distinct() %>%  # Each student is unique - got rid of duplicates that came from multiple orders
  mutate(
    zip_code = str_pad(str_sub(ZipCode, 1, 5), width = 5, pad = '0', side = 'left'),
    ceeb = str_pad(SchoolCode, width = 6, pad = '0', side = 'left')
  )

OOS_noneng_lists_by_zip <- OOS_noneng_lists %>% group_by(zip_code) %>% summarise(count = n()) %>% arrange(-count)  # Number of students per zip code

OOS_noneng_lists <- OOS_noneng_lists %>%
  left_join(ceeb_nces, by = 'ceeb') %>% 
  left_join(hs_data, by = 'ncessch')

OOS_purchased_hs <- OOS_noneng_lists %>% select(latitude, longitude) %>% filter(!is.na(latitude), !is.na(longitude)) %>% distinct()

OOS_noneng_orders <- OOS_orders %>% filter(order_num %in% c('483724', '470283', '371629', '456737', '386335', '403340'))
OOS_noneng_msa <- (OOS_noneng_orders$cbsa_name %>% na.omit() %>% unique() %>% str_match_all('([A-Z]{2}) - ([^|]+)'))[[1]] %>% as.data.frame()
names(OOS_noneng_msa) <- c('cbsa_full', 'cbsa_state', 'cbsa_title')

OOS_noneng_msa_grouped <- OOS_noneng_msa %>%
  group_by(cbsa_title) %>%
  summarise(cbsa_states = str_c(cbsa_state, collapse = '|')) %>% 
  left_join(msa_data, by = 'cbsa_title')

OOS_state_shp_from_state <- subset(state_shp, STUSPS %in% c('CA', 'CT', 'MO'))
OOS_state_shp_from_msa <- subset(state_shp, STUSPS %in% as.character(unique(OOS_noneng_msa$cbsa_state)))
OOS_msa_shp <- subset(cbsa_shp, str_detect(cbsa_title, str_c('CA|CT|MO|', str_c(unique(OOS_noneng_msa$cbsa_state), collapse = '|'))))

OOS_purchased_shp <- raster::intersect(OOS_state_shp_from_state , subset(cbsa_shp, str_detect(cbsa_title, 'CA|CT|MO') & GEOID != '28140'))  # purchased states

for (i in 1:nrow(OOS_noneng_msa_grouped)) {  # purchased msa regions
  msa <- subset(cbsa_shp, GEOID == OOS_noneng_msa_grouped[[i, 'cbsa_code']])
  state <- subset(state_shp, STUSPS %in% c('MO', str_split(OOS_noneng_msa_grouped[[i, 'cbsa_states']], '\\|')[[1]]))
  purchased_msa <- aggregate(raster::intersect(msa, state))
  OOS_purchased_shp <- bind(OOS_purchased_shp, purchased_msa)
}

OOS_purchased_msa <- OOS_noneng_msa_grouped$cbsa_states
names(OOS_purchased_msa) <- OOS_noneng_msa_grouped$cbsa_code

for (i in 1:nrow(OOS_msa_shp)) {
  msa <- as.character(OOS_msa_shp$GEOID[[i]])
  if (msa %in% names(OOS_purchased_msa)) {
    OOS_msa_shp$msa_title[[i]] <- str_replace_all(OOS_msa_shp$cbsa_title[[i]], str_c('(', OOS_purchased_msa[[msa]], ')'), '<span style="text-decoration: underline;">\\1</span>')
  } else {
    OOS_msa_shp$msa_title[[i]] <- OOS_msa_shp$cbsa_title[[i]]
  }
}

OOS_zip_shp_purchased <- subset(zip_shp, ZCTA5CE10 %in% OOS_noneng_lists_by_zip$zip_code) %>% 
  merge(OOS_noneng_lists_by_zip, by.x = 'ZCTA5CE10', by.y = 'zip_code', all.x = T)

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

zip_shp <- merge(zip_shp, zip_data, by.x = 'ZCTA5CE10', by.y = 'zip_code', all.x = T)

OOS_zip_shp <- subset(zip_shp, state_code %in% c('CA', 'CT', 'MO') | cbsa_1 %in% OOS_noneng_msa_grouped$cbsa_code | cbsa_2 %in% OOS_noneng_msa_grouped$cbsa_code | cbsa_3 %in% OOS_noneng_msa_grouped$cbsa_code | cbsa_4 %in% OOS_noneng_msa_grouped$cbsa_code)


# Create shared color scale functions
color_pop <- colorNumeric('YlGnBu', OOS_msa_shp$pop_total, n = 5)
color_pop_zip <- colorNumeric('YlGnBu', zip_shp$pop_total, n = 5)
color_pop_zip_purchased <- colorNumeric('YlOrRd', OOS_zip_shp_purchased$count, n = 10)

# Create popups
pop_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                  'Total Population: ', format(OOS_msa_shp$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)

income_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                     'Median Household Income: ', currency(OOS_msa_shp$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)

race_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                   '% Population of Color: ', sprintf('%.1f', OOS_msa_shp$pop_poc_pct)) %>% lapply(htmltools::HTML)

pop_zip <- paste0('<b>', OOS_zip_shp$zip_name, '</b><br>',
                  'Total Population: ', format(OOS_zip_shp$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)

income_zip <- paste0('<b>', OOS_zip_shp$zip_name, '</b><br>',
                     'Median Household Income: ', currency(OOS_zip_shp$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)

race_zip <- paste0('<b>', OOS_zip_shp$zip_name, '</b><br>',
                   '% Population of Color: ', sprintf('%.1f', OOS_zip_shp$pop_poc_pct)) %>% lapply(htmltools::HTML)

pop_zip_purchased <- paste0('<b>ZCTA5 ', OOS_zip_shp_purchased$ZCTA5CE10, '</b><br>',
                  'Total Purchased: ', format(OOS_zip_shp_purchased$count, big.mark = ',')) %>% lapply(htmltools::HTML)


map_OOS_nonENG <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addMiniMap(tiles = providers$CartoDB.Positron,
             toggleDisplay = TRUE) %>%
  
  addPolygons(data = OOS_state_shp_from_state, stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'orange', group = 'OOS') %>% 
  addPolygons(data = OOS_state_shp_from_msa, stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'gray', group = 'OOS') %>% 
  # addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop(pop_total), label = pop_msa, group = 'OOS by Population', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_zip_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop_zip(pop_total), label = pop_zip, group = 'OOS by Population', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_zip_shp_purchased, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop_zip_purchased(count), label = pop_zip_purchased, group = 'Purchased by Zip', highlightOptions = highlight_msa) %>%
  # addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = income_msa, group = 'OOS by Median Household Income', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_zip_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = income_zip, group = 'OOS by Median Household Income', highlightOptions = highlight_msa) %>%
  # addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks_nonwhiteasian), label = race_msa, group = 'OOS by Race/Ethnicity', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_zip_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks_nonwhiteasian), label = race_zip, group = 'OOS by Race/Ethnicity', highlightOptions = highlight_msa) %>%
  addPolylines(data = OOS_purchased_shp, weight = 1, color = 'black', fillOpacity = 0, group = 'Purchased MSA') %>% 
  
  addCircleMarkers(data = OOS_purchased_hs, lng = ~longitude, lat = ~latitude, group = 'Purchased by HS',
                   radius = 1, fill = TRUE, fillOpacity = 1, opacity = 1, weight = 1, color = 'red') %>%
  
  # add legends
  # addLegend(data = OOS_msa_shp,
  #           position = 'topright', pal = color_pop, values = ~pop_total,
  #           title = 'Population',
  #           className = 'info legend legend-base legend-pop',
  #           na.label = 'NA',
  #           opacity = 1) %>%
  
  addLegend(data = OOS_zip_shp,
            position = 'topright', pal = color_pop_zip, values = ~pop_total,
            title = 'Population',
            className = 'info legend legend-base legend-pop',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(data = cbsa_shp,
            position = 'topright', pal = color_income, values = ~inc_brks,
            title = 'Median Household Income',
            className = 'info legend legend-base legend-income',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(data = cbsa_shp,
            position = 'topright', pal = color_race, values = ~race_brks_nonwhiteasian,
            title = 'Black, Latinx, and <br>Native American Population',
            className = 'info legend legend-base legend-race',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(data = OOS_zip_shp_purchased,
            position = 'topright', pal = color_pop_zip_purchased, values = ~count,
            title = 'Purchased Students',
            className = 'info legend legend-purchased-zip',
            na.label = 'NA',
            opacity = 1) %>%
  
  # add options
  addLayersControl(
    position = c('bottomleft'),
    baseGroups = c('OOS', 'OOS by Population', 'OOS by Median Household Income', 'OOS by Race/Ethnicity'),
    overlayGroups = c('Purchased MSA', 'Purchased by Zip', 'Purchased by HS'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  hideGroup('Purchased by Zip') %>% 
  hideGroup('Purchased by HS') %>% 
  
  htmlwidgets::onRender("
        function(el, x) {
            var myMap = this;
            $('.legend').css('display', 'none');
            
            myMap.on('baselayerchange', function(e) {
                $('.legend-base').css('display', 'none');
                switch(e.name) {
                    case 'OOS by Population':
                        $('.legend-pop').css('display', 'inherit');
                        break;
                    case 'OOS by Median Household Income':
                        $('.legend-income').css('display', 'inherit');
                        break;
                    case 'OOS by Race/Ethnicity':
                        $('.legend-race').css('display', 'inherit');
                        break;
                }
                e.layer.bringToBack();
            });
            
            myMap.on('overlayadd', function(e) {
                if (e.name === 'Purchased by Zip') {
                    $('.legend-purchased-zip').css('display', 'inherit');
                }
            });
            
            myMap.on('overlayremove', function(e) {
                if (e.name === 'Purchased by Zip') {
                    $('.legend-purchased-zip').css('display', 'none');
                }
            });
    }")

# saveWidget(map_OOS_nonENG, '~/Downloads/map_OOS_nonENG.html', background = 'transparent')
saveWidget(map_OOS_nonENG, '~/Downloads/map_OOS_nonENG_zip.html', background = 'transparent')


# ---------------
# ENG orders
# ---------------

OOS_eng_orders <- OOS_orders %>% filter(is.na(cbsa_name))

# Lower test score criteria for female students
View(OOS_eng_orders %>% select(gender, sat_score_min, sat_score_max, psat_score_min, psat_score_max) %>% distinct() %>% arrange(sat_score_min))

# Fewer female students purchased
View(OOS_eng_orders %>% group_by(gender) %>% summarise(num_orders = n(), total_cost = sum(order_cost), total_students = sum(num_students)))


# -----------
# CA DOE EDA
# -----------

la_zip_codes <- (zip_cbsa_data %>% filter(cbsa_1 == '31080'))$zip_code

# Look at just College Board LA students
lists_df_sat_la <- lists_df_sat %>%
  select(-test_type, -order_num, -order_date) %>% distinct() %>%  # Each student is unique - got rid of duplicates that came from multiple orders
  mutate(
    zip_code = str_pad(str_sub(ZipCode, 1, 5), width = 5, pad = '0', side = 'left'),
    ceeb = str_pad(SchoolCode, width = 6, pad = '0', side = 'left'),
    is_white = as.integer(str_detect(Race, 'White'))
  ) %>% 
  filter(zip_code %in% la_zip_codes)

length(unique(lists_df_sat_la$zip_code))  # 307 zip codes
length(unique(lists_df_sat_la$ceeb))  # 348 HS

ceeb_nces %>%  # some ceeb may be mapped to multiple ncessch due to crosswalk coming from multiple sources & ncessch changing over the years
  filter(ceeb %in% lists_df_sat_la$ceeb) %>% 
  group_by(ceeb) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  View()

lists_df_sat_la_hs <- lists_df_sat_la %>%
  group_by(ceeb) %>% 
  summarise(count = n(), pct_white_purchased = mean(is_white, na.rm = T) * 100) %>% 
  arrange(-count) %>%
  left_join(ceeb_nces, by = 'ceeb') %>% 
  left_join(cds_nces, by = 'ncessch') %>% 
  left_join(hs_data %>% select(ncessch, total_students, pct_white), by = 'ncessch') %>% 
  left_join(df_sat_ca_20, by = c('CDSCode' = 'CDS'))

dupe_ceeb <- (lists_df_sat_la_hs %>% 
  group_by(ceeb) %>% 
  summarise(count = n()) %>% 
  filter(count > 1))$ceeb

lists_df_sat_la_hs %>%  # if ceeb mapped to multiple ncessch, at most 1 of those ncessch is valid
  filter(ceeb %in% dupe_ceeb) %>%
  View()


# ------------------------------------------------
# HS CHARACTERISTICS AGGREGATED TO ZIP-CODE LEVEL
# ------------------------------------------------

lists_df_all <- lists_df_pivot %>%  # 434120 unique students
  filter(Country == 'United States') %>% 
  select(-test_type, -order_num, -order_date) %>% distinct() %>%  # Each student is unique - got rid of duplicates that came from multiple orders
  mutate(
    ceeb = str_pad(SchoolCode, width = 6, pad = '0', side = 'left')
  )

ceeb_hs <- ceeb_nces %>% inner_join(hs_data, by = 'ncessch')  # get rid of rows w/o NCES data too

# 46 ceeb mapped to multiple ncessch w/ existing ccd data
nrow(ceeb_hs)  # 20084
length(unique(ceeb_hs$ceeb))  # 20038

ceeb_hs %>% group_by(ceeb) %>% summarise(count = n()) %>% View()

dupe_ceeb <- (ceeb_hs %>% 
                group_by(ceeb) %>% 
                summarise(count = n()) %>%
                filter(count > 1))$ceeb

dupe_ncessch <- (ceeb_hs %>% 
                   group_by(ncessch) %>% 
                   summarise(count = n()) %>% 
                   filter(count > 1))$ncessch

ceeb_hs %>% filter(ceeb == '030617') %>% View()  # Should be Arete Prep not Mesa HS
ceeb_hs %>% filter(ncessch == '040497000404') %>% View()  # There is a correct entry for Mesa HS in the crosswalk too
ceeb_hs %>% filter(ncessch == '040075702820') %>% View()  # Whereas Arete Prep only has the one entry

ceeb_hs %>% filter(ceeb == '051727') %>% View()  # Should be Milken Community School not Foxcroft School
ceeb_hs %>% filter(ncessch == '01433805') %>% View()  # There is a correct entry for Foxcroft School in the crosswalk too
ceeb_hs %>% filter(ncessch == 'A9101492') %>% View()  # Whereas Milken Community School only has the one entry

ceeb_hs %>% filter(ceeb == '051792') %>% View()  # School seems to have 2 entries in NCES DB (BB120057 & BB944617)
# https://nces.ed.gov/globallocator/sch_info_popup.asp?Type=Private&ID=BB120057
# https://nces.ed.gov/globallocator/sch_info_popup.asp?Type=Private&ID=BB944617

ambiguous_ceeb <- (ceeb_hs %>% filter(!(ceeb %in% dupe_ceeb & ncessch %in% dupe_ncessch)) %>% group_by(ceeb) %>% summarise(count = n()) %>% filter(count > 1))$ceeb

intersect(ambiguous_ceeb, lists_df_all$ceeb)

# Most look like duplicate entries in NCES
ceeb_hs %>% filter(ceeb %in% ambiguous_ceeb) %>% View()

lists_df_all_hs <- lists_df_all %>% left_join(ceeb_hs, by = 'ceeb')  # 476 repeated rows (matched to one of the 46 ceeb that had multiple ncessch entries in crosswalk)

# 322211 (80.7%) from public HS, 47544 (11.9%) from private HS, 29525 (7.4%) either no entry in crosswalk or no available NCES data
table(lists_df_all_hs$school_type, useNA = 'always')

lists_df_all_zip <- lists_df_all %>% right_join(ceeb_hs, by = 'ceeb')
# dropping NA students who did not match to available NCES data
# this also dropped HS whose ncessch did not exist in crosswalk (no chance of it merging w/ purchased students if there were any)

lists_df_all_zip %>% count(is.na(Ref))  # 369755 purchased students
length(unique(lists_df_all_zip$ncessch))  # 19986 HS

# Group by HS's zip code
lists_df_all_zip_agg <- lists_df_all_zip %>%
  mutate(
    ncessch_purchased = if_else(is.na(Ref), NA_character_, ncessch)
  ) %>% 
  group_by(zip_code, state_code) %>%
  summarise(
    num_hs = n_distinct(ncessch, na.rm = T),
    num_hs_purchased = n_distinct(ncessch_purchased, na.rm = T),
    num_students_purchased = sum(as.numeric(!is.na(Ref)))
  )

sum(lists_df_all_zip_agg$num_students_purchased)  # 369755 purchased students
sum(lists_df_all_zip_agg$num_hs)  # 19986 HS
sum(lists_df_all_zip_agg$num_hs_purchased)  # 6299 purchased HS

# Filter for just LA zip codes
lists_df_all_la <- lists_df_all %>%
  mutate(
    zip_code = str_pad(str_sub(ZipCode, 1, 5), width = 5, pad = '0', side = 'left')
  ) %>% 
  filter(zip_code %in% la_zip_codes)

# Unmerged CEEB based on student's home zip code being in LA
unmerged_la_ceeb <- (lists_df_all_la %>% anti_join(ceeb_hs, by = 'ceeb'))$ceeb %>% unique()

lists_df_all_la_zip_agg <- lists_df_all_la %>%  # may include students whose home zip code is LA but attended school not in LA
  select(-zip_code) %>% 
  right_join(ceeb_hs, by = 'ceeb') %>%
  mutate(
    ncessch_purchased = if_else(is.na(Ref), NA_character_, ncessch)
  ) %>% 
  group_by(zip_code, state_code) %>%
  summarise(
    num_hs = n_distinct(ncessch, na.rm = T),
    num_hs_purchased = n_distinct(ncessch_purchased, na.rm = T),
    num_students_purchased = sum(as.numeric(!is.na(Ref)))
  )

# We would ideally know the HS/EN clusters to differentiate between HS that weren't purchased vs. HS that were purchased but no students met other criteria

# Only look at LA schools that's in crosswalk and the students that were merged to those (259 HS)
View(lists_df_all_zip_agg %>% filter(zip_code %in% la_zip_codes) %>% arrange(desc(num_students_purchased)))

lists_df_all_la_zip <- lists_df_all_zip %>% filter(zip_code %in% la_zip_codes)

lists_df_all_la_zip %>% count(is.na(Ref))  # 18470 purchased students
length(unique(lists_df_all_la_zip$ncessch))  # 506 HS

lists_df_all_la_zip_by_purchase <- lists_df_all_la_zip %>%
  group_by(zip_code, state_code, ncessch, total_students, pct_white, pct_poc) %>% 
  summarise(
    num_students_purchased = sum(!is.na(Ref))
  ) %>% 
  mutate(is_hs_purchased = num_students_purchased > 0) %>%
  group_by(zip_code, state_code, is_hs_purchased) %>%
  summarise(
    num_hs = n(),
    num_students_purchased = sum(num_students_purchased),
    avg_hs_size = mean(total_students),
    avg_pct_white = mean(pct_white),
    avg_pct_poc = mean(pct_poc),
    avg_pct_white_weighted = sum(total_students / sum(total_students) * pct_white),
    avg_pct_poc_weighted = sum(total_students / sum(total_students) * pct_poc)
  ) %>% 
  arrange(zip_code, desc(is_hs_purchased))

sum(lists_df_all_la_zip_by_purchase$num_students_purchased)  # 18470 purchased students
sum(lists_df_all_la_zip_by_purchase$num_hs)  # 506 HS
lists_df_all_la_zip_by_purchase %>% group_by(is_hs_purchased) %>% summarise(count = sum(num_hs))  # 230 HS purchased (276 HS not purchased)
