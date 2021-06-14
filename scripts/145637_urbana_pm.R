library(tidyverse)
library(lubridate)
library(leaflet)
library(rgdal)
library(raster)
library(formattable)
library(scales)
library(htmlwidgets)

#===========================================================================================
#Prep data
#===========================================================================================

# Directory paths
output_dir <- file.path('.', 'outputs')
original_dir <- file.path('.', 'originals')
data_dir <- file.path('.', 'data')
plots_dir <- file.path('.', 'outputs', 'plots')

# Read in data
lists_df <- read_csv(file.path(output_dir, '145637_lists.csv'), col_types = cols(.default = 'c'))
orders_df <- read_csv(file.path(output_dir, '145637_orders.csv'), col_types = c('univ_id' = 'c', 'order_num' = 'c'))

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
  dplyr::select(-test_num) %>% 
  pivot_longer(
    cols = starts_with(c('sat_', 'act_')),
    names_to = c('test_type', '.value'),
    names_sep = '_',
    values_drop_na = T
  ) %>%
  rename(order_num = test, order_date = date) %>% 
  mutate(order_date = mdy(order_date))

# SAT orders
lists_df_sat <- lists_df_pivot %>% filter(test_type == 'sat')

# Missing order summary for 107713 of 415689 rows in lists_df_sat (15 distinct orders)
anti_join(lists_df_sat, orders_df, by = 'order_num') %>% nrow()
View(anti_join(lists_df_sat, orders_df, by = 'order_num') %>% select(order_num, order_date) %>% distinct())

# 3 order summaries w/ no lists entries, but these look like draft orders that weren't actually placed (i.e., "Edit name")
View(anti_join(orders_df, lists_df_sat, by = 'order_num'))

# Explore remaining matched rows (rows may be duplicates if they belong to multiple orders)
merged_df_sat <- inner_join(lists_df_sat, orders_df, by = 'order_num')

#Run some descriptive statistics
merged_df_sat %>% group_by(order_num) %>% count() #about 77 order summaries

#===========================================================================================
#Ozan said not to use IPEDS data for now
#===========================================================================================

#Read in ipeds inst. data (don't save yet to repo)
#hd2019 <- read_csv('~/Downloads/hd2019.csv')
#names(hd2019) <- str_to_lower(names(hd2019))

#subset df
#hd2019_sub <- hd2019 %>% select(unitid, instnm, city, stabbr, zip, longitud, latitude) %>%
#  filter(unitid == 145637)


#Read in ipeds fall enrollment data
#ef2019a <- read_csv('~/Downloads/ef2019a.csv') 
#names(ef2019a) <- str_to_lower(names(ef2019a))

#subset df
#ef2019a_sub <- ef2019a %>% select(unitid, !starts_with('x')) %>%
#  filter(unitid == 145637)

#filter for full-time, first-time, degree-seeking undergraduates
#ef2019a <- ef2019a %>%
#  filter(line == 1)

#ef2019a %>%
#  group_by(unitid) %>%
#  count() %>%
#  filter(n > 1) #check to make sure level of analysis is university

#ef2019a %>%
#  filter(unitid == 145637)

#merge hd and ef data
#ipeds <- inner_join(ef2019a_sub, hd2019_sub, by = 'unitid')

#calculate percent race/ethnicity
#ipeds <- ipeds %>%
#  mutate(
#    pct_white = efwhitt / eftotlt * 100,
#    pct_black = efbkaat / eftotlt * 100,
#    pct_latinx = efhispt / eftotlt * 100,
#    pct_asian = efasiat / eftotlt * 100,
#    pct_amerindian = efaiant / eftotlt * 100,
#    pct_nativehawaii = efnhpit / eftotlt * 100,
#    pct_tworaces = ef2mort / eftotlt * 100,
#    pct_unknownrace = efunknt / eftotlt * 100,
#    pct_nonres = efnralt / eftotlt * 100) %>%
#  select(!starts_with("ef"))

#===========================================================================================
#Investigate merged student list and order summary df 
#===========================================================================================

#names(merged_df_sat)
merged_df_sat %>% 
  group_by(Ref, order_num) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp) #Ref code is specific to a student? Order number is associated with the order summary made

#Check
merged_df_sat %>% 
  group_by(Ref, order_num) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  filter(n_per_grp > 1)

#Check order summary df
orders_df %>%
  group_by(order_num) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

orders_df$order_num[duplicated(orders_df$order_num)]
sum(duplicated(orders_df$order_num)==1)#none

#Check student lists
lists_df_sat %>%
  group_by(Ref, order_num) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

sum(duplicated(lists_df_sat$Ref)==1)

merged_df_sat %>% 
  group_by(Ref) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp) #So that means that a student can be purchased more than once?

sum(duplicated(merged_df_sat$Ref)==1)

#print duplicated elements
merged_df_sat$Ref[duplicated(merged_df_sat$Ref)]
#sum of duplicates
sum(duplicated(merged_df_sat$Ref)==1)
merged_df_sat %>% distinct(Ref)

ref_check <- merged_df_sat %>%
  select(Ref, GeoMarket, SchoolCode, order_num) %>%
  group_by(Ref) %>%
  filter(duplicated(Ref))

ref_check %>%
  group_by(Ref) %>%
  summarise(n_per_grp = n()) %>%
  count(n_per_grp > 1) #some students are purchased more than once from several order summaries

#===========================================================================================
# Question: Does number of student purchased vary by date order number was made?
#===========================================================================================

#let's use order summary data for now
orders_df %>% 
  select(order_num, date_start, num_students) %>%
  arrange(-num_students) #feb-may, aug-oct?

#===========================================================================================
# Question: Do order summary purchases made vary by time of year? 
#===========================================================================================
#subset orders by year maybe?

order_year_plot <- function(df, order_year, dir_name, plot_name) {
  df <- df %>%
    dplyr::select(order_num, date_start, num_students) %>%
    mutate(year = year(date_start),
           month = month(date_start)) %>%
    filter(year == order_year) %>%
    dplyr::select(-year)
  
  png(file.path(dir_name, plot_name))
  print(ggplot() +
          geom_point(data = df,
                     aes(x = month, y = num_students), size = .6, color = "grey50") +
          geom_text(data = df,
                    aes(x = month, y = num_students, label = order_num), family = "mono", size = 4) +
          xlab(str_c("Order purchased in", order_year)) + ylab("Number of students purchased") +
          xlim("Jan", "Feb","Mar", "April", "May", "June", "July", "Aug", "Sep", "Oct", "Nov")
  )
  dev.off()
  
  df

}

order_year_plot(orders_df, 2017, plots_dir, 'urbana_2017_plot.jpeg')
order_year_plot(orders_df, 2018, plots_dir, 'urbana_2018_plot.jpeg')
order_year_plot(orders_df, 2019, plots_dir, 'urbana_2019_plot.jpeg')
order_year_plot(orders_df, 2020, plots_dir, 'urbana_2020_plot.jpeg')

#===========================================================================================
# Question: What cbsa areas are selected as filters?
#===========================================================================================
#Read in census zip-code level data
acs <- read_csv(url("https://github.com/cyouh95/third-way-report/raw/master/assets/data/zip_to_state.csv"))
#might want to use the msa dataset if looking at msa?
msa <- read_csv(url("https://github.com/cyouh95/third-way-report/raw/master/assets/data/msa_metadata.csv"))

# check how many orders used cbsa and arrange descending by number of students purchased
orders_df %>% select(order_num, date_start, cbsa_name, num_students) %>%
  filter(!is.na(cbsa_name)) %>%
  arrange(-num_students)

# for simplicity just keep one order summary
order_371629 <- orders_df %>%
  filter(order_num == 371629)

# identify how many bars there are, 39
str_count(order_371629$cbsa_name, "\\|")
str_view_all(order_371629$cbsa_name, "\\|")

#split at the bar
cbsa <- str_split(order_371629$cbsa_name, "\\|")
cbsa <- cbsa[[1]] #grab first element of list, a vector of values

cbsa

msa %>%
  filter(cbsa_title %in% cbsa) #need to get rid of state abbr at the beginning

cbsa_clean <- str_match(cbsa, "^\\w{2}\\s-\\s(\\w+.+)")

cbsa_clean
cbsa <- cbsa_clean[,2]

msa %>%
  filter(cbsa_title %in% cbsa) %>%
  select(cbsa_code, is_msa, msa_name, cbsa_title)


#zipcode files https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#https://github.com/cyouh95/third-way-report/blob/master/scripts/third_way_map.R
#https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#==========================================================================================

#check order/list data race/ethnicity variables
race <- merged_df_sat %>%
  group_by(Race) %>%
  summarise(n_per_race = n())

race_lat <- merged_df_sat %>%
  group_by(Race) %>%
  count(Hispanic)

#Have to figure out how to merge race/ethnicity with ipeds data?
#try to create a Latinx/hispanic race variable
#check first
merged_df_sat %>%
  filter(Race == "White" & Hispanic == "Yes" | is.na(Race) & Hispanic == "Yes") %>%
  select(Race, Hispanic) 


merged_df_sat %>%
  mutate(race = case_when(
    Race == "White" & Hispanic == "Yes" ~ "latinx",
    (is.na(Race)) & Hispanic == "Yes" ~ "latinx"
  )) 



#===========================================================================================
#Crystal's script for IL orders
#===========================================================================================

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
zip_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_to_state.csv'))
msa_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/msa_metadata.csv'), na = c('', 'NULL')) %>% 
  mutate(pop_poc_pct = pop_black_pct + pop_hispanic_pct + pop_amerindian_pct)
hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c')) %>% 
  mutate(pct_poc = pct_black + pct_hispanic + pct_amerindian)
univ_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/meta_university.csv'), col_types = c('univ_id' = 'c', 'fips_state_code' = 'c', 'fips_county_code' = 'c')) %>% 
  select(-X1)

# Load shape files: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
cbsa_shp <- readOGR(file.path(data_dir, 'cb_2018_us_cbsa_500k', 'cb_2018_us_cbsa_500k.shp'))
state_shp <- readOGR(file.path(data_dir, 'cb_2018_us_state_500k', 'cb_2018_us_state_500k.shp'))


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
leaflet() %>%
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
                        if ($('.leaflet-control-layers-base input[type=radio]:checked~span').text().trim() !== 'IL by Race/Ethnicity') {
                        $('.legend-race').css('display', 'none');
                        }
                        });
                        }")


# ---------------
# Non-ENG orders
# ---------------

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

# Create shared color scale functions
color_pop <- colorNumeric('YlGnBu', OOS_msa_shp$pop_total, n = 5)

# Create popups
pop_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                  'Total Population: ', format(OOS_msa_shp$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)

income_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                     'Median Household Income: ', currency(OOS_msa_shp$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)

race_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                   '% Population of Color: ', sprintf('%.1f', OOS_msa_shp$pop_poc_pct)) %>% lapply(htmltools::HTML)


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addMiniMap(tiles = providers$CartoDB.Positron,
             toggleDisplay = TRUE) %>%
  
  addPolygons(data = OOS_state_shp_from_state, stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'orange', group = 'IL') %>% 
  addPolygons(data = OOS_state_shp_from_msa, stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'gray', group = 'IL') %>% 
  addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop(pop_total), label = pop_msa, group = 'IL by Population', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = income_msa, group = 'IL by Median Household Income', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks_nonwhiteasian), label = race_msa, group = 'IL by Race/Ethnicity', highlightOptions = highlight_msa) %>%
  addPolylines(data = OOS_purchased_shp, weight = 1, color = 'black', fillOpacity = 0, group = 'Purchased MSA') %>% 
  
  # add legends
  addLegend(data = OOS_msa_shp,
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
    overlayGroups = c('Purchased MSA'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
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
                        }")


# ---------------
# ENG orders
# ---------------

OOS_eng_orders <- OOS_orders %>% filter(is.na(cbsa_name))

# Lower test score criteria for female students
View(OOS_eng_orders %>% select(gender, sat_score_min, sat_score_max, psat_score_min, psat_score_max) %>% distinct() %>% arrange(sat_score_min))

# Fewer female students purchased
View(OOS_eng_orders %>% group_by(gender) %>% summarise(num_orders = n(), total_cost = sum(order_cost), total_students = sum(num_students)))

# ---------------
# MSA specific orders
# ---------------
View(OOS_msa_orders %>% dplyr::select(order_title, order_num, num_students, sat_score_min, sat_score_max, psat_score_min, psat_score_max) %>% distinct() %>% arrange(sat_score_min))

#grab OOS msa specific orders
OOS_msa_orders <- OOS_orders %>% filter(order_num %in% c('500590', '567376', '483751'))
#create df of msa areas filtered in order summary 500590
OOS_msa <- (OOS_msa_orders$cbsa_name %>% na.omit() %>% unique() %>% str_match_all('([A-Z]{2}) - ([^|]+)'))[[1]] %>% as.data.frame()
#OOS_specific_msa_567376 <- (OOS_msa_specific$cbsa_name %>% na.omit() %>% unique() %>% str_match_all('([A-Z]{2}) - ([^|]+)'))[[2]] %>% as.data.frame()
#OOS_specific_msa_483751 <- (OOS_msa_specific$cbsa_name %>% na.omit() %>% unique() %>% str_match_all('([A-Z]{2}) - ([^|]+)'))[[3]] %>% as.data.frame()

#name columns in df
names(OOS_msa) <- c('cbsa_full', 'cbsa_state', 'cbsa_title')

#Merge orders using specific msa areas with census msa data
OOS_msa_grouped <- OOS_msa %>%
  group_by(cbsa_title) %>% #group by cbsa_title
  summarise(cbsa_states = str_c(cbsa_state, collapse = '|')) %>% #group states with same msa area
  left_join(msa_data, by = 'cbsa_title')

#subset state shapefile for states from msa filters in order summary 
OOS_state_shp <- subset(state_shp, STUSPS %in% as.character(unique(OOS_msa$cbsa_state)))
#subset cbsa shapefule and grab the msa's from states in the order summary
OOS_msa_shp <- subset(cbsa_shp, str_detect(cbsa_title, str_c(unique(OOS_msa$cbsa_state), collapse = '|')))
#grab msa's purchased from order summary
OOS_purchased_shp <- subset(cbsa_shp, cbsa_title %in% OOS_msa_grouped$cbsa_title)


for (i in 1:nrow(OOS_msa_grouped)) {  # purchased msa regions
  msa <- subset(cbsa_shp, GEOID == OOS_msa_grouped[[i, 'cbsa_code']])
  state <- subset(state_shp, STUSPS %in% c(str_split(OOS_msa_grouped[[i, 'cbsa_states']], '\\|')[[1]]))
  purchased_msa <- aggregate(raster::intersect(msa, state))
  OOS_purchased_shp <- bind(OOS_purchased_shp, purchased_msa)
}

#vector of states
OOS_purchased_msa <- OOS_msa_grouped$cbsa_states
names(OOS_purchased_msa) <- OOS_msa_grouped$cbsa_code


for (i in 1:nrow(OOS_msa_shp)) {
  msa <- as.character(OOS_msa_shp$GEOID[[i]])
  if (msa %in% names(OOS_purchased_shp)) {
    OOS_msa_shp$msa_title[[i]] <- str_replace_all(OOS_msa_shp$cbsa_title[[i]], str_c('(', OOS_purchased_shp[[msa]], ')'), '<span style="text-decoration: underline;">\\1</span>')
  } else {
    OOS_msa_shp$msa_title[[i]] <- OOS_msa_shp$cbsa_title[[i]]
  }
}

# Create shared color scale functions
color_pop <- colorNumeric('YlGnBu', OOS_msa_shp$pop_total, n = 5)

# Create popups
pop_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                  'Total Population: ', format(OOS_msa_shp$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)

income_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                     'Median Household Income: ', currency(OOS_msa_shp$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)

race_msa <- paste0('<b>', OOS_msa_shp$msa_title, '</b><br>',
                   '% Population of Color: ', sprintf('%.1f', OOS_msa_shp$pop_poc_pct)) %>% lapply(htmltools::HTML)


#create map object
map_var <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addMiniMap(tiles = providers$CartoDB.Positron,
             toggleDisplay = TRUE) %>%
  
  addPolygons(data = OOS_state_shp, stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'orange', group = 'FL, TX, CA, GA, NY, NJ') %>% 
  addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop(pop_total), label = pop_msa, group = 'FL, TX, CA, GA, NY, NJ by Population', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = income_msa, group = 'FL, TX, CA, GA, NY, NJ by Median Household Income', highlightOptions = highlight_msa) %>%
  addPolygons(data = OOS_msa_shp, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks_nonwhiteasian), label = race_msa, group = 'FL, TX, CA, GA, NY, NJ by Race/Ethnicity', highlightOptions = highlight_msa) %>%
  addPolylines(data = OOS_msa_shp, weight = 1, color = 'black', fillOpacity = 0, group = 'Purchased MSA') %>% 
  
  # add legends
  addLegend(data = OOS_msa_shp,
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
    baseGroups = c('FL, TX, CA, GA, NY, NJ', 'FL, TX, CA, GA, NY, NJ by Population', 'FL, TX, CA, GA, NY, NJ by Median Household Income', 'FL, TX, CA, GA, NY, NJ by Race/Ethnicity'),
    overlayGroups = c('Purchased MSA'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  htmlwidgets::onRender("
                        function(el, x) {
                        var myMap = this;
                        $('.legend').css('display', 'none');
                        
                        myMap.on('baselayerchange', function(e) {
                        $('.legend').css('display', 'none');
                        switch(e.name) {
                        case 'FL, TX, CA, GA, NY, NJ by Population':
                        $('.legend-pop').css('display', 'inherit');
                        break;
                        case 'FL, TX, CA, GA, NY, NJ by Median Household Income':
                        $('.legend-income').css('display', 'inherit');
                        break;
                        case 'FL, TX, CA, GA, NY, NJ by Race/Ethnicity':
                        $('.legend-race').css('display', 'inherit');
                        break;
                        }
                        e.layer.bringToBack();
                        });
                        }")

#how to save map object
#saveWidget(map_var, 'OSS_regional_msa.html', background = 'transparent')

# ---------------
# Int orders
# ---------------
# Lower test score criteria for female students
View(intl_orders %>% dplyr::select(order_title, order_num, num_students, sat_score_min, sat_score_max, psat_score_min, psat_score_max) %>% distinct() %>% arrange(sat_score_min))

#Asian countries + Egypt
intl_regions_483721 <- (intl_orders$intl_region %>% na.omit() %>% unique() %>% str_match_all(pattern = "([A-Z]{2}-\\d+) - ([^|]+)"))[[1]] %>% as.data.frame()
names(intl_regions_483721) <- c('intl_region_full', 'intl_code', 'country')

#China search
intl_regions_372044 <- (intl_orders$intl_region %>% na.omit() %>% unique() %>% str_match_all(pattern = "([A-Z]{2}-\\d+) - ([^|]+)"))[[2]] %>% as.data.frame()
names(intl_regions_372044) <- c('intl_region_full', 'intl_code', 'country')

#Broad international search
intl_regions_470250 <- (intl_orders$intl_region %>% na.omit() %>% unique() %>% str_match_all(pattern = "([A-Z]{2}-\\d+) - ([^|]+)"))[[3]] %>% as.data.frame()
names(intl_regions_470250) <- c('intl_region_full', 'intl_code', 'country')

#Asian countries & Nigeria
intl_regions_371669 <- (intl_orders$intl_region %>% na.omit() %>% unique() %>% str_match_all(pattern = "([A-Z]{2}-\\d+) - ([^|]+)"))[[4]] %>% as.data.frame()
names(intl_regions_371669) <- c('intl_region_full', 'intl_code', 'country')

#===========================================================================================
#Student lists investigations of MSA specific orders
#===========================================================================================

merged_df_sat %>% count(order_num) %>% arrange(-n)

#check
merged_df_sat %>% distinct(order_num)

merged_df_sat %>% dplyr::select(order_num, Ref, State) %>%
  filter(order_num %in% orders_df$order_num) %>%
  count(order_num)

#get the order summary with the largest amount of purchases = 500590
#stu_500590 <- merged_df_sat %>% filter(order_num == 500590)

#investigate new object 
merged_df_sat %>%
  group_by(Ref, order_num) %>%
  summarize(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

View(merged_df_sat %>%
  group_by(Ref, order_num) %>%
  summarize(n_per_grp = n()) %>%
  ungroup() %>%
  filter(n_per_grp > 1)) #85 students were purchased twice

#---------------
# Merge student list data with high school-level data
#---------------

#read in crosswalk data
ceeb_nces <- read_csv(str_c(data_dir, file.path("/ceeb_nces_crosswalk/ceeb_nces_crosswalk.csv"))
)

#rename student lists SchoolCode column
student_eda <- merged_df_sat %>% rename(ceeb = SchoolCode, zip_order = zip_code)

#check how many match, 238,005
student_eda %>%
  filter(ceeb %in% ceeb_nces$ceeb)

#check the length of the ceeb variable
str_length(student_eda$ceeb)==5

#how to tell if there is a trailing zero or zero at the beginning?
#have to go back and fix

#student_eda <- student_eda %>%
 # mutate(ceeb_code = if_else(str_length(ceeb)==5, str_c(ceeb, "0"), ceeb))

#check
#student_eda %>%
#  dplyr::select(ceeb_code, ceeb)

#View(student_eda %>%
#  filter(str_length(student_eda$ceeb_code) < 5))

#merge ceeb_nces data to student list
student_eda_nces <- student_eda %>% left_join(ceeb_nces, by = "ceeb")

#see which rows did not merge
anti_eda_nces <- student_eda %>% anti_join(ceeb_nces, by = "ceeb")

anti_eda_nces %>%
  dplyr::select(ceeb)

#check length
str_length(anti_eda_nces$ceeb)

#---------------
# Merge student list data with high school- level data
#---------------
# read in high school level data

hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c')) %>% 
  mutate(pct_poc = pct_black + pct_hispanic + pct_amerindian) 

#join dfs
stu_eda_hs <- student_eda_nces %>% left_join(hs_data, by = "ncessch")

#check merge
anti_stu_hs <- student_eda_nces %>% anti_join(hs_data, by = "ncessch")
anti_stu_hs %>% filter(ncessch %in% stu_eda_hs$ncessch)

#---------------
#Merge census zip-code level data
#---------------

#check zip codes
stu_eda_hs %>%
  dplyr::select(ZipCode, zip_code)

#should we use zip code from student-level data or high school data?
  
# 34 obs where zipcode is less than 10 characters long
sum(str_length(stu_eda_hs$ZipCode) < 10)


stu_eda_hs %>%
  group_by(ZipCode) %>%
  arrange(num_students) %>%
  select(City, State, GeoMarket, num_students)
  
# have to ask Crystal where she gets the entire zip code level data
#stu_500590_nces <- stu_500590_nces %>%
#  mutate(zip_code = str_extract(stu_500590_nces$ZipCode, "\\d{5}"))

#---------------
#Out-of-state orders, MSA specific criteria
#---------------
#Out-of-state orders, MSA specific criteria
OOS_msa_stu <- merged_df_sat %>% filter(order_num %in% c('500590', '567376', '483751'))

#============================================================================
#Student lists investigations of MSA specific orders
#============================================================================

OOS_msa_stu %>% count(order_num) %>% arrange(-n)

#investigate new object 
OOS_msa_stu %>%
  group_by(Ref, order_num) %>%
  summarize(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

View(OOS_msa_stu %>%
       group_by(Ref, order_num) %>%
       summarize(n_per_grp = n()) %>%
       ungroup() %>%
       filter(n_per_grp > 1)) #4 students were purchased twice

View(OOS_msa_stu %>%
       filter(Ref %in% c(058607977, 	
                         058918440, 313230561, 964290266)) %>%
       dplyr::select(Ref:SchoolCode, order_num)) #not all are showing up?

#check how many schools are purchased more than once
OOS_msa_stu %>%
  group_by(SchoolCode) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

OOS_msa_stu %>%
  group_by(SchoolCode) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  filter(n_per_grp > 1) %>%
  arrange(-n_per_grp)

#check unique schools
length(unique(OOS_msa_stu$SchoolCode)) #2455


OOS_msa_stu %>%
  group_by(ZipCode) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  filter(n_per_grp>1) %>% arrange(-n_per_grp)

#---------------
# Merge census data with student-level data by zip code
#---------------

#read in zip-code level census data
zip_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_to_state.csv'))

#have to create a 5-digit zip code variable
OOS_msa_stu %>%
  mutate(zip_code = str_extract(ZipCode, "\\d{5}")) %>%
  dplyr::select(zip_code, ZipCode)

OOS_msa_stu <- OOS_msa_stu %>%
  mutate(zip_code = str_extract(ZipCode, "\\d{5}"))

#unique zipcodes
OOS_msa_stu %>%
  group_by(Ref, ZipCode) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  filter(n_per_grp>0) %>% arrange(-n_per_grp)

length(unique(OOS_msa_stu$zip_code)) #2427 unique zip codes

#join student-level & census data
OOS_msa_census <- OOS_msa_stu %>% left_join(zip_data, by = "zip_code")

OOS_msa_census %>%
  filter(zip_code != `zip_code(2)`)

#check merge
anti_msa_census <- OOS_msa_stu %>% anti_join(zip_data, by = "zip_code")

#general investigations
OOS_msa_census %>%
  group_by(zip_code) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

#zip codes where students are purchased, sort by ascending
OOS_zips <- OOS_msa_census %>%
  group_by(zip_code) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  filter(n_per_grp > 0) %>% arrange(-n_per_grp)

#merge census data to zip code data
OOS_msa_zip <- OOS_zips %>% left_join(zip_data, by = "zip_code")

#investigate zip codes 

#create race/ethnicity pct variables
OOS_msa_zip <- OOS_msa_zip %>%
  mutate(pct_black = pop_black/pop_total,
         pct_white = pop_white/pop_total,
         pct_asian = pop_asian/pop_total,
         pct_latinx = pop_hispanic/pop_total,
         pct_nat_am = pop_amerindian/pop_total,
         pct_nat_hi = pop_nativehawaii/pop_total,
         pct_two_races = pop_tworaces/pop_total,
         pct_other = pop_otherrace/pop_total)

#check
OOS_msa_zip %>%
  mutate(pct_tot = round(pct_black + pct_white + pct_asian + pct_latinx + pct_nat_am + pct_nat_hi + pct_other + pct_two_races)) %>%
  dplyr::select(pct_tot, pct_black, pct_white, pct_asian, pct_latinx, pct_nat_am, pct_nat_hi, pct_other, pct_two_races) %>%
  filter(pct_tot != 1)

# Table of Zip codes and demographic characteristics
OOS_msa_zip %>%
  dplyr::select(zip_code, n_per_grp, median_household_income, starts_with("pct"))

#grab top 5 zip codes
top_five_zip <- head(OOS_msa_zip$zip_code,5)

#filter msa/census df of top 5 zip codes
OOS_msa_top_5_zip <- OOS_msa_census %>%
  filter(zip_code %in% top_five_zip)

OOS_msa_top_5_zip %>%
  filter(zip_code == "77494") %>%
  dplyr::select(Ref:State, GeoMarket, Race, Hispanic, order_num, cbsa_name, segment) #%>% #565
#filter(Race == "White" & (is.na(Hispanic) | Hispanic == "No")) #172 non-hispanic white about 30% of purchases from this zip code
#filter(Race == "Black or African American" & (is.na(Hispanic) | Hispanic == "No")) #about 26 Black non-hispanic, about 5% of purchases from this zip code
#filter(Race == "White" & (!is.na(Hispanic) | Hispanic == "Yes")) #about 75 Latinx students, about 13% of purchases from this zip code
#filter(Race == "Asian" & (is.na(Hispanic) | Hispanic == "No")) #about 232 Asian students purchased, about 41% of purchases from this zip code

#create race/ethnicity student-level variables
OOS_msa_top_5_zip %>%
  group_by(Race) %>%
  summarise(n_per_grp = n()) 

OOS_msa_top_5_zip %>%
  filter(Hispanic == "Yes")

OOS_msa_top_5_zip <- OOS_msa_top_5_zip %>%
  group_by(zip_code) %>%
  #dplyr::select(Ref:State, GeoMarket, Race, Hispanic, order_num, cbsa_name, segment) %>% #565
  mutate(white = ifelse(Race == "White" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         black = ifelse(Race == "Black or African American" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         asian = ifelse(Race == "Asian" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         latinx = ifelse((!is.na(Hispanic) & Hispanic == "Yes"),1,0),
         nhpi = ifelse(Race == "Native Hawaiian or Other Pacific" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         nat_am = ifelse(Race == "American Indian or Alaska Native" &
                           (is.na(Hispanic) | Hispanic == "No"),1,0),
         none = ifelse(is.na(Race) & (is.na(Hispanic) | Hispanic == "No"),1,0),
         stu_pct_white = mean(white, na.rm=TRUE), #pct race/ethnicty for student-level data
         stu_pct_black = mean(black, na.rm=TRUE),
         stu_pct_asian = mean(asian, na.rm=TRUE),
         stu_pct_latinx = mean(latinx, na.rm=TRUE),
         stu_pct_nhpi = mean(nhpi, na.rm=TRUE),
         stu_pct_natam = mean(nat_am, na.rm=TRUE),
         stu_pct_none = mean(none, na.rm=TRUE),
         pct_black = pop_black/pop_total, #pct race/ethnicity for census data
         pct_white = pop_white/pop_total,
         pct_asian = pop_asian/pop_total,
         pct_latinx = pop_hispanic/pop_total,
         pct_nat_am = pop_amerindian/pop_total,
         pct_nat_hi = pop_nativehawaii/pop_total,
         pct_two_races = pop_tworaces/pop_total,
         pct_other = pop_otherrace/pop_total)



#sum(OOS_msa_top_5_zip$latinx, na.rm=T)
#sum(OOS_msa_top_5_zip$Hispanic=="Yes", na.rm=T)

#OOS_msa_top_5_zip %>%
#  dplyr::select(Race, Hispanic, latinx)

# Table comparing race/ethnicity of students' purchased within 77494 zip code and the population demographics
OOS_msa_77494_zip <- OOS_msa_top_5_zip %>%
  filter(zip_code == "77494") %>%
  dplyr::select(zip_code, stu_pct_white, pct_white, stu_pct_black, pct_black, stu_pct_asian, pct_asian, stu_pct_latinx, pct_latinx, stu_pct_nhpi, pct_nat_hi, stu_pct_natam, pct_nat_am) %>% head(n=1)

OOS_msa_77494_zip

# Table comparing race/ethnicity of students' purchased within 77479 zip code and the population demographics  

OOS_msa_77479_zip <- OOS_msa_top_5_zip %>%
  filter(zip_code == "77479") %>%
  dplyr::select(zip_code, stu_pct_white, pct_white, stu_pct_black, pct_black, stu_pct_asian, pct_asian, stu_pct_latinx, pct_latinx, stu_pct_nhpi, pct_nat_hi, stu_pct_natam, pct_nat_am) %>% head(n=1)

OOS_msa_77479_zip

# Table comparing race/ethnicity of students' purchased within 94582 zip code and the population demographics
OOS_msa_94582_zip <- OOS_msa_top_5_zip %>%
  filter(zip_code == "94582") %>%
  dplyr::select(zip_code, stu_pct_white, pct_white, stu_pct_black, pct_black, stu_pct_asian, pct_asian, stu_pct_latinx, pct_latinx, stu_pct_nhpi, pct_nat_hi, stu_pct_natam, pct_nat_am) %>% head(n=1)

OOS_msa_94582_zip

# Table comparing race/ethnicity of students' purchased within 77382 zip code and the population demographics
OOS_msa_77382_zip <- OOS_msa_top_5_zip %>%
  filter(zip_code == "77382") %>%
  dplyr::select(zip_code, stu_pct_white, pct_white, stu_pct_black, pct_black, stu_pct_asian, pct_asian, stu_pct_latinx, pct_latinx, stu_pct_nhpi, pct_nat_hi, stu_pct_natam, pct_nat_am) %>% head(n=1)

OOS_msa_77382_zip

# Table comparing race/ethnicity of students' purchased within 30024 zip code and the population demographics
OOS_msa_30024_zip <- OOS_msa_top_5_zip %>%
  filter(zip_code == "30024") %>%
  dplyr::select(zip_code, stu_pct_white, pct_white, stu_pct_black, pct_black, stu_pct_asian, pct_asian, stu_pct_latinx, pct_latinx, stu_pct_nhpi, pct_nat_hi, stu_pct_natam, pct_nat_am) %>% head(n=1)

OOS_msa_30024_zip

#============================================================================
#Student lists investigations of CA specific high schools
#============================================================================
# library
library(readxl)

# LOAD DATA
#-----------------------------------------------------------------------

# read in student list df of CA student purchases
df_sl_ca <- read_csv("data/145637_list_ca.csv")

# check that student list data is uniquely identified by Ref # given to each student
df_sl_ca %>%
  group_by(Ref) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

# read in ca DOE data on sat
df_sat_ca_20 <- read_xlsx("data/sat20.xlsx")  
df_sat_ca_19 <- read_xlsx("data/sat19.xlsx", skip = 5)

# check that DOE data on sat is uniquely identified by school or CDS code
df_sat_ca_20 %>%
  group_by(CDS) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

df_sat_ca_19 %>%
  group_by(CDS) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

# read in acs, zip-code-level data
acs_race_zipcode <- read_csv("data/acs_race_zipcode.csv")

# check that acs zip-code-level data is uniquely identified by zip-code
acs_race_zipcode %>%
  group_by(zipcode) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

# keep only CA zip-codes
acs_race_zipcode %>%
  count(state_fips_code) 

acs_race_ca_zipcode <- acs_race_zipcode %>%
  filter(state_fips_code == 6)

# read in nces high school data
hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c')) %>% 
  mutate(pct_poc = pct_black + pct_hispanic + pct_amerindian)

# check that nces data is uniquely identified by high school (ncessch)
hs_data %>%
  group_by(ncessch) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

# keep only schools in CA
hs_data %>%
  filter(state_code == "CA") %>%
  count()

hs_data %>%
  filter(state_code != "CA") %>%
  count()

hs_ca_data <- hs_data %>%
  filter(state_code == "CA")

# CHECK HIGH SCHOOLS IN DFS
#-----------------------------------------------------------------------

df_sl_ca %>%
  group_by(ncessch) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

#check unique schools
length(unique(df_sl_ca$ceeb)) #1034
length(unique(df_sl_ca$CDSCode)) #595
length(unique(df_sl_ca$ncessch)) #910


# create var for number of students purchased by highschool
df_sl_ca <- df_sl_ca %>%
  group_by(ncessch) %>%
  mutate(n_stu_nces = n()) %>%
  ungroup() 

# check to see how many schools from ca DOE match with student list
df_sl_ca %>%
  filter(CDSCode %in% df_sat_ca_20$CDS) #45,150

df_sl_ca %>%
  filter(CDSCode %in% df_sat_ca_19$CDS) #45,153

df_sl_ca %>%
  filter(is.na(CDSCode)) #7,616 NA

# MERGE DATA
#-----------------------------------------------------------------------

# merge DOE to CA student list data
df_sl_sat_CA <- df_sl_ca %>% rename(CDS=CDSCode) %>% left_join(df_sat_ca_19, by = "CDS")

# check obs that did not merge; PM comments: most are missing for CDS code
anti_merge <- df_sl_ca %>% rename(CDS=CDSCode) %>% anti_join(df_sat_ca_19, by = "CDS")

View(anti_merge %>% filter(!is.na(CDS)) %>% select(ncessch, CDS, City, State) %>% arrange(CDS))

    # 28662660128314, school closed https://napavalleyregister.com/community/eagle/news/local/school-board-approves-closure-of-legacy-high-school-in-american-canyon/article_6eadeed7-66cc-5444-a7fa-8c1a97a3fb7b.html
    # 30736503030467, school serves 40 students in grades K-12 https://www.publicschoolreview.com/alternative-education-san-joaquin-high-school-profile
    # 31668450121418, couldn't find school in CA DOE website https://www.cde.ca.gov/SchoolDirectory/districtschool?allSearch=31668450121418&simpleSearch=Y
    # 38684780128876, school closed https://www.cde.ca.gov/SchoolDirectory/details?cdscode=38684780128876
    # 39686500128215, school serves 30 students in grades 9-12 https://www.publicschoolreview.com/harvest-high-school-profile

# check to see how many schools from nces data match with student list
df_sl_sat_CA %>%
  filter(ncessch %in% hs_ca_data$ncessch) #49,969

df_sl_sat_CA %>%
  filter(is.na(ncessch)) #421

# create var for number of students purchased by CDS code
# removing all missing for nces and CDS: have to go back and check
df_sl_sat_CA <- df_sl_sat_CA %>%
  filter(!is.na(ncessch) & !is.na(CDS)) %>%
  group_by(CDS) %>%
  mutate(n_stu_cds = n()) %>%
  ungroup()

# compare nces and cds
df_sl_sat_CA %>%
  dplyr::select(n_stu_nces, n_stu_cds)


# check those that are not similar
df_sl_sat_CA %>%
  filter(n_stu_nces != n_stu_cds) %>%
  dplyr::select(n_stu_nces, n_stu_cds)

# merge NCES to CA student list & DOE data
df_sl_hs_sat_CA <- df_sl_sat_CA %>% left_join(hs_ca_data, by = "ncessch")

# check merge
anti_merge <- df_sl_sat_CA %>% anti_join(hs_ca_data, by = "ncessch")

anti_merge %>% filter(is.na(ncessch)) #421

View(anti_merge %>% filter(!is.na(ncessch)) %>% group_by(ncessch, CDS, City, State) %>% summarize(n_per_grp = n()))

anti_merge %>% filter(is.na(CDS)) #2080

View(anti_merge %>% filter(!is.na(ncessch) & !is.na(CDS)) %>% group_by(ncessch, CDS, City, State) %>% summarize(n_per_grp = n()))

    # 061062001176, multiple schools with same nces and CDS id?
    # 062271008887, Los Angeles Center for Enriched Studies (other/alternative)
    # 063441001276, Asawa (Ruth) SF Sch of the Arts A Public School (other/alternative)
    # 060141013692, Mountain House High (regular)

    # I guess for now, can just move on?


# RUN SOME CHECKS
#-----------------------------------------------------------------------
# student list + CA DOE + nces uniquely identified by Ref (student ID)
df_sl_hs_sat_CA %>%
  group_by(Ref) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

# check how many unique ncessch values
length(unique(df_sl_hs_sat_CA$ncessch)) #594
# check how many unique CDS values
length(unique(df_sl_hs_sat_CA$CDS)) #594
#names(df_sl_hs_sat_CA)

# check missing by column
sapply(df_sl_hs_sat_CA, function(x) sum(is.na(x)))


# EDA for all CA schools purchased from Urbana
#-----------------------------------------------------------------------

# Subset to LA county schools only
df_la_county <- df_sl_hs_sat_CA %>% filter(CName == "Los Angeles")

# what are the schools with the most students purchased from Urbana and how do they perform on the exams?
View(df_la_county %>%
  group_by(ncessch) %>%
  filter(row_number(ncessch) == 1, !is.na(CDS)) %>%
  dplyr::select(ncessch, SName, n_stu_nces, Enroll12, NumTSTTakr12, NumERWBenchmark12, PctERWBenchmark12, NumMathBenchmark12, PctMathBenchmark12,
         Enroll12, NumTSTTakr11, NumERWBenchmark11, PctERWBenchmark11, NumMathBenchmark11, PctMathBenchmark11) %>% 
  arrange(-n_stu_nces)) 

# compare the race/ethnicity of students from purchased lists to that of the high school race/ethnicity composition
View(df_la_county %>% 
  group_by(Race) %>%
  summarise(n_per_grp = n())) 

# get count of hispanic
df_la_county %>%
  count(Hispanic)

# crosstab of hispanic & other race/ethnicities
View(df_la_county %>%
  group_by(Race) %>%
    filter(Hispanic == "Yes") %>%
    count(Hispanic))


df_sl_la_race <- df_la_county %>%
  group_by(ncessch) %>%
  filter(!is.na(ncessch), !is.na(CDS)) %>%
  mutate(white = ifelse(Race == "White" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         black = ifelse(Race == "Black or African American" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         asian = ifelse(Race == "Asian" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         latinx = ifelse((!is.na(Hispanic) & Hispanic == "Yes"),1,0),
         nhpi = ifelse(Race == "Native Hawaiian or Other Pacific" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         nat_am = ifelse(Race == "American Indian or Alaska Native" &
                           (is.na(Hispanic) | Hispanic == "No"),1,0),
         other = ifelse(is.na(Race) & (is.na(Hispanic) | Hispanic == "No"),1,0),
         stu_pct_white = mean(white, na.rm=TRUE)*100, #pct race/ethnicty for student-level data
         stu_pct_black = mean(black, na.rm=TRUE)*100,
         stu_pct_asian = mean(asian, na.rm=TRUE)*100,
         stu_pct_latinx = mean(latinx, na.rm=TRUE)*100,
         stu_pct_nhpi = mean(nhpi, na.rm=TRUE)*100,
         stu_pct_natam = mean(nat_am, na.rm=TRUE)*100,
         stu_pct_other = mean(other, na.rm=TRUE)*100,
         stu_pct_tot = stu_pct_white + stu_pct_black + stu_pct_asian + stu_pct_latinx + stu_pct_nhpi + stu_pct_natam + stu_pct_other)


View(df_sl_la_race %>%
       group_by(ncessch) %>%
       filter(row_number(ncessch) == 1) %>%
       arrange(-n_stu_nces) %>%
       select(ncessch, n_stu_nces, SName, stu_pct_white, pct_white, stu_pct_black, pct_black ,stu_pct_asian, pct_asian ,stu_pct_latinx, pct_hispanic, stu_pct_natam, pct_amerindian, stu_pct_other, pct_other))

# check that percent race/ethnicity for a school match up
df_sl_hs_sat_CA %>%
  filter(ncessch == "064128007894") %>%
  group_by(Race) %>%
  count(Hispanic)

# get a count by graduating high school class
# could we filter sl data by one year?
# capture SAT data from test takers from CA DOE (e.g., orders made in 2017 & CA DOE data from 2019-2020)
    # maybe go back 1-2 years from CA DOE (e.g., 2018-2019)
    # seniors in 2019, juniors in 2020
# compare sat ranges they purchased vs. how students in the high school perform
# avg enrollment at this school from 11-12 graders 
# look at order summary

# Aggregate high schools to the zip-code level 
#-----------------------------------------------------------------------
# data frame for schools in LA county
df_la_zipcode <- df_la_county %>%
  group_by(zip_code) %>%
  summarise(n_per_grp = n()) 



# Compare characteristics of zipcodes purchased to those that were not purchased for 
# the LA msa area 
#-----------------------------------------------------------------------

# use student list data
df_sl_ca <- read_csv("data/145637_list_ca.csv")

#uniquely identified by student Ref ID
df_sl_ca %>%
  group_by(Ref) %>%
  summarise(n_per_grp = n()) %>%
  count(n_per_grp)

# read in acs, zip-code-level data
acs_race_zipcode <- read_csv("data/acs_race_zipcode.csv")

# uniquely identified by zip code
acs_race_zipcode %>%
  group_by(zipcode) %>%
  summarise(n_per_grp = n()) %>%
  count(n_per_grp)


# zip cbsa data
zip_cbsa_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_code_cbsa.csv'))

# grab zip codes in LA msa
la_zip_codes <- (zip_cbsa_data %>% filter(cbsa_1 == '31080'))$zip_code

# create a new 5-digit zip code and filter to LA msa for student list purchases
df_sl_la <- df_sl_ca %>%
  mutate(zipcode = str_pad(str_sub(ZipCode, 1, 5), width = 5, pad = '0', side = 'left')) %>%
  filter(zipcode %in% la_zip_codes)

# check
df_sl_la %>%
  group_by(Ref) %>%
  summarize(n_per_grp = n()) %>%
  count(n_per_grp)

# first subset df and then merge in census data to student list data

# get rid of SAT/ACT test scores for now
df_sl_la <- df_sl_la %>% dplyr::select(-contains("sat"), -contains("act"), -(Source:CDSCode))
length(unique(df_sl_la$zipcode)) #313 zip codes, according to google there are 378 zip codes in LA metro area

df_sl_la %>%
  group_by(zipcode) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

# create df to check work
stu_zip <- df_sl_la %>%
  group_by(zipcode) %>%
  summarise(n_per_grp = n()) %>%
  arrange(-n_per_grp)

# create 0/1 variable of race/ethnicity
View(df_sl_la %>% 
       group_by(Race) %>%
       summarise(n_per_grp = n())) 

# get count of hispanic
df_sl_la %>%
  count(Hispanic)

# crosstab of hispanic & other race/ethnicities
View(df_sl_la %>%
       group_by(Race) %>%
       filter(Hispanic == "Yes") %>%
       count(Hispanic))

df_sl_la_race <- df_sl_la %>%
  mutate(white = ifelse(Race == "White" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         black = ifelse(Race == "Black or African American" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         asian = ifelse(Race == "Asian" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         latinx = ifelse((!is.na(Hispanic) & Hispanic == "Yes"),1,0),
         nhpi = ifelse(Race == "Native Hawaiian or Other Pacific" & (is.na(Hispanic) | Hispanic == "No"),1,0),
         nat_am = ifelse(Race == "American Indian or Alaska Native" &
                           (is.na(Hispanic) | Hispanic == "No"),1,0),
         other = ifelse(is.na(Race) & (is.na(Hispanic) | Hispanic == "No"),1,0)) 

# aggregate to the zip code level
df_sl_la_race <- df_sl_la_race %>%
  group_by(zipcode) %>%
  summarise(n_stu_zip = n(), #not accurate because doesn't account for all race/ethnicities of students
            tot_white = sum(white, na.rm = TRUE),
            stu_pct_white = mean(white, na.rm=TRUE)*100, #pct race/ethnicty for student-level data
            tot_black = sum(black, na.rm = TRUE),
            stu_pct_black = mean(black, na.rm=TRUE)*100,
            tot_asian = sum(asian, na.rm = TRUE),
            stu_pct_asian = mean(asian, na.rm=TRUE)*100,
            tot_latinx = sum(latinx, na.rm = TRUE),
            stu_pct_latinx = mean(latinx, na.rm=TRUE)*100,
            tot_nhpi = sum(nhpi, na.rm = TRUE),
            stu_pct_nhpi = mean(nhpi, na.rm=TRUE)*100,
            tot_natam = sum(nat_am,na.rm = TRUE),
            stu_pct_natam = mean(nat_am, na.rm=TRUE)*100,
            tot_other = sum(other, na.rm = TRUE),
            stu_pct_other = mean(other, na.rm=TRUE)*100,
            stu_pct_tot = stu_pct_white + stu_pct_black + stu_pct_asian + stu_pct_latinx + stu_pct_nhpi + stu_pct_natam + stu_pct_other) %>%
  arrange(-n_stu_zip)


# keep only LA msa zip codes from acs data
acs_race_la_zipcode <- acs_race_zipcode %>%
  filter(zipcode %in% la_zip_codes) # 378 zip codes in LA msa

typeof(acs_race_la_zipcode$zipcode) #double
typeof(df_sl_la$zipcode) #character

# change to character
acs_race_la_zipcode$zipcode <- as.character(acs_race_la_zipcode$zipcode)

df_sl_la_acs <- df_sl_la_race %>% right_join(acs_race_la_zipcode, by = "zipcode")

length(unique(df_sl_la_acs$zipcode)) #378 

# check
anti_merge <- acs_race_la_zipcode %>% anti_join(df_sl_la_race, by = "zipcode")

length(unique(anti_merge$zipcode)) 


# check zipcodes that were not purchased
#----------------------------------------------------------------------------------------------
df_zip_non_purchase <- df_sl_la_acs %>%
  filter(zipcode %in% anti_merge$zipcode) %>%
  dplyr::select(zipcode, median_household_income, contains("15_19_pct")) %>%
  arrange(-median_household_income)


# Comments
# 90743 is zip code for Seal Beach, in Orange County
# 92678 is zip code for Trabuco Canyon in Orange County (small unincorporated community)
# 90713 is zip code for Lakewood close to Cerritos and Long Beach
# 90746 is zip code for Carson, CA (interesting because 55% of 15-19 year olds are Black)
# 90305 is zip code for Inglewood, CA (80K median income & 74% Black students)
# 91722 is zip code for Covina, CA (80K median income & large Hispanic population of 15-19 year olds)

# General observations
# Some zipcodes with relatively higher income and higher percent of POCs are not getting purchased
# As you go further down, zipcodes with lower median income tend to have more POCs and not likely to get visited

# check zipcodes that were purchased
#----------------------------------------------------------------------------------------------
df_zip_purchase <- df_sl_la_acs %>%
  filter(!is.na(n_stu_zip)) %>%
  dplyr::select(zipcode, n_stu_zip, -contains("tot_"), median_household_income, contains("stu_pct"), contains("15_19_pct"))
  
# Most students purchased in a zip code identify as asian. Higher percent of students purchased that identify as asian
  # compared to the percent of asain people between the ages of 15-19
# 90056 zip code for Windsor Hills (Ladera Heights), median income of 93k, about 75% Black and only one student purchased
# Seems to be more mixed for Latinx students, for example, zip codes with large percent of latinx students tend to have more
  # students that identify as Latinx purchased, although this is not always the case. And the number of students purchased from 
  # these zipcodes are a lot lower <6
# Zip codes that have a higher percentage of Asian students between the ages of 15-19, also have more asian students purchased
  # and a lot more student purchased by zip code 200>
# Zip codes that have a higher percentage of White students between the ages of 15-19, also tend to have more white students purchased
  # although not as much as the number of asian students purchased by zip code 100>

# map zip codes purchased and not purchased in LA metro area
#----------------------------------------------------------------------------------------------
#load msa data
msa_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/msa_metadata.csv'), na = c('', 'NULL')) %>% 
  mutate(pop_poc_pct = pop_black_pct + pop_hispanic_pct + pop_amerindian_pct)

# subset to LA metro area
msa_data <- msa_data %>% filter(cbsa_code == 31080)

hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c')) %>% 
  mutate(pct_poc = pct_black + pct_hispanic + pct_amerindian)

# Load shape files: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
cbsa_shp <- readOGR(file.path(data_dir, 'cb_2018_us_cbsa_500k', 'cb_2018_us_cbsa_500k.shp'))
state_shp <- readOGR(file.path(data_dir, 'cb_2018_us_state_500k', 'cb_2018_us_state_500k.shp'))
zip_shp <- readOGR(file.path(data_dir, 'cb_2018_us_zcta510_500k', 'cb_2018_us_zcta510_500k.shp'))

# Create var for race breaks
la_zip_data <- df_sl_la_acs %>%
  mutate(pop15_19_poc_pct = pop_black_15_19_pct + pop_hispanic_15_19_pct + pop_amerindian_15_19_pct + pop_nativehawaii_15_19_pct )
  

la_zip_data$race_brks_nonwhiteasian <- cut(la_zip_data$pop15_19_poc_pct, 
                   breaks = c(-1, 20, 40, 60, 80, 90, 101), 
                   labels = c('0-19%', '20-39%', '40-59%', 
                              '60-79%', '80-89%', '90-100%'))

# create var for race by high school
hs_data$race_brks_nonwhiteasian <- cut(hs_data$pct_poc, 
                                       breaks = c(-1, 20, 40, 60, 80, 90, 101), 
                                       labels = c('0-19%', '20-39%', '40-59%', 
                                                  '60-79%', '80-89%', '90-100%'))

# Create var for income breaks
la_zip_data$inc_brks <- cut(la_zip_data$median_household_income, 
                         breaks = c(-1, 50000, 75000, 100000, 150000, 200000, 10000000), 
                         labels = c('<$50k', '$50k-74k', '$75k-99k', 
                                    '$100k-149k', '$150k-199k', '$200k+'))


cbsa_shp <- merge(cbsa_shp, msa_data, by.x = 'GEOID', by.y = 'cbsa_code', all.x = T)
zip_shp <- merge(zip_shp, la_zip_data, by.x = 'ZCTA5CE10', by.y = 'zipcode', all.x = T)

#la msa area
la_msa_shp <- subset(cbsa_shp, GEOID %in% msa_data$cbsa_code)

# grab zip codes in LA metro area
la_zips_shp <- subset(zip_shp, ZCTA5CE10 %in% la_zip_codes)
# grab CA state
CA_state_shp <- subset(state_shp, STUSPS == 'CA')

CA_hs <- hs_data %>% filter(state_code == 'CA')


# Create shared color scale functions
color_income <- colorFactor('YlGnBu', la_zip_data$inc_brks)
color_race <- colorFactor('YlGnBu', la_zip_data$race_brks_nonwhiteasian)
color_pop <- colorNumeric('YlGnBu', la_zip_data$pop_total_15_19, n = 5)

# Create popups
pop_zip <- paste0('<b>', la_zips_shp$ZCTA5CE10, '</b><br>',
                  'Total Population: ', format(la_zips_shp$pop_total_15_19, big.mark = ',')) %>% lapply(htmltools::HTML)

income_zip <- paste0('<b>', la_zips_shp$ZCTA5CE10, '</b><br>',
                     'Median Household Income: ', currency(la_zips_shp$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)

race_zip <- paste0('<b>', la_zips_shp$ZCTA5CE10, '</b><br>',
                   '% Population of Color: ', sprintf('%.1f', la_zips_shp$race_brks_nonwhiteasian)) %>% lapply(htmltools::HTML)


highlight_zip <- highlightOptions(color = 'black',
                                  bringToFront = F)

# Create map
map_CA <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  addMiniMap(tiles = providers$CartoDB.Positron,
             toggleDisplay = TRUE) %>%
  
  addPolygons(data = CA_state_shp, stroke = F, fillOpacity = 0.1, smoothFactor = 0.2, color = 'gray', group = 'CA') %>% 
  addPolygons(data = raster::intersect(CA_state_shp, la_zips_shp), weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_pop(pop_total), label = pop_zip, group = 'CA by Population', highlightOptions = highlight_zip) %>%
  addPolygons(data = raster::intersect(CA_state_shp, la_zips_shp), weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = income_zip, group = 'CA by Median Household Income', highlightOptions = highlight_zip) %>%
  addPolygons(data = raster::intersect(CA_state_shp, la_zips_shp), weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks_nonwhiteasian), label = race_zip, group = 'CA by Race/Ethnicity', highlightOptions = highlight_zip) %>%
  addPolylines(data = raster::intersect(CA_state_shp, la_zips_shp), weight = 1, color = 'black', fillOpacity = 0, group = 'Zip codes') %>% 
  
  # add markers
  addCircleMarkers(data = CA_hs, lng = ~longitude, lat = ~latitude, group = 'CA HS by Race/Ethnicity',
                   radius = 3, fill = TRUE, fillOpacity = 1, weight = 1, color = 'white', fillColor = ~color_race(race_brks_nonwhiteasian)) %>%
  
  # add legends
  addLegend(data = la_zips_shp,
            position = 'topright', pal = color_pop, values = ~pop_total_15_19,
            title = 'Population',
            className = 'info legend legend-pop',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(data = la_zips_shp,
            position = 'topright', pal = color_income, values = ~inc_brks,
            title = 'Median Household Income',
            className = 'info legend legend-income',
            na.label = 'NA',
            opacity = 1) %>%
  
  addLegend(data = la_zips_shp,
            position = 'topright', pal = color_race, values = ~race_brks_nonwhiteasian,
            title = 'Black, Latinx, and <br>Native American Population',
            className = 'info legend legend-race',
            na.label = 'NA',
            opacity = 1) %>%
  
  # add options
  addLayersControl(
    position = c('bottomleft'),
    baseGroups = c('CA', 'CA by Population', 'CA by Median Household Income', 'CA by Race/Ethnicity'),
    overlayGroups = c('Purchased Zip codes', 'CA HS by Race/Ethnicity'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  hideGroup('CA HS by Race/Ethnicity') %>% 
  
  htmlwidgets::onRender("
                        function(el, x) {
                        var myMap = this;
                        $('.legend').css('display', 'none');
                        
                        myMap.on('baselayerchange', function(e) {
                        $('.legend').css('display', 'none');
                        switch(e.name) {
                        case 'CA by Population':
                        $('.legend-pop').css('display', 'inherit');
                        break;
                        case 'CA by Median Household Income':
                        $('.legend-income').css('display', 'inherit');
                        break;
                        case 'CA by Race/Ethnicity':
                        $('.legend-race').css('display', 'inherit');
                        break;
                        }
                        e.layer.bringToBack();
                        });
                        
                        myMap.on('overlayadd', function(e) {
                        if (e.name === 'CA HS by Race/Ethnicity') {
                        $('.legend-race').css('display', 'inherit');
                        }
                        });
                        
                        myMap.on('overlayremove', function(e) {
                        if (e.name === 'CA HS by Race/Ethnicity' && $('.leaflet-control-layers-base input[type=radio]:checked~span').text().trim() !== 'CA by Race/Ethnicity') {
                        $('.legend-race').css('display', 'none');
                        }
                        });
                        }")

# saveWidget(map_IL, '~/Downloads/map_IL.html', background = 'transparent')
