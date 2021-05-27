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
df_sat_ca <- read_xlsx("data/sat20.xlsx")

# check that DOE data on sat is uniquely identified by school or CDS code
df_sat_ca %>%
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
  mutate(n_stu_hs = n()) %>%
  ungroup()

# check to see how many schools from ca DOE match with student list
df_sl_ca %>%
  filter(CDSCode %in% df_sat_ca$CDS) #45,150

df_sl_ca %>%
  filter(is.na(CDSCode)) #7,616 NA

# MERGE DATA
#-----------------------------------------------------------------------

# merge DOE to CA student list data
df_sl_sat_CA <- df_sl_ca %>% rename(CDS=CDSCode) %>% left_join(df_sat_ca, by = "CDS")

# check obs that did not merge; PM comments: most are missing for CDS code
anti_merge <- df_sl_ca %>% rename(CDS=CDSCode) %>% anti_join(df_sat_ca, by = "CDS")

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

# merge NCES to CA student list & DOE data
df_sl_hs_sat_CA <- df_sl_sat_CA %>% left_join(hs_ca_data, by = "ncessch")

# check merge
anti_merge <- df_sl_sat_CA %>% anti_join(hs_ca_data, by = "ncessch")

anti_merge %>% filter(is.na(ncessch)) #421

View(anti_merge %>% filter(!is.na(ncessch)) %>% group_by(ncessch, CDS, City, State) %>% summarize(n_per_grp = n()))

anti_merge %>% filter(is.na(CDS)) #2080

View(anti_merge %>% filter(!is.na(ncessch) & !is.na(CDS)) %>% group_by(ncessch, CDS, City, State) %>% summarize(n_per_grp = n()))

    # 061062001176, Davis Senior High (regular)
    # 062271008887, Los Angeles Center for Enriched Studies (other/alternative)
    # 063441001276, Asawa (Ruth) SF Sch of the Arts A Public School (other/alternative)
    # 060141013692, Mountain House High (regular)

    # I guess for now, can just move on?

# RUN SOME CHECKS
#-----------------------------------------------------------------------

