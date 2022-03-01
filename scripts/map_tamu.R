library(leaflet)
library(rgdal)
library(rgeos)
library(formattable)
library(tidyverse)
library(htmlwidgets)


data_dir <- file.path('.', 'data')
outputs_dir <- file.path('.', 'outputs', 'maps')


# Load data

load(file.path(data_dir, 'map_data.RData'))

zip_data <- acs_race_zipcodev3 %>% 
  select(
    zip_code, state_code, cbsa_1, cbsa_1_ratio, cbsatitle_1,
    pop_total, median_household_income, ends_with('_15_19_pct')
  ) %>% 
  mutate(
    inc_brks = cut(
      median_household_income,
      breaks = c(-1, 50000, 75000, 100000, 150000, 200000, 10000000),
      labels = c('<$50k', '$50k-74k', '$75k-99k',  '$100k-149k', '$150k-199k', '$200k+')
    ),
    # Note: pop_native_15_19 = pop_amerindian_15_19 + pop_nativehawaii_15_19
    pop_poc_15_19_pct = pop_black_15_19_pct + pop_hispanic_15_19_pct + pop_native_15_19_pct,
    race_brks = cut(
      pop_poc_15_19_pct,
      breaks = c(-1, 20, 40, 60, 80, 90, 101),
      labels = c('0-19%', '20-39%', '40-59%', '60-79%', '80-89%', '90-100%')
    )
  )

zip_shp <- readOGR(file.path(data_dir, 'cb_2018_us_zcta510_500k', 'cb_2018_us_zcta510_500k.shp')) %>% 
  merge(zip_data, by.x = 'ZCTA5CE10', by.y = 'zip_code', all.x = T)


# Define function

create_metro_map <- function(cbsa_code, hs_df) {
  
  cbsa_zip_shps <- subset(zip_shp, ZCTA5CE10 %in% (zip_data %>% filter(cbsa_1 == cbsa_code))$zip_code)
  cbsa_outline <- gUnaryUnion(cbsa_zip_shps, id = NULL)
  
  hs_data <- hs_df %>% 
    select(
      school_control, ncessch, name, state_code, zip_code, latitude, longitude, pub_sch_type, priv_sch_type,
      total_students, total_amerindian, total_asian, total_black, total_hispanic, total_nativehawaii, total_tworaces, total_unknown, total_white, total_native,
      pct_amerindian, pct_asian, pct_black, pct_hispanic, pct_nativehawaii, pct_tworaces, pct_unknown, pct_white, pct_native,
      starts_with('stu_race_')
    ) %>% 
    mutate(
      pub_sch_type = recode(
        pub_sch_type,
        `1` = 'Regular',
        `2` = 'Special education',
        `3` = 'Career and technical',
        `4` = 'Alternative education'
      ),
      priv_sch_type = recode(
        priv_sch_type,
        `1` = 'Regular',
        `2` = 'Montessori',
        `3` = 'Special program emphasis',
        `4` = 'Special education',
        `5` = 'Career/technical/vocational',
        `6` = 'Alternative/other',
        `7` = 'Early childhood program/child care center'
      ),
      pct_poc = pct_black + pct_hispanic + pct_native,
      race_brks = cut(
        pct_poc,
        breaks = c(-1, 20, 40, 60, 80, 90, 101),
        labels = c('0-19%', '20-39%', '40-59%', '60-79%', '80-89%', '90-100%')
      ),
      stu_race_native = stu_race_aian + stu_race_nhpi,
      stu_race_poc = stu_race_black + stu_race_latinx + stu_race_native,
      stu_total = stu_race_noresponse + stu_race_missing + stu_race_aian + stu_race_asian + stu_race_black + stu_race_latinx + stu_race_nhpi + stu_race_white + stu_race_other + stu_race_multi
    )
  
  privhs_data <- hs_data %>% filter(school_control == 'private')
  pubhs_data <- hs_data %>% filter(school_control == 'public')
  
  # color scales
  color_income <- colorFactor('YlGnBu', zip_data$inc_brks)
  color_race <- colorFactor('YlGnBu', zip_data$race_brks)
  color_pop <- colorNumeric('YlGnBu', cbsa_zip_shps$pop_total, n = 5)
  
  # popup labels
  pop_msa <- paste0('<b>ZCTA5 ', cbsa_zip_shps$ZCTA5CE10, '</b><br>',
                    'Total Population: ', format(cbsa_zip_shps$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)
  
  income_msa <- paste0('<b>ZCTA5 ', cbsa_zip_shps$ZCTA5CE10, '</b><br>',
                       'Median Household Income: ', currency(cbsa_zip_shps$median_household_income, digits = 0L)) %>% lapply(htmltools::HTML)
  
  race_msa <- paste0('<b>ZCTA5 ', cbsa_zip_shps$ZCTA5CE10, '</b><br>',
                     '% 15-19yo Population of Color: ', sprintf('%.1f', cbsa_zip_shps$pop_poc_15_19_pct)) %>% lapply(htmltools::HTML)
  
  popup_privhs <- paste0('<b>',privhs_data$name,'</b><br>', 
                         '<b>School Type</b>: ', privhs_data$priv_sch_type, '<br><br>',
                         '<b>Total Enrollment</b>: ', format(privhs_data$total_students, big.mark = ','), '<br>',
                         '% Black: ', sprintf('%.1f', privhs_data$pct_black), '<br>',
                         '% Latinx: ', sprintf('%.1f', privhs_data$pct_hispanic), '<br>',
                         '% Native: ', sprintf('%.1f', privhs_data$pct_native), '<br>',
                         '% Asian: ', sprintf('%.1f', privhs_data$pct_asian), '<br>',
                         '% White: ', sprintf('%.1f', privhs_data$pct_white), '<br><br>',
                         '<b>Total Purchased</b>: ', format(privhs_data$stu_total, big.mark = ','), '<br>',
                         '# Black: ', privhs_data$stu_race_black, '<br>',
                         '# Latinx: ', privhs_data$stu_race_latinx, '<br>',
                         '# Native: ', privhs_data$stu_race_native, '<br>',
                         '# Asian: ', privhs_data$stu_race_asian, '<br>',
                         '# White: ', privhs_data$stu_race_white)
  
  popup_pubhs <- paste0('<b>',pubhs_data$name,'</b><br>', 
                        '<b>School Type</b>: ', pubhs_data$pub_sch_type, '<br><br>',
                        '<b>Total Enrollment</b>: ', format(pubhs_data$total_students, big.mark = ','), '<br>',
                        '% Black: ', sprintf('%.1f', pubhs_data$pct_black), '<br>',
                        '% Latinx: ', sprintf('%.1f', pubhs_data$pct_hispanic), '<br>',
                        '% Native: ', sprintf('%.1f', pubhs_data$pct_native), '<br>',
                        '% Asian: ', sprintf('%.1f', pubhs_data$pct_asian), '<br>',
                        '% White: ', sprintf('%.1f', pubhs_data$pct_white), '<br><br>',
                        '<b>Total Purchased</b>: ', format(pubhs_data$stu_total, big.mark = ','), '<br>',
                        '# Black: ', pubhs_data$stu_race_black, '<br>',
                        '# Latinx: ', pubhs_data$stu_race_latinx, '<br>',
                        '# Native: ', pubhs_data$stu_race_native, '<br>',
                        '# Asian: ', pubhs_data$stu_race_asian, '<br>',
                        '# White: ', pubhs_data$stu_race_white)
  
  highlight_shp <- highlightOptions(fillOpacity = 0.5, bringToFront = F)  # highlightOptions(color = 'black', bringToFront = T, sendToBack = T)
  
  # mapping
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addMiniMap(tiles = providers$CartoDB.Positron,
               toggleDisplay = T) %>%
    
    addPolygons(data = cbsa_outline, weight = 2, stroke = T, fillOpacity = 0.2,  color = 'black', group = 'MSA') %>% 
    # addPolygons(data = cbsa_zip_shps, weight = 1, stroke = T, fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pop(pop_total), label = pop_msa, group = 'MSA by Population', highlightOptions = highlight_shp) %>% 
    addPolygons(data = cbsa_zip_shps, stroke = F, fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pop(pop_total), label = pop_msa, group = 'MSA by Population', highlightOptions = highlight_shp) %>% 
    addPolygons(data = cbsa_zip_shps, stroke = F, fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_income(inc_brks), label = income_msa, group = 'MSA by Median Household Income', highlightOptions = highlight_shp) %>% 
    addPolygons(data = cbsa_zip_shps, stroke = F, fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_race(race_brks), label = race_msa, group = 'MSA by Race/Ethnicity', highlightOptions = highlight_shp) %>% 
    
    # add markers
    addCircleMarkers(data = pubhs_data, lng = ~longitude, lat = ~latitude, group = 'Public High Schools',
                     radius = ~sqrt(stu_total) + 2, fillOpacity = 0.2, fillColor = 'white', opacity = 1, weight = 1.5, color = ~color_race(race_brks),
                     popup = popup_pubhs) %>%
    
    addCircleMarkers(data = privhs_data, lng = ~longitude, lat = ~latitude, group = 'Private High Schools',
                     radius = ~sqrt(stu_total) + 2, fillOpacity = 0.2, fillColor = 'white', opacity = 1, weight = 1.5, color = ~color_race(race_brks),
                     popup = popup_privhs) %>%
    
    # add legends
    addLegend(data = cbsa_zip_shps,
              position = 'topright', pal = color_pop, values = ~pop_total,
              title = 'Population',
              className = 'info legend legend-pop',
              na.label = 'NA',
              opacity = 1) %>%
    
    addLegend(data = zip_shp,
              position = 'topright', pal = color_income, values = ~inc_brks,
              title = 'Median Household Income',
              className = 'info legend legend-income',
              na.label = 'NA',
              opacity = 1) %>%
    
    addLegend(data = zip_shp,
              position = 'topright', pal = color_race, values = ~race_brks,
              title = 'Black, Latinx, and <br>Native American Population',
              className = 'info legend legend-race',
              na.label = 'NA',
              opacity = 1) %>%
    
    # add options
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('MSA', 'MSA by Population', 'MSA by Median Household Income', 'MSA by Race/Ethnicity'),
      overlayGroups = c('Public High Schools', 'Private High Schools'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    hideGroup('Public High Schools') %>%
    hideGroup('Private High Schools') %>%
    
    htmlwidgets::onRender("
        function(el, x) {
            var myMap = this;
            $('.legend').css('display', 'none');
            
            myMap.on('baselayerchange', function(e) {
                $('.legend').css('display', 'none');
                switch(e.name) {
                    case 'MSA by Population':
                        $('.legend-pop').css('display', 'inherit');
                        break;
                    case 'MSA by Median Household Income':
                        $('.legend-income').css('display', 'inherit');
                        break;
                    case 'MSA by Race/Ethnicity':
                        $('.legend-race').css('display', 'inherit');
                        break;
                }
                e.layer.bringToBack();
            });
    }")
}


# Save maps

saveWidget(create_metro_map('26420', houston_pubprivhs), file.path(normalizePath(outputs_dir), 'map_houston.html'), background = 'transparent', selfcontained = T)
saveWidget(create_metro_map('31080', la_pubprivhs), file.path(normalizePath(outputs_dir), 'map_la.html'), background = 'transparent', selfcontained = T)
