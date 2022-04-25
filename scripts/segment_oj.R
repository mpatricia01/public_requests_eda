### Settings
rm(list = ls())
options(max.print=1000)

### Libraries

library(tidyverse)
library(lubridate)
library(labelled)
library(tidyr)
library(stringr)
library(eatATA)
library(readxl)
library(usmap)
library(haven)

################### OPEN DATA BY OJ


# directory paths
    data_dir <- file.path('.', 'data')
    list.files(path = data_dir)

    scripts_dir <- file.path('.', 'scripts')
    list.files(path = scripts_dir)
 
# source files for data    
    # Run script that creates data frames from secondary data sources (e.g., ACS, NCES)
    source(file = file.path(scripts_dir, 'create_secondary_datasets.R'))
    
    # Run script that creates analysis data frames from order data and list data
    # NOTE: this script relies on data frames created by above create_secondary_datasets.R script
    source(file = file.path(scripts_dir, 'create_combined_order_list_analysis_datasets.R'))

    # Workaround to Crystal errors with Ozan's source script
    #save(lists_orders_zip_hs_df, file = file.path(data_dir, 'tbl_fig_listdata.RData'))    

################### FINAL SAMPLE FOR EMPIRICAL REPORT
    
    #remove extra dataframes
    rm(lists_orders_df, lists_orders_zip_df)
    
    #remove MN universities from ordersdf
    orders_df %>% count(univ_id, univ_name)
    orders_df <- orders_df %>% filter(univ_id!="174358" &  univ_id!="174075") 
    
    #remove MN universities from lists dfs
    lists_df %>% count(univ_id, univ_name)
    lists_orders_zip_hs_df %>% count(univ_id, univ_name)
    
    lists_df <- lists_df %>% filter(univ_id!="174358" &  univ_id!="174075") 
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% filter(univ_id!="174358" &  univ_id!="174075") 
    
   
    # Orders-- 14 universities (includes NAU)
    orders_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    # Orders-- 835 total orders
    orders_df %>% 
      summarise(n=n_distinct(order_num)) 
    
    # Lists-- 13 universities (don't have any lists for NAU)
    lists_orders_zip_hs_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    lists_orders_zip_hs_df %>% 
      summarise(n=n_distinct(ord_num)) 
    
    # Lists-- 596 total lists
    lists_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    # number of orders & lists for each university
    orders_df %>% count(univ_name, univ_id)
    lists_df %>% count(univ_name)
    
    
    # create regional versus research university according to our sample
    orders_df <- orders_df %>% mutate(
      univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                         univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                         univ_id=="228723" , "research", "regional"))

    orders_df %>% count(univ_name, univ_type)
    
    lists_df <- lists_df %>% mutate(
      univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                           univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                           univ_id=="228723" , "research", "regional"))
    
    lists_df %>% count(univ_name, univ_type)
    
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% mutate(
      univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                           univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                           univ_id=="228723" , "research", "regional"))
    
    lists_orders_zip_hs_df %>% count(univ_name, univ_type)    
    
    
################### ANALYSIS & VISUALS FOR RQ3: ZIP CODE & TEST SCORES--  SERGMENT + TEST SCORES UNIV OF ILLINOIS URBANA-CHAMPAIGN Example
          
        #create filter dummies for student list data
        lists_orders_zip_hs_df <- lists_orders_zip_hs_df  %>%
            mutate(
                filter_hsgrad_class = ifelse(!is.na(ord_hs_grad_class), 1, 0),
                filter_zip = ifelse(!is.na(ord_zip_code) | !is.na(ord_zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
                filter_states_fil = ifelse(!is.na(ord_state_name), 1, 0), 
                filter_cbsa = ifelse(!is.na(ord_cbsa_name), 1, 0), 
                filter_intl = ifelse(!is.na(ord_intl_region), 1, 0), 
                filter_segment = ifelse(!is.na(ord_segment), 1, 0), 
                filter_race = ifelse(!is.na(ord_race_ethnicity), 1, 0), 
                filter_gender = ifelse(!is.na(ord_gender), 1, 0), 
                filter_sat = ifelse((!is.na(ord_sat_score_min) | !is.na(ord_sat_score_max)), 1, 0), 
                filter_psat = ifelse((!is.na(ord_psat_score_min) | !is.na(ord_psat_score_max)), 1, 0), 
                filter_gpa = ifelse((!is.na(ord_gpa_low) | !is.na(ord_gpa_high)), 1, 0), 
                filter_rank = ifelse((!is.na(ord_rank_low) | !is.na(ord_rank_high)), 1, 0), 
                filter_geomarket = ifelse(!is.na(ord_geomarket), 1, 0))
    
###### CHECK ORDER CRITERIA
        
  orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% select(order_num,order_title) %>% print(n=30)
          
  # 5 orders, non-eng          
    #'371629'    OOS Non-ENG - Feb 2018       
    #'386335'    OOS Non-ENG - May 2018       
    #'403340'    OOS Non-ENG August 2018      
    #'456737'    OOS Non-ENG - Feb 2019       
    #'470283'    OOS NON ENG March 2019
  
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% select(order_num,order_title,num_students,date_start,hs_grad_class,state_name,cbsa_name,segment,sat_score_min,sat_score_max,psat_score_min,psat_score_max,gpa_low,gpa_high)
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% summarise(num_students=sum(num_students))
    
    
    
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(hs_grad_class)
    
    c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(state_name))[1,])  # all use the same states
    c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% distinct(state_name, .keep_all = FALSE))[1,])
    
    c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(cbsa_name))[,])
    c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% distinct(cbsa_name, .keep_all = FALSE))[1,])
    
    c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(segment))[1,])
    c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% distinct(segment, .keep_all = FALSE))[1,])
    
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(sat_score_min) # 4 orders use min of 1240; one uses min of 123-
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(sat_score_max) # all use max of 1450
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(psat_score_min)  # 4 orders use min of 1240; one uses min of 123-
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(psat_score_max) # all use max of 1450
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(gpa_low) # all use min of B-
    orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% count(gpa_high) # all use max of A+
  
  # 3 orders, non-eng regional msas
  #'483751'    Regional Counselor MSAs      
  #'500590'    OOS Regional MSAs            
  #'567376'    OOS Regional MSAs
  
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% select(order_num,order_title,num_students,date_start,hs_grad_class,state_name,cbsa_name,segment,sat_score_min,sat_score_max,psat_score_min,psat_score_max,gpa_low,gpa_high)
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% summarise(num_students=sum(num_students)) 
  
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(hs_grad_class)
    
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(state_name)
  
    
    c((orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(cbsa_name))[,])
    c((orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% distinct(cbsa_name, .keep_all = FALSE))[,])
    
    c((orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(segment))[,])
    c((orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% distinct(segment, .keep_all = FALSE))[,])
  
    
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(sat_score_min) # all 3 orders use min of 1240
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(sat_score_max) # all use max of 1450
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(psat_score_min)  # 2 orders use PSAT min of 1220 (orders '500590','567376' ); one uses min of 1240 (order '483751')
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(psat_score_max) # all use max of 1450
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(gpa_low) # all use min of B-
    orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% count(gpa_high) # all use max of A+  
    
  # 13 orders engineering orders
  
    #'456710'    OOS ENG Male - Feb 2019        
    #'371662'    OOS ENG Male - Feb 2018      
    #'371665'    OOS ENG Female - Feb 2018    
    #'386336'    OOS ENG Female - May 2018    
    #'386441'    OOS ENG Male - May 2018      
    #'403314'    OOS ENG Male - August 2018   
    #'403333'    OOS ENG Female - August 2018 
    #'469731'    OOS ENG Male - APRIL 2019    
    #'470123'    OOS ENG-Female New March 2019
    #'483702'    OOS Female ENG               
    #'483701'    OOS ENG 1400-1600            
    #'500494'    OOS Female ENG 2020          
    #'567377'    OOS Female ENG 2021          
  
    orders_df %>% filter(order_num %in% c('456710','371662','371665','386336','386441','403314','403333','469731','470123','483702','483701','500494','567377')) %>% select(order_num,order_title,num_students,date_start,hs_grad_class,state_name,cbsa_name,segment,sat_score_min,sat_score_max,psat_score_min,psat_score_max,gpa_low,gpa_high)
    orders_df %>% filter(order_num %in% c('456710','371662','371665','386336','386441','403314','403333','469731','470123','483702','483701','500494','567377')) %>% summarise(num_students=sum(num_students)) 
    
    
  #### analyzing 8 non-engineering orders
    
    # HS grad class
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(hs_grad_class)
    
    # state
      c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(state_name))[,])  # 5 oos non-eng orders, states always the same; the 3 "regional" orders state always NA
    
    # CBSA
      c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(cbsa_name))[,]) # 5 oos non-eng orders always use the same CBSA; 3 "regional" orders: '483751' uses one set of CBSA; c('500590','567376') use the same CBSAs
    
      #5 oos non-eng orders always use the same CBSA
        #[1] "IN - Indianapolis-Carmel-Anderson, IN|OH - Cincinnati, OH-KY-IN|MD - Baltimore-Columbia-Towson, MD|NC - Charlotte-Concord-Gastonia, NC-SC|
          # FL - Orlando-Kissimmee-Sanford, FL|KS - Kansas City, MO-KS|GA - Atlanta-Sandy Springs-Roswell, GA|CO - Denver-Aurora-Lakewood, CO|CO - Boulder, CO|
          #NJ - New York-Newark-Jersey City, NY-NJ-PA|MN - Minneapolis-St. Paul-Bloomington, MN-WI|VA - Washington-Arlington-Alexandria, DC-VA-MD-WV|
          #TN - Nashville-Davidson--Murfreesboro--Franklin, TN|IA - Iowa City, IA|WI - Madison, WI|FL - Miami-Fort Lauderdale-West Palm Beach, FL|IN - Bloomington, IN|
          #FL - Naples-Immokalee-Marco Island, FL|MI - Ann Arbor, MI|NJ - Philadelphia-Camden-Wilmington, PA-NJ-DE-MD|OH - Columbus, OH|
          #MD - Washington-Arlington-Alexandria, DC-VA-MD-WV|OH - Cleveland-Elyria, OH|IN - Lafayette-West Lafayette, IN|
          #MA - Boston-Cambridge-Newton, MA-NH|NY - New York-Newark-Jersey City, NY-NJ-PA|NC - Durham-Chapel Hill, NC|NC - Raleigh, NC|
          #DC - Washington-Arlington-Alexandria, DC-VA-MD-WV|FL - Tampa-St. Petersburg-Clearwater, FL|MD - Philadelphia-Camden-Wilmington, PA-NJ-DE-MD|
          #OH - Dayton, OH|WI - Milwaukee-Waukesha-West Allis, WI|PA - Philadelphia-Camden-Wilmington, PA-NJ-DE-MD|IA - Davenport-Moline-Rock Island, IA-IL|
          #MI - Detroit-Warren-Dearborn, MI|GA - Athens-Clarke County, GA|WA - Seattle-Tacoma-Bellevue, WA|NJ - Trenton, NJ|AZ - Phoenix-Mesa-Scottsdale, AZ"
      
      # order_num = 483751, order_title = Regional Counselor MSAs
        #[1] "FL - Orlando-Kissimmee-Sanford, FL|FL - Sebring, FL|GA - Warner Robins, GA|GA - Dalton, GA|CA - Bakersfield, CA|
          # TX - San Angelo, TX|TX - College Station-Bryan, TX|GA - Valdosta, GA|FL - Palm Bay-Melbourne-Titusville, FL|GA - Columbus, GA-AL|
          # FL - Port St. Lucie, FL|GA - Savannah, GA|CA - Visalia-Porterville, CA|CA - Santa Rosa, CA|CA - Chico, CA|GA - Macon-Bibb County, GA|
          # FL - Tampa-St. Petersburg-Clearwater, FL|GA - Chattanooga, TN-GA|GA - Brunswick, GA|FL - Gainesville, FL|TX - Midland, TX|
          # FL - Cape Coral-Fort Myers, FL|FL - Crestview-Fort Walton Beach-Destin, FL|FL - Punta Gorda, FL|CA - San Diego-Carlsbad, CA|
          # CA - Oxnard-Thousand Oaks-Ventura, CA|TX - Beaumont-Port Arthur, TX|GA - Albany, GA|GA - Hinesville, GA|
          # FL - North Port-Sarasota-Bradenton, FL|FL - Jacksonville, FL|TX - San Antonio-New Braunfels, TX|TX - Killeen-Temple, TX|
          # TX - Abilene, TX|GA - Atlanta-Sandy Springs-Roswell, GA|NJ - New York-Newark-Jersey City, NY-NJ-PA|FL - Ocala, FL|
          # NJ - Vineland-Bridgeton, NJ|FL - Deltona-Daytona Beach-Ormond Beach, FL|TX - Waco, TX|GA - Augusta-Richmond County, GA-SC|
          # TX - Sherman-Denison, TX|FL - Lakeland-Winter Haven, FL|CA - San Luis Obispo-Paso Robles-Arroyo Grande, CA|CA - Vallejo-Fairfield, CA|
          # TX - Texarkana, TX-AR|CA - San Francisco-Oakland-Hayward, CA|TX - Laredo, TX|FL - Homosassa Springs, FL|CA - Stockton-Lodi, CA|
          # CA - Modesto, CA|TX - Odessa, TX|TX - Lubbock, TX|CA - Riverside-San Bernardino-Ontario, CA|GA - Gainesville, GA|TX - Amarillo, TX|
          # GA - Rome, GA|CA - Sacramento--Roseville--Arden-Arcade, CA|FL - Tallahassee, FL|FL - Miami-Fort Lauderdale-West Palm Beach, FL|
          # FL - Naples-Immokalee-Marco Island, FL|NJ - Philadelphia-Camden-Wilmington, PA-NJ-DE-MD|FL - Sebastian-Vero Beach, FL|
          # CA - Santa Cruz-Watsonville, CA|CA - San Jose-Sunnyvale-Santa Clara, CA|CA - Madera, CA|NY - New York-Newark-Jersey City, NY-NJ-PA|
          # TX - Brownsville-Harlingen, TX|FL - The Villages, FL|CA - El Centro, CA|NJ - Trenton, NJ|TX - Corpus Christi, TX|
          # NJ - Atlantic City-Hammonton, NJ|TX - El Paso, TX|TX - McAllen-Edinburg-Mission, TX|FL - Panama City, FL|
          # TX - Houston-The Woodlands-Sugar Land, TX|CA - Santa Maria-Santa Barbara, CA|CA - Hanford-Corcoran, CA|CA - Salinas, CA|
          # CA - Yuba City, CA|CA - Fresno, CA|NJ - Ocean City, NJ|FL - Pensacola-Ferry Pass-Brent, FL|CA - Los Angeles-Long Beach-Anaheim, CA|
          # TX - Dallas-Fort Worth-Arlington, TX|TX - Wichita Falls, TX|CA - Redding, CA|TX - Longview, TX|TX - Austin-Round Rock, TX|
          # CA - Napa, CA|TX - Tyler, TX|CA - Merced, CA|TX - Victoria, TX|GA - Athens-Clarke County, GA|NJ - Allentown-Bethlehem-Easton, PA-NJ"      
      
      # order_num = c('500590','567376'), order_title = Regional MSAs
        #[2] "NY - Syracuse, NY|FL - Orlando-Kissimmee-Sanford, FL|FL - Sebring, FL|GA - Warner Robins, GA|GA - Dalton, GA|CA - Bakersfield, CA|
        # TX - San Angelo, TX|TX - College Station-Bryan, TX|GA - Valdosta, GA|FL - Palm Bay-Melbourne-Titusville, FL|GA - Columbus, GA-AL|
        # FL - Port St. Lucie, FL|GA - Savannah, GA|CA - Visalia-Porterville, CA|NY - Rochester, NY|CA - Santa Rosa, CA|CA - Chico, CA|
        # GA - Macon-Bibb County, GA|FL - Tampa-St. Petersburg-Clearwater, FL|GA - Chattanooga, TN-GA|GA - Brunswick, GA|FL - Gainesville, FL|
        # TX - Midland, TX|FL - Cape Coral-Fort Myers, FL|FL - Crestview-Fort Walton Beach-Destin, FL|FL - Punta Gorda, FL|
        # CA - San Diego-Carlsbad, CA|CA - Oxnard-Thousand Oaks-Ventura, CA|TX - Beaumont-Port Arthur, TX|GA - Albany, GA|GA - Hinesville, GA|
        # FL - North Port-Sarasota-Bradenton, FL|FL - Jacksonville, FL|TX - San Antonio-New Braunfels, TX|TX - Killeen-Temple, TX|TX - Abilene, TX|
        # GA - Atlanta-Sandy Springs-Roswell, GA|NJ - New York-Newark-Jersey City, NY-NJ-PA|FL - Ocala, FL|NJ - Vineland-Bridgeton, NJ|
        # FL - Deltona-Daytona Beach-Ormond Beach, FL|TX - Waco, TX|NY - Buffalo-Cheektowaga-Niagara Falls, NY|GA - Augusta-Richmond County, GA-SC|
        # TX - Sherman-Denison, TX|NY - Glens Falls, NY|FL - Lakeland-Winter Haven, FL|CA - San Luis Obispo-Paso Robles-Arroyo Grande, CA|
        # CA - Vallejo-Fairfield, CA|TX - Texarkana, TX-AR|CA - San Francisco-Oakland-Hayward, CA|TX - Laredo, TX|FL - Homosassa Springs, FL|
        # CA - Stockton-Lodi, CA|CA - Modesto, CA|TX - Odessa, TX|TX - Lubbock, TX|NY - Albany-Schenectady-Troy, NY|
        # CA - Riverside-San Bernardino-Ontario, CA|GA - Gainesville, GA|TX - Amarillo, TX|GA - Rome, GA|
        # CA - Sacramento--Roseville--Arden-Arcade, CA|FL - Tallahassee, FL|FL - Miami-Fort Lauderdale-West Palm Beach, FL|
        # FL - Naples-Immokalee-Marco Island, FL|NJ - Philadelphia-Camden-Wilmington, PA-NJ-DE-MD|FL - Sebastian-Vero Beach, FL|
        # CA - Santa Cruz-Watsonville, CA|NY - Watertown-Fort Drum, NY|CA - San Jose-Sunnyvale-Santa Clara, CA|CA - Madera, CA|
        # NY - New York-Newark-Jersey City, NY-NJ-PA|TX - Brownsville-Harlingen, TX|FL - The Villages, FL|CA - El Centro, CA|
        # NJ - Trenton, NJ|TX - Corpus Christi, TX|NJ - Atlantic City-Hammonton, NJ|NY - Ithaca, NY|NY - Binghamton, NY|TX - El Paso, TX|
        # TX - McAllen-Edinburg-Mission, TX|FL - Panama City, FL|TX - Houston-The Woodlands-Sugar Land, TX|CA - Santa Maria-Santa Barbara, CA|
        # CA - Hanford-Corcoran, CA|CA - Salinas, CA|CA - Yuba City, CA|CA - Fresno, CA|NJ - Ocean City, NJ|FL - Pensacola-Ferry Pass-Brent, FL|
        # CA - Los Angeles-Long Beach-Anaheim, CA|TX - Dallas-Fort Worth-Arlington, TX|TX - Wichita Falls, TX|NY - Elmira, NY|CA - Redding, CA|
        # TX - Longview, TX|TX - Austin-Round Rock, TX|NY - Utica-Rome, NY|CA - Napa, CA|TX - Tyler, TX|CA - Merced, CA|TX - Victoria, TX|
        # NY - Kingston, NY|GA - Athens-Clarke County, GA|NJ - Allentown-Bethlehem-Easton, PA-NJ"
       
    # segment
        c((orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(segment))[1,]) # always the same segments used!
    
    # hs gpa min
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(gpa_low) # always B-
      
    # hs gpa max
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(gpa_high) # always A+
    
    # sat score min
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(sat_score_min) # 7 orders use SAT min = 1240; 1 order (order = 403340) SAT min = 1230
      
      #1 order (order = 403340) SAT min = 1230; order title = OOS Non-ENG August 2018; 2026 students
      orders_df %>% filter(order_num %in% c('403340')) %>% select(order_num,order_title,num_students,date_start,hs_grad_class,state_name,cbsa_name,segment,sat_score_min,sat_score_max,psat_score_min,psat_score_max,gpa_low,gpa_high)
  
    # sat score max
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(sat_score_max) # always 1450
    
    # psat score min
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(psat_score_min) # 5 orders psat min = 1240; 1 order (403340) psat min = 1230; 2 orders ('500590','567376') psat min = 1220
  
      #1 order (403340) psat min = 1230; 2 orders ('500590','567376') psat min = 1220
      orders_df %>% filter(order_num %in% c('403340','500590','567376')) %>% select(order_num,order_title,num_students,date_start,hs_grad_class,state_name,cbsa_name,segment,sat_score_min,sat_score_max,psat_score_min,psat_score_max,gpa_low,gpa_high)
      
    # psat score max
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count(psat_score_max) # always 1450
    
    
  # COUNT THE NUMBER OF STUDENTS IN THE VARIOUS GROUPS OF ORDERS
      
    # All 8 non-engineering orders, 115294 students
      # order-level data, 115294 students
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% summarise(num_students=sum(num_students))
      # prospect-level data, 
      lists_orders_zip_hs_df %>% filter(ord_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count()
    
    # 5 oos non-eng orders
      # order-level data, 66451 students
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% summarise(num_students=sum(num_students))
      orders_df %>% filter(order_num %in% c('371629','386335','403340','456737','470283')) %>% select(order_num,num_students)
      
      # prospect-level data, 
      lists_orders_zip_hs_df %>% filter(ord_num %in% c('371629','386335','403340','456737','470283')) %>% count()
      lists_orders_zip_hs_df %>% filter(ord_num %in% c('371629','386335','403340','456737','470283')) %>% count(ord_num)
      
      # note: '470283' has 794 students on order-level data and 6,000 students on prospect-level data
        # discrepancy is because pdf order summary says 797 students "available today" and 794 students in "file output actuals" but says 6,000 students "total actuals (to date)"
    
    # 3 "regional" orders
      # order-level data, 48843 students
      orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% summarise(num_students=sum(num_students))
      orders_df %>% filter(order_num %in% c('483751','500590','567376')) %>% select(order_num,num_students)
      
      # prospect-level data, 
      lists_orders_zip_hs_df %>% filter(ord_num %in% c('483751','500590','567376')) %>% count()
      lists_orders_zip_hs_df %>% filter(ord_num %in% c('483751','500590','567376')) %>% count(ord_num)
      
      # note: order '500590' has 23,985 students on order-level data and 35,057 students on prospect-level data
        # discrepancy is because pdf order summary has 23,985 students in "file output actuals" but says 35,060 students "total actuals (to date)"
      
      
    # Karina code to create/check analysis dataset
      

        # ENGINEERING ORDERS check filters across orders that use segment; these filter by segment at the state level but not cbsa
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_states_fil==1 &  filter_cbsa!=1 &  filter_psat==1) %>% count(ord_num)
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_states_fil==1 &  filter_cbsa!=1 & filter_psat==1) %>% count(ord_state_name)
          
          # check filters across orders that use segment; these filter by segment at the state & CBSA level
          
        
    # Create data frame for non-engineering oos orders that use segment
          
          ui_oos_seg <- lists_orders_zip_hs_df %>% filter(ord_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376'))
          #ui_uc <-lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1)
          

          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count()
          lists_orders_zip_hs_df %>% filter(ord_num %in% c('371629','386335','403340','456737','470283','483751','500590','567376')) %>% count()
          ui_oos_seg %>% count()
          
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_num)
          ui_oos_seg %>% count(ord_num)
          
          x1 <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% select(ord_num|ord_cbsa_name|starts_with("filter_"))
          x1 <- distinct(x1, ord_num, .keep_all = TRUE)
              
              # see full list of cbsa across three groupings
        
              c(x1[1,2])
              c(x1[5,2])
              c(x1[6,2])
              
              # descriptives on filters
              ui_oos_seg %>% count(ord_hs_grad_class)
              ui_oos_seg %>% count(ord_psat_score_max)
              ui_oos_seg %>% count(ord_psat_score_min)
              
              ui_oos_seg %>% count(ord_sat_score_max)
              ui_oos_seg %>% count(ord_sat_score_min)
              
              ui_oos_seg %>% count(ord_gpa_low)
              ui_oos_seg %>% count(ord_gpa_high)  
          
              ui_oos_seg %>% count(ord_segment)
              x <- ui_oos_seg %>% count(ord_segment)  
              
                    # see full list of cbsa across three groupings
                    c(x[1,1])
                    
    
  # Investigate CBSAs w/ greatest number of students
ui_oos_seg %>% count()
        ui_oos_seg %>% mutate(zip_cbsa_name_code = str_c(zip_cbsatitle_1,zip_cbsa_1, sep='; ')) %>% count(zip_cbsa_name_code) %>% arrange(desc(n)) %>% print(n=30)
        
#   zip_cbsa_name_code                                      n
# 1 New York-Newark-Jersey City, NY-NJ-PA; 35620        27932
# 2 Los Angeles-Long Beach-Anaheim, CA; 31080           12307
# 3 Philadelphia-Camden-Wilmington, PA-NJ-DE-MD; 37980   9126
# 4 San Francisco-Oakland-Hayward, CA; 41860             8113
# 5 Atlanta-Sandy Springs-Roswell, GA; 12060             7309
# 6 Washington-Arlington-Alexandria, DC-VA-MD-WV; 47900  5728
# 7 Dallas-Fort Worth-Arlington, TX; 19100               5354
# 8 Houston-The Woodlands-Sugar Land, TX; 26420          4981
# 9 San Jose-Sunnyvale-Santa Clara, CA; 41940            4885
#10 Boston-Cambridge-Newton, MA-NH; 14460                4700
#11 Miami-Fort Lauderdale-West Palm Beach, FL; 33100     3653
#12 San Diego-Carlsbad, CA; 41740                        2967
#13 Austin-Round Rock, TX; 12420                         2105
#14 Baltimore-Columbia-Towson, MD; 12580                 1854
        
# candidate metro areas
  # san fran
  # atlanta
  # dallas
  # DC

        # Philadelphia-Camden-Wilmington, PA-NJ-DE-MD; 37980
        # students purchased across all three types of segment orders 
          ui_oos_seg %>% filter(zip_cbsa_1 == '37980') %>% count()
          
              
          philly_studentlist <- ui_oos_seg %>% filter(zip_cbsa_1 == '37980') %>%    
              count(stu_race_cb) %>% mutate(metro= "Philadelphia", pct = n / sum(n) * 100) #%>% print(n=50)
                    
          philly_studentlist <- philly_studentlist %>% mutate(
                                                    stu_race_cb = as.character(stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
          
          philly_studentlist <- philly_studentlist[, c("metro", "stu_race_cb", "pct")]
          philly_studentlist <- spread(philly_studentlist, key = c("stu_race_cb"), value = "pct")
          philly_studentlist$population <- "Prospects Purchased"
          philly_studentlist$tot_students <- (ui_oos_seg %>% filter(zip_cbsa_1 == '37980') %>% count())$n
          philly_studentlist <- philly_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "NHPI", "Multiracial","NoResponse")]
          philly_studentlist
        
        # racial composition of all students in public high schools in the CBSA
          philly_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '37980') %>%
            summarize(
                metro = "Philadelphia",
                population = "Public HS Students",
                #n_obs = sum(total_students, na.rm = TRUE),
                tot_students = sum(total_students, na.rm = TRUE),
                #tot_white = sum(total_white, na.rm = TRUE),
                #tot_asian = sum(total_asian, na.rm = TRUE),
                White = sum(total_white, na.rm = TRUE)/tot_students*100,
                Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
                Black = sum(total_black, na.rm = TRUE)/tot_students*100,
                Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
                AIAN =  sum(total_amerindian, na.rm = TRUE)/tot_students*100,
                NHPI =  sum(total_nativehawaii, na.rm = TRUE)/tot_students*100,                
                #AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
                Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
                NoResponse = sum(total_unknown, na.rm = TRUE)/tot_students*100,
                #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
            )
            philly_metro
            #philly_metro$pct_stu_unknown <- NULL

            philly_studentlist
            philly_metro
        
            
        # New York; 35620
        # students purchased across all three types of segment orders 
        ui_oos_seg %>% filter(zip_cbsa_1 == '35620') %>% count()
        
        ny_studentlist <- ui_oos_seg %>% filter(zip_cbsa_1 == '35620') %>%    
          count(stu_race_cb) %>% mutate(metro= "New York", pct = n / sum(n) * 100) #%>% print(n=50)
        
        ny_studentlist <- ny_studentlist %>% mutate(
          stu_race_cb = as.character(stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        ny_studentlist <- ny_studentlist[, c("metro", "stu_race_cb", "pct")]
        ny_studentlist <- spread(ny_studentlist, key = c("stu_race_cb"), value = "pct")
        ny_studentlist$population <- "Prospects Purchased"
        ny_studentlist$tot_students <- (ui_oos_seg %>% filter(zip_cbsa_1 == '35620') %>% count())$n
        ny_studentlist
        ny_studentlist <- ny_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "NHPI", "Multiracial","NoResponse")]
        ny_studentlist
        
        # racial composition of all students in public high schools in the CBSA
        ny_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '35620') %>%
          summarize(
            metro = "New York",
            population = "Public HS Students",
            #n_obs = sum(total_students, na.rm = TRUE),
            tot_students = sum(total_students, na.rm = TRUE),
            #tot_white = sum(total_white, na.rm = TRUE),
            #tot_asian = sum(total_asian, na.rm = TRUE),
            White = sum(total_white, na.rm = TRUE)/tot_students*100,
            Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
            Black = sum(total_black, na.rm = TRUE)/tot_students*100,
            Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
            AIAN =  sum(total_amerindian, na.rm = TRUE)/tot_students*100,
            NHPI =  sum(total_nativehawaii, na.rm = TRUE)/tot_students*100,                
            #AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            NoResponse = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
          )
            ny_metro
            #ny_metro$pct_stu_unknown <- NULL
        
        ny_studentlist
        ny_metro
        
        
        # Los Angeles; 31080
        # students purchased across all three types of segment orders 
        ui_oos_seg %>% filter(zip_cbsa_1 == '31080') %>% count()
        
        la_studentlist <- ui_oos_seg %>% filter(zip_cbsa_1 == '31080') %>%    
          count(stu_race_cb) %>% mutate(metro= "Los Angeles", pct = n / sum(n) * 100) #%>% print(n=50)
        
        la_studentlist <- la_studentlist %>% mutate(
          stu_race_cb = as.character(stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        la_studentlist <- la_studentlist[, c("metro", "stu_race_cb", "pct")]
        la_studentlist <- spread(la_studentlist, key = c("stu_race_cb"), value = "pct")
        la_studentlist$population <- "Prospects Purchased"
        la_studentlist$tot_students <- (ui_oos_seg %>% filter(zip_cbsa_1 == '31080') %>% count())$n
        la_studentlist <- la_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "NHPI", "Multiracial","NoResponse")]
        
        
        # racial composition of all students in public high schools in the CBSA
        la_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '31080') %>%
          summarize(
            metro = "Los Angeles",
            population = "Public HS Students",
            #n_obs = sum(total_students, na.rm = TRUE),
            tot_students = sum(total_students, na.rm = TRUE),
            #tot_white = sum(total_white, na.rm = TRUE),
            #tot_asian = sum(total_asian, na.rm = TRUE),
            White = sum(total_white, na.rm = TRUE)/tot_students*100,
            Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
            Black = sum(total_black, na.rm = TRUE)/tot_students*100,
            Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
            AIAN =  sum(total_amerindian, na.rm = TRUE)/tot_students*100,
            NHPI =  sum(total_nativehawaii, na.rm = TRUE)/tot_students*100,                
            #AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            NoResponse = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
          )
            la_metro
            #la_metro$pct_stu_unknown <- NULL
      
  # 5 Atlanta-Sandy Springs-Roswell, GA; 12060             7309
        atl_studentlist <- ui_oos_seg %>% filter(zip_cbsa_1 == '12060') %>%    
          count(stu_race_cb) %>% mutate(metro= "Atlanta", pct = n / sum(n) * 100) #%>% print(n=50)
        
        atl_studentlist <- atl_studentlist %>% mutate(
          stu_race_cb = as.character(stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        atl_studentlist <- atl_studentlist[, c("metro", "stu_race_cb", "pct")]
        atl_studentlist <- spread(atl_studentlist, key = c("stu_race_cb"), value = "pct")
        atl_studentlist$population <- "Prospects Purchased"
        atl_studentlist$tot_students <- (ui_oos_seg %>% filter(zip_cbsa_1 == '12060') %>% count())$n
        atl_studentlist <- atl_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "NHPI", "Multiracial","NoResponse")]
        
        atl_studentlist
        
        # racial composition of all students in public high schools in the CBSA
        atl_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '12060') %>%
          summarize(
            metro = "Atlanta",
            population = "Public HS Students",
            #n_obs = sum(total_students, na.rm = TRUE),
            tot_students = sum(total_students, na.rm = TRUE),
            #tot_white = sum(total_white, na.rm = TRUE),
            #tot_asian = sum(total_asian, na.rm = TRUE),
            White = sum(total_white, na.rm = TRUE)/tot_students*100,
            Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
            Black = sum(total_black, na.rm = TRUE)/tot_students*100,
            Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
            AIAN =  sum(total_amerindian, na.rm = TRUE)/tot_students*100,
            NHPI =  sum(total_nativehawaii, na.rm = TRUE)/tot_students*100,                
            #AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            NoResponse = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
          )
            atl_metro
            #atl_metro$pct_stu_unknown <- NULL
        
        atl_studentlist
        atl_metro        
        
  # 6 Washington-Arlington-Alexandria, DC-VA-MD-WV; 47900  5728
        dc_studentlist <- ui_oos_seg %>% filter(zip_cbsa_1 == '47900') %>%    
          count(stu_race_cb) %>% mutate(metro= "Washington, DC", pct = n / sum(n) * 100) #%>% print(n=50)
        
        dc_studentlist <- dc_studentlist %>% mutate(
          stu_race_cb = as.character(stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        dc_studentlist <- dc_studentlist[, c("metro", "stu_race_cb", "pct")]
        dc_studentlist <- spread(dc_studentlist, key = c("stu_race_cb"), value = "pct")
        dc_studentlist$population <- "Prospects Purchased"
        dc_studentlist$tot_students <- (ui_oos_seg %>% filter(zip_cbsa_1 == '47900') %>% count())$n
        dc_studentlist <- dc_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "NHPI", "Multiracial","NoResponse")]
        
        dc_studentlist
        
        dc_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '47900') %>%
          summarize(
            metro = "Washington, DC",
            population = "Public HS Students",
            #n_obs = sum(total_students, na.rm = TRUE),
            tot_students = sum(total_students, na.rm = TRUE),
            #tot_white = sum(total_white, na.rm = TRUE),
            #tot_asian = sum(total_asian, na.rm = TRUE),
            White = sum(total_white, na.rm = TRUE)/tot_students*100,
            Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
            Black = sum(total_black, na.rm = TRUE)/tot_students*100,
            Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
            AIAN =  sum(total_amerindian, na.rm = TRUE)/tot_students*100,
            NHPI =  sum(total_nativehawaii, na.rm = TRUE)/tot_students*100,                
            #AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            NoResponse = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
          )
            dc_metro
            #dc_metro$pct_stu_unknown <- NULL
        
        dc_studentlist
        dc_metro
        
        
  
  # 4 San Francisco-Oakland-Hayward, CA; 41860             8113        
        bay_studentlist <- ui_oos_seg %>% filter(zip_cbsa_1 == '41860') %>%    
          count(stu_race_cb) %>% mutate(metro= "San Fran-Oakland", pct = n / sum(n) * 100) #%>% print(n=50)
        
        bay_studentlist <- bay_studentlist %>% mutate(
          stu_race_cb = as.character(stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        bay_studentlist <- bay_studentlist[, c("metro", "stu_race_cb", "pct")]
        bay_studentlist <- spread(bay_studentlist, key = c("stu_race_cb"), value = "pct")
        bay_studentlist$population <- "Prospects Purchased"
        bay_studentlist$tot_students <- (ui_oos_seg %>% filter(zip_cbsa_1 == '41860') %>% count())$n
        bay_studentlist <- bay_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "NHPI", "Multiracial","NoResponse")]
        
        bay_studentlist
        
        bay_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '41860') %>%
          summarize(
            metro = "San Fran-Oakland",
            population = "Public HS Students",
            #n_obs = sum(total_students, na.rm = TRUE),
            tot_students = sum(total_students, na.rm = TRUE),
            #tot_white = sum(total_white, na.rm = TRUE),
            #tot_asian = sum(total_asian, na.rm = TRUE),
            White = sum(total_white, na.rm = TRUE)/tot_students*100,
            Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
            Black = sum(total_black, na.rm = TRUE)/tot_students*100,
            Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
            AIAN =  sum(total_amerindian, na.rm = TRUE)/tot_students*100,
            NHPI =  sum(total_nativehawaii, na.rm = TRUE)/tot_students*100,                
            #AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            NoResponse = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
          )
            bay_metro
            #bay_metro$pct_stu_unknown <- NULL

        
        bay_studentlist
        bay_metro
        
        
        
    fig_rq3_segment_race <- rbind(ny_studentlist, ny_metro, la_studentlist, la_metro, philly_studentlist, philly_metro, dc_studentlist, dc_metro)
    fig_rq3_segment_race
        
        
    # income of prospects across all three metros [don't know how to incorporate income for the metro area]
    philly_studentlist_inc <- ui_oos_seg %>% filter(zip_cbsa_1 == '37980') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T)) 
      
    ny_studentlist_inc <- ui_oos_seg %>% filter(zip_cbsa_1 == '35620') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))
    
    la_studentlist_inc <- ui_oos_seg %>% filter(zip_cbsa_1 == '31080') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))
    
    dc_studentlist_inc <- ui_oos_seg %>% filter(zip_cbsa_1 == '47900') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))
    
    bay_studentlist_inc <- ui_oos_seg %>% filter(zip_cbsa_1 == '41860') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))    
    
    # 2019 ACS
    acs_race_zipcodev3 %>% filter(cbsa_1=='35620') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector() # ny
    acs_race_zipcodev3 %>% filter(cbsa_1=='31080') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector() # la
    acs_race_zipcodev3 %>% filter(cbsa_1=='37980') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector() # philadelphia
    acs_race_zipcodev3 %>% filter(cbsa_1=='47900') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector() # dc
    acs_race_zipcodev3 %>% filter(cbsa_1=='41860') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector() # bay
      
        # Philadelphia = 74,533
        # New York = 83,160
        # LA metro =77,774

    fig_rq3_segment_race_inc <- fig_rq3_segment_race %>% 
      mutate(
        income = c(
          ny_studentlist_inc$stu_mean_inc, acs_race_zipcodev3 %>% filter(cbsa_1=='35620') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector() ,
          la_studentlist_inc$stu_mean_inc, acs_race_zipcodev3 %>% filter(cbsa_1=='31080') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector(),
          philly_studentlist_inc$stu_mean_inc, acs_race_zipcodev3 %>% filter(cbsa_1=='37980') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector(),
          dc_studentlist_inc$stu_mean_inc, acs_race_zipcodev3 %>% filter(cbsa_1=='47900') %>% summarise(mean_inc = mean(median_household_income, na.rm =TRUE)) %>% as_vector()
        )
      )    
    fig_rq3_segment_race_inc
