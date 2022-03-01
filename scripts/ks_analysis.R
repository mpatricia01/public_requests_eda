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
    
    
################### NEED TO REDUCE OBS TO MAKE MANIPULATIONS MANAGEABLE         
    
    #remove extra lists_orders_dfs
    rm(lists_orders_df, lists_orders_zip_df)
    
    #remove ASU due to memory issues
    lists_df <- lists_df %>% filter(univ_id!="104151") 
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% filter(univ_id!="104151") 
    
    #removing secondary R1 in CA (UC Davis); IL (UI Chicago)
    #lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% filter(univ_id!="145600" & univ_id!="110644")

    # remove ASU from orders_df too
    orders_df <- orders_df %>% filter(univ_id!="104151") 
    
    orders_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    orders_df %>% 
      summarise(n=n_distinct(order_num)) 
    
    lists_orders_zip_hs_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    lists_orders_zip_hs_df %>% 
      summarise(n=n_distinct(ord_num)) 
    
    lists_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    orders_df %>% count(univ_id)
    lists_df %>% count(univ_id)
    
################### CHECKING URBANA CHAMPAIGN
  
  #BELOW USES RAW DATA TO UNDERSTAND HOW OZAN CREATED RACE/ETHNICITY COMMON VAR
    # checking raw data; does not include Ozan's maniputlations 
    #load(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/combined_data.RData'))
    
    # create list_df of just urbana
    urbana <- lists_df %>% filter(univ_id==145637)
    
    # Urbana says that 74% of respondents did not fill out the race/ethnicity question; true NAs
    # Ozan's race var creation (line 587 in create_combined. R) assumes if is_hispanic_orgin is NA then is_hispanic_orgin==0
    urbana %>%
      group_by(is_hispanic_origin) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n))
    
    # how many missing race & ethnicity?
    urbana %>%
      group_by(is_hispanic_origin, race) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>% print(n=200)
    
    
  # compare to other research univs; UC Davis (47% Hispanic)
    lists_df %>% filter(univ_id==110644 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
  # compare to other research univs; UC San Diego (26% Hispanic)
    lists_df %>% filter(univ_id==110680 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
    # compare to other research univs; UI Chicago (17% Hispanic)
    lists_df %>% filter(univ_id==145600 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
    # compare to other research univs; Texas A&M CS (29% Hispanic)
    lists_df %>% filter(univ_id==228723 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
    
    #looks across two similar orders in one metro
    
    
################### CHECKING UNIV ILLINOIS SPRINGFIELD
    
    orders_df %>% filter(univ_id==148654) %>% count(order_num, num_students) %>% print(n=220)
    
    orders_df %>% filter(univ_id==148654) %>%  summarise(across(num_students, sum)) #229,541 total students we cant link to order_num
    
    load("~/public_requests_eda/data/148654_data.RData")
    
    lists_df_148654 %>% count(source) %>% View() 
    
    
    
################### CREATING AND CLEANING OUT_OF_STATE & GENDER VARS NEEDED FOR FUNCTION
    
    # KS CHECKS
    # # non-res categories based on country + state (check for missingness)
    # lists_orders_zip_hs_df %>% count(stu_country) %>% print(n=200) #1245 missing countries
    # 
    # 
    # lists_orders_zip_hs_df %>% filter(stu_in_us==1 | is.na(stu_state)) %>% count(stu_country, stu_state) %>% print(n=200) #1245 missing countries
    # 
    #     #lots of foreign "states/cities" listed as US & US states with NA for country
    #     lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>%
    #          mutate(stu_country = ifelse((stu_state=="Ankara" | stu_state=="Icerenkoy" | stu_state=="Istanbul" | stu_state=="Mudanya"), "turkey", stu_country),
    #                 stu_country = ifelse(stu_state=="Beijing", "china", stu_country), 
    #                 stu_country = ifelse((stu_state=="Central Singapore" | stu_state=="Singapore"), "singapore", stu_country),
    #                 stu_country = ifelse(stu_state=="CHINA", "china", stu_country),
    #                 stu_country = ifelse(stu_state=="Doha", "qatar", stu_country),
    #                 stu_country = ifelse(stu_state=="Karnataka", "india", stu_country),
    #                 stu_country = ifelse(stu_state=="Minas Gerais", "brazil", stu_country),
    #                 stu_country = ifelse((stu_state=="Taipei"|stu_state=="Taiwan"), "taiwan", stu_country),
    #                 stu_country = ifelse(stu_state=="VIC", "australia", stu_country),
    #                 stu_country = ifelse((stu_state=="CA"|stu_state=="IL" | stu_state=="TX"), "united states", stu_country)
    #                 )
    # 
    #     
    #     # 58,225 students with missing country & state [checking school state, city, zip]
    #     lists_orders_zip_hs_df %>% filter(is.na(stu_country) | is.na(stu_state)) %>% select(stu_country, stu_state, hs_state_code, stu_city, stu_zip) %>% print(n=200) 
    #     
    
    #non res categories based on OJ's stu_in_us var
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>%
        mutate(stu_internat = ifelse(stu_in_us==1, 0, 1),
               stu_nonres = ifelse((stu_in_us==1 & univ_state==stu_state), 0, NA),
               stu_nonres = ifelse((stu_in_us==1 & univ_state!=stu_state), 1, stu_nonres))
    
    lists_orders_zip_hs_df %>% count(stu_in_us, stu_nonres)
    
    
    # consolidate gender/sex categories
    
    lists_orders_zip_hs_df %>% count(stu_gender)
    
    lists_orders_zip_hs_df <-  lists_orders_zip_hs_df %>% mutate(
      stu_women_dummy = ifelse(stu_gender=="F"| stu_gender=="Female", 1, NA_integer_),
      stu_women_dummy = ifelse(stu_gender=="M"| stu_gender=="Male", 0, stu_women_dummy)
      
    )
    
    lists_orders_zip_hs_df %>% count(stu_gender, stu_women_dummy)
    
################### ANALYSIS VISUALS FOR RQ1: CHARACTERISTICS OF ORDERS
    
    # unique IDs for order nums
    orders_df %>% 
      summarise(n=n_distinct(order_num)) 
    
    # how many orders total + students total; then by university/carnegie
        orders_df %>% count()
        orders_fig_totals <- orders_df %>% 
            group_by(univ_id) %>%
            summarise(total_orders = n(),
                      total_students = sum(num_students, na.rm = T))
        
        orders_fig_totals <-  orders_fig_totals %>% arrange(-total_students) %>%
            mutate(university = as.factor(row_number()),
                   total_orders = as.character(total_orders))
        
        orders_fig_totals$total_orders_st <- str_c(orders_fig_totals$total_orders, ' orders')
        
        orders_fig_totals<- merge(x = orders_fig_totals, y = univ_data[ , c("c15basic", "univ_id", "univ_name")], by = "univ_id", all.x=TRUE)
        
        
        orders_fig_totals<- orders_fig_totals %>%
            mutate(carnegie = recode(c15basic,
                                     `15`= "Research Extensive",
                                     `18`= "Master's",
                                     `19`= "Master's",
                                     `22`= "Baccalaureate"))
        
    
        ggplot(orders_fig_totals, aes(x=reorder(university, -total_students), y=total_students, fill=carnegie)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label=total_orders_st), vjust=0, size=2.5) 
        
        
        orders_df %>% select(num_students) %>%
          summarise(across(
            .cols = where(is.numeric), 
            .fns = list(Mean = mean, SD=sd, median =median), na.rm = TRUE, 
            .names = "{col}_{fn}"
          ))
        
        orders_df<- orders_df %>%
          mutate(carnegie = recode(univ_c15basic,
                                   `15`= "Research Extensive",
                                   `18`= "Master's",
                                   `19`= "Master's",
                                   `22`= "Baccalaureate"))
        
        orders_df %>% group_by(carnegie) %>% select(num_students) %>%
          summarise(across(
            .cols = where(is.numeric), 
            .fns = list(Mean = mean, SD=sd, median =median), na.rm = TRUE, 
            .names = "{col}_{fn}"
          ))
        
        
        orders_df<-orders_df %>% mutate_if(is.character, list(~na_if(.,""))) 
        
        
    # Frequency of Filters Used Across Orders
        orders_filters <- orders_df %>% 
                        select(hs_grad_class, zip_code, zip_code_file, state_name, cbsa_name, intl_region, segment, race_ethnicity,
                                gender,sat_score_min, sat_score_max, sat_score_old_min, sat_score_old_max,
                                psat_score_min, psat_score_max, psat_score_old_min, psat_score_old_max,
                                gpa_low, gpa_high, rank_low, rank_high, geomarket, ap_scores) %>%
            mutate(
                hsgrad_class = ifelse(!is.na(hs_grad_class), 1, 0),
                zip = ifelse(!is.na(zip_code) | !is.na(zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
                states_fil = ifelse(!is.na(state_name), 1, 0), 
                cbsa = ifelse(!is.na(cbsa_name), 1, 0), 
                intl = ifelse(!is.na(intl_region), 1, 0), 
                segment = ifelse(!is.na(segment), 1, 0), 
                race = ifelse(!is.na(race_ethnicity), 1, 0), 
                gender = ifelse(!is.na(gender), 1, 0), 
                sat = ifelse((!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max)), 1, 0), 
                psat = ifelse((!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max)), 1, 0), 
                gpa = ifelse((!is.na(gpa_low) | !is.na(gpa_high)), 1, 0), 
                rank = ifelse((!is.na(rank_low) | !is.na(rank_high)), 1, 0), 
                geomarket = ifelse(!is.na(geomarket), 1, 0), 
                ap_score = ifelse(!is.na(ap_scores), 1, 0))
        
        
        orders_filters1 <- orders_filters %>% 
            select(hsgrad_class, zip, states_fil, cbsa, 
                   intl, segment, race, gender,sat, psat,
                   gpa, rank, geomarket, ap_score) %>%
            summarize_if(is.numeric, sum, na.rm=TRUE)
        
        orders_filters1  <- as.data.frame(t(orders_filters1))
        orders_filters1$filters <- rownames(orders_filters1)

        orders_filters1  <- orders_filters1 %>%
            mutate(
                percent= round((V1/486)*100)
            )
        
        orders_filters1$percent <- str_c(orders_filters1$percent, '%')
        
        ggplot(orders_filters1, aes(x=reorder(filters, V1), y=V1)) +
            geom_bar(stat = "identity") +
            ylab("Number of Orders") +
            geom_text(aes(label = percent), hjust = -0.1, colour = "black", size=2) +
            coord_flip()
        
        
     # descriptive stats on GPA Filter
        orders_df %>% count(gpa_low)
        orders_df %>% count(gpa_high)
        
        #replace empty strings with NA
        orders_df <- orders_df %>%
            mutate(across(c("gpa_low","gpa_high"), ~ifelse(.=="", NA, as.character(.))))
        
        orders_df %>% count(gpa_low)
        orders_df %>% count(gpa_high)
        
        table_gpalow <- orders_df %>% group_by(gpa_low) %>%
            summarise(n_low = n()) %>%
            mutate(pct_low = round(n_low / sum(n_low)*100, digits=1))
        
        table_gpahigh <-orders_df %>% group_by(gpa_high) %>%
            summarise(n_high = n()) %>%
            mutate(pct_high = round(n_high / sum(n_high)*100, digits=1))
                                     
        table_gpa <- merge(table_gpalow, table_gpahigh, by.x = "gpa_low", by.y = "gpa_high", all = T)
        table_gpa <- table_gpa %>%
            rename(gpa = gpa_low)
        
        #remove orders that did not use GPA filter
        table_gpa <- table_gpa %>% filter(!is.na(gpa), nchar(gpa) > 0)
        
        # descriptive stats on PSAT/SAT Filter
        orders_df %>% count(psat_score_max)
        orders_df %>% count(psat_score_min) 
        
        orders_df %>% count(psat_score_old_max)
        orders_df %>% count(psat_score_old_min)
        
        # PSAT cutoffs tabulations
        
        orders_df$psat_minbrks <- cut(orders_df$psat_score_min, 
                               breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501), 
                               labels=c("<1000", "1000-1100", "1110-1200", 
                                        "1210-1300", "1310-1400", "1410-1500"))
        
        orders_df %>% group_by(psat_score_min) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        

        orders_df %>% filter(!is.na(psat_minbrks)) %>% group_by(psat_minbrks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
        orders_df$psat_maxbrks <- cut(orders_df$psat_score_max, 
                                      breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501), 
                                      labels=c("<1000", "1000-1100", "1110-1200", 
                                               "1210-1300", "1310-1400", "1410-1500"))
        
        orders_df %>% filter(!is.na(psat_maxbrks)) %>% group_by(psat_maxbrks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
        orders_df %>% group_by(psat_score_max) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
        
        # SAT cutoffs tabulations

        orders_df$sat_minbrks <- cut(orders_df$sat_score_min, 
                                      breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501), 
                                      labels=c("<1000", "1000-1100", "1110-1200", 
                                               "1210-1300", "1310-1400", "1410-1500"))
        
        orders_df %>% group_by(sat_score_min) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
        orders_df %>% filter(!is.na(sat_minbrks)) %>% group_by(sat_minbrks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
        orders_df %>% group_by(sat_score_max) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
        orders_df$sat_maxbrks <- cut(orders_df$sat_score_max, 
                                      breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                                      labels=c("<1000", "1000-1100", "1110-1200", 
                                               "1210-1300", "1310-1400", "1410-1500", "1500+"))
        
        orders_df %>% filter(!is.na(sat_maxbrks)) %>% group_by(sat_maxbrks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
    
        
        
        test_scores <- orders_df %>% group_by(univ_c15basic) %>%
            select(psat_score_min, psat_score_max, 
                   sat_score_min, sat_score_max, 
                   sat_score_old_min, sat_score_old_max) %>%
            summarise(across(
                .cols = where(is.numeric), 
                .fns = list(Mean = mean, SD=sd), na.rm = TRUE, 
                .names = "{col}_{fn}"
            ))
        
        
        #average max/min scores by institution type
        test_scores <- orders_df %>% group_by(carnegie) %>%
          select(psat_score_min, psat_score_max, 
                 sat_score_min, sat_score_max, 
                 sat_score_old_min, sat_score_old_max) %>%
          summarise(across(
            .cols = where(is.numeric), 
            .fns = list(Mean = mean, SD=sd), na.rm = TRUE, 
            .names = "{col}_{fn}"
          ))
           
    # descriptive stats on HS RANK Filter
        orders_df %>% count(rank_high)
        orders_df %>% count(rank_low)    
        
        
    # descriptive stats on AP SCORES Filter
        orders_df %>% count(ap_scores)
        orders_df %>% filter(!is.na(ap_scores)) %>% count(univ_id) # only UC Davis (5) and UCSD (17)
        
        ap_filters <- orders_df %>% filter(!is.na(ap_scores)) %>% select(ap_scores, order_num)
        #ap_filters_list <- as.list(ap_filters$ap_scores)
        
        
        ap_filters_list <- ap_filters %>%
            unnest(ap_scores)
        
        ap_filters_list<-  ap_filters_list %>% group_by(order_num) %>% data.frame(do.call("rbind", strsplit(as.character(ap_filters_list$ap_scores), "|", fixed = TRUE)))
    
        
        ap_filters_list_long <- ap_filters_list %>% gather(ap_score_filters, value, -c(ap_scores, order_num))
        ap_filters_list_long$ap_score_filters<-gsub("X","",as.character(ap_filters_list_long$ap_score_filters))
        
        
        table_ap <- ap_filters_list_long %>% group_by(value) %>%
            summarise(n = n()) %>%
            mutate(pct = round(n / sum(n)*100, digits=1))
        
        ggplot(table_ap, aes(x=reorder(value, n), y=n)) +
            geom_bar(stat = "identity") +
            ylab("Number of Orders") +
            geom_text(aes(label = pct), hjust = -0.1, colour = "black", size=2) +
            coord_flip()
       
        
  # GEOGRAPHIC FILTERS
        
        #zip code filters
        orders_df %>% count(zip_code)
          #KS NOTES: all 3-digits: https://en.wikipedia.org/wiki/List_of_ZIP_Code_prefixes
        
        #ZIP CODES
        orders_df %>% filter(!is.na(zip_code)) %>% count(carnegie)
        orders_df %>% filter(!is.na(zip_code)) %>% count(zip_code, univ_state)
        orders_df %>% filter(!is.na(zip_code)) %>% count(zip_code, univ_name)
        
        
        # descriptive stats on SEGMENT Filter
        orders_df %>% count(segment)
        orders_df %>% filter(!is.na(segment)) %>% count(univ_id) # only Urbana-Champagne (21) and Northeastern (1)
        
        segment_filters <- orders_df %>% filter(!is.na(segment))%>% select(segment, order_num)
        
        
        # descriptive stats of STATE filter  
        
        orders_df %>% count(state_name) %>% print(n=50)
        
        orders_df %>% count(state_name, univ_state) %>% print(n=50)
        
        
        # descriptive stats for segment filter
        orders_df %>% count(segment, univ_id)
        orders_df %>% filter(univ_id == '145637') %>% count(segment)
        orders_df %>% filter(univ_id == '147776') %>% count(segment) #just says include all students, did this Northeastern order use segment?
        
        
    # Demographic filters
        
        orders_df %>% count(race_ethnicity) %>% print(n=40)
        
        orders_df %>% count(gender) %>% print(n=40)
        
    # Descriptives on Filter Combos
        
       filter_combos <- orders_filters %>%
            select(hsgrad_class, zip, states_fil, cbsa, 
                   intl, segment, race, gender,sat, psat,
                   gpa, rank, geomarket, ap_score) %>%
            mutate(filter_sum = hsgrad_class + zip + states_fil + cbsa + 
                             intl + segment + race + gender + sat + psat +
                             gpa + rank + geomarket + ap_score)
        
       filter_combosum <- filter_combos %>% count(filter_sum)
        colnames(filter_combosum) <- c("num_of_filters", "freq")
       
        ggplot(filter_combosum, aes(x = "", y=freq, fill = factor(num_of_filters))) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y") 
        
        
        filter_combos <- filter_combos %>% 
            mutate(
                hsgrad_class = ifelse(hsgrad_class==1, "grad_class", NA),
                zip = ifelse(zip==1, "zip", NA), #KSshould this include zip_code_file not missing too?
                states_fil = ifelse(states_fil==1, "state", NA), 
                cbsa = ifelse(cbsa==1, "cbsa", NA), 
                intl = ifelse(intl==1, "intl", NA), 
                segment = ifelse(segment==1, "segment", NA), 
                race = ifelse(race==1, "race", NA), 
                gender = ifelse(gender==1, "gender", NA), 
                sat = ifelse(sat==1, "sat", NA), 
                psat = ifelse(psat==1, "psat", NA), 
                gpa = ifelse(gpa==1, "gpa", NA), 
                rank = ifelse(rank==1, "rank", NA), 
                geomarket = ifelse(geomarket==1, "geomarket", NA), 
                ap_score = ifelse(ap_score==1, "APscores", NA))
        
        
        filter_combos[filter_combos == "NA"] <- NA_character_
        
        
       combos <- unique(filter_combos[c("hsgrad_class", "zip", "states_fil", "cbsa", "intl", "segment", "race",
                      "gender","sat", "psat","gpa", "rank" , "geomarket", "ap_score")], na.rm = TRUE)
        
        
            filter_combos %>% count(hsgrad_class, zip, states_fil, cbsa, intl, 
                                    segment, race, gender, sat, psat, gpa, rank, geomarket, ap_score, sort = TRUE) %>% top_n(30, n)
            
       
            
            df_0 <- group_by(filter_combos, hsgrad_class, zip, states_fil, 
                          cbsa, intl, segment, race, gender, 
                          sat, psat, gpa, rank, geomarket, ap_score) %>% count()
            
            df_0 %>% arrange(-n)

            df_0 <- df_0  %>% unite("string", c(hsgrad_class, zip, states_fil, 
                                                cbsa, intl, segment, race, gender, 
                                                sat, psat, gpa, rank, geomarket, ap_score), sep=",", remove = TRUE, na.rm = TRUE)
            
            
            df_0 <- df_0 %>% arrange(-n) %>% head(10)
            
            sum(df_0$n)
          
            
          # Descriptives for geomarket
             orders_df %>% count(geomarket)
            
            
################### ANALYSIS & VISUALS FOR RQ2 
    
             
    # how many students lists do we have?
        lists_orders_zip_hs_df %>% 
            summarise(n=n_distinct(ord_num)) 
             
                
    #FUNCTION FOR TABLE ON N, RACE, INCOME, PUB/PRIV SCHOOL CHARACTERISTICS OF STUDENT LIST PROSPECTS
        table_rq2a <- function(variables, columns) {
            
            #create counter
            counter = 0

            #loop through columns via filters (ex: all students, in-state, out-state, etc. )
            for (i in columns) {
                
                counter = counter+1
                
                if(i=="all_domestic") {
                 filter_string=c("stu_in_us==1")
                } else if(i=="in_state")
                {filter_string=c("stu_nonres==0")
                } else if(i=="out_of_state")
                {filter_string=c("stu_nonres==1")
                } else if(i=="research_univ")
                {filter_string=c("stu_in_us==1 & univ_c15basic=='15'")
                } else if(i=="regional_univ")
                {filter_string=c("stu_in_us==1 & univ_c15basic!='15'")
                } else if(i=="research_univ_instate")
                {filter_string=c("stu_in_us==1 & stu_nonres==0 & univ_c15basic=='15'")
                } else if(i=="research_univ_outofstate")
                {filter_string=c("stu_in_us==1 & stu_nonres==1 & univ_c15basic=='15'")
                } else if(i=="regional_univ_instate")
                {filter_string=c("stu_in_us==1 & stu_nonres==0 & univ_c15basic!='15'")
                } else if(i=="regional_univ_outofstate")
                {filter_string=c("stu_in_us==1 & stu_nonres==1 & univ_c15basic!='15'")}
                
                
                #create N row
                n <- as_data_frame(t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% count()))
                row.names(n) <- "Total N"
                
                #create race rows
                race <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
                
                race <- race %>% select(stu_race_cb, V1)
                race <- race %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "Pct Race-Missing", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==0, "Pct Race-No Response", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==1, "Pct AI/AN", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==2, "Pct Asian", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==3, "Pct Black", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==4, "Pct Latinx", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==8, "Pct NH/PI", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==9, "Pct White", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==10, "Pct Other Race", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==12, "Pct Multiracial", stu_race_cb)                                        )
                
                
                race<- race %>% remove_rownames %>% column_to_rownames(var="stu_race_cb")

                #create income row
                income <- as_data_frame (t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% 
                                                summarise (mean_inc = mean(zip_median_household_income, na.rm=T))))
                row.names(income) <- "Median Household Income (mean)"
                
                
                #create school type rows
                schtype <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
                    count(hs_school_control) %>% mutate(school_type = n / sum(n) * 100)
                
                schtype <- schtype %>% select(hs_school_control, school_type)
                schtype <- schtype %>% mutate(hs_school_control = ifelse(is.na(hs_school_control), "school unknown", hs_school_control))
                schtype<- schtype %>% remove_rownames %>% column_to_rownames(var="hs_school_control")
                schtype<- schtype %>% rename(V1 = school_type)
                row.names(schtype) <- c("Pct Private", "Pct Public", "Pct School Unknown") #NEED TO RE_DO THIS LIKE RACE ABOVE
                
                
                #concatenate all row_dfs for i-column
                temp_df <- bind_rows(mget(variables))
                #temp_df <- bind_rows(n, race, income, schtype)
                temp_df <- temp_df %>% rename(!!paste0("", i) := V1)
                temp_df <- rownames_to_column(temp_df, "row_subj")

                
                #first loop creates the master_df
                #second + loops appends the master df
                if(counter==1){master_df <- as.data.frame(temp_df)}
                if(counter>1){master_df <- merge(master_df,temp_df, by="row_subj", sort=FALSE)}

                
            }
        
            return(master_df)
            
        }
        
        
    
    # CALL FUNCTION TO CREATE TABLE 1
        
        #all possible vars: n, race, income, schtype
        vars <- c("n", "race", "income", "schtype") #all possible vars: n, race, income, schtype
        
        #all possible columns: all_domestic, in_state, out_of_state, research_univ, regional_univ, research_univ_instate, research_univ_outofstate, regional_univ_instate, regional_univ_outofstate,
        cols <- c("all_domestic","in_state", "out_of_state", "research_univ", "regional_univ", "research_univ_instate", "research_univ_outofstate", "regional_univ_instate", "regional_univ_outofstate") 
        df_rq2a<- table_rq2a(vars, cols) 
        
        #format table
        # df_rq2a <- df_rq2a %>% mutate_if(is.numeric, round, 0)
        # df_rq2a <- df_rq2a %>%  mutate_each(funs(prettyNum(., big.mark=",")))

 
      # international students
        
        df_int <- lists_orders_zip_hs_df %>% filter(stu_country!="united states") %>% group_by(stu_country) %>%
          summarise(n= n()) %>%
          mutate(pct = round(n / sum(n)*100, digits=1))
        
        df_int <- df_int %>% arrange(-n)
        
        df_int2 <- lists_orders_zip_hs_df %>% count(stu_internat)
        
        
    # checking for missingness
        
        lists_orders_zip_hs_df %>% count(hs_school_control)
        
        
################### ANALYSIS & VISUALS FOR RQ3
        
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
        
        
    # PROSPECT CHARS ACROSS INDIVIDUAL FILTERS
        
        # Create table function for lists across single filters
        #FUNCTION FOR TABLE ON N, RACE, INCOME, PUB/PRIV SCHOOL CHARACTERISTICS OF STUDENT LIST PROSPECTS
        table_rq3 <- function(variables, columns) {
          
          #create counter
          counter = 0
          
          #loop through columns via filters (ex: all students, in-state, out-state, etc. )
          for (i in columns) {
            
            counter = counter+1
            
            if(i=="all_domestic") {
              filter_string=c("stu_in_us==1")
            } else if(i=="GPA")
            {filter_string=c("stu_in_us==1 & filter_gpa==1")
            } else if(i=="PSAT")
            {filter_string=c("stu_in_us==1 & filter_psat==1")
            } else if(i=="SAT")
            {filter_string=c("stu_in_us==1 & filter_sat==1")
            } else if(i=="ZIP")
            {filter_string=c("stu_in_us==1 & filter_zip==1")
            } else if(i=="STATE")
            {filter_string=c("stu_in_us==1 & filter_states_fil==1")
            } else if(i=="RACE")
            {filter_string=c("stu_in_us==1 & filter_race==1")
            } else if(i=="HS Rank")
            {filter_string=c("stu_in_us==1 & filter_rank==1")
            } else if(i=="GENDER")
            {filter_string=c("stu_in_us==1 & filter_gender==1")
            } else if(i=="SEGMENT")
            {filter_string=c("stu_in_us==1 & filter_segment==1")
            } else if(i=="CBSA")
            {filter_string=c("stu_in_us==1 & filter_cbsa==1")}
            
            
            #create N row
            n <- as_data_frame(t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% count()))
            row.names(n) <- "Total N"
            
            #create race rows
            race <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
            
            race <- race %>% select(stu_race_cb, V1)
            race <- race %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "Pct Race-Missing", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==0, "Pct Race-No Response", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==1, "Pct AI/AN", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==2, "Pct Asian", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==3, "Pct Black", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==4, "Pct Latinx", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==8, "Pct NH/PI", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==9, "Pct White", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==10, "Pct Other Race", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==12, "Pct Multiracial", stu_race_cb)                                        )
            
            
            race<- race %>% remove_rownames %>% column_to_rownames(var="stu_race_cb")
            
            
            #create gender row
            #gender <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              #count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
            

            #create income row
            income <- as_data_frame (t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% 
                                         summarise (mean_inc = mean(zip_median_household_income, na.rm=T))))
            row.names(income) <- "Median Household Income (mean)"
            
            # create in-state versus out-of-state rows
            oos <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              count(stu_nonres) %>% mutate(V1 = n / sum(n) * 100)
            
            oos <- oos %>% select(stu_nonres, V1)
            oos <- oos %>% mutate(stu_nonres = ifelse(is.na(stu_nonres), "Pct- Residency Missing", stu_nonres),
                                  stu_nonres = ifelse(stu_nonres==0, "Pct In-State", stu_nonres),
                                  stu_nonres = ifelse(stu_nonres==1, "Pct Out-of-State", stu_nonres))
                                  
            oos<- oos %>% remove_rownames %>% column_to_rownames(var="stu_nonres")
            
            
            #create school type rows
            schtype <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              count(hs_school_control) %>% mutate(school_type = n / sum(n) * 100)
            
            schtype <- schtype %>% select(hs_school_control, school_type)
            schtype <- schtype %>% mutate(hs_school_control = ifelse(is.na(hs_school_control), "school unknown", hs_school_control))
            schtype<- schtype %>% remove_rownames %>% column_to_rownames(var="hs_school_control")
            schtype<- schtype %>% rename(V1 = school_type)
            row.names(schtype) <- c("Pct Private", "Pct Public", "Pct School Unknown") #NEED TO RE_DO THIS LIKE RACE ABOVE
            
            
            #concatenate all row_dfs for i-column
            temp_df <- bind_rows(mget(variables))
            #temp_df <- bind_rows(n, race, income, schtype)
            temp_df <- temp_df %>% rename(!!paste0("", i) := V1)
            temp_df <- rownames_to_column(temp_df, "row_subj")
            
            
            #first loop creates the master_df
            #second + loops appends the master df
            if(counter==1){master_df <- as.data.frame(temp_df)}
            if(counter>1){master_df <- merge(master_df,temp_df, by="row_subj", sort=FALSE)}
            
            
          }
          
          return(master_df)
          
        }
        
        
        
        # CALL FUNCTION TO CREATE TABLE 2
        
        #all possible vars: n, race, income, oos, schtype
        vars <- c("n", "race", "income", "oos","schtype") #all possible vars: n, race, income, schtype
        
        #all possible columns: all_domestic, in_state, out_of_state, research_univ, regional_univ, research_univ_instate, research_univ_outofstate, regional_univ_instate, regional_univ_outofstate,
        cols <- c("all_domestic", "GPA", "PSAT", "SAT","HS RANK", "RACE", "GENDER", "ZIP", "STATE","SEGMENT", "CBSA") 
        df_rq3<- table_rq3(vars, cols) 
        
        
        #format table
        df_rq3 <- df_rq3 %>% mutate_if(is.numeric, round, 0)
        df_rq3 <- df_rq3 %>%  mutate_each(funs(prettyNum(., big.mark=",")))
        

  ## PROSPECT CHARS ACROSS COMBOS of FILTERS-- IN-STATE/ZIP TEXAS A&M Texerkana Example
      
        
        # create categorical variable that use different combos of filters
        lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% 
                          mutate(filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_psat==1 & filter_gpa==1, "HS Grad, Zip, PSAT, GPA", NA),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_sat==1 & filter_gpa==1, "HS Grad, Zip, SAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_race==1 & filter_sat==1 & filter_psat==1 & filter_gpa==1 & filter_rank==1, "HS Grad, State, Race, SAT, PSAT, GPA, Rank", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_sat==1 & filter_psat==1 & filter_gpa==1, "HS Grad, Zip, SAT, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_sat==1 & filter_gpa==1, "HS Grad, State, SAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_psat==1 & filter_gpa==1, "HS Grad, State, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_race==1 & filter_psat==1 & filter_gpa==1, "HS Grad, State, Race, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_segment==1 & filter_gender==1 & filter_sat==1 & filter_gpa==1, "HS Grad, State, Segment, Gender, SAT, GPA", filter_combo))
    
        
        # number of orders across common filter combos
        lists_orders_zip_hs_df %>% 
          group_by(filter_combo) %>%
          summarise(n=n_distinct(ord_num)) 
        
        lists_orders_zip_hs_df %>% 
          group_by(filter_combo) %>%
          count(ord_num) 
        
        # top two filter combos across race/ethnicity
        common_combo_chars <- lists_orders_zip_hs_df %>% group_by(filter_combo) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
        
        #code doesnt work
        common_combo_chars <- common_combo_chars %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "Pct Race-Missing", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==0, "Pct Race-No Response", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==1, "Pct AI/AN", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==2, "Pct Asian", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==3, "Pct Black", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==4, "Pct Latinx", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==8, "Pct NH/PI", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==9, "Pct White", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==10, "Pct Other Race", stu_race_cb),
                                stu_race_cb = ifelse(stu_race_cb==12, "Pct Multiracial", stu_race_cb))
        
        #### ZOOM INTO TEXAS A&M ZIP CODE ORDERS
       
         # average out racial chars across orders using zip filters by Texas A&M Texerkana
        texasam <- lists_orders_zip_hs_df %>% filter(univ_id=="224545" & filter_combo=="HS Grad, Zip, PSAT, GPA")
        
        texasam %>% count(ord_zip_code)
        texasam %>% count(ord_zip_code, ord_num)
        
        texasam <- texasam %>% mutate(order_type_zips = recode(ord_zip_code, 
                                         "754|717|747|719|712|762|711|710|758|759|757" = "1",
                                         "754|773|770|774|775|762|758|759|757|717|747|719|712|711|710" = "2",
                                         "755|752|718|750|760|751|761|756" = "3",
                                         "773|770|774|775" = "4",
                                         .default = NA_character_))
        
        texasam %>% count(ord_zip_code, order_type_zips)
        
            # how many orders using this combo?
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% summarise(n=n_distinct(ord_num)) 
            
            # descriptives on filters
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_hs_grad_class)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_psat_score_max)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_psat_score_min)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_gpa_low)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_gpa_high)
            
            #number of orders within each zip code grouping
            texasam %>% group_by(order_type_zips) %>% summarise(n=n_distinct(ord_num)) 
            
            
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(order_type_zips, ord_psat_score_max)
            
        # racial characteristics
         texasam %>% group_by(order_type_zips) %>%
                count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=50)
        
        

      # Get ZIPCODE Characteristics 
         
         #switch zip to character
        acs_race_zipcodev3 <- acs_race_zipcodev3 %>% mutate(
                zip_char = as.character(zip_code)
              )
        
       texasam_zips_order1 <-  dplyr::filter(acs_race_zipcodev3, grepl('^710|^711|^712|^717|^719|^747|^754|^758|^759|^757|^762', zip_char))
       texasam_zips_order2 <-  dplyr::filter(acs_race_zipcodev3, grepl('^710|^711|^712|^717|^719|^747|^754|^757|^758|^759|^762|^770|^773|^774|^775', zip_char))
       texasam_zips_order3 <-  dplyr::filter(acs_race_zipcodev3, grepl('^718|^750|^751|^752|^755|^756|^760|^761', zip_char))
       texasam_zips_order4 <-  dplyr::filter(acs_race_zipcodev3, grepl('^770|^773|^774|^775', zip_char))
       texasam_zips_orderall <-  dplyr::filter(acs_race_zipcodev3, grepl('^710|^711|^712|^717|^718|^719|^747|^750|^751|^752|^754|^755|^756|^757|^758|^759|^760|^761|^762|^770|^773|^774|^775', zip_char))
       
        
       
       
       
       # create vars for zip codes at 3-digit
       texasam <- texasam %>% mutate(
         zip_3digit = str_sub(stu_zip_code, 1, 3)  
       )
        
       texasam_zips_orderall <- texasam_zips_orderall %>% mutate(
         zip_3digit = str_sub(zip_char, 1, 3)  
       )
       
       
      stu_zips_race <- texasam %>% filter(zip_3digit!="060" & zip_3digit!="201" & zip_3digit!="274" & zip_3digit!="301" & zip_3digit!="303" & zip_3digit!="778" & zip_3digit!="780" & zip_3digit!="781" & zip_3digit!="786" & zip_3digit!="800" & zip_3digit!="804" & zip_3digit!="917" & zip_3digit!="953") %>%
        group_by(zip_3digit) %>%
         count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) #%>% print(n=50)
      
      
      stu_zips_race <- as.data.frame(stu_zips_race)
      
      
                          # NEED TO EXPLORE THESE IN JANUARY-- but % is MINIMAL
                          stu_zips_race <- stu_zips_race %>% mutate(stu_race_cb= as.character(unclass(stu_race_cb)))
                          #stu_zips <- stu_zips %>% filter(stu_race_cb>=0) # IDK where the NA came from
                          
                          
                          #Can't get this to work to apply labels
                          # stu_zips_race <- stu_zips_race %>% mutate(
                          #   zip_char = as.character(zip_code)
                          # )
                          
                          stu_zips_race <- stu_zips_race %>% select(-n)
                          stu_zips_race <-  rename(stu_zips_race, stu_pct=V1)
                          
                          stu_zips_race <- stu_zips_race %>% mutate(stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
           
                 
                          
                          
        # NOTE  ZIPS less than 750 are out of state             
       
      pop_zips_race <- texasam_zips_orderall %>% 
        group_by(zip_3digit) %>%
        summarize(
          #n_obs = sum(n()),
          pop_pct.White =  mean(pop_white_15_19_pct, na.rm = TRUE),
          pop_pct.Asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
          pop_pct.Black =  mean(pop_black_15_19_pct, na.rm = TRUE),
          pop_pct.Latinx =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
          #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
          #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
          pop_pct.AIAN =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
          pop_pct.Multiracial =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
          #pop_med.inc = mean(median_household_income, na.rm = TRUE)
        )   
      
      # reshape pop wide to long, drops NA stu_race_cb
      pop_zips_race <- pop_zips_race %>% gather(stu_race_cb, pop_pct, -c(zip_3digit))
      pop_zips_race <- pop_zips_race %>% mutate_all(~gsub("pop_pct.", "", .))
      
      
      # merge by three digit zip CREATE FIGURE OBJECT
      table_texasam_zip <- merge(stu_zips_race, pop_zips_race, by=c("zip_3digit", "stu_race_cb"))
      table_texasam_zip$pop_pct <- as.numeric( table_texasam_zip$pop_pct)
      table_texasam_zip <- table_texasam_zip %>% mutate_if(is.numeric, round, 0)
      table_texasam_zip <- table_texasam_zip %>%  mutate_each(funs(prettyNum(., big.mark=",")))
      
      table_texasam_zip <- table_texasam_zip %>%  mutate(
                              ppt_diff_stu_pop = as.numeric(stu_pct) - as.numeric(pop_pct))
      
      table_texasam_zip <- table_texasam_zip %>% arrange(stu_race_cb, ppt_diff_stu_pop)
      
      # NOTE  ZIPS less than 750 are out of state             
      ggplot(table_texasam_zip, aes(fill=stu_race_cb, y=ppt_diff_stu_pop, x=zip_3digit)) + 
        geom_bar(position="dodge", stat="identity") + coord_flip()
      
      
      
      
      # median income of zip codes fro student prospects versus population
      
          # economic characteristics of population at zip
          pop_zip_inc <- texasam_zips_orderall %>% 
              group_by(zip_3digit) %>%
              summarize(
                pop_med_inc = mean(median_household_income, na.rm = TRUE)
              )   
      
          # economic characteristics of prospects
          stu_zip_inc <-texasam %>% group_by(zip_3digit) %>% 
            summarise (stu_mean_inc = mean(zip_median_household_income, na.rm=T))
          
          table_texasam_zip_inc <- merge(stu_zip_inc, pop_zip_inc, by="zip_3digit")
          
      # EXPLORATORY ANALYSIS BY AVERAGING ACROSS ORDER GROUPINGS
       #  # racial & economic characteristics by filter order for texas a&m 
       # texasam_zips_orderall %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   )   
       # 
       # 
       # texasam_zips_order1 %>% 
       #    summarize(
       #      n_obs = sum(n()),
       #      pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #      pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #      pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #      pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #      #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #      #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #      pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #      pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #      avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #    )   
       # 
       # texasam_zips_order2 %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   )   
       # 
       # texasam_zips_order3 %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   ) 
       #  
       # 
       # texasam_zips_order4 %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   ) 
       # 
       
       
    ## PROSPECT CHARS ACROSS COMBOS of FILTERS-- IN-STATE/STATE FILTER EXAMPLE 
          
          lists_orders_zip_hs_df %>% 
            group_by(filter_combo) %>%
            summarise(n=n_distinct(ord_num)) 
          
          lists_orders_zip_hs_df %>% 
            filter(filter_combo=="HS Grad, State, Race, PSAT, GPA") %>%
            count(univ_name) %>% print(n=400)
          
          lists_orders_zip_hs_df %>% 
            filter(filter_combo=="HS Grad, State, Race, PSAT, GPA") %>%
            count(ord_state_name, univ_name) %>% print(n=400)
          
          # Texas A & M University-College Station OR UC San Diego?
          lists_orders_zip_hs_df %>% 
            filter(filter_combo=="HS Grad, State, Race, PSAT, GPA") %>%
            count(ord_state_name, univ_name) %>% print(n=400)
       
     
          
      ## PROSPECT CHARS ACROSS COMBOS of FILTERS-- Out of State/Segment Illinois Example
          
          # check filters across orders that use segment; these filter by segment at the state level but not cbsa
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_states_fil==1 &  filter_cbsa!=1 &  filter_psat==1) %>% count(ord_num)
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_states_fil==1 &  filter_cbsa!=1 & filter_psat==1) %>% count(ord_state_name)
          
          # check filters across orders that use segment; these filter by segment at the state & CBSA level
          ui_uc <-lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1)
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_num)
          x1 <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% select(ord_num|ord_cbsa_name|starts_with("filter_"))
          x1 <- distinct(x1, ord_num, .keep_all = TRUE)
              
              # see full list of cbsa across three groupings
              c(x1[1,2])
              c(x1[5,2])
              c(x1[6,2])
              
              # descriptives on filters
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_hs_grad_class)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_psat_score_max)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_psat_score_min)
              
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_sat_score_max)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_sat_score_min)
              
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_gpa_low)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_gpa_high)  
          
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_segment)
              x <- ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_segment)  
              
                    # see full list of cbsa across three groupings
                    c(x[1,1])
        
        # Philadelphia-Camden-Wilmington, PA-NJ-DE-MD; 37980
        # students purchased across all three types of segment orders 
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>% count()
              
          philly_studentlist <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>%    
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
          philly_studentlist$tot_students <- (lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>% count())$n
          philly_studentlist <- philly_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "Multiracial")]
          
        
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
                AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
                Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
                #pct_stu_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
                #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
            )
        
        
        # New York; 35620
        # students purchased across all three types of segment orders 
        lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>% count()
        
        ny_studentlist <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>%    
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
        ny_studentlist$tot_students <- (lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>% count())$n
        ny_studentlist <- ny_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "Multiracial")]
        
        
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
            AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            #pct_stu_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
          )
        
        
        # Los Angeles; 31080
        # students purchased across all three types of segment orders 
        lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>% count()
        
        la_studentlist <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>%    
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
        la_studentlist$tot_students <- (lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>% count())$n
        la_studentlist <- la_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "Multiracial")]
        
        
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
            AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            #pct_stu_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
          )  
        
    fig_rq3_segment_race <- rbind(philly_studentlist, philly_metro, ny_studentlist, ny_metro, la_studentlist, la_metro)
        
        
        
    # income of prospects across all three metros [don't know how to incorporate income for the metro area]
    philly_studentlist_inc <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T)) 
      
    ny_studentlist_inc <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))
    
    la_studentlist_inc <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))
    
    
    # 2019 ACS
        # Philadelphia = 74,533
        # New York = 83,160
        # LA metro =77,774

    fig_rq3_segment_race_inc <- fig_rq3_segment_race %>% 
      mutate(
        income = c(
          philly_studentlist_inc$stu_mean_inc, 74533,
          ny_studentlist_inc$stu_mean_inc, 83160,
          la_studentlist_inc$stu_mean_inc, 77774
        )
      )
        
        
    ## PROSPECT CHARS ACROSS COMBOS of FILTERS-- Women in STEM Example for UC San Diego
    
    #check orders that used female
    orders_df %>% filter(gender=="Female") %>% count(univ_name,univ_id)
    
      # Focus on UC San Diego Orders
      orders_gender <- orders_df %>% filter(gender=="Female" & univ_id==110680)
      
        # order titles
          orders_gender %>% count(order_title) #6 order for in-state, 6 for out-of-state
          orders_gender <- orders_gender %>% mutate(
            instate = ifelse(str_detect(order_title, "CA"), 1, 0)
          )
          
          orders_gender %>% count(instate) #5 order for in-state, 6 for out-of-state
          
        # filters used
          
              # GPA-- all orders used high A+ and low of B
              orders_gender %>% group_by(instate) %>% count(gpa_high, gpa_low)
          
              #SAT
              orders_gender %>% group_by(instate) %>% count(sat_score_min, sat_score_max)
              
              #For Field they used EITHER major OR AP scores
              orders_gender %>% group_by(instate) %>% count(major, ap_scores)
              orders_gender %>% count(ap_scores)
              
              # two different types of order filters by AP Scores
              #exact same fields, except one set scores are filtered 4-5 another are 3-5
               # 1 type --- 2 type 
              "Biology~4~5 --- Biology~3~5
              Chemistry~4~5 --- Chemistry~3~5
              Computer Science A~4~5 --- Computer Science A~3~5
              CompSciP~4~5 --- CompSciP~3~5
              Environmental Science~4~5 --- Environmental Science~3~5
              Calculus AB~4~5 --- Calculus AB~3~5
              Calculus BC~4~5 --- Calculus BC~3~5
              Physics 1 ~4~5 --- Physics 1~3~5
              Physics 2 ~4~5 --- Physics 2~3~5
              Physics B ~4~5 --- Physics B~3~5
              Physics C: Electricity and Magnetism~4~5 --- Physics C: Electricity and Magnetism~3~5
              Physics C: Mechanics~4~5 --- Physics C: Mechanics~3~5
              Statistics~4~5 ---  Statistics~3~5" 
              
              #create score range var for AP filters
              
              orders_gender <- orders_gender %>% mutate(
                ap_score_range = ifelse(str_detect(ap_scores, "3~5"), "3~5", "4-5")
              )
              
              orders_gender %>% count(ap_scores, ap_score_range)
              orders_gender %>% group_by(instate) %>% count(ap_score_range) #in-state orders used only 3-5, both orders for 4-5 are out of state
              
              # how many students purchased
              orders_gender %>%  summarise(total_orders = n(),
                                           total_students = sum(num_students, na.rm = T))
              
        # resulting student lists 
         list_gender  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_gender$order_num)
    
           # how many students purchased
            list_gender %>%  summarise(total_students = n())
            # unique IDs for order nums
            lists_orders <- list_gender %>%
              count(ord_num)  #ONLY have data for 8 orders???
            
            #which orders do I have lists for?
            orderswlists_gender  <-   subset(orders_gender, order_num %in% lists_orders$ord_num)
            orderswlists_gender %>% group_by(instate) %>% count(major, ap_scores)
                  # 5-outofstate: 3 use Major and 2 use AP scores 4-5
                  # 3 in-state: 1 uses Major and 2 use AP scores 3-5
            
            #orders in MA
            list_gender %>% count(stu_state)
            
            #create order vars in lists df
            list_gender <- merge(x = list_gender, y = orderswlists_gender[ , c("instate", "major", "ap_score_range", "order_num")], by.x  = "ord_num", by.y  = "order_num", all.x=TRUE)

            
       #income characteristics
            list_gender %>% group_by(instate) %>% count() #10.6k out of state versus 2.3k in-state
            
            list_gender %>% group_by(instate) %>% select(zip_median_household_income) %>%
              summarise(across(
                .cols = where(is.numeric), 
                .fns = list(Mean = mean, SD=sd), na.rm = TRUE, 
                .names = "{col}_{fn}"
              ))
            
          #race characteristics
            list_gender %>% group_by(instate) %>%
              count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
            
              #instate= 56% White; 30% Asian; 2% Black, 5% Latinx, 4%multiracial, 0% Native
              #outofstate= 32% White; 41% Asian; 1% Black, 14% Latinx, 6%multiracial, 0% Native
       
          
            
            
    ## PROSPECT CHARS ACROSS COMBOS of FILTERS-- Targeting Students of Color
            
            #check orders that used race/ethnicity explicitly
            orders_df %>%  count(race_ethnicity)
            orders_df$race_ethnicity[orders_df$race_ethnicity==''] <- NA
            
            race_orders <- orders_df %>% filter(!is.na(race_ethnicity))
            race_orders %>%  count(race_ethnicity)
            race_orders %>%  pull(race_ethnicity) #printing really long string
            
            
        # create new categorical race filters var
            race_orders <- race_orders %>% mutate(
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native", "Native American", NA_character_),
              race_filter = ifelse(race_ethnicity=="Black or African American", "Black", race_filter),
              race_filter = ifelse(race_ethnicity=="Cuban|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx", race_filter),
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian", race_filter),
              
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Native American, Native Hawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Native American", race_filter),
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)|Native Hawaiian or Other Pacific Islander", "Asian, NativeHawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Black", race_filter),
              race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban", "Latinx, Black, Native American", race_filter), 
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|White (including Middle Eastern origin)|Other", "Asian, White", race_filter), 
              race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban", "Latinx, Black, Native American, NativeHawaii/PI ", race_filter),
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|\rAsian (including Indian subcontinent and Philippines origin)|Cuban|\rBlack or African American|\rHispanic or Latino (including Spanish origin)|\rMexican|\rPuerto Rican|\rOther Hispanic or Latino|\rNative Hawaiian or Other Pacific Islander", "Latinx, Black, Asian, Native American", race_filter)            )
            
            
            race_orders %>%  count(race_filter, race_ethnicity)
            race_orders %>%  count(race_filter)
            
        
          #universities using race/ethnicity filter
            race_orders %>% count(univ_name)
            
            
              # Texas A&M CS- 10 orders for Latinx, Black students (4 instate, 6 out of state)
                race_orders %>% filter(univ_name=="Texas A & M University-College Station") %>%  count(race_filter)
                race_orders %>% filter(univ_name=="Texas A & M University-College Station") %>%  count(race_filter, state_name)
                race_orders %>%  filter(univ_name=="Texas A & M University-College Station") %>% summarise(total_orders = n(),
                                            total_students = sum(num_students, na.rm = T))

                orders_num <- race_orders %>% filter(univ_name=="Texas A & M University-College Station") %>%  count(order_num)
                orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
                orderswlists_race %>% count(ord_num) # we have 8/10 orders' resulting student lists; we don't have orders for Latinx+Black

                    #other filters used with race/ethnicity
                    race_orders_univ <- race_orders %>% filter(univ_name=="Texas A & M University-College Station")
                    race_orders_univ %>% count(order_num, hs_grad_class, order_title, race_filter) #only have latinx orders

                    
             # # University of Illinois at Urbana-Champaign - Across all race/ethnicity but All are for in-state students
             #  race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign") %>%  count(race_filter)
             #  race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign") %>%  count(race_filter, state_name)
             #  race_orders %>%  filter(univ_name=="University of Illinois at Urbana-Champaign") %>% summarise(total_orders = n(),
             #                                                                                             total_students = sum(num_students, na.rm = T))
             # 
             #  orders_num <- race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign") %>%  count(order_num)
             #  orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
             #  orderswlists_race %>% count(ord_num) # we have 51/53 orders' resulting student lists
             # 
             #          #other filters used with race/ethnicity
             #          race_orders_univ <- race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign")
             #          race_orders_univ %>% count(hs_grad_class,order_num, order_title, race_filter) %>% print(n=60)
             #          
             #          
             #        #focus on pre-pandemic orders made in August 2017-then make inferences post pandemic "catch ups"  
             #          orders_num <- race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>%  count(order_num)
             #          orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
             #          orderswlists_race %>% count(ord_num) # we have 5/5 orders for 2018/2019/2020 HS classes
             #          race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(hs_grad_class,order_num, order_title, race_filter) %>% print(n=60)
             #          race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% summarise(total_prosp_purchased = sum(num_students, na.rm = T)) #22,311
             #          orderswlists_race %>% count() #we have 22,310 total students
             #          
              # University of California-San Diego- 7 orders for Latinx, Native American (2 instate, 5 out of state)
                # race_orders %>% filter(univ_name=="University of California-San Diego") %>%  count(race_filter)
                # race_orders %>% filter(univ_name=="University of California-San Diego") %>%  count(race_filter, state_name)
                # race_orders %>%  filter(univ_name=="University of California-San Diego") %>% summarise(total_orders = n(),
                #                                                                                            total_students = sum(num_students, na.rm = T))                
                # 
                # orders_num <- race_orders %>% filter(univ_name=="University of California-San Diego") %>%  count(order_num)
                # orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
                # orderswlists_race %>% count(ord_num) # we have 7/7 orders' resulting student lists
                # 
                #       #other filters used with race/ethnicity
                #       race_orders_univ <- race_orders %>% filter(univ_name=="University of California-San Diego")
                #       race_orders_univ %>% count(order_title, race_filter) %>% print(n=60)
                #       
           
           
          # # Use UI Urbana-Champaign as in-state example-- IL
          #             
          #             #check for missing school id
          #              orderswlists_race %>% count(is.na(stu_hs_code)) #only 52 missing school IDs
          #              orderswlists_race %>% count(ord_title) 
          #              orderswlists_race %>% count() 
          #              
          #              #how many from each metro 
          #              orderswlists_race %>% count(hs_cbsatitle_1) %>% arrange(-n) #77% from Chicago; look at Zoomed Map of Chicago-- then explore rural schools?
          #              orderswlists_race %>% filter(hs_cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI") %>% count() 
          #              orderswlists_race %>% filter(hs_cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI") %>% count(stu_race_cb) #77% from Chicago; look at Zoomed Map of Chicago-- then explore rural schools?
          #              
          #             
          #              #Chicago DF
          #              orderswlists_race_chi <- orderswlists_race %>% filter(hs_cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI") 
          #              
          #              
          #              #create dummies of race/ethnicity & order filters to aggregate
          #              orderswlists_race_chi <- orderswlists_race_chi %>% mutate(stu_race_missing = ifelse(is.na(stu_race_cb), 1, 0),
          #                                                   stu_race_noresponse = ifelse(stu_race_cb==0, 1, 0),
          #                                                   stu_race_aian = ifelse(stu_race_cb==1, 1, 0),
          #                                                   stu_race_asian = ifelse(stu_race_cb==2, 1, 0),
          #                                                   stu_race_black = ifelse(stu_race_cb==3, 1, 0),
          #                                                   stu_race_latinx = ifelse(stu_race_cb==4, 1, 0),
          #                                                   stu_race_nhpi = ifelse(stu_race_cb==8, 1, 0),
          #                                                   stu_race_white = ifelse(stu_race_cb==9, 1, 0),
          #                                                   stu_race_other = ifelse(stu_race_cb==10, 1, 0),
          #                                                   stu_race_multi = ifelse(stu_race_cb==12, 1, 0),
          #                                                   stu_ordertype1 = ifelse(ord_title=="IL 1450+ August 2017", 1, 0),
          #                                                   stu_ordertype2 = ifelse(ord_title=="IL Lower Range August 2017", 1, 0),
          #                                                   stu_ordertype3 = ifelse(ord_title=="IL Middle 50% August 2017 ", 1, 0),
          #                                                   stu_ordertype4 = ifelse(ord_title=="PAP Honors August 2017", 1, 0),
          #                                                   stu_ordertype5 = ifelse(ord_title=="PAP Traditional August 2017", 1, 0),
          #                                                   stu_ordertype6 = ifelse(ord_title=="URM Lower Range August 2017", 1, 0))
          # 
          # 
          #                           #aggregate student list data to school-level with total num students + race/ethnicity
          #                         school_lists_chi <- orderswlists_race_chi %>% select(hs_ncessch, stu_race_noresponse, stu_race_missing,
          #                                                              stu_race_aian, stu_race_asian, stu_race_black,
          #                                                              stu_race_latinx, stu_race_nhpi, stu_race_white,
          #                                                              stu_race_other, stu_race_multi, stu_ordertype1, stu_ordertype2, stu_ordertype3,
          #                                                              stu_ordertype4, stu_ordertype5, stu_ordertype6) %>% group_by(hs_ncessch) %>% summarize_all(sum)
          # 
          # 
          #                          # now create school df with total students versus student prosp purchased for Houston
          #                            chi_pubprivhs <- pubhs_privhs_data %>% filter(cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI")
          # 
          #                         # merge in purchased prospects
          #                            chi_pubprivhs<- merge(x = chi_pubprivhs, y = school_lists_chi, by.x  = "ncessch",  by.y  = "hs_ncessch", all.x=TRUE)
          # 
          #                         # replace NAs to zeros on student propects purchased from schools
          #                            chi_pubprivhs <- mutate(chi_pubprivhs, across(starts_with("stu_race"), ~ifelse(is.na(.x),0,.x)))
          #                            chi_pubprivhs <- mutate(chi_pubprivhs, across(starts_with("stu_ordertype"), ~ifelse(is.na(.x),0,.x)))
          # 
          # 
          #                               #KS analyses
          #                               
          #                               # #academic filters used
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(gpa_high, gpa_low)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, sat_score_min, sat_score_max)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, psat_score_min, psat_score_max)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, rank_high, rank_low)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, ap_scores)
          #                                   
          #                                   #all six orders use GPA=A+ to B-; RANK= highest tenth to second fifth; No AP score filters
          #                                   
          #                                   #total students purchased across different orders
          #                                   orderswlists_race_chi %>%  group_by(ord_title, ord_race_ethnicity) %>% count(stu_race_cb) %>% print(n=40)
          #                                               
          #                                   #prospects from order "IL Middle 50% August 2017  FOR Asian, NativeHawaii/PI" (P)SAT=1280-1440
          #                                   chi_pubprivhs %>% filter(stu_ordertype1==1) %>% summarise(total_prosp_asian = sum(stu_race_asian, na.rm = T))
          #                                   
          #                                   chi_pubprivhs %>% filter(stu_ordertype1==1) %>%
          #                                       group_by(ncessch, private) %>%
          #                                       summarise(total_stu_pctwhite = sum(pct_white, na.rm = T),
          #                                                 total_prosp_asian = sum(stu_race_asian, na.rm = T),
          #                                                 total_prosp_black = sum(stu_race_black, na.rm = T),
          #                                                 total_prosp_latinx = sum(stu_race_latinx, na.rm = T)) %>% arrange(-total_stu_pctwhite) %>% print(n=150)
          # 
          #                                     houston_pubprivhs %>% group_by(private) %>% summarise(total_stu_latinx =  sum(total_hispanic, na.rm = T),
          #                                                                                           total_prosp_black =  sum(stu_race_black, na.rm = T),
          #                                                                                           total_prosp_latinx =  sum(stu_race_latinx, na.rm = T),)
          # 
          #                                     orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX") %>% count(ord_num)
          #                                     orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX" & stu_race_cb==3) %>% count(ord_num)
          #                                     race_orders %>% filter(order_num==449030) %>% count(race_filter)
          #                                     race_orders %>% filter(order_num==549428) %>% count(race_filter)
          #                                     race_orders %>% filter(order_num==449339) %>% count(race_filter)

                                
              # Use Texas A&M -- Look Houston (in-state) and Los Angeles (out of state); School-Level Aggregates           

                    #check for missing school id
                    orderswlists_race %>% count(is.na(stu_hs_code)) #only 79 missing school IDs
                    orderswlists_race %>% count(ord_title) #

                    #how many from each metro in-state versus out-of-state
                    orderswlists_race %>% count(hs_cbsatitle_1) %>% arrange(-n) #houston, dallas, austin/san antonio

                    #houston df
                    orderswlists_race_tx <- orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX")

                        #create dummies of race/ethnicity & order filters to aggregate
                        orderswlists_race_tx <- orderswlists_race_tx %>% mutate(stu_race_missing = ifelse(is.na(stu_race_cb), 1, 0),
                                                stu_race_noresponse = ifelse(stu_race_cb==0, 1, 0),
                                                stu_race_aian = ifelse(stu_race_cb==1, 1, 0),
                                                stu_race_asian = ifelse(stu_race_cb==2, 1, 0),
                                                stu_race_black = ifelse(stu_race_cb==3, 1, 0),
                                                stu_race_latinx = ifelse(stu_race_cb==4, 1, 0),
                                                stu_race_nhpi = ifelse(stu_race_cb==8, 1, 0),
                                                stu_race_white = ifelse(stu_race_cb==9, 1, 0),
                                                stu_race_other = ifelse(stu_race_cb==10, 1, 0),
                                                stu_race_multi = ifelse(stu_race_cb==12, 1, 0),
                                                stu_ordertype1 = ifelse(ord_title=="2020 PSAT NH 1270-1420 NM,OK,AR (H)", 1, 0),
                                                stu_ordertype2 = ifelse(ord_title=="2020 PSAT NH 1270-1470 TX (H_include all)", 1, 0),
                                                stu_ordertype3 = ifelse(ord_title=="2020 PSAT NH 1280-1470 CO,CA (H)", 1, 0),
                                                stu_ordertype4 = ifelse(ord_title=="2020 PSAT NH 1370-1420 MS,AL,SC (H)", 1, 0),
                                                stu_ordertype5 = ifelse(ord_title=="2020 PSAT NH 1370-1450 LA,KY,TN,FL (H)", 1, 0),
                                                stu_ordertype6 = ifelse(ord_title=="2020 PSAT NH 1370-1470 GA,VA (H)", 1, 0),
                                                stu_ordertype7 = ifelse(ord_title=="2021 PSAT NH 1290-1520 TX (H)", 1, 0),
                                                stu_ordertype8 = ifelse(ord_title=="PSAT NH 1310-1470 MO,IL (H)", 1, 0))

                      
                        #aggregate student list data to school-level with total num prospects + prospect race/ethnicity       
                         school_lists_tx <- orderswlists_race_tx %>% select(hs_ncessch, stu_race_noresponse, stu_race_missing,
                                                           stu_race_aian, stu_race_asian, stu_race_black,
                                                           stu_race_latinx, stu_race_nhpi, stu_race_white,
                                                           stu_race_other, stu_race_multi, stu_ordertype1, stu_ordertype2, stu_ordertype3,
                                                           stu_ordertype4, stu_ordertype5, stu_ordertype6, stu_ordertype8) %>% group_by(hs_ncessch) %>% summarize_all(sum)


                       # now create school df with total students versus student prosp purchased for Houston
                         houston_pubprivhs <- pubhs_privhs_data %>% filter(cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX")

                      # merge in purchased prospects
                         houston_pubprivhs<- merge(x = houston_pubprivhs, y = school_lists_tx, by.x  = "ncessch",  by.y  = "hs_ncessch", all.x=TRUE)

                      # replace NAs to zeros
                         houston_pubprivhs <- mutate(houston_pubprivhs, across(starts_with("stu_race"), ~ifelse(is.na(.x),0,.x)))
                         houston_pubprivhs <- mutate(houston_pubprivhs, across(starts_with("stu_ordertype"), ~ifelse(is.na(.x),0,.x)))

                      # merge in pub HS data on SAT scores
                         houston_pubhs <- houston_pubprivhs %>% filter(private==0 & pub_sch_type==1) #only regular schools
                         txhs<-read_xlsx("data/achievement data/satact-campus-data-class-2020.xlsx", sheet = "satact-campus-data-class-2020")
                         
                         txhs$CampName <- toupper(txhs$CampName)
                              #fix 5 obs not merging correctly
                                txhs <- txhs %>% mutate(
                                  CampName= ifelse(CampName=="DEER PARK H S", "DEER PARK HS", CampName),
                                  CampName= ifelse(CampName=="CARVER H S FOR APPLIED TECH/ENGINE", "CARVER H S FOR APPLIED TECH/ENGINEERING/ARTS", CampName),
                                  CampName= ifelse(CampName=="ENERGIZED FOR STEM ACADEMY SOUTHEA", "ENERGIZED FOR STEM ACADEMY SOUTHEAST H S", CampName),
                                  CampName= ifelse(CampName=="ENERGIZED FOR STEM ACADEMY SOUTHWE", "ENERGIZED FOR STEM ACADEMY SOUTHWEST H S", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF ADVANCEMENT-HOUS", "HARMONY SCHOOL OF ADVANCEMENT-HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF DISCOVERY - HOUS", "HARMONY SCHOOL OF DISCOVERY - HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF INGENUITY - HOUS", "HARMONY SCHOOL OF INGENUITY - HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF INNOVATION - HOUS", "HARMONY SCHOOL OF INNOVATION - HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HOUSTON ACADEMY FOR INTERNATIONAL", "HOUSTON ACADEMY FOR INTERNATIONAL STUDIES", CampName),
                                  CampName= ifelse(CampName=="HOUSTON GATEWAY ACADEMY - CORAL CA", "HOUSTON GATEWAY ACADEMY - CORAL CAMPUS", CampName),
                                  CampName= ifelse(CampName=="HOUSTON MATH SCIENCE AND TECHNOLOG", "HOUSTON MATH SCIENCE AND TECHNOLOGY CENTER", CampName),
                                  CampName= ifelse(CampName=="ILTEXAS KATY WESTPARK H S", "ILTEXAS - KATY/WESTPARK H S", CampName),
                                  CampName= ifelse(CampName=="MICKEY LELAND COLLEGE PREP ACAD FO", "MICKEY LELAND COLLEGE PREP ACAD FOR YOUNG MEN", CampName),
                                  CampName= ifelse(CampName=="KINDER H S FOR PERFORMING AND VISU", "PERFOR & VIS ARTS H S", CampName),
                                  CampName= ifelse(CampName=="HOUSTON T-STEM AND EARLY COLLEGE H", "RAUL YZAGUIRRE SCHOOL FOR SUCCESS", CampName),
                                  CampName= ifelse(CampName=="ROBERT TURNER COLLEGE AND CAREER H", "ROBERT TURNER COLLEGE AND CAREER H S", CampName),
                                  CampName= ifelse(CampName=="TEXAS CONNECTIONS ACADEMY AT HOUST", "TEXAS CONNECTIONS ACADEMY AT HOUSTON", CampName),
                                  CampName= ifelse(CampName=="WESTCHESTER ACADEMY FOR INTERNATIO", "WESTCHESTER ACADEMY FOR INTERNATIONAL STUDIES", CampName),
                                  CampName= ifelse(CampName== "HARMONY SCHOOL OF INGENUITY-HOUSTO","HARMONY SCHOOL OF INGENUITY-HOUSTON", CampName),
                                  CampName= ifelse(CampName== "HARMONY SCHOOL OF INNOVATION - KAT","HARMONY SCHOOL OF INNOVATION - KATY", CampName),
                                  CampName= ifelse(CampName=="CARVER H S FOR APPLIED TECH/ENGINE", "CARVER H S FOR APPLIED TECH/ENGINEERING/ARTS", CampName))
                         txhs$name2<-as.factor(txhs$CampName)
                         txhs %>% count(RegnName) 
                         txhs %>% count(CntyName) %>% print(n=300)
                         
                         txhs<- txhs %>% filter(
                           CntyName=="Austin County"|CntyName=="Brazoria County"|CntyName=="Fort Bend County" |
                             CntyName=="Galveston County" | CntyName=="Harris County" | CntyName=="Liberty County" | CntyName=="Chambers County" |
                             CntyName=="Montgomery County" | CntyName=="Waller County" | CntyName=="Fayette County" | CntyName=="Taylor County" | CntyName=="Grimes County" | RegnName=="Houston")

                         
                         txhs %>% 
                           summarise(n=n_distinct(Campus)) #248 schools
                         
                         
                        #reshape wide to long
                         txhs <- txhs %>% filter(Group=="All Students"| Group=="African American" | Group=="American Indian"| Group=="Asian" | 
                                                   Group=="Hispanic" |  Group=="White"| Group=="Pacific Islander" |  Group=="Multiracial"|  Group=="Missing Ethnicity")
                         
                         txhs <- txhs %>% mutate(
                           race=ifelse(Group=="All Students", "all", NA_character_),
                           race= ifelse(Group=="African American", "black", race),
                           race= ifelse(Group=="American Indian", "native", race),
                           race= ifelse(Group=="Asian", "asian", race),
                           race= ifelse(Group=="Hispanic", "latinx", race),
                           race= ifelse(Group=="White", "white", race),
                           race= ifelse(Group=="Pacific Islander", "pacisland", race),
                           race= ifelse(Group=="Multiracial", "multirace", race),
                           race= ifelse(Group=="Missing Ethnicity", "missingrace", race))
                         
                         txhs %>% count(race,Group)
                         
                         txhs <- txhs %>% select(race, Campus, CampName, name2, Grads_Mskd,Exnees_Mskd, Part_Rate, Crit_Mskd, Above_Crit_Rate, TSI_Both_Mskd, Above_TSI_Both_Rate)
                         
                         
                         txhs_wide <- txhs %>%
                           gather(key, value, -Campus, -race, -name2, -CampName) %>%
                           unite(col, key, race) %>%
                           spread(col, value)
                         
                         #merge to CCD to get ncessch
                         txhs<-merge(x=houston_pubhs[, c("name", "ncessch")], y=txhs_wide, by.x="name",by.y="name2", all.x=TRUE)
                         
                         txhs %>% 
                           summarise(n=n_distinct(ncessch)) #only 230 schools
                         
                         txhs %>% 
                           count(is.na(Campus)) #11 missing TEA ID
                         
                         txhs %>% select(ncessch, name, Campus) %>%
                           filter(is.na(Campus))
                         
                        #check_merge <- txhs %>% select(name, CampName) 
                         
                            #KS analyses
                              houston_pubprivhs %>% filter(stu_race_black>1 |stu_race_latinx>1) %>%
                                group_by(ncessch, private) %>%
                                summarise(total_stu_pctwhite = sum(pct_white, na.rm = T),
                                          total_prosp_black = sum(stu_race_black, na.rm = T),
                                          total_prosp_latinx = sum(stu_race_latinx, na.rm = T)) %>% arrange(-total_stu_pctwhite) %>% print(n=150)

                              houston_pubprivhs %>% group_by(private) %>% summarise(total_stu_latinx =  sum(total_hispanic, na.rm = T),
                                                                                    total_prosp_black =  sum(stu_race_black, na.rm = T),
                                                                                    total_prosp_latinx =  sum(stu_race_latinx, na.rm = T),)

                              orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX") %>% count(ord_num)
                              orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX" & stu_race_cb==3) %>% count(ord_num)
                              race_orders %>% filter(order_num==449030) %>% count(race_filter)
                              race_orders %>% filter(order_num==549428) %>% count(race_filter)
                              race_orders %>% filter(order_num==449339) %>% count(race_filter)
                              
                              
                  #los angeles df
                    orderswlists_race_la <- orderswlists_race %>% filter(hs_cbsatitle_1=="Los Angeles-Long Beach-Anaheim, CA")

                         #create dummies of race/ethnicity & order filters to aggregate
                         orderswlists_race_la <- orderswlists_race_la %>% mutate(stu_race_missing = ifelse(is.na(stu_race_cb), 1, 0),
                                                                                 stu_race_noresponse = ifelse(stu_race_cb==0, 1, 0),
                                                                                 stu_race_aian = ifelse(stu_race_cb==1, 1, 0),
                                                                                 stu_race_asian = ifelse(stu_race_cb==2, 1, 0),
                                                                                 stu_race_black = ifelse(stu_race_cb==3, 1, 0),
                                                                                 stu_race_latinx = ifelse(stu_race_cb==4, 1, 0),
                                                                                 stu_race_nhpi = ifelse(stu_race_cb==8, 1, 0),
                                                                                 stu_race_white = ifelse(stu_race_cb==9, 1, 0),
                                                                                 stu_race_other = ifelse(stu_race_cb==10, 1, 0),
                                                                                 stu_race_multi = ifelse(stu_race_cb==12, 1, 0),
                                                                                 stu_ordertype1 = ifelse(ord_title=="2020 PSAT NH 1270-1420 NM,OK,AR (H)", 1, 0),
                                                                                 stu_ordertype2 = ifelse(ord_title=="2020 PSAT NH 1270-1470 TX (H_include all)", 1, 0),
                                                                                 stu_ordertype3 = ifelse(ord_title=="2020 PSAT NH 1280-1470 CO,CA (H)", 1, 0),
                                                                                 stu_ordertype4 = ifelse(ord_title=="2020 PSAT NH 1370-1420 MS,AL,SC (H)", 1, 0),
                                                                                 stu_ordertype5 = ifelse(ord_title=="2020 PSAT NH 1370-1450 LA,KY,TN,FL (H)", 1, 0),
                                                                                 stu_ordertype6 = ifelse(ord_title=="2020 PSAT NH 1370-1470 GA,VA (H)", 1, 0),
                                                                                 stu_ordertype7 = ifelse(ord_title=="2021 PSAT NH 1290-1520 TX (H)", 1, 0),
                                                                                 stu_ordertype8 = ifelse(ord_title=="PSAT NH 1310-1470 MO,IL (H)", 1, 0))


                         #aggregate student list data to school-level with total num students + race/ethnicity
                         school_lists_la <- orderswlists_race_la %>% select(hs_ncessch, stu_race_noresponse, stu_race_missing,
                                                                            stu_race_aian, stu_race_asian, stu_race_black,
                                                                            stu_race_latinx, stu_race_nhpi, stu_race_white,
                                                                            stu_race_other, stu_race_multi, stu_ordertype1, stu_ordertype2, stu_ordertype3,
                                                                            stu_ordertype4, stu_ordertype5, stu_ordertype6, stu_ordertype8) %>% group_by(hs_ncessch) %>% summarize_all(sum)


                         # now create school df with total students versus student prosp purchased for Houston
                         la_pubprivhs <- pubhs_privhs_data %>% filter(cbsatitle_1=="Los Angeles-Long Beach-Anaheim, CA")

                         # merge in purchased prospects
                         la_pubprivhs<- merge(x = la_pubprivhs, y = school_lists_la, by.x  = "ncessch",  by.y  = "hs_ncessch", all.x=TRUE)

                         # replace NAs to zeros
                         la_pubprivhs <- mutate(la_pubprivhs, across(starts_with("stu_race"), ~ifelse(is.na(.x),0,.x)))
                         la_pubprivhs <- mutate(la_pubprivhs, across(starts_with("stu_ordertype"), ~ifelse(is.na(.x),0,.x)))

                         
                         # merge in pub HS data on SAT scores [can't find newer data; CA DATA DOESNT HAVE BREAKDOWNS BY RACE]
                         la_pubhs <- la_pubprivhs %>% filter(private==0 & pub_sch_type==1) #only regular schools
                         cahs<-read.csv("data/achievement data/sat16-17.csv", na.strings=c("","NA"), colClasses=c("cds"="factor", "Ccode"="factor", "Scode"="factor"), encoding="UTF-8")
                         
                         cahs2<-read.csv("data/achievement data/CDS_NCES_crosswalk.csv", na.strings=c("","NA"), colClasses=c("CDSCode"="factor", "NCESSchool"="factor"), encoding="UTF-8")
                         cahs2$nces<-paste0(as.character(cahs2$NCESDist), as.character(cahs2$NCESSchool))
                         cahs2$nces<-as.factor(cahs2$nces)
                         
                         lahs<-merge(x = cahs, y = cahs2[ , c("CDSCode", "nces")], by.x="cds", by.y="CDSCode", all.x=TRUE)
                         
                         la_pubhs<-merge(x = la_pubhs, y = lahs, by.x="ncessch", by.y="nces", all.x=TRUE)
                         
                         
                         

                  #        #KS analyses
                  #        la_pubprivhs %>% filter(stu_race_black>1 |stu_race_latinx>1) %>% 
                  #          group_by(ncessch, private) %>% 
                  #          summarise(total_stu_pctwhite = sum(pct_white, na.rm = T),
                  #                    total_prosp_black = sum(stu_race_black, na.rm = T),
                  #                    total_prosp_latinx = sum(stu_race_latinx, na.rm = T)) %>% arrange(-total_stu_pctwhite) %>% print(n=150)
                  #        
                  #        
                         
                  
                    
                    
                    
                    
                    
                    
                    
                         
lists_df_summary <- lists_orders_zip_hs_df %>% count(univ_id, univ_state, univ_c15basic, ord_num)
        
# FOR CRYSTAL
save(lists_orders_zip_hs_df, file = file.path("/Users/karinasalazar/Dropbox", 'lists_orders_zip_hs_df.RData'))
save(orders_df, file = file.path("/Users/karinasalazar/Dropbox", 'orders_df.RData'))
save(acs_race_zipcodev3, file = file.path("/Users/karinasalazar/Dropbox", 'acs_race_zipcodev3.RData'))

save(orders_df, orders_fig_totals, orders_filters1, table_gpa, df_0, df_rq2a, df_int, df_int2, df_rq3, lists_df_summary, table_texasam_zip, table_texasam_zip_inc, fig_rq3_segment_race_inc, file = file.path(data_dir, 'tbl_fig_data.RData'))
save(houston_pubprivhs, houston_pubhs, txhs, la_pubprivhs, la_pubhs, lahs, acs_race_zipcodev3, file = file.path(data_dir, 'map_data.RData'))
            