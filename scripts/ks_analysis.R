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
                race <- race %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "Pct- Race Missing", stu_race_cb),
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
            race <- race %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "Pct- Race Missing", stu_race_cb),
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
        
        
        
        # create categorical variable that use different combos of filters
        lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% 
                          mutate(filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_psat==1 & filter_gpa==1, "HS Grad, Zip, PSAT, GPA", NA),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_sat==1 & filter_gpa==1, "HS Grad, Zip, SAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil=1 & filter_race==1 & filter_sat==1 & filter_psat==1 & filter_gpa==1 & filter_rank==1, "HS Grad, State, Race, SAT, PSAT, GPA, Rank", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_sat==1 & filter_psat==1 & filter_gpa==1, "HS Grad, Zip, SAT, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_state==1 & filter_sat==1 & filter_gpa==1, "HS Grad, State, SAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_state==1 & filter_psat==1 & filter_gpa==1, "HS Grad, State, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_state==1 & filter_race==1 & filter_psat==1 & filter_gpa==1, "HS Grad, State, Race, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_state==1 & filter_segment==1 & filter_gender==1 & filter_sat==1 & filter_gpa==1, "HS Grad, State, Segment, Gender, SAT, GPA", filter_combo))
    
        
        # number of orders across common filter combos
        lists_orders_zip_hs_df %>% 
          group_by(filter_combo) %>%
          summarise(n=n_distinct(ord_num)) 
        
        # top two filter combos across race/ethnicity
        common_combo_race <- lists_orders_zip_hs_df %>% group_by(filter_combo) %>%
                              summarize(
                                n_obs = sum(n()),
                                pct_stu_white =  mean(stu_white_common, na.rm = TRUE)*100,
                                pct_stu_asian =  mean(stu_asian_common, na.rm = TRUE)*100,
                                pct_stu_black =  mean(stu_black_common, na.rm = TRUE)*100,
                                pct_stu_hispanic =  mean(stu_is_hisp_common, na.rm = TRUE)*100,
                                #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
                                #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
                                pct_stu_native =  mean(stu_american_indian_common, na.rm = TRUE)*100,
                                pct_stu_tworaces =  mean(stu_multi_race_common, na.rm = TRUE)*100,
                              )         
        
        
        # racial characteristics by filters
        lists_orders_zip_hs_df %>% filter() %>%
          summarize(
            n_obs = sum(n()),
            pct_stu_white =  mean(stu_white_common, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian_common, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black_common, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_is_hisp_common, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_american_indian_common, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_multi_race_common, na.rm = TRUE)*100,
          )         
        
########## RESEARCH QUESTION 3: CHARACTERISTICS OF STUDENT LISTS IN COMPARISON TO OTHER MSA STUDENTS
        
        
        # Philadelphia-Camden-Wilmington, PA-NJ-DE-MD; 37980
        # students purchased
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment),zip_cbsa_1 == '37980') %>%
            summarize(
                n_obs = sum(n()),
                pct_stu_white =  mean(stu_white_common, na.rm = TRUE)*100,
                pct_stu_asian =  mean(stu_asian_common, na.rm = TRUE)*100,
                pct_stu_black =  mean(stu_black_common, na.rm = TRUE)*100,
                pct_stu_hispanic =  mean(stu_is_hisp_common, na.rm = TRUE)*100,
                #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
                #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
                pct_stu_native =  mean(stu_american_indian_common, na.rm = TRUE)*100,
                pct_stu_tworaces =  mean(stu_multi_race_common, na.rm = TRUE)*100,
            ) 
    
        
        # racial composition of all students in public high schools in the CBSA
        pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '37980') %>%
            summarize(
                n_obs = sum(n()),
                tot_students = sum(total_students, na.rm = TRUE),
                #tot_white = sum(total_white, na.rm = TRUE),
                #tot_asian = sum(total_asian, na.rm = TRUE),
                pct_white = sum(total_white, na.rm = TRUE)/tot_students*100,
                pct_asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
                pct_black = sum(total_black, na.rm = TRUE)/tot_students*100,
                pct_hispanic = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
                pct_native = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
                pct_tworaces = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
                pct_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
                #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
            )
        

save(orders_df, orders_fig_totals, orders_filters1, table_gpa, df_0, df_rq2a, df_int, df_rq3, file = file.path(data_dir, 'tbl_fig_data.RData'))
            