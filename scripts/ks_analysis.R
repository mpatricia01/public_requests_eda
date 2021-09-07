### Settings
rm(list = ls())
options(max.print=1000)

### Libraries

library(tidyverse)
library(lubridate)
library(labelled)
library(tidyr)


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
    

################### CREATING OUT_OF_STATE VAR NEEDED FOR FUNCTION
    
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
    


    
    
################### BUILDING TABLE for RQ2A
    
                
    #FUNCTION FOR TABLE_RQ2A
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
                race <- race %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "race missing", stu_race_cb))
                race<- race %>% remove_rownames %>% column_to_rownames(var="stu_race_cb")
                row.names(race) <- c("Pct Race-No Response", "Pct AI/AN", "Pct Asian", "Pct Black", "Pct Latinx", "Pct NH/PI", "Pct White", "Multiracial", "Pct Race- Missing")
                
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
                row.names(schtype) <- c("Pct Private", "Pct Public", "Pct School Unknown")
                
                
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
        
        
    
    # Call Function to Create Table for RQ2A
        
        #all possible vars: n, race, income, schtype
        vars <- c("n", "race", "income", "schtype") #all possible vars: n, race, income, schtype
        
        #all possible columns: all_domestic, in_state, out_of_state, research_univ, regional_univ, research_univ_instate, research_univ_outofstate, regional_univ_instate, regional_univ_outofstate,
        cols <- c("all_domestic", "in_state", "out_of_state", "research_univ", "regional_univ", "research_univ_instate", "research_univ_outofstate") 
        df_rq2a<- table_rq2a(vars, cols) 
        
        #format table
        df_rq2a <- df_rq2a %>% mutate_if(is.numeric, round, 0)
        df_rq2a <- df_rq2a %>%  mutate_each(funs(prettyNum(., big.mark=",")))

        
    