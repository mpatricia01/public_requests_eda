### Settings
rm(list = ls())
options(max.print=1000)

### Libraries

library(tidyverse)
library(lubridate)
library(labelled)


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
    

    
    
################### BUILDING TABLE for RQ2A

                
#FUNCTION FOR TABLE_RQ2A
        table_rq2a <- function(variables, columns) {
            
            #create counter
            counter = 0

            #loop through columns via filters (ex: all students, in-state, out-state, etc. )
            for (i in columns) {
                
                counter = counter+1
                
                if(i=="all"){filter_string=c("stu_country=='united states'")}

                #create N row
                n <- as_data_frame(t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% count()))
                row.names(n) <- "Total N"
                
                
                #create race rows
                race <- as_data_frame (t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%  #rlang turns strings into expressions
                                                     summarize(pctwhite = mean(stu_white_common, na.rm=TRUE)*100,
                                                               pctasian = mean(stu_asian_common, na.rm=TRUE)*100,
                                                               pctblack = mean(stu_black_common, na.rm=TRUE)*100,
                                                               pcthisp = mean(stu_is_hisp_common, na.rm=TRUE)*100)))
                row.names(race) <- c("Pct White", "Pct Asian", "Pct Black", "Pct Latinx")
                
                
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
                row.names(schtype) <- c("Pct Private", "Pct Public", "Pct Unknown")
                
                
                
                #concatenate all row_dfs to for i-column
                temp_df <- bind_rows(mget(variables))
                #temp_df <- bind_rows(n, race, income, schtype)
                temp_df <- temp_df %>% rename(!!paste0("", i) := V1)
                
                
                #first loop creates the master_df
                #second + loops appends the master df
                if(counter==1){master_df <<- as.data.frame(temp_df)}
                if(counter==2){master_df <<- merge(master_df,temp_df)}
                
                
                
            }
   

        }
        
        
    
    # Create Table for RQ2A
        vars <- c("n", "race", "income", "schtype")
        cols <- c("all")
        table_rq2a(vars, cols) #returns masterdf 
        
        #format table
        master_df$all <- formatC(master_df$all, format="f", big.mark=",", digits=0)
        
        
    