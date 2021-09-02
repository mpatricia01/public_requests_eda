################################################################################
##
## [ PROJ ] < student list project, EDA >
## [ FILE ] < oj_eda/combined_eda_oj.R >
## [ AUTH ] < Ozan Jaquette / ozanj >
## [ INIT ] < 8/31/2021 >
## [ DESC ] < EDA for analyses of order data, deidentified prospect dat, and prospect-level data frames that contains both list and order data>

################################################################################

## ---------------------------
## libraries
## ---------------------------

#library(leaflet)
#library(rgdal)
#library(raster)
#library(formattable)
library(tidyverse)
#library(readxl)
#library(lubridate)
#library(htmlwidgets)
#library(sf)
library(labelled)

#library(haven)
## ---------------------------
## system settings
## ---------------------------

rm(list = ls())
options(max.print=100)

#options(tibble.width = Inf, width = 10000, scipen = 999) # does this work for scripts or just rmd?
options(scipen = 999)


## ---------------------------
## directory paths
## ---------------------------

data_dir <- file.path('.', 'data')
#data_dir
#getwd()
list.files(path = data_dir)

scripts_dir <- file.path('.', 'scripts')
#scripts_dir
list.files(path = scripts_dir)

## -----------------------------------------------------------------------------
## FUNCTIONS
## -----------------------------------------------------------------------------

## -------------
## LOAD OBJECTS (SECONDARY DATA, ORDER AND LIST DATA)
## -------------

# Run script that creates data frames from secondary data sources (e.g., ACS, NCES)
  source(file = file.path(scripts_dir, 'create_secondary_datasets.R'))

# Run script that creates analysis data frames from order data and list data
  # NOTE: this script relies on data frames created by above create_secondary_datasets.R script
  source(file = file.path(scripts_dir, 'create_combined_order_list_analysis_datasets.R'))



## -----------------------------------------------------------------------------
## EDA: (RQ2A) WHAT ARE THE CHARACTERISTICS OF PROSPECTS PURCHASED BY STUDENT LISTS? HOW DO THESE CHARACTERISTICS DIFFER ACROSS UNIVERSITY TYPE, GEOGRAPHIC FOCUS, AND ACROSS FILTER CRITERIA
## -----------------------------------------------------------------------------

# EDA GOOGLE DOCS
  # 'exploratory data analysis': https://docs.google.com/document/d/17XGsoYYmqODmdUik-5q5-0GuBc0LNb6KOrs_lhAYUpU/edit# 
  # 'OJ and KS analysis plans' https://docs.google.com/document/d/17lpSfaXgKCc3bRBsuiqKEPMhLSL6agyv2lWQxeGf9a4/edit
    
  
#############      
######### PROSPECT CHARACTERISTICS OF INTEREST; BY ALL AND IN-STATE VS. OUT-OF-STATE PROSPECTS
#############    
    
# number of students
    lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count()
lists_orders_zip_hs_df %>% count(univ_name)
    
# number of students by in vs. out-of-state
    
    lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% group_by(stu_out_st) %>% count()

# CBSAs prospects are from
    
  # in state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,stu_out_st==0,!is.na(zip_cbsatitle_1)) %>% 
    count(zip_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=30)
    
  # out of state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,stu_out_st==1,!is.na(zip_cbsatitle_1)) %>% 
    count(zip_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=30)
  
# Combined statistical areas prospects are from
  
  # in state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,stu_out_st==0,!is.na(zip_csatitle)) %>% 
    count(zip_csatitle) %>% arrange(desc(n)) %>% print(n=30)
    
  # out of state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,stu_out_st==1,!is.na(zip_csatitle)) %>% 
    count(zip_csatitle) %>% arrange(desc(n)) %>% print(n=30)  
  
    lists_orders_zip_hs_df %>% glimpse()
    
    
# geomarkets prospects are from
  # stu_geomarket               <chr> "INT-SA Saudi Arabia", "INT-ID Indonesia", "INT-VM Vietnam", "INT-KS South Korea", "INT-HK Hong Kong S.A.R.", "INT-IN India", "INT-VM Vietnam", "IN~
  # NEED TO FIX THE GEOMARKET VARIABLE; IT IS FORMATTED DIFFERENTLY ACROSS UNIVERSITIES
    # maybe find a crosswalk betwen statecode-two-digit code and name
    
  # in state
  lists_orders_zip_hs_df %>% filter(stu_out_st==0) %>% 
    count(stu_geomarket) %>% arrange(desc(n)) %>% print(n=30)
    
  # out of state
  lists_orders_zip_hs_df %>% filter(stu_out_st==1) %>% 
    count(stu_geomarket) %>% arrange(desc(n)) %>% print(n=50)  
   
# international prospects
  # stu_country                 <chr> "saudi arabia", "indonesia", "vietnam", "south korea", "hong kong s.a.r.", "india", "vietnam", "india", "vietnam", "united arab emirates", "south k~
  
  lists_orders_zip_hs_df %>% count(stu_country) %>% arrange(desc(n)) %>% print(n=50)

  lists_orders_zip_hs_df %>% filter(stu_country != 'united states') %>% count(univ_name) %>% arrange(desc(n)) %>% print(n=50)  
    
  lists_orders_zip_hs_df %>% filter(stu_country != 'united states') %>% count(stu_geomarket) %>% arrange(desc(n)) %>% print(n=50)  
  
# median income of zip-code where student lives
  # avg of purchased prospects
    
    # all
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )    
    
    # by 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0) %>% group_by(stu_out_st) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
    
    # by 0/1 prospect is out of state and by university name
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0) %>% group_by(univ_name,stu_out_st) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
    
    # check on UC SD
    
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0,univ_id == '110680') %>% group_by(stu_out_st) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
    
    # income by order num
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0,univ_id == '110680') %>% group_by(stu_out_st,ord_num) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      ) %>% print(n=150)
    
    # race by order num
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0,univ_id == '110680') %>% group_by(stu_out_st,ord_num) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
        pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
        pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
        pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
        pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
        #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
        #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
        pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
        pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
        #pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
      ) %>% print(n=150)
    
# number/percent of prospects by race
  
  # all
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100)  
  # same
  #lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(stu_race_cb) %>% summarise (n = n()) %>%  mutate(pct = 100*(n / sum(n)))

  # check number unknown by university
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(univ_name) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )    
  
  # create from 0/1 variables defined by the categorical variable 
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb)),stu_race_cb !=0) %>%    
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      #pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )  
  
  # by 0/1 prospect is out of state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(stu_out_st) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100)  
  
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(stu_out_st) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      #pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )    
  
  # by 0/1 prospect is out of state and by university name
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(univ_name,stu_out_st) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      #pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )    
  
# prospect attends public vs. private school
  
  # all
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)  
  #lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% count(hs_school_control) %>% mutate(pct = (n / sum(n)) * 100)
  
  # by 0/1 prospect is out of state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% group_by(stu_out_st) %>% count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)  
  
# Racial composition of prospects by public/private high school
  
  # all
    
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(hs_private) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100)  
  
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(hs_private) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )

  # by 0/1 prospect is out of state
  
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(hs_private,stu_out_st) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100) %>% print(n=40)

  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(stu_out_st,hs_private) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )

#############  
######### PROSPECT CHARACTERISTICS BY UNIVERSITY TYPE AND IN/OUT OF STATE
#############
  
#### which universities are which carnegie type

    #lists_orders_zip_hs_df %>% group_by(univ_c15basic,univ_name) %>% count()
  
    lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% group_by(univ_c15basic,univ_name) %>% filter(row_number()==1) %>% ungroup() %>% arrange(univ_c15basic) %>% select(univ_name,univ_id,univ_c15basic)
  
# number of students    

    # by university type
    lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% group_by(univ_c15basic) %>% count()
    
    # by university type and 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% group_by(univ_c15basic,stu_out_st) %>% count()
    lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% group_by(univ_name,stu_out_st) %>% count()

    
# median income of zip-code where student lives
  # avg of purchased prospects
    
    # by university type
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0) %>% group_by(univ_c15basic) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )    
    
    # by university type and 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs==0) %>% group_by(univ_c15basic,stu_out_st) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
  

# number/percent of prospects by race
  
  # by university type
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(univ_c15basic) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100) %>% print(n=30)

  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb)),stu_race_cb !=0) %>%    
    group_by(univ_c15basic) %>% summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )  
  
  # by university type and 0/1 prospect is out of state
  
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(univ_c15basic,stu_out_st) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100)  %>% print(n=50)
  
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(univ_c15basic,stu_out_st) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )
  

#### prospect attends public vs. private school
  
  # by university type
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% group_by(univ_c15basic) %>% count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)  
  
  # by university type and 0/1 prospect is out of state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% group_by(univ_c15basic,stu_out_st) %>% count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)  
  
# Racial composition of prospects by public/private high school
  
  # by university type
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(univ_c15basic,hs_private) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )

  # by 0/1 prospect is out of state
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(univ_c15basic,hs_school_control,stu_out_st) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100) %>% print(n=40)

  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb)),stu_race_cb !=0) %>% group_by(univ_c15basic,stu_out_st,hs_private) %>% 
    summarize(
      n_obs = sum(n()),
      n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
      pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
      pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
      pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
      pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
      #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
      #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
      pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
      pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
      pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
    )    
    

#############  
######### PROSPECT CHARACTERISTICS OF INTEREST BY FILTER CRITERIA
#############

# LIST OF ORDER SUMMARY VARIABLES

# POTENTIAL ORDER SUMMARY VARIABLES OF INTEREST
  # general
    # ord_num            <chr> "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "483721", "4~
    # ord_title          <chr> "1150-1500 INTL Travel Countries Only", "1150-1500 INTL Travel Countries Only", "1150-1500 INTL Travel Countries Only", "1150-1500 INTL Travel Countries~  
    # ord_date_start     <date> 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06-27, 2019-06~  
    # ord_hs_grad_class  <chr> "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|2022", "2021|202~
    # na_ord_summ        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~  
  
  # geographic order filter variables
  
    # ord_zip_code       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    # ord_segment        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    # ord_state_name     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    # ord_cbsa_name      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    # ord_geomarket      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    # ord_intl_region    <chr> "AS-3 - Egypt|AC-7 - Hong Kong|AS-4 - India|AC-8 - Indonesia|AS-7 - Jordan|AC-16 - Korea, South (ROK)|AC-29 - Macao|AC-10 - Malaysia|AS-14 - Saudi Arabi~
  
  # score variables
    # ord_sat_score_min  <dbl> 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 12~
    # ord_sat_score_max  <dbl> 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 15~
    # ord_psat_score_min <dbl> 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 1150, 11~
    # ord_psat_score_max <dbl> 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 1450, 14~
    
  # HS gpa variables
    # ord_gpa_low        <chr> "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B-", "B~
    # ord_gpa_high       <chr> "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A+", "A~
  # ord_rank_low       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
  # ord_rank_high      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
  
  # demographic filter variables
    # ord_gender         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    # ord_race_ethnicity <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
  
  lists_orders_zip_hs_df %>% glimpse()
  lists_orders_zip_hs_df %>% select(contains('ord')) %>% glimpse()
  
  # which observations are you missing order summary data for?
  
    lists_orders_zip_hs_df %>% count(univ_name,na_ord_summ) %>% print(n=100)

  # geographic order filter variables  
    
##### analysis of segment orders (for U Illinois-Urbana)

  # on prospect-level data, there are 21 different orders from U. Urbana that utilize segment
    lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>% group_by(ord_num) %>% filter(row_number()==1) %>% ungroup() %>% select(ord_num) %>% count()
  
  # on prospect-level data, these 21 orders are associated with 158,190 students
    lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>% count()
    #lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>% count(stu_country) %>% print(n=40) # all but like 100 of these students are from united states
    
  # this is the one out of state order that does not use segment
    orders_df %>% filter(univ_id == '145637', order_num == '483724') %>% View()
  
  # order-level df; 21 orders use segment
  orders_df %>% filter(univ_id == '145637') %>% count(segment) # 21 orders use segment
  orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% arrange(order_num) %>% View()

  # which segments for these 21 orders
    #orders_df %>% filter(univ_id == '145637') %>% count(segment)
    # always the same set of neighborhood and high school segments:
      #'EN:51, HS:68|EN:58, HS:70|EN:53, HS:70|EN:51, HS:65|EN:69, HS:70|EN:60, HS:68|EN:73, HS:70|EN:60, HS:65|EN:69, HS:68|EN:70, HS:68|EN:63, HS:70|EN:70, HS:66|EN:78, HS:ALL|EN:58, HS:65|EN:58, HS:64|EN:51, HS:79|EN:60, HS:70|EN:51, HS:70|EN:53, HS:65|EN:60, HS:79|EN:69, HS:75|EN:70, HS:70|EN:63, HS:65|EN:61, HS:ALL|EN:73, HS:65|EN:70, HS:79'
      #EN:51, HS:65 | EN:51, HS:68 | EN:51, HS:70 | EN:51, HS:79
      #EN:53, HS:65 | EN:53, HS:70 |
      #EN:58, HS:64 | EN:58, HS:65 | EN:58, HS:70
      #EN:60, HS:65 | EN:60, HS:68 | EN:60, HS:70 | EN:60, HS:79
      #EN:61, HS:ALL
      #EN:63, HS:65 | EN:63, HS:70
      #EN:69, HS:68 | EN:69, HS:70 | EN:69, HS:75
      #EN:70, HS:66 | EN:70, HS:68 | EN:70, HS:70 | EN:70, HS:79
      #EN:73, HS:65 | EN:73, HS:70
      #EN:78, HS:ALL
    # how to read this:
      # for census tracts assigned to neighborhood cluster 51, they want students who attend high schools assigned to one of the following high school clusters %in% c(65,68,70,79)
      
orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% View()    
  # state_name [21 segment orders]
    # 13 orders use this state_name:
      # 'Vermont|Rhode Island|Hawaii|Maine|Virginia|Michigan|Idaho|Delaware|Iowa|Maryland|Massachusetts|Arkansas|Utah|Indiana|Minnesota|Arizona|Missouri|Montana|Mississippi|New Hampshire|New Jersey|New Mexico|Alaska|Texas|Alabama|North Carolina|North Dakota|Nebraska|New York|Georgia|Nevada|Tennessee|California|Oklahoma|Ohio|Wyoming|Florida|South Dakota|South Carolina|Connecticut|West Virginia|District of Columbia|Wisconsin|Kentucky|Kansas|Oregon|Louisiana|Washington|Colorado|Pennsylvania'
      orders_df %>% filter(univ_id == '145637', !is.na(segment), state_name == 'Vermont|Rhode Island|Hawaii|Maine|Virginia|Michigan|Idaho|Delaware|Iowa|Maryland|Massachusetts|Arkansas|Utah|Indiana|Minnesota|Arizona|Missouri|Montana|Mississippi|New Hampshire|New Jersey|New Mexico|Alaska|Texas|Alabama|North Carolina|North Dakota|Nebraska|New York|Georgia|Nevada|Tennessee|California|Oklahoma|Ohio|Wyoming|Florida|South Dakota|South Carolina|Connecticut|West Virginia|District of Columbia|Wisconsin|Kentucky|Kansas|Oregon|Louisiana|Washington|Colorado|Pennsylvania') %>% count() # 14
      # these 13 orders do not filter on cbsa name
      orders_df %>% filter(univ_id == '145637', !is.na(segment), state_name == 'Vermont|Rhode Island|Hawaii|Maine|Virginia|Michigan|Idaho|Delaware|Iowa|Maryland|Massachusetts|Arkansas|Utah|Indiana|Minnesota|Arizona|Missouri|Montana|Mississippi|New Hampshire|New Jersey|New Mexico|Alaska|Texas|Alabama|North Carolina|North Dakota|Nebraska|New York|Georgia|Nevada|Tennessee|California|Oklahoma|Ohio|Wyoming|Florida|South Dakota|South Carolina|Connecticut|West Virginia|District of Columbia|Wisconsin|Kentucky|Kansas|Oregon|Louisiana|Washington|Colorado|Pennsylvania') %>% count(cbsa_name) # 14
    
    # 5 orders use this state_name [and also condition on CBSA]
      # 'Armed Forces Americas (Except Canada)|Connecticut|Armed Forces Canada, Europe, Middle East, Africa|Missouri|California|Armed Forces Pacific'
      orders_df %>% filter(univ_id == '145637', !is.na(segment), state_name == 'Armed Forces Americas (Except Canada)|Connecticut|Armed Forces Canada, Europe, Middle East, Africa|Missouri|California|Armed Forces Pacific') %>% count() # 5
      
      # QUESTION: FOR THESE ORDERS, HOW DO YOU KNOW THAT THE FILTER IS STATES OR MSA RATHER THAN STATE *AND* MSA?
      
    # 3 orders have NA state_name but condition on CBSA
        orders_df %>% filter(univ_id == '145637', !is.na(segment), is.na(state_name)) %>% count() # 3
        orders_df %>% filter(univ_id == '145637', !is.na(segment), is.na(state_name)) %>% count(state_name) # 3
        orders_df %>% filter(univ_id == '145637', !is.na(segment), is.na(state_name)) %>% count(cbsa_name) # 3
        orders_df %>% filter(univ_id == '145637', !is.na(segment), is.na(state_name)) %>% View() # 3
      # two orders filter on these CBSAs
        # 'NY - Syracuse, NY|FL - Orlando-Kissimmee-Sanford, FL|FL - Sebring, FL|GA - Warner Robins, GA|GA - Dalton, GA|CA - Bakersfield, CA|TX - San Angelo, TX|TX - College Station-Bryan, TX|GA - Valdosta, GA|FL - Palm Bay-Melbourne-Titusville, FL|GA - Columbus, GA-AL|FL - Port St. Lucie, FL|GA - Savannah, GA|CA - Visalia-Porterville, CA|NY - Rochester, NY|CA - Santa Rosa, CA|CA - Chico, CA|GA - Macon-Bibb County, GA|FL - Tampa-St. Petersburg-Clearwater, FL|GA - Chattanooga, TN-GA|GA - Brunswick, GA|FL - Gainesville, FL|TX - Midland, TX|FL - Cape Coral-Fort Myers, FL|FL - Crestview-Fort Walton Beach-Destin, FL|FL - Punta Gorda, FL|CA - San Diego-Carlsbad, CA|CA - Oxnard-Thousand Oaks-Ventura, CA|TX - Beaumont-Port Arthur, TX|GA - Albany, GA|GA - Hinesville, GA|FL - North Port-Sarasota-Bradenton, FL|FL - Jacksonville, FL|TX - San Antonio-New Braunfels, TX|TX - Killeen-Temple, TX|TX - Abilene, TX|GA - Atlanta-Sandy Springs-Roswell, GA|NJ - New York-Newark-Jersey City, NY-NJ-PA|FL - Ocala, FL|NJ - Vineland-Bridgeton, NJ|FL - Deltona-Daytona Beach-Ormond Beach, FL|TX - Waco, TX|NY - Buffalo-Cheektowaga-Niagara Falls, NY|GA - Augusta-Richmond County, GA-SC|TX - Sherman-Denison, TX|NY - Glens Falls, NY|FL - Lakeland-Winter Haven, FL|CA - San Luis Obispo-Paso Robles-Arroyo Grande, CA|CA - Vallejo-Fairfield, CA|TX - Texarkana, TX-AR|CA - San Francisco-Oakland-Hayward, CA|TX - Laredo, TX|FL - Homosassa Springs, FL|CA - Stockton-Lodi, CA|CA - Modesto, CA|TX - Odessa, TX|TX - Lubbock, TX|NY - Albany-Schenectady-Troy, NY|CA - Riverside-San Bernardino-Ontario, CA|GA - Gainesville, GA|TX - Amarillo, TX|GA - Rome, GA|CA - Sacramento--Roseville--Arden-Arcade, CA|FL - Tallahassee, FL|FL - Miami-Fort Lauderdale-West Palm Beach, FL|FL - Naples-Immokalee-Marco Island, FL|NJ - Philadelphia-Camden-Wilmington, PA-NJ-DE-MD|FL - Sebastian-Vero Beach, FL|CA - Santa Cruz-Watsonville, CA|NY - Watertown-Fort Drum, NY|CA - San Jose-Sunnyvale-Santa Clara, CA|CA - Madera, CA|NY - New York-Newark-Jersey City, NY-NJ-PA|TX - Brownsville-Harlingen, TX|FL - The Villages, FL|CA - El Centro, CA|NJ - Trenton, NJ|TX - Corpus Christi, TX|NJ - Atlantic City-Hammonton, NJ|NY - Ithaca, NY|NY - Binghamton, NY|TX - El Paso, TX|TX - McAllen-Edinburg-Mission, TX|FL - Panama City, FL|TX - Houston-The Woodlands-Sugar Land, TX|CA - Santa Maria-Santa Barbara, CA|CA - Hanford-Corcoran, CA|CA - Salinas, CA|CA - Yuba City, CA|CA - Fresno, CA|NJ - Ocean City, NJ|FL - Pensacola-Ferry Pass-Brent, FL|CA - Los Angeles-Long Beach-Anaheim, CA|TX - Dallas-Fort Worth-Arlington, TX|TX - Wichita Falls, TX|NY - Elmira, NY|CA - Redding, CA|TX - Longview, TX|TX - Austin-Round Rock, TX|NY - Utica-Rome, NY|CA - Napa, CA|TX - Tyler, TX|CA - Merced, CA|TX - Victoria, TX|NY - Kingston, NY|GA - Athens-Clarke County, GA|NJ - Allentown-Bethlehem-Easton, PA-NJ'
      # one order filters on these CBSAs
        # 'FL - Orlando-Kissimmee-Sanford, FL|FL - Sebring, FL|GA - Warner Robins, GA|GA - Dalton, GA|CA - Bakersfield, CA|TX - San Angelo, TX|TX - College Station-Bryan, TX|GA - Valdosta, GA|FL - Palm Bay-Melbourne-Titusville, FL|GA - Columbus, GA-AL|FL - Port St. Lucie, FL|GA - Savannah, GA|CA - Visalia-Porterville, CA|CA - Santa Rosa, CA|CA - Chico, CA|GA - Macon-Bibb County, GA|FL - Tampa-St. Petersburg-Clearwater, FL|GA - Chattanooga, TN-GA|GA - Brunswick, GA|FL - Gainesville, FL|TX - Midland, TX|FL - Cape Coral-Fort Myers, FL|FL - Crestview-Fort Walton Beach-Destin, FL|FL - Punta Gorda, FL|CA - San Diego-Carlsbad, CA|CA - Oxnard-Thousand Oaks-Ventura, CA|TX - Beaumont-Port Arthur, TX|GA - Albany, GA|GA - Hinesville, GA|FL - North Port-Sarasota-Bradenton, FL|FL - Jacksonville, FL|TX - San Antonio-New Braunfels, TX|TX - Killeen-Temple, TX|TX - Abilene, TX|GA - Atlanta-Sandy Springs-Roswell, GA|NJ - New York-Newark-Jersey City, NY-NJ-PA|FL - Ocala, FL|NJ - Vineland-Bridgeton, NJ|FL - Deltona-Daytona Beach-Ormond Beach, FL|TX - Waco, TX|GA - Augusta-Richmond County, GA-SC|TX - Sherman-Denison, TX|FL - Lakeland-Winter Haven, FL|CA - San Luis Obispo-Paso Robles-Arroyo Grande, CA|CA - Vallejo-Fairfield, CA|TX - Texarkana, TX-AR|CA - San Francisco-Oakland-Hayward, CA|TX - Laredo, TX|FL - Homosassa Springs, FL|CA - Stockton-Lodi, CA|CA - Modesto, CA|TX - Odessa, TX|TX - Lubbock, TX|CA - Riverside-San Bernardino-Ontario, CA|GA - Gainesville, GA|TX - Amarillo, TX|GA - Rome, GA|CA - Sacramento--Roseville--Arden-Arcade, CA|FL - Tallahassee, FL|FL - Miami-Fort Lauderdale-West Palm Beach, FL|FL - Naples-Immokalee-Marco Island, FL|NJ - Philadelphia-Camden-Wilmington, PA-NJ-DE-MD|FL - Sebastian-Vero Beach, FL|CA - Santa Cruz-Watsonville, CA|CA - San Jose-Sunnyvale-Santa Clara, CA|CA - Madera, CA|NY - New York-Newark-Jersey City, NY-NJ-PA|TX - Brownsville-Harlingen, TX|FL - The Villages, FL|CA - El Centro, CA|NJ - Trenton, NJ|TX - Corpus Christi, TX|NJ - Atlantic City-Hammonton, NJ|TX - El Paso, TX|TX - McAllen-Edinburg-Mission, TX|FL - Panama City, FL|TX - Houston-The Woodlands-Sugar Land, TX|CA - Santa Maria-Santa Barbara, CA|CA - Hanford-Corcoran, CA|CA - Salinas, CA|CA - Yuba City, CA|CA - Fresno, CA|NJ - Ocean City, NJ|FL - Pensacola-Ferry Pass-Brent, FL|CA - Los Angeles-Long Beach-Anaheim, CA|TX - Dallas-Fort Worth-Arlington, TX|TX - Wichita Falls, TX|CA - Redding, CA|TX - Longview, TX|TX - Austin-Round Rock, TX|CA - Napa, CA|TX - Tyler, TX|CA - Merced, CA|TX - Victoria, TX|GA - Athens-Clarke County, GA|NJ - Allentown-Bethlehem-Easton, PA-NJ'
      
      # these three orders have following score/GPA criteria
        # sat_score_min = 1240; sat_score_max = 1450; psat_score_min = 1220; psat_score_max = 1450; gpa_low = B-; gpa_high = A+
      
        orders_df %>% filter(univ_id == '145637', order_num %in% c('500590','567376','483751')) %>% count(sat_score_min)
        orders_df %>% filter(univ_id == '145637', order_num %in% c('500590','567376','483751')) %>% count(sat_score_max)
        
        # looking at prospect-level data for these orders
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('500590','567376','483751')) %>% count()
      
     # 6 out-of-state non-engineering orders [[5 use segment and one does not]; this one doesn't use segment 483724
      
        #orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335','403340')) %>% View()
        # all six use the same state_name and cbsa_name
        orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335','403340')) %>% count(state_name)
        
        # prospect-level, 6 orders (including the non-segment)
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335','403340')) %>% count()
        
        orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335','403340')) %>% count(cbsa_name)
        
        # 5 orders use sat_min = 1240 and sat_max = 1450
          # order num = 403340 uses sat_min = 1230 and sat_max = 1450

          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(state_name)
          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(cbsa_name)

          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(sat_score_min)
          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(sat_score_max)
          
          
        # looking at prospect-level data for these orders
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335')) %>% count()
          
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335')) %>% 
          group_by(ord_num) %>% count()
      
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335')) %>% 
          group_by(ord_num) %>% summarize(
          n_obs = sum(n()),
          n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
          pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
          pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
          pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
          pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
          #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
          #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
          pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
          pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
          pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
        )    
        
      # 13 engineering orders
        orders_df %>% filter(univ_id == '145637', order_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count()
        orders_df %>% filter(univ_id == '145637', order_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count(sat_score_min)
        orders_df %>% filter(univ_id == '145637', order_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count(sat_score_max)
        
        orders_df %>% filter(univ_id == '145637', order_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count(state_name)
        orders_df %>% filter(univ_id == '145637', order_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count(cbsa_name)
        
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count()
        
      # making sure the 21 segment orders add up to the right number of prospects
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>% count() %>% as_vector() # total number of prospects
        
        # 3 msa orders [filter on msa, but not state]
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('500590','567376','483751')) %>% count()
        
        # 5 non-engineering orders [filter on segment, state, and cbsa]
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335')) %>% count()

        # 13 engineering orders [filter on segment and state, but not CBSA]
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count()
        
        # add them up; about right
        (lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('500590','567376','483751')) %>% count() %>% as_vector()) + (lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335')) %>% count() %>% as_vector()) + (lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483701','403333','371665','386336','469731','403314','371662','456710','386441','470123','483702','500494','567377')) %>% count() %>% as_vector())
          
    # sort descending by cbsa [or csa] 
        lists_orders_zip_hs_df %>% glimpse()
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>% count()
        
        # number of prospects, descending by cbsa
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>%
          mutate(zip_cbsa_name_code = str_c(zip_cbsatitle_1,zip_cbsa_1, sep='; ')) %>%
          count(zip_cbsa_name_code) %>% arrange(desc(n)) %>% print(n=30)
        
        # number of prospects, descending by csa
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>%
          mutate(zip_csa_name_code = str_c(zip_csatitle,zip_csacode, sep = '; ')) %>%
          count(zip_csa_name_code) %>% arrange(desc(n)) %>% print(n=30)
        
    # investigate racial composition of prospects purchased in particular CBSAs or CSAs
        
        #Miami-Fort Lauderdale-West Palm Beach, FL; 33100     4034
          # student-level data
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment),zip_cbsa_1 == '33100') %>%
          summarize(
            n_obs = sum(n()),
            n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
            pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
            pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
          )
          
          # compared to composition of zip-codes
          acs_race_zipcodev3 %>% filter(cbsa_1 == '33100') %>% 
            summarize(
              n_obs = sum(n()),
              n_nonmiss_zip_race = sum(is.na(pop_otherrace_15_19_pct)==0),
              pct_zip_white = mean(pop_white_15_19_pct, na.rm = TRUE),
              pct_zip_asian = mean(pop_asian_15_19_pct, na.rm = TRUE),
              pct_zip_black = mean(pop_black_15_19_pct, na.rm = TRUE),
              pct_zip_hispanic = mean(pop_hispanic_15_19_pct, na.rm = TRUE),
              #pct_zip_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
              #pct_zip_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
              pct_zip_native = mean(pop_native_15_19_pct, na.rm = TRUE),
              pct_zip_otherrace = mean(pop_otherrace_15_19_pct, na.rm = TRUE),
              pct_zip_tworaces = mean(pop_tworaces_15_19_pct, na.rm = TRUE),
            )
          
          # compare to racial composition of public high schools in the CBSA
          
            pubhs_privhs_data %>% glimpse()
            #pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '33100') %>% count()
            #pubhs_privhs_data %>% filter(school_control == 'public',total_11>0, cbsa_1 == '33100') %>% count()
            #pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '33100') %>% count(pub_sch_type)
            #pubhs_privhs_data %>% filter(school_control == 'public',total_12>10, cbsa_1 == '33100') %>% count()
            #pubhs_privhs_data %>% filter(school_control == 'public',total_12>10, cbsa_1 == '33100') %>% count(pub_sch_type)
          
          # average racial composition of public schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '33100') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
            )          
          
          # racial composition of all students in public high schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '33100') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
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
          
          
          # racial composition of all 11th grade students in public high schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_11>0, cbsa_1 == '33100') %>%
          mutate(g11_native = g11_nativehawaii + g11_amerindian) %>% 
            summarize(
              n_obs = sum(n()),
              n_nonmiss_g11_race = sum(is.na(g11_white)==0),
              tot_g11_students = sum(total_11, na.rm = TRUE),
              #tot_white = sum(total_white, na.rm = TRUE),
              #tot_asian = sum(total_asian, na.rm = TRUE),
              pct_g11_white = sum(g11_white, na.rm = TRUE)/tot_g11_students*100,
              pct_g11_asian = sum(g11_asian, na.rm = TRUE)/tot_g11_students*100,
              pct_g11_black = sum(g11_black, na.rm = TRUE)/tot_g11_students*100,
              pct_g11_hispanic = sum(g11_hispanic, na.rm = TRUE)/tot_g11_students*100,
              pct_g11_native = sum(g11_native, na.rm = TRUE)/tot_g11_students*100, # native american + alaska native + native hawaiaan + other pacific islander
              pct_g11_tworaces = sum(g11_tworaces, na.rm = TRUE)/tot_g11_students*100,
              pct_g11_unknown = sum(g11_unknown, na.rm = TRUE)/tot_g11_students*100,
              #pct_all = pct_white + pct_asian + pct_black + pct_hispanic + pct_native + pct_tworaces + pct_unknown
            )
          
        # Philadelphia-Camden-Wilmington, PA-NJ-DE-MD; 37980
          # student-level
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment),zip_cbsa_1 == '37980') %>%
          summarize(
            n_obs = sum(n()),
            n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
            pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
            pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
          ) 
          
          # racial composition of all students in public high schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '37980') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
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
          
          
          # average racial composition of public schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '37980') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
            )          
          

          
        # Baltimore-Columbia-Towson, MD; 12580
          # student-level
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment),zip_cbsa_1 == '12580') %>%
          summarize(
            n_obs = sum(n()),
            n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
            pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
            pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
          )          

          # racial composition of all students in public high schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '12580') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
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
          
          # average racial composition of public schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '12580') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
            )          

          # compared to composition of zip-codes
          acs_race_zipcodev3 %>% filter(cbsa_1 == '12580') %>% 
            summarize(
              n_obs = sum(n()),
              n_nonmiss_zip_race = sum(is.na(pop_otherrace_15_19_pct)==0),
              pct_zip_white = mean(pop_white_15_19_pct, na.rm = TRUE),
              pct_zip_asian = mean(pop_asian_15_19_pct, na.rm = TRUE),
              pct_zip_black = mean(pop_black_15_19_pct, na.rm = TRUE),
              pct_zip_hispanic = mean(pop_hispanic_15_19_pct, na.rm = TRUE),
              #pct_zip_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
              #pct_zip_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
              pct_zip_native = mean(pop_native_15_19_pct, na.rm = TRUE),
              pct_zip_otherrace = mean(pop_otherrace_15_19_pct, na.rm = TRUE),
              pct_zip_tworaces = mean(pop_tworaces_15_19_pct, na.rm = TRUE),
            )            
          
        # Boston-Cambridge-Newton, MA-NH; 14460
          # student-level
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment),zip_cbsa_1 == '14460') %>%
          summarize(
            n_obs = sum(n()),
            n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
            pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
            pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
          )          

          # racial composition of all students in public high schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '14460') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
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
          
          # average racial composition of public schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '14460') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
            )          
          
          # compared to composition of zip-codes
          acs_race_zipcodev3 %>% filter(cbsa_1 == '14460') %>% 
            summarize(
              n_obs = sum(n()),
              n_nonmiss_zip_race = sum(is.na(pop_otherrace_15_19_pct)==0),
              pct_zip_white = mean(pop_white_15_19_pct, na.rm = TRUE),
              pct_zip_asian = mean(pop_asian_15_19_pct, na.rm = TRUE),
              pct_zip_black = mean(pop_black_15_19_pct, na.rm = TRUE),
              pct_zip_hispanic = mean(pop_hispanic_15_19_pct, na.rm = TRUE),
              #pct_zip_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
              #pct_zip_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
              pct_zip_native = mean(pop_native_15_19_pct, na.rm = TRUE),
              pct_zip_otherrace = mean(pop_otherrace_15_19_pct, na.rm = TRUE),
              pct_zip_tworaces = mean(pop_tworaces_15_19_pct, na.rm = TRUE),
            )                      
         
          
##### checking on others         
  # 5 Atlanta-Sandy Springs-Roswell, GA; 12060             8286
  # 6 Washington-Arlington-Alexandria, DC-VA-MD-WV; 47900  7546
  # 7 Dallas-Fort Worth-Arlington, TX; 19100               6458
  # 9 Houston-The Woodlands-Sugar Land, TX; 26420          6151
  # 18 Indianapolis-Carmel-Anderson, IN; 26900              1663
  # 15 Seattle-Tacoma-Bellevue, WA; 42660                   2144
  # 20 Tampa-St. Petersburg-Clearwater, FL; 45300           1628 
  # 17 Detroit-Warren-Dearborn, MI; 19820                   1726

          

          # student-level
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment),zip_cbsa_1 == '12060') %>%
          summarize(
            n_obs = sum(n()),
            n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
            pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
            pct_stu_unknown =  mean(stu_unknown, na.rm = TRUE)*100,
          )          

          # racial composition of all students in public high schools in the CBSA
          pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '12060') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
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

          
##### EXAMINING HIGH SCHOOLS IN CBSA THAT HAVE ZERO VS. GT ZERO NAMES PURCHASED FOR SEGMENT ANALYSIS ORDERS
          
  # focus on out of state orders that use segment and are not focused on engineering
    # three orders condition on MSA but not state
      #c('500590','567376','483751'))
    # 5 orders condition on state OR MSA
      # 'Armed Forces Americas (Except Canada)|Connecticut|Armed Forces Canada, Europe, Middle East, Africa|Missouri|California|Armed Forces Pacific'
        # essentially, Connecticut, California, Missouri
      #c('483724','470283','371629','456737','386335'))

    # all 8 orders have the following criteria for score/achievement
      # sat_score_min = 1240; sat_score_max = 1450; psat_score_min = 1220; psat_score_max = 1450; gpa_low = B-; gpa_high = A+
      orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>% count(sat_score_min)
      orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>% count(sat_score_max)
          
      orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>% count(gpa_low)
      orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>% count(gpa_high)

    # total number of prospects purchased for these orders [132868 out of 158190 purchased using segment]
      lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>% count()
      #lists_orders_zip_hs_df %>% filter(univ_id == '145637', !is.na(ord_segment)) %>% count()

        # 5 orders use sat_min = 1240 and sat_max = 1450
          # order num = 403340 uses sat_min = 1230 and sat_max = 1450
  
          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(state_name)
          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(cbsa_name)

          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(sat_score_min)
          orders_df %>% filter(univ_id == '145637', order_num %in% c('483724','470283','371629','456737','386335')) %>% count(sat_score_max)

  # which segments for these orders
    # always the same set of neighborhood and high school segments:
      #'EN:51, HS:68|EN:58, HS:70|EN:53, HS:70|EN:51, HS:65|EN:69, HS:70|EN:60, HS:68|EN:73, HS:70|EN:60, HS:65|EN:69, HS:68|EN:70, HS:68|EN:63, HS:70|EN:70, HS:66|EN:78, HS:ALL|EN:58, HS:65|EN:58, HS:64|EN:51, HS:79|EN:60, HS:70|EN:51, HS:70|EN:53, HS:65|EN:60, HS:79|EN:69, HS:75|EN:70, HS:70|EN:63, HS:65|EN:61, HS:ALL|EN:73, HS:65|EN:70, HS:79'
      #EN:51, HS:65 | EN:51, HS:68 | EN:51, HS:70 | EN:51, HS:79
      #EN:53, HS:65 | EN:53, HS:70 |
      #EN:58, HS:64 | EN:58, HS:65 | EN:58, HS:70
      #EN:60, HS:65 | EN:60, HS:68 | EN:60, HS:70 | EN:60, HS:79
      #EN:61, HS:ALL
      #EN:63, HS:65 | EN:63, HS:70
      #EN:69, HS:68 | EN:69, HS:70 | EN:69, HS:75
      #EN:70, HS:66 | EN:70, HS:68 | EN:70, HS:70 | EN:70, HS:79
      #EN:73, HS:65 | EN:73, HS:70
      #EN:78, HS:ALL
    # how to read this:
      # for census tracts assigned to neighborhood cluster 51, they want students who attend high schools assigned to one of the following high school clusters %in% c(65,68,70,79)
         
          
          ############ CONTEXT:  IL IN STATE
        
          # compare to in-state orders for UI Urbana
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', stu_state == 'IL',!(is.na(stu_race_cb)),stu_race_cb !=0) %>%
          summarize(
            n_obs = sum(n()),
            n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
            pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
          )
          
          # IL PUBLIC SCHOOLS
          ceeb_hs %>% filter(school_control == 'public',total_12>0, state_code == 'IL') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
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
          
          
    # number of prospects purchased by CBSA
        lists_orders_zip_hs_df %>% filter(univ_id == '145637', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>%
          mutate(zip_cbsa_name_code = str_c(zip_cbsatitle_1,zip_cbsa_1, sep='; ')) %>%
          count(zip_cbsa_name_code) %>% arrange(desc(n)) %>% print(n=30)
        
      # 1 New York-Newark-Jersey City, NY-NJ-PA; 35620        28101
      # 2 Los Angeles-Long Beach-Anaheim, CA; 31080           12280
      # 3 Philadelphia-Camden-Wilmington, PA-NJ-DE-MD; 37980   9132
      # 4 San Francisco-Oakland-Hayward, CA; 41860             8105
      # 5 Atlanta-Sandy Springs-Roswell, GA; 12060             7456
      # 6 Washington-Arlington-Alexandria, DC-VA-MD-WV; 47900  5779
      # 7 Dallas-Fort Worth-Arlington, TX; 19100               5355
      # 8 Houston-The Woodlands-Sugar Land, TX; 26420          4981
      # 9 San Jose-Sunnyvale-Santa Clara, CA; 41940            4944
      #10 Boston-Cambridge-Newton, MA-NH; 14460                4729
      #11 Miami-Fort Lauderdale-West Palm Beach, FL; 33100     3703
      #12 San Diego-Carlsbad, CA; 41740                        2983
      #13 Austin-Round Rock, TX; 12420                         2104
      #14 Baltimore-Columbia-Towson, MD; 12580                 1914
      #15 Charlotte-Concord-Gastonia, NC-SC; 16740             1791
      #16 Tampa-St. Petersburg-Clearwater, FL; 45300           1522
      #17 Seattle-Tacoma-Bellevue, WA; 42660                   1502
      #18 Indianapolis-Carmel-Anderson, IN; 26900              1435
      #19 Sacramento--Roseville--Arden-Arcade, CA; 40900       1411
      #20 Orlando-Kissimmee-Sanford, FL; 36740                 1397
      #21 Raleigh, NC; 39580                                   1377
      #22 Trenton, NJ; 45940                                   1327
      #23 Detroit-Warren-Dearborn, MI; 19820                   1272
      #24 Bridgeport-Stamford-Norwalk, CT; 14860               1040
      #25 Riverside-San Bernardino-Ontario, CA; 40140           994

  # focus on atlanta
    # 5 Atlanta-Sandy Springs-Roswell, GA; 12060             7456


          
          ############ ATL
          # student-level racial composition of prospects
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751'),!(is.na(stu_race_cb)),stu_race_cb !=0) %>%
          summarize(
            n_obs = sum(n()),
            n_nonmiss_stu_race_cb = sum(is.na(stu_race_cb)==0),
            pct_stu_white =  mean(stu_white, na.rm = TRUE)*100,
            pct_stu_asian =  mean(stu_asian, na.rm = TRUE)*100,
            pct_stu_black =  mean(stu_black, na.rm = TRUE)*100,
            pct_stu_hispanic =  mean(stu_hispanic, na.rm = TRUE)*100,
            #pct_stu_amerindian =  mean(stu_amerindian, na.rm = TRUE)*100,
            #pct_stu_nativehawaii =  mean(stu_nativehawaii, na.rm = TRUE)*100,
            pct_stu_native =  mean(stu_native, na.rm = TRUE)*100,
            pct_stu_tworaces =  mean(stu_tworaces, na.rm = TRUE)*100,
          )          

          # racial composition of all students in public high schools [with a ceeb code] in the CBSA
            # data frames: ceeb_hs pubhs_privhs_data
          ceeb_hs %>% filter(school_control == 'public',total_12>0, cbsa_1 == '12060') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
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
          
          # average racial composition of public schools in the CBSA
          ceeb_hs %>% filter(school_control == 'public',total_12>0, cbsa_1 == '12060') %>%
            summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
            )      

  # high schools in CBSA with purchased prospects
      lists_orders_zip_hs_df %>% glimpse()
      # stu_ceeb
      ceeb_hs %>% glimpse()
    
    # number of prospects with non-missing ceeb code and that merge to high school-level data
      # 7,266 out of 7,456 prospects
    lists_orders_zip_hs_df %>% 
      filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>%
      # has a ceeb code; and ceeb code merged to high-school level data
      filter(!is.na(stu_ceeb), na_hs == 0) %>% count()

    #number of prospects by high school
    lists_orders_zip_hs_df %>% 
      filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>%
      # has a ceeb code; and ceeb code merged to high-school level data
      filter(!is.na(stu_ceeb), na_hs == 0) %>% count(stu_ceeb) %>% print(n=200)
    
    # number of prospects by high school at public high schools
    lists_orders_zip_hs_df %>% 
      filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>%
      # public
      filter(!is.na(stu_ceeb), na_hs == 0,hs_school_control == 'public') %>% 
      count(stu_ceeb) %>% arrange(desc(n)) %>% print(n=200)
    
    # create school level data frame w/ one obs per HS in ATL w/ GT0 prospects  
    atl_hs_seg <- lists_orders_zip_hs_df %>% 
      filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>%
      #filter(!is.na(stu_ceeb), na_hs == 0,hs_school_control == 'public') %>%
      filter(!is.na(stu_ceeb), na_hs == 0) %>% 
      group_by(stu_ceeb,hs_private) %>% summarize(
        n_stu = n(),
        n_stu_nonmiss_race_cb = sum(is.na(stu_race_cb)==0),
        n_stu_white = sum(stu_white, na.rm = TRUE),
        n_stu_asian = sum(stu_asian, na.rm = TRUE),
        n_stu_black = sum(stu_black, na.rm = TRUE),
        n_stu_hispanic = sum(stu_hispanic, na.rm = TRUE),
        n_stu_native = sum(stu_native, na.rm = TRUE),
        n_stu_tworaces = sum(stu_tworaces, na.rm = TRUE),
        n_stu_unknown = sum(stu_unknown, na.rm = TRUE),
      ) %>% arrange(hs_private,desc(n_stu))
      
atl_hs_seg %>% glimpse()
    # school-level dataset of all schools in atl that have ceeb code

      ceeb_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% count()
      ceeb_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% count(school_control)
      
      # create value labels for categorical variable of number of prospects purchased per high school
      lbls_cat5 <- c('zero','1-10','11-40','41-100','100+')
      lbls_cat4 <- c('zero','1-10','11-100','100+')
      lbls_cat3 <- c('zero','1-10','11+')
      
      atl_hs <- ceeb_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% left_join(atl_hs_seg, by = c('ceeb' = 'stu_ceeb')) %>%
        mutate(
          n_stu = if_else(!is.na(n_stu),n_stu,0L, missing = NULL),
          n_stu_white = if_else(!is.na(n_stu_white),n_stu_white,0, missing = NULL),
          n_stu_asian = if_else(!is.na(n_stu_asian),n_stu_asian,0, missing = NULL),
          n_stu_black = if_else(!is.na(n_stu_black),n_stu_black,0, missing = NULL),
          n_stu_hispanic = if_else(!is.na(n_stu_hispanic),n_stu_hispanic,0, missing = NULL),
          n_stu_native = if_else(!is.na(n_stu_native),n_stu_native,0, missing = NULL),
          n_stu_tworaces = if_else(!is.na(n_stu_tworaces),n_stu_tworaces,0, missing = NULL),
          n_stu_unknown = if_else(!is.na(n_stu_unknown),n_stu_unknown,0, missing = NULL),
          gt0_stu = if_else(n_stu>0,1,0, missing = NULL),
          n_stu_black_hispanic = n_stu_black + n_stu_hispanic,
          gt0_stu_black_hispanic = if_else(n_stu_black_hispanic>0,1,0, missing = NULL),
          n_stu_cat5 = cut(n_stu, breaks=c(-Inf, 0, 10, 40, 100, +Inf),labels = lbls_cat5),
          n_stu_cat4 = cut(n_stu, breaks=c(-Inf, 0, 10, 100, +Inf),labels = lbls_cat4),
          n_stu_cat3 = cut(n_stu, breaks=c(-Inf, 0, 10, +Inf),labels = lbls_cat3),
          n_stu_black_cat3 = cut(n_stu_black, breaks=c(-Inf, 0, 10, +Inf),labels = lbls_cat3),
          n_stu_hispanic_cat3 = cut(n_stu_hispanic, breaks=c(-Inf, 0, 10, +Inf),labels = lbls_cat3),
          n_stu_black_hispanic_cat3 = cut(n_stu_black_hispanic, breaks=c(-Inf, 0, 10, +Inf),labels = lbls_cat3),
          )

      atl_hs %>% glimpse()
      
      # average racial composition of public schools in the CBSA, with 0 vs. gt0 prospects purchased
      atl_hs %>% filter(school_control == 'public',total_12>0) %>%
        group_by(gt0_stu) %>% summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
        )
      
      # examine public high schools 
      atl_hs %>% filter(school_control == 'public',total_12>0) %>%
        group_by(n_stu_cat4) %>% summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
        )
      
      # examine private high schools
      atl_hs %>% filter(school_control == 'private',total_12>0) %>%
        group_by(n_stu_cat3) %>% summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
        )      

      #atl_hs %>% glimpse()
        #%>% mutate(pct = (n / sum(n)) * 100)  
      atl_hs %>% count(n_stu) %>% print(n=100)
      atl_hs %>% count(gt0_stu)
      

    ############ ATL - WHO ARE THE BLACK/LATINX PROSPECTS IN ATL PURCHASED VIA SEGMENT? WHAT ARE THEIR CHARACTERISTICS? HOW DO THEIR SCHOOLS COMPARE TO OTHER SCHOOLS IN ATL?
      
      lists_orders_zip_hs_df %>% count(stu_race_cb)
      
      lists_orders_zip_hs_df %>% 
        filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751'), stu_race_cb %in% c(3,4)) %>% count(stu_race_cb)
      
      ##### 
      ##### median income
      # avg. of median zip-code income for all ATL prospects purchased by segment; about $101,000
      lists_orders_zip_hs_df %>% 
        filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>% 
        summarize(
          n_obs = sum(n()),
          n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
          mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
      
      # avg. of median zip-code income for Black and Latinx ATL prospects purchased by segment; about $93,000
      lists_orders_zip_hs_df %>% 
        filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751'), stu_race_cb %in% c(3,4)) %>% 
        summarize(
          n_obs = sum(n()),
          n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
          mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
      
      # higher for latinx than for black      
      lists_orders_zip_hs_df %>% 
        filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751'), stu_race_cb %in% c(3,4)) %>% 
        group_by(stu_race_cb) %>% summarize(
          n_obs = sum(n()),
          n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
          mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )      
      
      
      ##### 
      ##### attendance at public or private school
      
      # all purchased prospects; about 12% attend private
      lists_orders_zip_hs_df %>% 
        filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751'),!is.na(hs_private)) %>% 
        count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)
      
      # black and latinx; about 13% attend private
      lists_orders_zip_hs_df %>% 
        filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751'),!is.na(hs_private),stu_race_cb %in% c(3,4)) %>% 
        count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)

      # higher percent private for latinx      
      lists_orders_zip_hs_df %>% 
        filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751'),!is.na(hs_private),stu_race_cb %in% c(3,4)) %>% 
        group_by(stu_race_cb) %>% count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)
      
      
      ##### 
      ##### racial composition of schools that purchased prospects attend
      

      # avg of school-level racial composition, all ATL schools; zero vs. at least one prospect purchased by Segment
      atl_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% 
        group_by(private,gt0_stu) %>% summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
        )   
      
      # racial composition of all students in ATL, separately for public vs. private
      atl_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% 
        group_by(private) %>% summarize(
              n_obs = sum(n()),
              #n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              tot_students = sum(total_students, na.rm = TRUE),
              pct_hs_white = sum(total_white, na.rm = TRUE)/tot_students*100,
              pct_hs_asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
              pct_hs_black = sum(total_black, na.rm = TRUE)/tot_students*100,
              pct_hs_hispanic = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
              pct_hs_native = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
        )

      # racial composition of all students in ATL, separately for public vs. private and by categorical number of black students purchased
      atl_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% 
        group_by(private,n_stu_black_cat3) %>% summarize(
              n_obs = sum(n()),
              #n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              tot_students = sum(total_students, na.rm = TRUE),
              pct_hs_white = sum(total_white, na.rm = TRUE)/tot_students*100,
              pct_hs_asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
              pct_hs_black = sum(total_black, na.rm = TRUE)/tot_students*100,
              pct_hs_hispanic = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
              pct_hs_native = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
        )      
      
      # FINDINGS
        # across all public HS in ATL (187 schools, 299K total students), 38.8% of all students are black
      
        # in public schools where no black students were purchased (120 schools, 152K total students), 52% of all students are black
        # in public schools where 1-10 black students were purchased (56 schools, 119K total students), 24.0% of all students are black
        # in public schools where 11 or more black students were purchased (11 schools, 28K total students), 29% of all students are black
      
        # takaway: schools where black prospects were purchased using Urbana Segment enrolled a lower percentage of black students than all ATL public schools
        # caveat: disparities are not huge
          # can we say that Black students attending schools with a lower percentage of black students were more likely to be purchased than black students attending schools that enrolled a higher percentage of black students?
        
      

      atl_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% 
        group_by(private,n_stu_black_cat3) %>% summarize(
              n_obs = sum(n()),
              #n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              tot_students = sum(total_students, na.rm = TRUE),
              pct_hs_white = sum(total_white, na.rm = TRUE)/tot_students*100,
              pct_hs_asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
              pct_hs_black = sum(total_black, na.rm = TRUE)/tot_students*100,
              pct_hs_hispanic = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
              pct_hs_native = sum(total_native, na.rm = TRUE)/tot_students*100, # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
        )      
      
      # school-level avg of racial composition by number of black prospects purchased
        # school-level average percent of students who are black in ATL is 44.8%
      atl_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% 
        group_by(private,n_stu_black_cat3) %>% summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
        )      
      
      # schools with at least one black/latinx prospect purchased
      atl_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% 
        group_by(private,n_stu_black_hispanic_cat3) %>% summarize(
              n_obs = sum(n()),
              n_nonmiss_hs_race = sum(is.na(pct_white)==0),
              pct_hs_white = mean(pct_white, na.rm = TRUE),
              pct_hs_asian = mean(pct_asian, na.rm = TRUE),
              pct_hs_black = mean(pct_black, na.rm = TRUE),
              pct_hs_hispanic = mean(pct_hispanic, na.rm = TRUE),
              pct_hs_native = mean(pct_native, na.rm = TRUE), # native american + alaska native + native hawaiaan + other pacific islander
              pct_hs_tworaces = mean(pct_tworaces, na.rm = TRUE),
              pct_hs_unknown = mean(pct_unknown, na.rm = TRUE),      
        )
      
      
atl_hs_seg %>% glimpse()
    # school-level dataset of all schools in atl that have ceeb code

      ceeb_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% count()
      ceeb_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% count(school_control)
      
      # create value labels for categorical variable of number of prospects purchased per high school
      lbls_cat5 <- c('zero','1-10','11-40','41-100','100+')
      lbls_cat4 <- c('zero','1-10','11-100','100+')
      lbls_cat3 <- c('zero','1-10','11+')
      
      atl_hs <- ceeb_hs %>% filter(total_12>0, cbsa_1 == '12060') %>% left_join(atl_hs_seg, by = c('ceeb' = 'stu_ceeb')) %>%
        mutate(
          n_stu = if_else(!is.na(n_stu),n_stu,0L, missing = NULL),
          gt0_stu = if_else(n_stu>0,1,0, missing = NULL),
          n_stu_cat5 = cut(n_stu, breaks=c(-Inf, 0, 10, 40, 100, +Inf),labels = lbls_cat5),
          n_stu_cat4 = cut(n_stu, breaks=c(-Inf, 0, 10, 100, +Inf),labels = lbls_cat4),
          n_stu_cat3 = cut(n_stu, breaks=c(-Inf, 0, 10, +Inf),labels = lbls_cat3),
          )

      atl_hs %>% glimpse()      
##########
          
          
  # U. Urbana international purchases; 4 orders; these don't use segment
      orders_df %>% filter(univ_id == '145637', order_num %in% c('483721','372044','470250','371669')) %>% View()
      
  orders_df %>% glimpse()
  orders_df %>% filter(univ_id == '145637', order_num == '483724') %>% View()
  
    # segment
      lists_orders_zip_hs_df %>% count(univ_name,ord_segment) %>% print(n=100)
      lists_orders_zip_hs_df %>% filter(univ_id == '145637') %>% count(univ_name,ord_segment) %>% print(n=100)
      
      lists_orders_zip_hs_df %>% filter(univ_id == '145637') %>% count(ord_segment) %>% print(n=100)
      orders_df %>% filter(univ_id == '145637') %>% count(segment)
      
      # orders that use segment
      orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% count(segment)
      orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% count(state_name)
      orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% count(cbsa_name)
      # some orders just use state_name in conjunction w/ segment; others use CBSA in conjunction w/ segment; others use CBSA and state name in conjunction w/ segment
      orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% count(state_name,cbsa_name)
      
      orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% count(sat_score_min)
      orders_df %>% filter(univ_id == '145637', !is.na(segment)) %>% count(sat_score_max)
      
##### analysis of segment orders (for U Illinois-Urbana)
        
  
      
  lists_orders_zip_hs_df %>% count(univ_name,ord_intl_region) %>% print(n=100)
  
  ord_segment
  lists_orders_zip_hs_df %>% count(univ_name,ord_geomarket) %>% print(n=100)
  lists_orders_zip_hs_df %>% count(univ_name,ord_cbsa_name) %>% print(n=100)
  lists_orders_zip_hs_df %>% count(univ_name,ord_zip_code) %>% print(n=100)
  
  lists_orders_zip_hs_df %>% count(univ_name,ord_rank_low) %>% print(n=100)
  
  lists_orders_zip_hs_df %>% count(univ_name,ord_zip_code_file) %>% print(n=100)
  
  
  ord_intl_region


#############################################  
## -----------------------------------------------------------------------------
## RQ2: what are the characteristics of schools/communities included vs. not included in purchased lists
## -----------------------------------------------------------------------------  
#############################################
  

# median income      
  # Avg for set of zip-codes where purchased prospect lives
    
    # all
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs ==0) %>% 
      # group_by student zip_code [and potentially in-state/out-of-state purchase] and then keep only first obs for each zip-code
      group_by(stu_zip_code) %>% filter(row_number()==1) %>% ungroup() %>% 
      summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
    
    # by 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs ==0) %>% 
      # group_by student zip_code [and potentially in-state/out-of-state purchase] and then keep only first obs for each zip-code
      group_by(stu_zip_code,stu_out_st) %>% filter(row_number()==1) %>% ungroup() %>% 
      group_by(stu_out_st) %>%summarize(
        n_obs = sum(n()),
        n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
        mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
      )
    
# racial composition of high school student attend
  
  #Avg of purchased prospects
  
    #all
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% 
      summarize(
        n_obs = sum(n()),
        n_nonmiss_hs_race = sum(is.na(hs_pct_white)==0),
        pct_hs_white = mean(hs_pct_white, na.rm = TRUE),
        pct_hs_asian = mean(hs_pct_asian, na.rm = TRUE),
        pct_hs_black = mean(hs_pct_black, na.rm = TRUE),
        pct_hs_hispanic = mean(hs_pct_hispanic, na.rm = TRUE),
        #pct_hs_amerindian = mean(hs_pct_amerindian, na.rm = TRUE),
        #pct_hs_nativehawaii = mean(hs_pct_nativehawaii, na.rm = TRUE),
        pct_hs_native = mean(hs_pct_native, na.rm = TRUE),
        pct_hs_tworaces = mean(hs_pct_tworaces, na.rm = TRUE),
        pct_hs_unknown = mean(hs_pct_unknown, na.rm = TRUE),      
      )
  
    # by 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% group_by(stu_out_st) %>% 
      summarize(
        n_obs = sum(n()),
        n_nonmiss_hs_race = sum(is.na(hs_pct_white)==0),
        pct_hs_white = mean(hs_pct_white, na.rm = TRUE),
        pct_hs_asian = mean(hs_pct_asian, na.rm = TRUE),
        pct_hs_black = mean(hs_pct_black, na.rm = TRUE),
        pct_hs_hispanic = mean(hs_pct_hispanic, na.rm = TRUE),
        #pct_hs_amerindian = mean(hs_pct_amerindian, na.rm = TRUE),
        #pct_hs_nativehawaii = mean(hs_pct_nativehawaii, na.rm = TRUE),
        pct_hs_native = mean(hs_pct_native, na.rm = TRUE),
        pct_hs_tworaces = mean(hs_pct_tworaces, na.rm = TRUE),
        pct_hs_unknown = mean(hs_pct_unknown, na.rm = TRUE),      
      )    
      
  #Avg for set of high schools where purchased prospect lives
    
    # all
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% 
      # group_by student zip_code [and potentially in-state/out-of-state purchase] and then keep only first obs for each zip-code
      group_by(stu_ceeb) %>% filter(row_number()==1) %>% ungroup() %>%
      # group_by out-of-state and then create summary statistics
      summarize(
        n_obs = sum(n()),
        n_nonmiss_hs_race = sum(is.na(hs_pct_white)==0),
        pct_hs_white = mean(hs_pct_white, na.rm = TRUE),
        pct_hs_asian = mean(hs_pct_asian, na.rm = TRUE),
        pct_hs_black = mean(hs_pct_black, na.rm = TRUE),
        pct_hs_hispanic = mean(hs_pct_hispanic, na.rm = TRUE),
        #pct_hs_amerindian = mean(hs_pct_amerindian, na.rm = TRUE),
        #pct_hs_nativehawaii = mean(hs_pct_nativehawaii, na.rm = TRUE),
        pct_hs_native = mean(hs_pct_native, na.rm = TRUE),
        pct_hs_tworaces = mean(hs_pct_tworaces, na.rm = TRUE),
        pct_hs_unknown = mean(hs_pct_unknown, na.rm = TRUE),      
      )  
    
    # by 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% 
      # group_by student zip_code [and potentially in-state/out-of-state purchase] and then keep only first obs for each zip-code
      group_by(stu_ceeb,stu_out_st) %>% filter(row_number()==1) %>% ungroup() %>%
      # group_by out-of-state and then create summary statistics
      group_by(stu_out_st) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_hs_race = sum(is.na(hs_pct_white)==0),
        pct_hs_white = mean(hs_pct_white, na.rm = TRUE),
        pct_hs_asian = mean(hs_pct_asian, na.rm = TRUE),
        pct_hs_black = mean(hs_pct_black, na.rm = TRUE),
        pct_hs_hispanic = mean(hs_pct_hispanic, na.rm = TRUE),
        #pct_hs_amerindian = mean(hs_pct_amerindian, na.rm = TRUE),
        #pct_hs_nativehawaii = mean(hs_pct_nativehawaii, na.rm = TRUE),
        pct_hs_native = mean(hs_pct_native, na.rm = TRUE),
        pct_hs_tworaces = mean(hs_pct_tworaces, na.rm = TRUE),
        pct_hs_unknown = mean(hs_pct_unknown, na.rm = TRUE),      
      )  
    
# racial composition of zip-code
  
  #Avg of purchased prospects
  
    # all
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs ==0) %>% 
      summarize(
        n_obs = sum(n()),
        n_nonmiss_zip_race = sum(is.na(zip_pop_otherrace_15_19_pct)==0),
        pct_zip_white = mean(zip_pop_white_15_19_pct, na.rm = TRUE),
        pct_zip_asian = mean(zip_pop_asian_15_19_pct, na.rm = TRUE),
        pct_zip_black = mean(zip_pop_black_15_19_pct, na.rm = TRUE),
        pct_zip_hispanic = mean(zip_pop_hispanic_15_19_pct, na.rm = TRUE),
        #pct_zip_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
        #pct_zip_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
        pct_zip_native = mean(zip_pop_native_15_19_pct, na.rm = TRUE),
        pct_zip_otherrace = mean(zip_pop_otherrace_15_19_pct, na.rm = TRUE),
        pct_zip_tworaces = mean(zip_pop_tworaces_15_19_pct, na.rm = TRUE),
      )
    
    # by 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs ==0) %>% group_by(stu_out_st) %>% 
      summarize(
        n_obs = sum(n()),
        n_nonmiss_zip_race = sum(is.na(zip_pop_otherrace_15_19_pct)==0),
        pct_zip_white = mean(zip_pop_white_15_19_pct, na.rm = TRUE),
        pct_zip_asian = mean(zip_pop_asian_15_19_pct, na.rm = TRUE),
        pct_zip_black = mean(zip_pop_black_15_19_pct, na.rm = TRUE),
        pct_zip_hispanic = mean(zip_pop_hispanic_15_19_pct, na.rm = TRUE),
        #pct_zip_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
        #pct_zip_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
        pct_zip_native = mean(zip_pop_native_15_19_pct, na.rm = TRUE),
        pct_zip_otherrace = mean(zip_pop_otherrace_15_19_pct, na.rm = TRUE),
        pct_zip_tworaces = mean(zip_pop_tworaces_15_19_pct, na.rm = TRUE),
      )
    
  #Avg for set of zip-codes where purchased prospect lives
    
    #all
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs ==0) %>% 
      # group_by student zip_code [and potentially in-state/out-of-state purchase] and then keep only first obs for each zip-code
      group_by(stu_zip_code) %>% filter(row_number()==1) %>% ungroup() %>% 
      # group_by out-of-state and then create summary statistics
      summarize(
        n_obs = sum(n()),
        n_nonmiss_zip_race = sum(is.na(zip_pop_otherrace_15_19_pct)==0),
        pct_zip_white = mean(zip_pop_white_15_19_pct, na.rm = TRUE),
        pct_zip_asian = mean(zip_pop_asian_15_19_pct, na.rm = TRUE),
        pct_zip_black = mean(zip_pop_black_15_19_pct, na.rm = TRUE),
        pct_zip_hispanic = mean(zip_pop_hispanic_15_19_pct, na.rm = TRUE),
        #pct_zip_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
        #pct_zip_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
        pct_zip_native = mean(zip_pop_native_15_19_pct, na.rm = TRUE),
        pct_zip_otherrace = mean(zip_pop_otherrace_15_19_pct, na.rm = TRUE),
        pct_zip_tworaces = mean(zip_pop_tworaces_15_19_pct, na.rm = TRUE),
      )
  
    # by 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_zip_acs ==0) %>% 
      # group_by student zip_code [and potentially in-state/out-of-state purchase] and then keep only first obs for each zip-code
      group_by(stu_zip_code,stu_out_st) %>% filter(row_number()==1) %>% ungroup() %>% 
      # group_by out-of-state and then create summary statistics
      group_by(stu_out_st) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_zip_race = sum(is.na(zip_pop_otherrace_15_19_pct)==0),
        pct_zip_white = mean(zip_pop_white_15_19_pct, na.rm = TRUE),
        pct_zip_asian = mean(zip_pop_asian_15_19_pct, na.rm = TRUE),
        pct_zip_black = mean(zip_pop_black_15_19_pct, na.rm = TRUE),
        pct_zip_hispanic = mean(zip_pop_hispanic_15_19_pct, na.rm = TRUE),
        #pct_zip_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
        #pct_zip_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
        pct_zip_native = mean(zip_pop_native_15_19_pct, na.rm = TRUE),
        pct_zip_otherrace = mean(zip_pop_otherrace_15_19_pct, na.rm = TRUE),
        pct_zip_tworaces = mean(zip_pop_tworaces_15_19_pct, na.rm = TRUE),
      )
    


######### PROSPECT CHARACTERISTICS OF INTEREST; BY UNIVERSITY TYPE AND IN/OUT OF STATE

# racial composition of high school student attend
  
  #Avg of purchased prospects
  
    # by university type
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% 
      group_by(univ_c15basic) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_hs_race = sum(is.na(hs_pct_white)==0),
        pct_hs_white = mean(hs_pct_white, na.rm = TRUE),
        pct_hs_asian = mean(hs_pct_asian, na.rm = TRUE),
        pct_hs_black = mean(hs_pct_black, na.rm = TRUE),
        pct_hs_hispanic = mean(hs_pct_hispanic, na.rm = TRUE),
        #pct_hs_amerindian = mean(hs_pct_amerindian, na.rm = TRUE),
        #pct_hs_nativehawaii = mean(hs_pct_nativehawaii, na.rm = TRUE),
        pct_hs_native = mean(hs_pct_native, na.rm = TRUE),
        pct_hs_tworaces = mean(hs_pct_tworaces, na.rm = TRUE),
        pct_hs_unknown = mean(hs_pct_unknown, na.rm = TRUE),      
      )
  
    # by university type and 0/1 prospect is out of state
    lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% 
      group_by(univ_c15basic,stu_out_st) %>% summarize(
        n_obs = sum(n()),
        n_nonmiss_hs_race = sum(is.na(hs_pct_white)==0),
        pct_hs_white = mean(hs_pct_white, na.rm = TRUE),
        pct_hs_asian = mean(hs_pct_asian, na.rm = TRUE),
        pct_hs_black = mean(hs_pct_black, na.rm = TRUE),
        pct_hs_hispanic = mean(hs_pct_hispanic, na.rm = TRUE),
        #pct_hs_amerindian = mean(hs_pct_amerindian, na.rm = TRUE),
        #pct_hs_nativehawaii = mean(hs_pct_nativehawaii, na.rm = TRUE),
        pct_hs_native = mean(hs_pct_native, na.rm = TRUE),
        pct_hs_tworaces = mean(hs_pct_tworaces, na.rm = TRUE),
        pct_hs_unknown = mean(hs_pct_unknown, na.rm = TRUE),      
      )