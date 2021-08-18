################################################################################
##
## [ PROJ ] < student list project, EDA >
## [ FILE ] < combined_oj.R >
## [ AUTH ] < Ozan Jaquette / ozanj >
## [ INIT ] < 8/5/2021 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(leaflet)
library(rgdal)
library(raster)
library(formattable)
library(tidyverse)
library(readxl)
library(lubridate)
library(htmlwidgets)
library(sf)
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

## -----------------------------------------------------------------------------
## FUNCTIONS
## -----------------------------------------------------------------------------

# Function to remove NA columns [for order summary data only?]Categories of orders
  # removes columns wehre all values are NA?
remove_NA_cols <- function(data_df) {
  data_df[!sapply(data_df, function(x) all(is.na(x)))]
}

# Add 11 + 12 columns for the SAT test takers datasets
add_testtakers_cols <- function(sat_df) {
  sat_df %>% mutate(
    Enroll1112 = as.numeric(Enroll12) + as.numeric(Enroll11),
    NumTSTTakr1112 = NumTSTTakr11 + NumTSTTakr12,
    NumERWBenchmark1112 = as.numeric(NumERWBenchmark11) + as.numeric(NumERWBenchmark12),
    PctERWBenchmark1112 = as.numeric(NumERWBenchmark1112) / as.numeric(NumTSTTakr1112),
    NumMathBenchmark1112 = as.numeric(NumMathBenchmark11) + as.numeric(NumMathBenchmark12),
    PctMathBenchmark1112 = as.numeric(NumMathBenchmark1112) / as.numeric(NumTSTTakr1112),
    TotNumBothBenchmark1112 = as.numeric(TotNumBothBenchmark11) + as.numeric(TotNumBothBenchmark12),
    PctBothBenchmark1112 = as.numeric(TotNumBothBenchmark1112) / as.numeric(NumTSTTakr1112)
  )
}

#df_sat_ca_20 <- add_testtakers_cols(df_sat_ca_20)
#df_sat_ca_19 <- add_testtakers_cols(df_sat_ca_19)
## -----------------------------------------------------------------------------
## SECONDARY DATA
## -----------------------------------------------------------------------------

# 
zip_cbsa_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_code_cbsa.csv'))
  
  #zip_cbsa_data %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
  #zip_cbsa_data %>% glimpse() # one observation per zip-code; for each zip-code indicates which CBSA(s) that zip code belongs to
  
hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c'))
  
  #hs_data %>% glimpse() # 
  #hs_data %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # ncessch uniquely identifies obs
  #hs_data %>% count(school_type) # public and private

ceeb_nces <- read_csv(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/ceeb_nces_crosswalk.csv'))

  # this dataset is a crosswalk between nces school code and ceeb code (college board school code)
  #ceeb_nces %>% glimpse() # two variables: ceeb code (college board school code); ncessch code
  #ceeb_nces %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs
  #ceeb_nces %>% group_by(ceeb) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs

cds_nces <- readr::with_edition(1, read_csv(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/CDS_NCES_crosswalk.csv'))) %>% mutate(ncessch = str_c(NCESDist, NCESSchool))
  # note: because using readr version 2.0, must use readr::with_edition() to overcome this error: The size of the connection buffer (131072) was not large enough to fit a complete line: * Increase it by setting `Sys.setenv("VROOM_CONNECTION_SIZE")`

  # This seems to be data about California public schools
  cds_nces %>% glimpse()
  cds_nces %>% group_by(CDSCode) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
  cds_nces %>% count(SOCType)
  cds_nces %>% count(State) # california or missing


# load 2017-18 University data from IPEDS
  # NOTE: I THINK THIS MAY HAVE BEEN CREATED FROM SUBSET OF STATA IPEDS PANEL WHICH WAS THEN USED IN PRIVATE HIGH SCHOOL RECRUITING CHAPTER; 
    # MAY WANT TO CHANGE THIS LATER TO PULL FROM RAW IPEDS DATA AND FOR MORE RECENT YEAR(S)
  # ./data/ipeds_1718.RDS has over 400 variables
univ_data <- readRDS('./data/ipeds_1718.RDS') %>% 
  select(univ_id,opeid,univ_name,ialias,addr,city,state_code,zip_code,fips_state_code,obereg,region,sector,locale,starts_with('c15'),
         ccbasic,carnegie,landgrnt,instsize,cbsa,csa,countycd,countynm,cngdstcd,longitude,latitude,room,board,roomamt,boardamt,rmbrdamt,
         starts_with('pct_freshman'),starts_with('applcn'),starts_with('admssn'),enrlt,enrlft,enrlpt,starts_with('sat'),starts_with('act'),
         -act,tuition1,fee1,tuit_fees_instate,tuit_fees_outstate) %>% 
  rename(tuit_indist = tuition1, fee_indist = fee1, tuit_fee_instate = tuit_fees_instate, tuit_fee_outstate = tuit_fees_outstate) %>%
  mutate(tuit_fee_indist = tuit_indist + fee_indist)

state_codes <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')  
## -----------------------------------------------------------------------------
## LOAD/INVESTIGATE ORDER SUMMARY DATA AND LIST DATA
## -----------------------------------------------------------------------------

load(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/combined_data.RData'))
orders_df %>% glimpse()
lists_df %>% glimpse()

orders_df <- univ_data %>% select(univ_id, univ_name, state_code, zip_code, sector, c15basic) %>% rename(univ_state = state_code, univ_zip = zip_code, univ_sector = sector, univ_c15basic = c15basic) %>%
  right_join(orders_df, by = 'univ_id') %>% select(-univ_sector)

# LABEL ORDER SUMMARY DATA
orders_df %>% glimpse()

var_label(orders_df[['univ_id']]) <- 'University IPEDS/unitid'

var_label(orders_df[['po_num']]) <- 'Purchase order number (multiple student list purchases per purchase order number)'
  #orders_df %>% count(order_num)
  # each value of order_num -- which uniquely identifies obs - is associated with a purchase order number
  # orders_df %>% count(po_num,order_num) %>% print(n=100)
  # orders_df %>% count(univ_name,po_num) %>% print(n=100)

var_label(orders_df[['order_num']]) <- 'Student list order number; each student list order number represents a unique student list'
  #orders_df %>% group_by(order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key)
  # note: the variable order_num uniquely identifies observations

var_label(orders_df[['order_title']]) <- 'Character descripter (e.g., score range, region, year) associated with each student list'
  #orders_df %>% count(order_title) %>% print(n=100)
  #orders_df %>% group_by(order_title) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key)
    # almost unique

var_label(orders_df[['order_cost']]) <- 'Price of the student list purchase'
var_label(orders_df[['num_students']]) <- 'Number of prospecive students purchased in the student list'
  #orders_df %>% count(num_students) %>% print(n=100)
var_label(orders_df[['hs_grad_class']]) <- 'Year(s) of high school graduating class specified in filter'  
  #orders_df %>% count(hs_grad_class) %>% print(n=100)
var_label(orders_df[['state_name']]) <- 'State(s) of prospects included in student list'
var_label(orders_df[['cbsa_name']]) <- 'Name of CBSAs included in student list; NA if student list is at state-level (I think)'
  #orders_df %>% count(state_name,cbsa_name) %>% print(n=100)
var_label(orders_df[['intl_region']]) <- 'Code and names of international regions included in student list'
  #orders_df %>% count(intl_region) %>% print(n=100)
var_label(orders_df[['segment']]) <- 'Segments (neighborhood codes and/or high school codes) filtered in purchase; only non-missing for lists purchased using segment analysis product'
  #orders_df %>% count(segment) %>% print(n=100)
  #orders_df %>% count(segment,state_name) %>% print(n=100)
  #orders_df %>% count(segment,cbsa_name) %>% print(n=100)

var_label(orders_df[['sat_score_min']]) <- 'minimum SAT score specified in filter for student list (sometimes combined w/ PSAT score as an OR filter)'
  #orders_df %>% count(sat_score_min) %>% print(n=100)
orders_df %>% count(sat_score_min) %>% print(n=100)
var_label(orders_df[['sat_score_max']]) <- 'maximum SAT score specified in filter for student list (sometimes combined w/ PSAT score as an OR filter)'
  #orders_df %>% count(sat_score_max) %>% print(n=100)
var_label(orders_df[['sat_score_old_min']]) <- 'minimum SAT score (old scoring) specified in filter for student list (sometimes combined w/ PSAT score as an OR filter)'
  #orders_df %>% count(sat_score_old_min) %>% print(n=100)
var_label(orders_df[['sat_score_old_max']]) <- 'maximum SAT score (old scoring) specified in filter for student list (sometimes combined w/ PSAT score as an OR filter)'
  #orders_df %>% count(sat_score_old_max) %>% print(n=100)
var_label(orders_df[['psat_score_min']]) <- 'minimum PSAT score specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  #orders_df %>% count(psat_score_min) %>% print(n=100)
var_label(orders_df[['psat_score_max']]) <- 'maximum PSAT score specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  #orders_df %>% count(psat_score_max) %>% print(n=100)
var_label(orders_df[['psat_score_old_min']]) <- 'minimum PSAT score (old scoring) specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  #orders_df %>% count(psat_score_old_min) %>% print(n=100)  
var_label(orders_df[['psat_score_old_max']]) <- 'minimum PSAT score (old scoring) specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  #orders_df %>% count(psat_score_old_max) %>% print(n=100)  
var_label(orders_df[['gpa_high']]) <- 'highest HS GPA specified in filter for student list purchase'
  #orders_df %>% count(gpa_high) %>% print(n=100)
var_label(orders_df[['gpa_low']]) <- 'lowest HS GPA specified in filter for student list purchase'
  #orders_df %>% count(gpa_low) %>% print(n=100)
var_label(orders_df[['rank_high']]) <- 'highest high school rank specified in filter for student list purchase'
  #orders_df %>% count(rank_high) %>% print(n=100)  
var_label(orders_df[['rank_low']]) <- 'lowest high school rank specified in filter for student list purchase'
  #orders_df %>% count(rank_low) %>% print(n=100)

var_label(orders_df[['race_ethnicity']]) <- 'race/ethnicity categories (character var) specified in filter for student list purchase'
  #orders_df %>% count(race_ethnicity) %>% print(n=100)
var_label(orders_df[['gender']]) <- 'gender categories (character var) specified in filter for student list purchase'
  #orders_df %>% count(gender) %>% print(n=100)
var_label(orders_df[['college_type']]) <- 'type of college prospective student interested in attending (e.g., four-year) specified in filter for student list purchase' # var always NA for urbana

var_label(orders_df[['edu_aspirations']]) <- 'type of college prospective student interested in attending (e.g., four-year) specified in filter for student list purchase' # var always NA for urbana
var_label(orders_df[['rotc_plans']]) <- 'RPTC participation plans of prospective student interested in attending (e.g., four-year) specified in filter for student list purchase' # var always NA for urbana
var_label(orders_df[['major']]) <- 'intended major of  prospective student interested in attending (e.g., four-year) specified in filter for student list purchase' # var always NA for urbana

######## VARIABLES YOU ARE NOT REALLY SURE ABOUT
var_label(orders_df[['num_runs']]) <- 'Number of runs? DO NOT KNOW WHAT THIS VARIABLE REALLY REFERS TO'
  #orders_df %>% count(num_runs) %>% print(n=100)
var_label(orders_df[['date_start']]) <- '????'
var_label(orders_df[['date_end']]) <- '????'
  #orders_df %>% count(date_start) %>% print(n=100)
  #orders_df %>% count(date_end) %>% print(n=100)
var_label(orders_df[['zip_code_file']]) <- '????' # var always NA for urbana
  #orders_df %>% count(zip_code_file) %>% print(n=100)
  # orders_df %>% count(univ_name,zip_code_file) %>% print(n=100)
var_label(orders_df[['zip_code']]) <- 'zip codes (always 3 digit?) specified in filter for student list purchase' # var always NA for urbana
  
  #orders_df %>% count(zip_code) %>% print(n=100)
  #orders_df %>% count(univ_name,zip_code) %>% print(n=100)
  #orders_df %>% count(univ_name,zip_code) %>% print(n=100)
var_label(orders_df[['county']]) <- 'Name of county specified in filter for student list purchase' # var always NA for urbana
var_label(orders_df[['date_updated']]) <- '????'

var_label(orders_df[['created_by']]) <- 'Name of person (always university employee?) who made the student list purchase' # var always NA for urbana
var_label(orders_df[['source_file']]) <- 'Name or document that was scraped to create tabular order summary data' # var always NA for urbana
var_label(orders_df[['market']]) <- 'Name the university assigned to the (geographic) market associated with the student list purchase (Texas A&M-Texarkana)' # var always NA for urbana
var_label(orders_df[['geomarket']]) <- 'Name the university assigned to the (geographic) market associated with the student list purchase (Stephen F. Austin)' # var always NA for urbana
# check that vars have variable labels
orders_df %>% var_label()

## -----------------------------------------------------------------------------
## INVESTIGATE LIST DATA
## -----------------------------------------------------------------------------

lists_df %>% glimpse()

#### URBANA **** (data manipulation Crystal did to create Urbana obs in lists_df)
  # read in prospect data from College Board and ACT student lists
    #lists_df <- read_csv(file.path(data_dir, '145637_lists.csv'), col_types = cols(.default = 'c'))
    # data structure
      # 434,120 obs, one obs per "Ref", which I think is a unique identifier to represent prospects
         #lists_df %>% group_by(Ref) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key)
    # variable "Source"
      # str_count(lists_df$Source, 'SAT|ACT') %>% sum()  # str_count() counts the number of matches in a string
        # above code counts number of times the variable 'Source' contained the string 'SAT' OR 'ACT'; 
        # some prospects purchased by multiple SAT purchases or by multiple ACT purchases; or by at least one SAT purchase and at least one ACT purchase
        #465231 matches number of rows in lists_df_pivot
      #lists_df %>% count(Source) %>% print(n=100) 
        # values of variable source indicate the following: date (or student list purchase?); testing agency purchased from (ACT, SAT); order number of student list purchase
  # pivot longer step
    # https://cathblatter.rbind.io/blog/2020/03/16/using-pivot-longer-and-regex-for-data-wrangling/
    #lists_df_pivot <- lists_df %>% 
    #  pivot_longer(
    #    cols = starts_with(c('sat_', 'act_')),
    #    names_to = c('.value', 'test_num'),
    #    names_pattern = '(^\\w+)_(\\d+)'
    #  ) %>%
    #  select(-test_num) %>% 
    #  pivot_longer(
    #    cols = starts_with(c('sat_', 'act_')),
    #    names_to = c('test_type', '.value'),
    #    names_sep = '_',
    #    values_drop_na = T
    #  ) %>%
    #  rename(order_num = test, order_date = date) %>% 
    #  mutate(order_date = mdy(order_date)) %>%
    #  distinct()      
      # what first pivot is doing:
      # what second pivot is doing; 
    # goal of step: 
      # go from:  one observation per value of 'Ref' (representing prospect ID) -- but each 'Ref' may be targeted by more than one student list
      # go to: one observation per each unique comination of 'Ref' and student list purchase number
        # so now each value of 'Ref' (prospect ID) may appear more than once
    # check new data structure
        
    #    lists_df_pivot %>% group_by(Ref) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # multiple obs per ref
    #    lists_df_pivot %>% group_by(Ref, order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # multiple obs per ref,order_num (which i didn't expect)
    #    lists_df_pivot %>% group_by(Ref, order_num, order_date) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # one obs per ref,order_num, order_date
        
    #    lists_df_pivot %>% filter(Ref == '595894623') %>% View()

    #    lists_df_pivot %>% count(Source) %>% print(n=100) # for each obs, variable 'Source' might contain: multiple SAT purchases or by multiple ACT purchases; or by at least one SAT purchase and at least one ACT purchase
    #     lists_df_pivot %>% count(test_type) # can be either SAT or ACT
         
  # create df w/ only data from College Board
      #lists_df_sat <- lists_df_pivot %>% filter(test_type == 'sat')
      #lists_df_sat %>% glimpse()
    # data structure
      #lists_df_sat %>% group_by(Ref) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # multiple obs per ref
      #lists_df_sat %>% group_by(Ref, order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # multiple obs per ref,order_num (which i didn't expect)
      #lists_df_sat %>% group_by(Ref, order_num, order_date) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # one obs per ref,order_num, order_date
         

#### LABEL ALL VARIABLES

# merge in IPEDS vars
lists_df <- univ_data %>% select(univ_id, univ_name, state_code, zip_code, sector, c15basic) %>% rename(univ_state = state_code, univ_zip = zip_code, univ_sector = sector, univ_c15basic = c15basic) %>%
  right_join(lists_df, by = 'univ_id') %>% select(-univ_sector)

var_label(lists_df[['student_id']]) <- 'Student id; some universities do not provide student id' # 
  
  # urbana, univ_id = 145637
    #lists_df %>% filter(univ_id == '145637') %>% group_by(student_id, order_no) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs (once we make a minor fix upstream)

  # Stephen F Austin, univid = '228431'; student_id always missing
    #lists_df %>% filter(univ_id == '228431') %>% count(univ_name,student_id) %>% print(n=100)
  
  # Texarkana, univid = '224545'; student_id non-missing
    #lists_df %>% filter(univ_id == '224545') %>% count(univ_name,student_id) %>% print(n=100)
    #lists_df %>% filter(univ_id == '224545') %>% group_by(student_id) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identifies obs
    #lists_df %>% filter(univ_id == '224545') %>% group_by(student_id, order_no) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
    
var_label(lists_df[['city']]) <- 'City of prospect (always home city or sometimes school city?)' # 
   # alwasy missing in data provided by Stephen F. Austin

   #lists_df %>% filter(univ_id == '228431') %>% group_by(univ_name) %>% summarise(na_city = sum(is.na(city)))
   lists_df %>% group_by(univ_name) %>% summarise(
     n_obs = sum(n()),
     n_na_city = sum(is.na(city)),
     n_city = sum(is.na(city)==0)
    )

#$ state                 <chr> "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX~      
var_label(lists_df[['state']]) <- 'state of prospect' # 
  # missing most often for UI-Urbana (33,875 NA out of 415,458)
  # lists_df %>% count(univ_name,) %>% print(n=100)
   #lists_df %>% group_by(univ_name) %>% summarise(
    # n_obs = sum(n()),
    # n_miss = sum(is.na(state)),
    # n_nonmiss = sum(is.na(state)==0)
    #)

#$ zip                   <chr> "77498-1804", "79707-4543", "75040-1082", "77059-3139", "75094-4442", "75211-4647", "75205", "75078-7945", "79911-3052", "79423-3629", "77845~      
var_label(lists_df[['zip']]) <- 'zip code, 5 + 4 digits; sometimes missing' # 
  #lists_df %>% count() %>% print(n=100)
  # lists_df %>% count(univ_name,) %>% print(n=100)

  # count frequency of length of zip code
  #lists_df %>% mutate(zip_len = str_length(zip)) %>% count(univ_name,zip_len) %>% print(n=100) 
  
  #lists_df %>% group_by(univ_name) %>% summarise(
  #   n_obs = sum(n()),
  #   n_miss = sum(is.na(zip)),
  #   n_nonmiss = sum(is.na(zip)==0)
  #  )

var_label(lists_df[['zip_code']]) <- '5 digit zip code; sometimes missing' # 
  #lists_df %>% count() %>% print(n=100)
  # lists_df %>% count(univ_name,) %>% print(n=100)

    #lists_df %>% mutate(zip_code_len = str_length(zip_code)) %>% count(univ_name,zip_code_len) %>% print(n=100)
  
   #lists_df %>% group_by(univ_name) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(zip_code)), n_nonmiss = sum(is.na(zip_code)==0))
   
var_label(lists_df[['country']]) <- 'country of prospect' # usually non-missing for the three universities
  #lists_df %>% count() %>% print(n=100)
  # lists_df %>% count(univ_name,) %>% print(n=100)

   #lists_df %>% group_by(univ_name) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(country)),n_nonmiss = sum(is.na(country)==0))

   #lists_df %>% count(univ_name,country) %>% print(n=300)

#$ geomarket             <chr> "TX16", "TX03", "TX22", "TX17", "TX19", "TX19", "TX19", "TX23", "TX02", "TX01", "TX12", "TX06", "TX16", "TX06", "TX11", "TX03", "TX15", "TX22~
var_label(lists_df[['geomarket']]) <- 'College Board designated geo market of the prospect; usually non-mising' # interesting variable; 
  
  #lists_df %>% group_by(univ_name) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(geomarket)))
  #lists_df %>% count(univ_name,geomarket) %>% print(n=400)
  #lists_df %>% filter(univ_id == '145637') %>% count(geomarket) %>% print(n=500)

var_label(lists_df[['hs_code']]) <- 'High school code (college board code?)' # 
  #lists_df %>% group_by(univ_name) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(hs_code)))
  #lists_df %>% count() %>% print(n=100)
  # lists_df %>% count(univ_name,) %>% print(n=100)

var_label(lists_df[['source']]) <- 'Combination of date, testing vendor, and order number' # non-missing for Urbana only
  #lists_df %>% count() %>% print(n=100)
  #lists_df %>% count(univ_name,source) %>% print(n=100)
  #lists_df %>% filter(univ_id != '145637') %>% count(source)

var_label(lists_df[['order_no']]) <- 'Student list purchase order number' # 
  #lists_df %>% count() %>% print(n=100)
  #lists_df %>% count(univ_name,order_no) %>% print(n=200)

var_label(lists_df[['order_date']]) <- 'Student list purchase order date' # non-missing for Urbana only

  #lists_df %>% count(univ_name,order_date) %>% print(n=200)

var_label(lists_df[['univ_id']]) <- 'University IPEDS ID (unitid)' # 

var_label(lists_df[['run_no']]) <- 'Texarkana only' # 
  #lists_df %>% count(univ_name,run_no) %>% print(n=200)

var_label(lists_df[['run_no']]) <- 'Canadian province, mostly missing' # 
  #lists_df %>% count(univ_name,province) %>% print(n=200)

var_label(lists_df[['county_code']]) <- 'County code; missing for Urbana' # 

  #lists_df %>% group_by(univ_name) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(county_code)))

var_label(lists_df[['post_del']]) <- '??? non-missing for Texarkana and Stephen F. Austin, but dont know what it means' # 
  #lists_df %>% count(univ_name,post_del) %>% print(n=300)

var_label(lists_df[['post_corr']]) <- '??? non-missing for Texarkana and Stephen F. Austin, but dont know what it means' # 

  #lists_df %>% count(univ_name,post_corr) %>% print(n=300)
  #lists_df %>% count(univ_name,gender) %>% print(n=300) # missing for Urbana

var_label(lists_df[['is_hispanic_origin']]) <- 'Hispanic origin; Urbana only; character var; about 12%=No; about 13%=yes;about 75%=NA' #

var_label(lists_df[['race']]) <- 'Race; Urbana only; the following categories in isolation or in conjunction with others: American Indian or Alaska Native; Asian; Black or African American; Native Hawaiian or other Pacific Islander; White' # 

var_label(lists_df[['grad_year']]) <- 'High school graduation year' # 
  #lists_df %>% count(univ_name,grad_year) %>% print(n=300) # missing for Urbana

var_label(lists_df[['major_1']]) <- 'Intended major' # non-missing only for Texarkana
  #lists_df %>% count(univ_name,major_1) %>% print(n=500) # missing for Urbana

  #lists_df %>% count(univ_name,ap1) %>% print(n=500) # non-missing only for Texarkana
  #lists_df %>% count(univ_name,satsub1) %>% print(n=500) # non-missing only for Texarkana
  #lists_df %>% count(univ_name,name_source) %>% print(n=500) # non-missing only for Texarkana; don't know what this var means
  #lists_df %>% count(univ_name,update_date) %>% print(n=500) # non-missing only for Texarkana; don't know what this var means

var_label(lists_df[['homeschool']]) <- 'Prospect is home schooled; missing for Urbana' # non-missing only for Texarkana
  #lists_df %>% count(univ_name,homeschool) %>% print(n=500) # non-missing only for Texarkana; don't know what this var means

var_label(lists_df[['low_ses']]) <- 'always missing' # 
  #lists_df %>% count(univ_name,low_ses) %>% print(n=500) # 

var_label(lists_df[['hs_cluster']]) <- 'always missing' # 
  #lists_df %>% count(univ_name,hs_cluster) %>% print(n=500) # always missing 

var_label(lists_df[['en_cluster']]) <- 'always missing' # 
  #lists_df %>% count(univ_name,en_cluster) %>% print(n=500) # always missing 

var_label(lists_df[['nhrp']]) <- 'National Hispanic Recognition Program recipient' # mising for Urbana
  #lists_df %>% count(univ_name,nhrp) %>% print(n=500) #

var_label(lists_df[['first_gen']]) <- 'First generation college student status. 1 = no college; 2 = some college; 3 = not first generation; 4 = no response' # mising for Urbana

  #lists_df %>% count(univ_name,first_gen) %>% print(n=500) # Numeric; 1 = no college; 2 = some college; 3 = not first generation; 4 = no response

var_label(lists_df[['pltw']]) <- 'always missing' # 
  #lists_df %>% count(univ_name,pltw) %>% print(n=500) # pretty much always missing

var_label(lists_df[['interest_me']]) <- 'always missing' # 
#lists_df %>% count(univ_name,interest_me) %>% print(n=500) # 

var_label(lists_df[['pref_inst1']]) <- 'always missing' # 
#lists_df %>% count(univ_name,pref_inst1) %>% print(n=500) # 

var_label(lists_df[['source_file']]) <- 'name of raw data file for student list data' # 
  #lists_df %>% count(univ_name,source_file) %>% print(n=500) 

var_label(lists_df[['score_range']]) <- 'SAT score range; non-missing only for Stephen F. Austin' # 
  #lists_df %>% count(univ_name,score_range) %>% print(n=500) 

var_label(lists_df[['satsub1']]) <- 'Numeric code of SAT subject test taken by prospect, for subject test 1' # 
  #lists_df %>% count(univ_name,satsub1) %>% print(n=500) 
  # key linking code to subject test
    #SAT Subject Test Code	SAT Subject Test
    #39	U.S. History
    #40	World History
    #41	Literature
    #43	Chemistry
    #44	Physics
    #45	Latin
    #46	Modern Hebrew
    #47	French
    #48	German
    #49	Italian
    #51	Spanish
    #52	Mathematics Level 2
    #54	Chinese with Listening
    #55	French with Listening
    #56	German with Listening
    #57	Japanese with Listening
    #58	Spanish with Listening
    #59	Korean with Listening
    #61	Mathematics Level 1
    #62	Biology E
    #63	Biology M

var_label(lists_df[['ap1']]) <- 'Numeric code of AP test taken by prospect, for AP test 1 (dont have a table that tells me what these codes mean)' # 


###################### CREATE RACE/ETHNICITY VARIABLE THAT IS CONSISTENT ACROSS UNIVERSITIES

  # INVESTIGATIONS OF ETHNICITY VARIABLES FOR TEXARKANA AND STEPHEN F. AUSTIN DATA
    # count number of prospects that identify as "non_hispanic"
      # 234379 obs
      #lists_df %>% filter(univ_id %in% c('228431','224545')) %>%filter(non_hispanic == 'Y') %>% count()
    # count number of prospects that identify as "non_hispanic" and the other four ethnicity variables all equal NA
      # 234046 obs
      #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% filter(non_hispanic=='Y', is.na(cuban)==1,is.na(mexican)==1,is.na(puerto_rican)==1, is.na(other_hispanic)==1) %>% count() # 
    # count number of prospects that identify as "non_hispanic" AND also identify as at least one hispanic
      # 333 obs
      #234046 + 333
      #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% filter(non_hispanic=='Y' & (cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y')) %>%
        #count() 
        #select(univ_name,student_id,cuban,mexican,puerto_rican,other_hispanic,non_hispanic,ethnicity_no_response) %>% print(n=400)
    # count number of obs that have value of NA for all ethnicity variables [including ethnicity_no_response]
      # 12893 obs
      #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% filter(is.na(cuban)==1,is.na(mexican)==1,is.na(puerto_rican)==1, is.na(other_hispanic)==1, is.na(non_hispanic)==1) %>% count() # 8391 obs
    # count number of obs that have value of NA for all ethnicity variables [including ethnicity_no_response]
      # 8391 obs    
      #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% filter(is.na(cuban)==1,is.na(mexican)==1,is.na(puerto_rican)==1, is.na(other_hispanic)==1, is.na(non_hispanic)==1, is.na(ethnicity_no_response)==1) %>% count() # 8391 obs    
    # count number of obs where ethnicity_no_response=='Y'
      # 4502 obs
      #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% filter(ethnicity_no_response == 'Y') %>% count()
    # count number of obs that have ethnicity_no_response == 'Y' and have at least one 'Y' for one of the five ethnicity variables
      # 0 obs
      #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% filter(ethnicity_no_response=='Y' & (cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y' | non_hispanic=='Y')) %>% count()    
    
lists_df <- lists_df %>% mutate(
  # create common measure of hispanic origin
  is_hisp_common = case_when(
    # U. Illinois-Urbana
    univ_id == '145637' & is_hispanic_origin == 'Yes' ~ 1,
    univ_id == '145637' & (is_hispanic_origin %in% c('No')| is.na(is_hispanic_origin)==1) ~ 0,
    # Texarkana and Stephen F. Austin, rules:
    univ_id %in% c('228431','224545') & (cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y') ~ 1, # == 1 if at least one of the four categories (cuban, mexican, puerto_rican, other_hispanic) == 'Y'
    univ_id %in% c('228431','224545') & (non_hispanic=='Y' & is.na(cuban)==1 & is.na(mexican)==1 & is.na(puerto_rican)==1 & is.na(other_hispanic)==1) ~ 0   # == 0 if non_hispanic == 1 and all four categories == NA
    # Note: variable is_hisp_common is NA for 12893 obs for following reasons:
      # all ethnicity variables [including ethnicity_no_respone] == NA ; 8391 obs
      # ethnicity_no_response== 'Y'; 4502 obs
  ))

  # %>% filter(univ_id %in% c('228431','224545')) 

        
###################### CREATE INPUT VARIABLE RACE (FOR URBANA) THAT REMOVES DUPLICATE RACE CATEGORIES (E.G., "ASIAN, ASIAN" BECOMES "ASIAN")
    
  
  # create vectorized version of the unique() function, so that it works within elements
    vunique <- Vectorize(unique)
  
    #c('a','a','b','c','c') %>% unique()
    #c('a','a','b','c','c') %>% vunique() 
  
    #list(c('a','a','b','c','c'),c('d','e','f')) %>% str()
  
    #list(c('a','a','b','c','c'),c('d','e','f')) %>% unique()
    #list(c('a','a','b','c','c'),c('d','e','f'),c('q')) %>% vunique()
  
  # create object that is list of length = number of obs in student list; each element is a character vector w/ length = number of race groups in variable 'race'
    race_groups_list <- str_extract_all(string = lists_df$race, pattern = '(\\w+[\\w|\\s]+)')
    
  # create list object that removes duplicate race categories within an element
    race_groups_list_unique <- race_groups_list %>% vunique()  
  
  # add create character vector version of list object as variable within lists_df data frame
    
    lists_df$race2 <- race_groups_list_unique %>% str_c()  
    #lists_df %>% glimpse()
    
  # remove the following characters from variable race2: c(" , " , ")
  lists_df <- lists_df %>% mutate(
    race2 = str_replace_all(race2,'c\\(\\"',''),
    race2 = str_replace_all(race2,'\\"\\)',''),
    race2 = str_replace_all(race2,'\\"',''),
    race2 = tolower(race2),
    # create variable that is the number of different racial groups in variable race
    ct_race_groups_urbana=1 + str_count(race2,',')
  ) #%>% select(-race) %>% rename(race=race2)
  # checks
    #lists_df %>% count(race) %>% print(n=100)
    #lists_df %>% count(race2) %>% print(n=100)
    #lists_df %>% count(ct_race_groups) %>% print(n=100)
    #lists_df %>% filter(univ_id == '145637') %>% select(student_id,race,race2,ct_race_groups) %>% View()
  
  # create indicator for having two or more race groups, for urbana
  lists_df <- lists_df %>% mutate(
    multi_race_urbana = if_else(ct_race_groups_urbana >=2,1,0, missing = NULL)
  ) 
  # checks
    # lists_df %>% count(multi_race)
    #lists_df %>% count(univ_name,multi_race)
    #lists_df  %>% filter(univ_id == '145637') %>% count(race) %>% print(n=100)
    #lists_df %>% filter(univ_id == '145637') %>% select(student_id,race,race2,ct_race_groups,multi_race) %>% View()
  
###################### 0/1 CREATE RACE-SPECIFIC INDICATORS FOR URBANA
  
  lists_df <- lists_df %>% mutate(
    american_indian_urbana = if_else(str_detect(race2, 'indian')==1,1,0, missing = NULL),
    asian_urbana = if_else(str_detect(race2, 'asian')==1,1,0, missing = NULL),
    black_urbana = if_else(str_detect(race2, 'black')==1,1,0, missing = NULL),
    native_hawaiian_urbana = if_else(str_detect(race2, 'hawaiian')==1,1,0, missing = NULL),
    white_urbana = if_else(str_detect(race2, 'white')==1,1,0, missing = NULL),
    race_no_response_urbana = case_when(
      univ_id == '145637' & is.na(race2)==1 ~ 1,
      univ_id == '145637' & is.na(race2)==0 ~ 0,      
    )
  ) # %>% count(univ_name,race_no_response_urbana)
  

###################### 0/1 CREATE RACE-SPECIFIC INDICATORS, COMMON ACROSS ALL UNIVERSITIES
  

  lists_df <- lists_df %>% mutate(
    american_indian_common = case_when(
      univ_id == '145637'  ~ american_indian_urbana,
      univ_id %in% c('228431','224545') & american_indian =='Y' ~ 1,
      univ_id %in% c('228431','224545') & is.na(american_indian) & is.na(race_no_response) ~ 0,
    ),
    asian_common = case_when(
      univ_id == '145637'  ~ asian_urbana,
      univ_id %in% c('228431','224545') & asian =='Y' ~ 1,
      univ_id %in% c('228431','224545') & is.na(asian) & is.na(race_no_response) ~ 0,
    ),
    black_common = case_when(
      univ_id == '145637'  ~ black_urbana,
      univ_id %in% c('228431','224545') & black =='Y' ~ 1,
      univ_id %in% c('228431','224545') & is.na(black) & is.na(race_no_response) ~ 0,
    ),
    native_hawaiian_common = case_when(
      univ_id == '145637'  ~ native_hawaiian_urbana,
      univ_id %in% c('228431','224545') & native_hawaiian =='Y' ~ 1,
      univ_id %in% c('228431','224545') & is.na(native_hawaiian) & is.na(race_no_response) ~ 0,
    ),
    white_common = case_when(
      univ_id == '145637'  ~ white_urbana,
      univ_id %in% c('228431','224545') & white =='Y' ~ 1,
      univ_id %in% c('228431','224545') & is.na(white) & is.na(race_no_response) ~ 0,
    ),
    race_no_response_common = case_when(
      univ_id == '145637'  ~ race_no_response_urbana,
      univ_id %in% c('228431','224545') & race_no_response =='Y' ~ 1,
      univ_id %in% c('228431','224545') & is.na(race_no_response) ~ 0,
    ),
    other_common = case_when( # confused about this variable; not sure which boxes respondent could have checked to get this; 
      # no relevant input var for urbana; for Texarkana/SF Austin there are 152 total cases; not sure where they come from
        # note from xls_sat-esr-data-file-layout-crosswalk-fixed-width.xls:
          # "Note, "other" will be maintained until all students have responded to the new question."
      univ_id == '145637'  ~ 0,
      univ_id %in% c('228431','224545') & other =='Y' ~ 1,
      univ_id %in% c('228431','224545') & is.na(other) & is.na(race_no_response) ~ 0,
    ),
    # create measure that counts number of race groups
    ct_race_groups_common = rowSums(dplyr::across(c(american_indian_common,asian_common,black_common,native_hawaiian_common,white_common,other_common), na.rm = TRUE)),
      # case_when(univ_id %in% c('228431','224545') ~ rowSums(dplyr::across(c(american_indian_common,asian_common,black_common,native_hawaiian_common,white_common,other_common), na.rm = TRUE)))
      # note: for urbanna, this variable is exactly the same as ct_race_groups_urbana, so can just use this one
        # %>% filter(univ_id == '145637') %>% count(ct_race_groups,ct_race_groups_urbana)
    # checks
      # for texarkana and SF austin, 28220 obs have ct_race_groups ==0
        #lists_df %>% filter(univ_id %in% c('228431','224545'), ct_race_groups ==0) %>% count()
      # all of these obs have NA for the underlying race variables
        #lists_df %>% filter(univ_id %in% c('228431','224545'),ct_race_groups ==0) %>% select(univ_name,american_indian,asian,black,native_hawaiian,white,other,race_no_response) %>% View()
        # lists_df %>% filter(univ_id %in% c('228431','224545'), ct_race_groups ==0) %>% count(race_no_response)
      # majority of these observations identify as hispanic
        # so likely they answered the ethnicity/hispanic questions but did not answer the race questions
        #lists_df %>% filter(univ_id %in% c('228431','224545'),ct_race_groups ==0) %>% select(univ_name,cuban,mexican,puerto_rican,other_hispanic,non_hispanic,ethnicity_no_response,is_hisp_common) %>% View()
        #lists_df %>% filter(univ_id %in% c('228431','224545'),ct_race_groups ==0) %>% count(is_hisp_common)      
    
    # create measure of whether two or more races
      # definition: two or more races [ignore ethnicity for now]
    multi_race_common = if_else(ct_race_groups_common >=2,1,0, missing = NULL)
  )

  # checks of ct_race_groups and multi_race variables
    # ct_race_groups_common
      # urbana
        #lists_df %>% filter(univ_id == '145637') %>% count(ct_race_groups_common,ct_race_groups_urbana)
      # other univs
        #lists_df %>% filter(univ_id %in% c('228431','224545'), ct_race_groups_common ==0) %>% count()
        #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,ct_race_groups_common)
    # multi_race_common
      #lists_df %>% count(ct_race_groups_common,multi_race_common)
      # urbana
        #lists_df %>% filter(univ_id == '145637') %>% count(ct_race_groups_common,multi_race_common)
      # other univs
        #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(ct_race_groups_common,multi_race_common)

      
  # checks of race indicators
    #lists_df %>% count(univ_name,other_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,other_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(other,other_common)
    
    #lists_df %>% count(univ_name,race_no_response_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,race_no_response_common)
  
    
    #lists_df %>% count(univ_name,white_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,white_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(white,white_common)
    
    #lists_df %>% count(univ_name,native_hawaiian_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,native_hawaiian_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(native_hawaiian,native_hawaiian_common)
    
    
    #lists_df %>% count(univ_name,black_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,black_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(black,black_common)
    
    #lists_df %>% count(univ_name,asian_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,asian_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(asian,asian_common)
      
    #lists_df %>% count(univ_name,american_indian_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response,american_indian_common)
    #lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(american_indian_common)

  # measure of 'other'; this variable only exists for non-urbana universities
    #lists_df %>% filter(univ_id %in% c('228431','224545'), other=='Y') %>% count(univ_name)
    #lists_df %>% filter(univ_id %in% c('228431','224545'), other=='Y') %>% count(race_no_response)
    #lists_df %>% filter(univ_id %in% c('228431','224545'), other=='Y') %>% count(white)
    #lists_df %>% filter(univ_id %in% c('228431','224545'), other=='Y') %>% count(grad_year)
  
# CREATE COLLEGE BOARD DERIVED AGGREGATE RACE/ETHNICITY VARIABLE COLLEGE BOARD CREATES FROM SEPARATE VARS FOR ETHNICITY (HISPANIC ORIGIN) ABND RACE
    #Code Description
    #0    No Response
    #1    American Indian/Alaska Native
    #2    Asian
    #3    Black/African American
    #4    Hispanic/Latino
    #8    Native Hawaiian or Other Pacific Islander
    #9    White
    #10   Other
    #12   Two Or More Races, Non-Hispanic

lists_df <- lists_df %>% 
  mutate(
    race_cb = case_when(
      is.na(is_hisp_common)==1 | (is_hisp_common==0 & race_no_response_common==1) ~ 0, #0    No Response [ethnicity/Hispanic is NA; OR hispanic==0 AND race_no_response indicator indicates they chose not to respond]
      american_indian_common==1 & multi_race_common == 0 & is_hisp_common == 0 ~ 1, #1    American Indian/Alaska Native [american_indian_common ==1; AND multi_race_common == 0; AND is_hisp_common == 0 ]
      asian_common==1 & multi_race_common == 0 & is_hisp_common == 0 ~ 2, #2    Asian
      black_common==1 & multi_race_common == 0 & is_hisp_common == 0 ~ 3, #3    Black/African American
      is_hisp_common ==1 ~ 4, #4    Hispanic/Latino
      native_hawaiian_common==1 & multi_race_common == 0 & is_hisp_common == 0 ~ 8, #8    Native Hawaiian or Other Pacific Islander
      white_common==1 & multi_race_common == 0 & is_hisp_common == 0 ~ 9, #9    White
      other_common==1 & multi_race_common == 0 & is_hisp_common == 0 ~ 10, #10   Other
      multi_race_common == 1 & is_hisp_common == 0 ~ 12 #12   Two Or More Races, Non-Hispanic      
    ) # case_when
  ) %>% # mutate
  # create value labels for level of urbanization
  set_value_labels(
    race_cb = c(
      'no response' = 0,
      'American Indian/Alaska Native' = 1,
      'Asian' = 2,
      'Black/African American' = 3,
      'Hispanic/Latino' = 4,
      'Native Hawaiian/Pacific Islander' = 8,
      'white' = 9,
      'other' = 10,
      'two or more races, non-Hispanic' = 12
    )
  ) %>% # set_value_labels
  mutate(urbana = if_else(univ_id == '145637',1,0, missing = NULL))

  var_label(lists_df[['race_cb']]) <- 'College Board derived federal race/ethnic categories; NA if hispanic == 0 and all input race variables (including race_no_response) are NA'

# checks of variable race_cb  
  #lists_df %>% count(race_cb)
  #lists_df %>% count(urbana,race_cb)
  #lists_df %>% count(urbana,is_hisp_common,race_cb)
  #lists_df %>% count(urbana,is_hisp_common)

  # checks for Urbana
    #lists_df %>% filter(urbana==1) %>% count(is_hisp_common) # matches number w/ race_cb == hispanic
    #lists_df %>% filter(urbana==1) %>% count(is_hisp_common,race_no_response_common) # obs that have is_hisp_common ==0 and race_no_response == 1 match number w/ race_cb == no_response
  
  # checks for non-urbana
    #lists_df %>% filter(urbana==0) %>% count(race_cb)
    #lists_df %>% filter(urbana==0) %>% count(is_hisp_common) # matches number w/ race_cb == hispanic
    
    # obs that have is_hispanic==NA
    #lists_df %>% filter(urbana==0,is.na(is_hisp_common)) %>% count(race_cb) # all 12893 obs = "no response"; 12893+389 = 13282, which are total number of obs w/ "no_response" in non-urbana
    
    # checking obs that have hispanic ==0
    #lists_df %>% filter(urbana==0,is_hisp_common==0) %>% count(race_cb) # 389 obs = "no response"; 1574 obs = NA
    #lists_df %>% filter(urbana==0,is_hisp_common==0) %>% count(race_no_response_common,race_cb) # 389 obs w/ "no response" all have race_no_response_common ==1
    
    #lists_df %>% filter(urbana==0,is_hisp_common==0, race_no_response_common==0) %>% count(race_cb) # same values as count(race_cb) by itself, except for no_response, and hispanic
    
    # checking obs that have hispanic ==0, race_no_response_common ==0 and race_cb NA; 
    #lists_df %>% filter(urbana==0,is_hisp_common==0, race_no_response_common==0, is.na(race_cb)) %>% count(race_cb) # these include all 1574 NA obs for the variable
    
    #lists_df %>% filter(urbana==0,is_hisp_common==0, race_no_response_common==0, is.na(race_cb)) %>% count(ethnicity_no_response) # always NA
    #lists_df %>% filter(urbana==0,is_hisp_common==0, race_no_response_common==0, is.na(race_cb)) %>% count(race_no_response) # always NA
    
    #lists_df %>% filter(urbana==0,is_hisp_common==0, race_no_response_common==0, is.na(race_cb)) %>% select(american_indian,asian,black,native_hawaiian,white,other) %>% View()
    
    # SUMMARY OF INVESTIGATION OF OBS NA FOR race_cb
      # these are obs that had NA for: all input race vars; for ethnicity_no_response; and for race_no_response
  
# delete input/work variables that are no longer needed
  lists_df <- lists_df %>% select(-contains('urbana'),-is_hispanic_origin,-race,-race2,-cuban,-mexican,-puerto_rican,-other_hispanic,-non_hispanic,-american_indian,-asian,-black,-native_hawaiian,-white,-other,-race_no_response,-ethnicity_no_response) %>% glimpse()

    
lists_df %>% glimpse()



## -----------------------------------------------------------------------------
## RESEARCH QUESTION 1: RQ1, What are the characteristics of student list purchases
## -----------------------------------------------------------------------------

      
## -----------------------------------------------------------------------------
## RESEARCH QUESTION 2: WHAT ARE THE CHARACTERISTICS OF PROSPECTS PURCHASED BY STUDENT LISTS? HOW DO THESE CHARACTERISTICS DIFFER ACROSS UNIVERSITY TYPE, GEOGRAPHIC FOCUS, AND ACROSS FILTER CRITERIA
## -----------------------------------------------------------------------------
      
# CREATE DATA FRAME THAT MERGES ORDER SUMMARY DATA AND LIST DATA
  
  # INVESTIGATE DATA STRUCTURE 
  #order summary, data structure
    #orders_df  %>% group_by(univ_id, order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
    #orders_df  %>% group_by(order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # also uniquely identifies obs
    
    #orders_df %>% filter(univ_id == '145637') %>% group_by(order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # also uniquely identifies obs
    
    # urbana
    #orders_df %>% filter(univ_id == '145637') %>% select(order_num) %>% distinct() # returns vector with all distinct values of order_num; one element per each distinct value
    #orders_df %>% filter(univ_id == '145637') %>% select(order_num) %>% distinct() %>% count() # counts the number of distinct values; 80
    # texarkana
    #orders_df %>% filter(univ_id == '224545') %>% select(order_num) %>% distinct() %>% count() # 91 distinct values of order_num
    # Stephen F. Austin
    #orders_df %>% filter(univ_id == '228431') %>% select(order_num) %>% distinct() %>% count() # 16 distinct values of order_num
      
  # student list, data structure
    # Urbana
      #lists_df %>% filter(univ_id == '145637') %>% group_by(student_id, order_no) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
      #lists_df %>% filter(univ_id == '145637') %>% group_by(student_id) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs
      #lists_df %>% filter(univ_id == '145637') %>% group_by(order_no) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) %>% print(n=100) # 92 orders; 90 lines (obs; some have n=2)
    
      #lists_df %>% filter(univ_id == '145637') %>% select(order_no) %>% distinct() %>% count() # 92 distinct values of order_no
    # texarkana
      #lists_df %>% filter(univ_id == '224545') %>% group_by(order_no) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) %>% print(n=100) # 87 different orders
      #lists_df %>% filter(univ_id == '224545') %>% select(order_no) %>% distinct() %>% count() # 91 distinct values of order_no
    
    # stephen F. Austin
      #lists_df %>% filter(univ_id == '228431') %>% group_by(order_no) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) %>% print(n=100) # 15 different orders
      #lists_df %>% filter(univ_id == '228431') %>% select(order_no) %>% distinct() %>% count() # 15 distinct values of order_no

  # MERGE; BY UNIV_ID AND ORDER_NO
lists_orders_df <- orders_df %>% 
  # bulk rename columns that start with "order"
  rename_with(.fn = function(x){str_replace(pattern = "order",replacement = "ord", x)}, .cols = starts_with('order')) %>% 
  # bulk rename columns that don't start with "univ" or "order"
  rename_with(.fn = function(x){paste0("ord_", x)}, .cols = !(starts_with('univ')|starts_with('ord'))) %>% 
  # select specific vars
  #starts_with('univ'),
  select(univ_id,ord_num,ord_title,ord_cost,ord_po_num,ord_date_start,ord_hs_grad_class,ord_zip_code,ord_zip_code_file,ord_segment,ord_state_name,
         ord_state_name,ord_cbsa_name,ord_geomarket,ord_intl_region,ord_sat_score_min,ord_sat_score_max,ord_psat_score_min,ord_psat_score_max,ord_gpa_low,ord_gpa_high,ord_rank_low,ord_rank_high,
         ord_gender,ord_race_ethnicity)  %>% mutate(one=1) %>% 
  # merge in student list data
  right_join(y = (lists_df %>% select(starts_with('univ'),student_id,city,state,zip,zip_code,country,geomarket,hs_code,order_no,county_code,post_del,post_corr,gender,
                    is_hisp_common,american_indian_common,asian_common,black_common,native_hawaiian_common,white_common,race_no_response_common,other_common,ct_race_groups_common,multi_race_common,race_cb,
                    grad_year,major_1,major_2,major_3,name_source,homeschool,hs_cluster,en_cluster,nhrp,first_gen,score_range) %>% # deleted these vars order_date,update_date, note that "order_date" is specific to Urbana, taken from the "source" column in raw data
  rename(id = student_id) %>% rename_with(.fn = function(x){paste0("stu_", x)}, .cols = !(order_no|starts_with('univ')))), by = c('univ_id', 'ord_num' = 'order_no')) %>% # note: same result of you merge just by order number
  # create indicator of whether order summary data missing
  mutate(na_ord_summ = if_else(is.na(one),1,0)) %>% select(-one) %>%
  # other variables used later
  mutate(
    # indicator variable for whether student is in 50 US states + DC [note: variable stu_country sometimes missing/unreliable]
    stu_country = tolower(stu_country),
    #stu_in_us = if_else(stu_country == 'united states' & str_length(stu_state)==2 & !(stu_state %in% c('AA','AE','AP','MH','PR')),1,0, missing = NULL),
    #stu_in_us = if_else(stu_country == 'united states' & stu_state %in% state_codes,1,0, missing = NULL)
    stu_in_us = if_else(stu_state %in% state_codes,1,0, missing = NULL)
  )



var_label(lists_orders_df[['stu_in_us']]) <- '0/1 measure of whether prospect has country == united staes and state is one of 50 states or DC'

  #lists_orders_df %>% count(stu_in_us)

  #lists_orders_df %>% filter(is.na(stu_state)) %>% count(stu_in_us)
  #lists_orders_df %>% count(stu_state) %>% print(n=200)

  #lists_orders_df %>% filter(is.na(stu_in_us)) %>% count(stu_country)
  #lists_orders_df %>% filter(is.na(stu_in_us)) %>% count(stu_state)


  
  # order filters not included for now
    #$ ord_college_type       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    #$ ord_edu_aspirations    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    #$ ord_rotc_plans         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~
    #$ ord_major              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ~

# Investigating observations that do not merge
  # 5 observations (order numbers) from orders_df that did not merge to lists_df\
    orders_df %>% anti_join(lists_df, by = c('univ_id', 'order_num' = 'order_no')) 
    # 3 orders from Urbana; 1 order from Texarkana; 1 order from Stephen F. Austin
    orders_df %>% anti_join(lists_df, by = c('univ_id', 'order_num' = 'order_no')) %>% select(order_num, univ_name)
  # observations from lists_df (prospects) where the order_no does not match to a corresponding order_num in orders_df
    # 113,000 obs (prospects) that have an order_no that does not match to orders_df
      # 107,541 obs from Urbana
      # 5,520 obs from Texarkana
    lists_df %>% anti_join(orders_df, by = c('order_no' = 'order_num')) %>% count(univ_name)
    # 16 order numbers from lists_df do not have a match in orders_df; 15 from Urbana-Illinois; 1 from Texarkana
    lists_df %>% anti_join(orders_df, by = c('order_no' = 'order_num')) %>% select(order_no,univ_name) %>% distinct()
 
lists_orders_df %>% glimpse()
    
###################### MERGE IN SECONDARY DATA

###### LOAD CENSUS ZIP-CODE LEVEL DATA
  
  # load ACS data w/ zipcode-level data on population and median household income; one obs per zip-code 
  acs_race_zipcode <- read_csv(file.path(data_dir, 'acs_race_zipcode.csv')) %>% arrange(zipcode)
  
    #acs_race_zipcode %>% glimpse()
    #acs_race_zipcode %>% group_by(zipcode) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
  
    # create numeric zip code
    acs_race_zipcode <- acs_race_zipcode %>% mutate(zip_code = substr(msa_name, 7, 11)) %>%
    mutate(
      # turn negative values of median household income to NA
      median_household_income = if_else(median_household_income >0,median_household_income,NA_real_, missing = NULL),
      # turn character pct race variables into numeric  
      pop_white_15_19_pct = as.numeric(pop_white_15_19_pct),
      pop_black_15_19_pct = as.numeric(pop_black_15_19_pct),
      pop_asian_15_19_pct = as.numeric(pop_asian_15_19_pct),
      pop_amerindian_15_19_pct = as.numeric(pop_amerindian_15_19_pct),
      pop_nativehawaii_15_19_pct = as.numeric(pop_nativehawaii_15_19_pct),
      pop_otherrace_15_19_pct = as.numeric(pop_otherrace_15_19_pct),
      pop_tworaces_15_19_pct = as.numeric(pop_tworaces_15_19_pct),
      pop_hispanic_15_19_pct = as.numeric(pop_hispanic_15_19_pct)      
    )
    #acs_race_zipcode %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
    #acs_race_zipcode %>% count(median_household_income) %>% print(n=100)
    #acs_race_zipcode %>% filter(is.na(median_household_income)) %>% count()
  
  # load different ACS data w/ zip-code level data; 
  zip_to_state <- read_csv(file.path(data_dir, 'zip_to_state.csv')) %>% arrange(zip_code)
  
    #zip_to_state %>% glimpse()
    #zip_to_state %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
    zip_to_state <- zip_to_state %>% select(state_code, zip_code)
  
  # add variable state_code to acs_race_zipcode
  acs_race_zipcodev2 <- left_join(acs_race_zipcode, zip_to_state, by = "zip_code")
    #acs_race_zipcodev2 %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
  
  # check on weird values of median household income
  acs_race_zipcodev2 %>% count(median_household_income) %>% print(n=100)

#### Merge zip-code level data to data frame w/ student_list/order summary data frame
  
  # which zip-code variable to use from lists_orders_df dataframe
    #lists_orders_df %>% select(contains('stu_zip')) %>% var_label()
    #lists_orders_df %>% select(contains('stu_zip')) %>% glimpse() # character

  # investigate var = stu_zip_code [5 digit]
    #lists_orders_df %>% mutate(stu_zip_code_len = str_length(stu_zip_code)) %>% count(univ_name,stu_zip_code_len) %>% print(n=100)
    # very small number of missing when student country restricted to united states
    #lists_orders_df %>% filter(stu_country == 'united states') %>% mutate(stu_zip_code_len = str_length(stu_zip_code)) %>% count(univ_name,stu_zip_code_len) %>% print(n=100)
    
  # investigate var = stu_zip [5+4 dogots]
    #lists_orders_df %>% mutate(stu_zip_len = str_length(stu_zip)) %>% count(univ_name,stu_zip_len) %>% print(n=100) # yuk
    # when restricted to united states
    #lists_orders_df %>% filter(stu_country == 'united states') %>% mutate(stu_zip_len = str_length(stu_zip)) %>% count(univ_name,stu_zip_len) %>% print(n=100) # obs that are missing in 5-digit zip code are also missing in 5+4 digit zip code

    #lists_orders_df %>% count(stu_country) %>% print(n=400)
    
lists_orders_zip_df <- lists_orders_df %>% 
  left_join(y=acs_race_zipcodev2 %>% select(-zipcode,-msa_name) %>% rename_with(.fn = function(x){paste0("zip_", x)}, .cols = !(starts_with('zip'))) %>% mutate(one=1),by = c('stu_zip_code' = 'zip_code')) %>%
  mutate(na_zip_acs = if_else(is.na(one),1,0)) %>% select(-one)

  # INVESTIGATE MERGE
    #lists_orders_zip_df %>% count(na_zip_acs)
    #lists_orders_zip_df %>% filter(stu_country == 'united states') %>% count(na_zip_acs)
  
    lists_orders_zip_anti <- lists_orders_df %>% filter(stu_country == 'united states') %>% anti_join(acs_race_zipcodev2, by = c('stu_zip_code' = 'zip_code'))
    
    #lists_orders_zip_anti %>% count(stu_state) %>% print(n=100)
    #lists_orders_zip_anti %>% count(stu_zip_code) %>% print(n=500)
    #lists_orders_zip_anti %>% count(ord_hs_grad_class) %>% print(n=500) # missing mostly from 2019, 2020, 2021 HS classes
    #lists_orders_zip_df %>% count(ord_hs_grad_class) %>% print(n=500)
    #lists_orders_zip_anti %>% count(ord_date_start) %>% print(n=500) #
    
    #lists_orders_zip_anti %>% mutate(ord_year = year(ord_date_start)) %>% count(ord_year) %>% print(n=500) # 
    #lists_orders_zip_df %>% mutate(ord_year = year(ord_date_start)) %>% count(ord_year) %>% print(n=500) # 
    
    #lists_orders_zip_anti %>% mutate(ord_year = year(ord_date_start)) %>% filter(is.na(ord_year)==0) %>% count(ord_year) %>% mutate(freq = (n / sum(n)) * 100)
    #lists_orders_zip_df %>% mutate(ord_year = year(ord_date_start)) %>% filter(is.na(ord_year)==0) %>% count(ord_year) %>% mutate(freq = (n / sum(n)) * 100)
  
    rm(lists_orders_zip_anti)
    
    # summary of investigation
      # a little over 6K prospects w/ stu_country == 'united states' that don't merge
      # some are in US territories rather than states
        # note that ACS zip-code file only contains the 50 states, DC, and Puerto Rico
      # vast majority seem to be prospects in US states with zip-codes that appear legit based on spot checks from Google maps
      # about 5,700 obs that don't merge are from TX
      # compared to all prospects, the ones that don't merge are a little more likely to be from orders made in 2019
      # DECISION: need to improve the ACS zip-code file; seems to be missing legit zip-codes
        # is the problem that these are new zip codes? 
        # is the problem that these are zip codes that have been around for a while but were excluded from ACS for some reason?
  
   # check out missingness of median household income
    
    #lists_orders_zip_df %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(zip_median_household_income)),n_nonmiss = sum(is.na(zip_median_household_income)==0))
    
    #lists_orders_zip_df %>% filter(stu_in_us==1) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(zip_median_household_income)),n_nonmiss = sum(is.na(zip_median_household_income)==0),mean_med_inc = mean(zip_median_household_income, na.rm = TRUE))
    
    # compare to acs zip-code level data (all zip codes)
    #acs_race_zipcodev2 %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(median_household_income)),n_nonmiss = sum(is.na(median_household_income)==0),mean_med_inc = mean(median_household_income, na.rm = TRUE))    

#### Merge in secondary data on schools

# investigate high school data [from NCES common core for public schools and PSS for private schools]

  #hs_data %>% glimpse() # 
  #hs_data %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # ncessch uniquely identifies obs
  #hs_data %>% count(school_type) # public and private
  
# investigate ceeb code on ceeb_nces crosswalk, merge w/ nces hs data

  #ceeb_nces %>% glimpse()
    # script that created this crosswalk: https://github.com/ksalazar3/public_requests/blob/master/ceeb_nces_crosswalk.R
      # based on three sources:
        # 1. a crosswalk available online: https://ire.uncg.edu/research/NCES_CEEB_Table/ 
        # 2. crosswalk from CU-Boulder: https://github.com/cu-boulder/ceeb_nces_crosswalk
        # 3. NICHE high school rankings
  
  # duplicate obs per ceeb code and duplicate obs per NCES code; because crosswalk created by aggregating across several sources
    #ceeb_nces %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs
    #ceeb_nces %>% group_by(ceeb) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs
  
  # ceeb_nces %>% mutate(ceeb_len = str_length(ceeb)) %>% count(ceeb_len)
   #ceeb_nces %>% mutate(ceeb_len = str_length(ceeb)) %>% arrange(desc(ceeb_len),ceeb) %>% View()
  

# merge NCES high school data to ceeb code crosswalk
  
  ceeb_hs <- ceeb_nces %>% inner_join(hs_data, by = 'ncessch')  # get rid of rows w/o NCES data too
    # lots of obs that don't merge; will have to improve quality of ceeb_nces crosswalk
  #glimpse(ceeb_hs)
  #ceeb_hs %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs
  
  #ceeb_hs %>% group_by(ceeb) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs; 
  
  # investigate obs where there are two observations for one ceeb code (each ceeb code associated with a different NCES code)
    #ceeb_hs %>% group_by(ceeb) %>% mutate(n_per_ceeb = n()) %>% ungroup() %>% filter(n_per_ceeb==2) %>% arrange(ceeb,ncessch) %>% View()

    # when merging this to student list data a ceeb code that is associated with two different nces codes will have two observations in ceeb_hs and will cause students to be counted twice once you merge to student list data
  
    # when there are two nces codes associated with one ceeb, keep the obs w/ higher number of students [FOR NOW]
    ceeb_hs <- ceeb_hs %>% arrange(ceeb,desc(total_students)) %>% group_by(ceeb) %>% filter(row_number()==1) %>% ungroup()
    
    #ceeb_hs %>% group_by(ceeb) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # now, uniquely identifies obs
    

# investigate/ ceeb code on student list data
    
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% count(stu_hs_code_len)
  
  # stu_hs_code length==1; 648 obs
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==1) %>% select(univ_name,ord_num,stu_hs_code,stu_state,stu_city,stu_zip_code) %>% View()
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==1) %>% count(stu_state) %>% print(n=100)
  
  # stu_hs_code length==3; 3 obs
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==3) %>% count(stu_hs_code) %>% print(n=100)
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==3) %>% select(univ_name,ord_num,stu_hs_code,stu_state,stu_city,stu_zip_code) %>% View()
  
  # stu_hs_code length==4; 143 obs
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==4) %>% count(stu_hs_code) %>% print(n=100)
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==4) %>% select(univ_name,ord_num,stu_hs_code,stu_state,stu_city,stu_zip_code) %>% View()
  
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% count(stu_hs_code_len)
  
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==1) %>% count(stu_hs_code) # values are either 3 or 4
  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==4) %>% count(stu_hs_code) %>% print(n=100)

  # stu_hs_code length ==7
    # always Urbana; 
    # stu_hs_code starts with "E00"...; don't think these would merge to CEEB if we deleted the "E"
    #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% filter(stu_hs_code_len==7) %>% select(univ_name,ord_num,stu_hs_code,stu_state,stu_city,stu_zip_code) %>% View()
    
  # summary of investigation
    # lowest value of ceeb code on ceeb_nces crosswalk is '010000'
    # this means that adding a leading zero will only work for stu_hs_code that have length of 5
    # for stu_hs_code w/ length==7, all of these obs starts with "E00" (e.g., 'E003798'); 
      # if we remove the leading "E", the highest value would be 00XXXX which is lower than lowest value of ceeb on ceeb_nces crosswalk ('010000')
      # so these obs also will not merge w/ ceeb code
  # decision for creation of ceeb code:
    # for obs where stu_hs_code length==5, add a leading '0'
    # for obs where stu_hs_code length==6, leave unchanged
    # for all other obs, ceeb should be NA
  
# create ceeb code on student list data
    

  #lists_orders_zip_df %>% filter(stu_in_us ==1) %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% count(stu_hs_code_len)  
  #lists_orders_zip_df %>% mutate(stu_hs_code_len = str_length(stu_hs_code)) %>% count(stu_hs_code_len)

  lists_orders_zip_df <- lists_orders_zip_df %>%  mutate(
    stu_hs_code_len = str_length(stu_hs_code),
    stu_ceeb = case_when(
      stu_hs_code_len == 5 ~ str_pad(stu_hs_code, width = 6, pad = '0', side = 'left'),
      stu_hs_code_len == 6 ~ stu_hs_code
    ),
    stu_ceeb_len = str_length(stu_ceeb)
  )
  # checks on variable stu_ceeb
  #lists_orders_zip_df %>% filter(!(stu_hs_code_len %in% c(5,6))) %>% count(stu_hs_code_len)
  #lists_orders_zip_df %>% filter(!(stu_hs_code_len %in% c(5,6))) %>% count(stu_ceeb) # always NA
  
  #lists_orders_zip_df %>% mutate(stu_ceeb_len = str_length(stu_ceeb)) %>% count(stu_ceeb_len)
  #lists_orders_zip_df %>% mutate(stu_ceeb_len = str_length(stu_ceeb)) %>% count(stu_hs_code_len,stu_ceeb_len)
  
  #lists_orders_zip_df %>% mutate(stu_ceeb_len = str_length(stu_ceeb)) %>% filter(stu_in_us==1) %>% count(stu_ceeb_len) 
  #lists_orders_zip_df %>% mutate(stu_ceeb_len = str_length(stu_ceeb)) %>% filter(stu_in_us==0) %>% count(stu_ceeb_len) 
  #lists_orders_zip_df %>% mutate(stu_ceeb_len = str_length(stu_ceeb)) %>% filter(stu_country != 'united states') %>% count(stu_ceeb_len) 
  #lists_orders_zip_df %>% mutate(stu_ceeb_len = str_length(stu_ceeb)) %>% filter(stu_in_us==0) %>% select(stu_hs_code,stu_hs_code_len,stu_ceeb,stu_ceeb_len,stu_country,stu_state,stu_city,stu_zip_code,univ_name,ord_num) %>% View()

# merge student list data (left) to high school data (right) by ceeb code
  
  lists_orders_zip_hs_df <- lists_orders_zip_df %>% 
    left_join(y= (ceeb_hs %>% rename_with(.fn = function(x){paste0("hs_", x)}, .cols = !(starts_with('ceeb'))) %>% mutate(one=1)), by = c('stu_ceeb' = 'ceeb')) %>%
    mutate(na_hs = if_else(is.na(one),1,0)) %>% select(-one)
  
  # INVESTIGATE MERGE
    # SUMMARY OF INVESTIGATION
      # prospects w/ missing hs_level data somewhat more likely to be hispanic/black, somewhat less likely to be white
      # don't see other huge differences
  
    #lists_orders_zip_hs_df %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 85.4% of students merge
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 89.4% of students merge
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,!is.na(stu_ceeb)) %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 89.8% of students merge
    
  # anti-merge
    lists_orders_zip_hs_anti <- lists_orders_zip_df %>% anti_join(ceeb_hs, by = c('stu_ceeb' = 'ceeb')) #112,867 obs
    lists_orders_zip_hs_anti <- lists_orders_zip_hs_anti %>% filter(!is.na(stu_ceeb)) # 102,583 obs that have a 6 digit ceeb code
    
    #lists_orders_zip_hs_anti %>% glimpse() 
    #lists_orders_zip_hs_anti %>% count(stu_in_us) %>% mutate(freq = (n / sum(n)) * 100) # 72.9% in US
    
    #lists_orders_zip_hs_anti %>% count(univ_name) %>% mutate(freq = (n / sum(n)) * 100) #
    #lists_orders_zip_hs_df %>% count(univ_name) %>% mutate(freq = (n / sum(n)) * 100) #
    
    # investigate obs that don't merge where student is in us
    #lists_orders_zip_hs_anti %>% filter(stu_in_us==1) %>% count(stu_state) %>% mutate(freq = (n / sum(n)) * 100) %>% print(n=100) # compared to all prospects, disproportion number of missing hs data are from TX
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count(stu_state) %>% mutate(freq = (n / sum(n)) * 100) %>% print(n=100)
    
    #lists_orders_zip_hs_anti %>% filter(stu_in_us==1) %>% mutate(ord_year = year(ord_date_start)) %>% filter(is.na(ord_year)==0) %>% count(ord_year) %>% mutate(freq = (n / sum(n)) * 100) %>% print(n=100) # no huge pattern
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% mutate(ord_year = year(ord_date_start)) %>% filter(is.na(ord_year)==0) %>% count(ord_year) %>% mutate(freq = (n / sum(n)) * 100) %>% print(n=100) # 
    
    # student race/ethnicity
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count(stu_race_cb) %>% mutate(freq = (n / sum(n)) * 100) # 
    #lists_orders_zip_hs_anti %>% filter(stu_in_us==1) %>% count(stu_race_cb) %>% mutate(freq = (n / sum(n)) * 100) # missing hs data slightly more likely to be hispanic, and less likely to be white
    
    # zip_median_household_income [missing actually has higher avg median income]
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(zip_median_household_income)),n_nonmiss = sum(is.na(zip_median_household_income)==0),mean_med_inc = mean(zip_median_household_income, na.rm = TRUE))
    #lists_orders_zip_hs_anti %>% filter(stu_in_us==1) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(zip_median_household_income)),n_nonmiss = sum(is.na(zip_median_household_income)==0),mean_med_inc = mean(zip_median_household_income, na.rm = TRUE))
    
    # zip code-level race
      # all prospects
      #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% summarise(
      #  n_obs = sum(n()),
      #  n_nonmiss_pct_white = sum(is.na(zip_pop_white_15_19_pct)==0),
      #  mean_pct_white = mean(zip_pop_white_15_19_pct, na.rm = TRUE),
      #  mean_pct_black = mean(zip_pop_black_15_19_pct, na.rm = TRUE),
      #  mean_pct_asian = mean(zip_pop_asian_15_19_pct, na.rm = TRUE),
      #  mean_pct_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
      #  mean_pct_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
      #  mean_pct_otherrace = mean(zip_pop_otherrace_15_19_pct, na.rm = TRUE),
      #  mean_pct_tworaces = mean(zip_pop_tworaces_15_19_pct, na.rm = TRUE),
      #  mean_pct_hispanic = mean(zip_pop_hispanic_15_19_pct, na.rm = TRUE),
      #)
        #48.1  +         10.2    +       8.73      +         0.420   +             0.0950    +           4.53      +        4.46   +           23.5
      
      # all prospects with missing hs data
      #lists_orders_zip_hs_anti %>% filter(stu_in_us==1) %>% summarise(
      #  n_obs = sum(n()),
      #  n_nonmiss_pct_white = sum(is.na(zip_pop_white_15_19_pct)==0),
      #  mean_pct_white = mean(zip_pop_white_15_19_pct, na.rm = TRUE),
      #  mean_pct_black = mean(zip_pop_black_15_19_pct, na.rm = TRUE),
      #  mean_pct_asian = mean(zip_pop_asian_15_19_pct, na.rm = TRUE),
      #  mean_pct_amerindian = mean(zip_pop_amerindian_15_19_pct, na.rm = TRUE),
      #  mean_pct_nativehawaii = mean(zip_pop_nativehawaii_15_19_pct, na.rm = TRUE),
      #  mean_pct_otherrace = mean(zip_pop_otherrace_15_19_pct, na.rm = TRUE),
      #  mean_pct_tworaces = mean(zip_pop_tworaces_15_19_pct, na.rm = TRUE),
      #  mean_pct_hispanic = mean(zip_pop_hispanic_15_19_pct, na.rm = TRUE),
      #)
      # prospects w/ missing hs data live in zip-codes with lower pct white, and somewhat higher pct black, higher pct asian, higher pct hispanic
    
    rm(lists_orders_zip_hs_anti)

    
## -----------------------------------------------------------------------------
## EQUALITY OF RACE/ETHNICITY VARIABLES ACROSS DATA SOURCES (prospect level; prospect zip-code; prospect high school)
## -----------------------------------------------------------------------------
    
# student level
  # 0/1 variables [generally, not exclusive]
    
    #stu_is_hisp_common, stu_american_indian_common,stu_asian_common, stu_black_common, stu_native_hawaiian_common, stu_white_common, stu_race_no_response_common, stu_other_common, stu_ct_race_groups_common, stu_multi_race_common

  # categorical
    #stu_race_cb
    lists_orders_zip_hs_df %>% count(stu_race_cb) %>% mutate(freq = (n / sum(n)) * 100)
    lists_orders_zip_hs_df %>% filter(!(stu_race_cb %in% (0) | is.na(stu_race_cb))) %>% count(stu_race_cb) %>% mutate(freq = (n / sum(n)) * 100)
    
# zip code level [ACS]
  # zip_pop_white_15_19_pct
  # zip_pop_black_15_19_pct
  # zip_pop_asian_15_19_pct
  # zip_pop_amerindian_15_19_pct
  # zip_pop_nativehawaii_15_19_pct
  # zip_pop_otherrace_15_19_pct
  # zip_pop_tworaces_15_19_pct
  # zip_pop_hispanic_15_19_pct
    
# high-school level    
  # hs_pct_white
  # hs_pct_black
  # hs_pct_hispanic
  # hs_pct_asian
  # hs_pct_amerindian
  # hs_pct_other
    
# questions:
    # does the ACS/zipcode variable for "tworaces" include students who identify as hispanic? the student-level measure of two or more races excludes students who identify as two or more races

    
## -----------------------------------------------------------------------------
## RQ2A: WHAT ARE THE CHARACTERISTICS OF PROSPECTS PURCHASED BY STUDENT LISTS? HOW DO THESE CHARACTERISTICS DIFFER ACROSS UNIVERSITY TYPE, GEOGRAPHIC FOCUS, AND ACROSS FILTER CRITERIA
## -----------------------------------------------------------------------------

lists_orders_zip_hs_df %>% glimpse()
# potential tables/figures to create
  # from EDA google doc: https://docs.google.com/document/d/17XGsoYYmqODmdUik-5q5-0GuBc0LNb6KOrs_lhAYUpU/edit# 
    
  #Row1: characteristics of all prospects purchased from all lists across all universities
  #Columns: number of students by race, average income in zip-code, average share of race by zip-code, etc
  #Row2: in-state prospects
  #Row3: out-of-state prospects
  #Other rows
  #Regional university; research university
  #Table of student characteristics by filter?
#Show results via map too?
    
# prospect characteristics of potential interest
    # number of students
    # number/percent of students by race
    # number of students by public/private high school
    # median income of zip-code where student lives
    # racial composition of high school student attend
    # 
    
# university characteristics. how do prospect characteristics differ by following university characteristics
    # university (e.g., univ_id)
      # in-state vs. out-of-state purchases (but this is technically a filter)
    # university carnegie type
    
# student list purchase filters. how do prospect characteristics differ by the following filters (individually and/or in conjunction) chosen for the student list purchase
    # in-state vs. out-of-state
    # international
    # Segment analysis [0/1]
    # Score range
    # 

lists_orders_zip_hs_df %>% glimpse()
######### EDA
    
# in-state vs. out-of-steate [focusing on state of the prospect rather than states of the order]    
  
  #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count(univ_state)
  #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count(stu_state) %>% print(n=100)
  #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% mutate(stu_out_st = if_else(stu_state != univ_state,1,0, missing = NULL)) %>% count(stu_out_st)

  # income
  lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% mutate(stu_out_st = if_else(stu_state != univ_state,1,0, missing = NULL)) %>%
    group_by(stu_out_st) %>% summarize(
      n_obs = sum(n()),
      n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
      mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
    )

  lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% mutate(stu_out_st = if_else(stu_state != univ_state,1,0, missing = NULL)) %>%
    group_by(univ_name,stu_out_st) %>% summarize(
      n_obs = sum(n()),
      n_nonmiss_inc = sum(is.na(zip_median_household_income)==0),
      mean_med_inc = mean(zip_median_household_income, na.rm = TRUE),
    )

  # prospect race [stark results]
  lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% mutate(stu_out_st = if_else(stu_state != univ_state,1,0, missing = NULL)) %>%
    group_by(stu_out_st) %>% count(stu_race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% mutate(stu_out_st = if_else(stu_state != univ_state,1,0, missing = NULL)) %>%
    group_by(univ_name,stu_out_st) %>% count(stu_race_cb) %>% mutate(freq = (n / sum(n)) * 100) %>% print(n=100)

  
START HERE:
  NEXT FIND THOSE FILTER CRITERUA (VARIABLES STARTING W/ ORD_) THAT ARE ASSOCIATED WITH SYSTEMIC RACE/CLASS INEQUALITY
## -----------------------------------------------------------------------------
## INVESTIGATING HISPANIC ORIGIN [ethnicity] AND RACE VARIABLES;
## -----------------------------------------------------------------------------

  # CENSUS DEFINITIONS OF ETHNICITY AND RACE
    # https://en.wikipedia.org/wiki/Race_and_ethnicity_in_the_United_States_census
    # ethnicity
      # definition: whether person is hispanic/latino or not
      # categories (Census):  "Hispanic or Latino" and "Not Hispanic or Latino"
    # race 
      # definition:
        # racial categories represent a social-political construct for the race or races that respondents consider themselves to be and, "generally reflect a social definition of race recognized in this country."[3] OMB defines the concept of race as outlined for the U.S. census as not "scientific or anthropological" and takes into account "social and cultural characteristics as well as ancestry", using "appropriate scientific methodologies" that are not "primarily biological or genetic in reference."[4] The race categories include both racial and national-origin groups
      # categories: white, asian, etc.

  # ETHNICITY AND RACE QUESTIONS ASKED ON COLLEGE BOARD QUESTIONNAIRE

    # link to college board template:
      # google drive folder for file layouts: https://drive.google.com/drive/u/0/folders/1UyuWxR6wUSkZILYYCpB5gDcirsXewud6
        # look at file for sat-registration-booklet-students... for different years
    # QUESTIONS
    # Please answer both questions about Hispanic origin and about race. For the following questions about your identity, Hispanic origins are not races.
      # a. Are you of Hispanic, Latino, or Spanish origin? (You may check all that apply.)
        # a. No, not of Hispanic, Latino, or Spanish origin
        # b. Yes, Cuban
        # c. Yes, Mexican
        # d. Yes, Puerto Rican
        # e. Yes, another Hispanic, Latino, or Spanish origin
      # b. What is your race? (You may check all that apply.)
        # a. American Indian or Alaska Native
        # b. Asian (including Indian subcontinent and Philippines origin)
        # c. Black or African American (including African and Afro-Caribbean origin)
        # d. Native Hawaiian or other Pacific Islander
        # e. White (including Middle Eastern origin)

  # DERIVED AGGREGATE RACE/ETHNICITY VARIABLE COLLEGE BOARD CREATES FROM SEPARATE VARS FOR ETHNICITY (HISPANIC ORIGIN) ABND RACE
    #Code Description
    #0    No Response
    #1    American Indian/Alaska Native
    #2    Asian
    #3    Black/African American
    #4    Hispanic/Latino
    #8    Native Hawaiian or Other Pacific Islander
    #9    White
    #10   Other
    #12   Two Or More Races, Non-Hispanic

  # SAMPLE FILE LAYOUT OF A COLLEGE BOARD STUDENT LIST
    # https://drive.google.com/file/d/1Qvc_QRi9izEF1W78Lh4nNi5NsXjCZqUE/view


################ HISPANIC ORIGIN [ethnicity] AND RACE VARIABLES; UNIVERSITY ILLINOIS-URBANA 


#var_label(lists_df[['race']]) <- 'Race; Urbana only; the following categories in isolation or in conjunction with others: American Indian or Alaska Native; Asian; Black or African American; Native Hawaiian or other Pacific Islander; White' # 
  # Non-missing only for Urbana; about 25K NA out of 415K
  #lists_df %>% group_by(univ_name) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(race)))
  #lists_df %>% filter(univ_id == '145637') %>% count(race) %>% print(n=500)

#var_label(lists_df[['is_hispanic_origin']]) <- 'Hispanic origin; Urbana only; character var; about 12%=No; about 13%=yes;about 75%=NA' #
# breakdown: about 12%=No; about 13%=yes;about 75%=NA
  # non-missing only for Urbana
  #lists_df %>% group_by(univ_name) %>% summarise(n_obs = sum(n()),n_miss = sum(is.na(is_hispanic_origin)))
  lists_df %>% filter(univ_id == '145637') %>% count(is_hispanic_origin)
  
# cross tab of is_haspinic_origin and race for Urbana
  
  lists_df %>% filter(univ_id == '145637') %>% count(is_hispanic_origin,race) %>% print(n=120)
  
  # is_hispanic_origin == 'Yes'; 58053 obs
  lists_df %>% filter(univ_id == '145637',is_hispanic_origin == 'Yes') %>% count(race) %>% print(n=120) 
    # white: 33361; 33361/58053*100 = 57.5%
    # Asian: 1837; 1837/58053*100 = 3.2%
    # Black: 1745; 1745/58053*100 = 3.0%
  
  # is_hispanic_origin == 'No'; 48533 obs
  lists_df %>% filter(univ_id == '145637',is_hispanic_origin == 'No') %>% count(race) %>% print(n=120) # 48553 obs with is_hispanic_origin == 'No'
    # white: 23947/48533*100 = 49.3%
    # Asian: 18144/48533*100 = 37.4%
    # Black: 3826/48533*100 = 7.9%
  
  # is_hispanic_origin == NA; # 308826 obs (assumption is that these prospects are not of hispanic origin?)
    # white: 173771/308826*100 = 56.3%
    # Asian: 91458/308826*100 = 29.6%
    # Black: 13711/308826*100 = 4.4%  
  lists_df %>% filter(univ_id == '145637',is.na(is_hispanic_origin)) %>% count(race) %>% print(n=120) # 308826 obs with is_hispanic_origin == NA
  
################ HISPANIC ORIGIN AND ETHNICITY VARIABLES; TEXARKANA AND STEPHEN F. AUSTIN
  
  # hispanic origin [ethnicity]
  lists_df %>% count(univ_name,cuban) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,mexican) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,puerto_rican) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,other_hispanic) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,non_hispanic) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,ethnicity_no_response) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  
  
  # race categries
  lists_df %>% count(univ_name,american_indian) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,asian) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,black) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,native_hawaiian) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,white) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,other) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA
  lists_df %>% count(univ_name,race_no_response) %>% print(n=300) # missing for Urbana; for Texarkana/Austin either 'Y' or NA

  ######### investigating hispanic origin [ethnicity] and race for Texarkana and Stephen F. Austin combined
  
    # Stephen F Austin, univid = '228431'; 185349 obs
    # Texarkana, univid = '224545'; 171601 obs
    # Austin + Texarkana = 356950 obs
  
  # is_hispanic = no response [1.26 pct of obs]
    # count no ethnicity; 4502 obs
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(ethnicity_no_response)
    # pct no ethnicity
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(no_eth = if_else(ethnicity_no_response=='Y',1,0, missing= 0)) %>% summarize(pct_no_eth = mean(no_eth)*100)
  
  # is_hispanic = non_hispanic [65.7 pct of obs]
    # count; 234379
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(non_hispanic)
    # pct no ethnicity
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(non_hisp = if_else(non_hispanic=='Y',1,0, missing= 0)) %>% summarize(pct_non_hisp = mean(non_hisp)*100)
  
  # is_hispanic = yes [calculated as saying yes to at least one of the categories]
    
    # count:  110011 
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      is_hispanic_origin = if_else((cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y'),1,0, missing = 0)
    ) %>% count(is_hispanic_origin)
   
  # Note: sum of non_hispanic (234379 obs), ethnicity_no_response (4502 obs), and at least one hispanic group (110011) = 348892, which is less than 356950 total obs in these two universities
      #234379+ 4502 + 110011
      #234379+ 4502 + 110011 + 8391 = 357283 total obs
    # is this due to missing for international? what else?
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      is_hispanic_origin = if_else((cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y'),1,0, missing = 0) 
    ) %>% filter(is.na(ethnicity_no_response)==1, is.na(non_hispanic)==1, is_hispanic_origin==0) %>% count() # 8391 obs
    
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      is_hispanic_origin = if_else((cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y'),1,0, missing = 0) 
    ) %>% filter(is.na(ethnicity_no_response)==1, is.na(non_hispanic)==1, is_hispanic_origin==0) %>% View()

    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      is_hispanic_origin = if_else((cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y'),1,0, missing = 0) 
    ) %>% filter(is.na(ethnicity_no_response)==1, is.na(non_hispanic)==1, is_hispanic_origin==0) %>% count(cuban) # all obs NA for all the ethnicity variables
    
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      is_hispanic_origin = if_else((cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y'),1,0, missing = 0) 
    ) %>% filter(is.na(ethnicity_no_response)==1, is.na(non_hispanic)==1, is_hispanic_origin==0) %>% count(univ_name) # pretty balanced

    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      is_hispanic_origin = if_else((cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y'),1,0, missing = 0) 
    ) %>% filter(is.na(ethnicity_no_response)==1, is.na(non_hispanic)==1, is_hispanic_origin==0) %>% count(country) # all US...
        

  # count number of obs that have value of NA for all ethnicity variables
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% 
      filter(is.na(cuban)==1,is.na(cuban)==1,is.na(mexican)==1,is.na(cuban)==1,is.na(puerto_rican)==1, is.na(other_hispanic)==1, is.na(non_hispanic)==1, is.na(ethnicity_no_response)==1) %>%
      count() # 8391 obs
    
  # RACE
    
    # 234379+ 4502 + 110011 + 8391 = 357283 total obs
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(american_indian) 
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(asian) 
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(black)
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(native_hawaiian) 
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(white) 
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(other) 
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% count(race_no_response)
  
    # count of race by group
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      american_indian = if_else(american_indian == 'Y',1,0, missing = 0),
      asian = if_else(asian == 'Y',1,0, missing = 0),
      black = if_else(black == 'Y',1,0, missing = 0),
      native_hawaiian = if_else(native_hawaiian == 'Y',1,0, missing = 0),
      white = if_else(white == 'Y',1,0, missing = 0),
      other = if_else(other == 'Y',1,0, missing = 0),
      race_no_response = if_else(race_no_response == 'Y',1,0, missing = 0),      
    ) %>% summarize(
      american_indian = sum(american_indian),
      asian = sum(asian),
      black = sum(black),
      native_hawaiian = sum(native_hawaiian),
      white = sum(white),
      other = sum(other),
      race_no_response = sum(race_no_response)
    )
    
    # pct who identify w/ race by group
    lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
      american_indian = if_else(american_indian == 'Y',1,0, missing = 0),
      asian = if_else(asian == 'Y',1,0, missing = 0),
      black = if_else(black == 'Y',1,0, missing = 0),
      native_hawaiian = if_else(native_hawaiian == 'Y',1,0, missing = 0),
      white = if_else(white == 'Y',1,0, missing = 0),
      other = if_else(other == 'Y',1,0, missing = 0),
      race_no_response = if_else(race_no_response == 'Y',1,0, missing = 0),      
    ) %>% summarize(
      american_indian = mean(american_indian),
      asian = mean(asian),
      black = mean(black),
      native_hawaiian = mean(native_hawaiian),
      white = mean(white),
      other = mean(other),
      race_no_response = mean(race_no_response)
    )    
    
    ##### pct who identify w/ racial group by hispanic origin
    
      # at least one hispanic origin category
      lists_df %>% filter(univ_id %in% c('228431','224545')) %>% mutate(
        is_hispanic_origin = if_else((cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y'),1,0, missing = 0) 
      ) %>% filter(is_hispanic_origin==1) %>% mutate(
      american_indian = if_else(american_indian == 'Y',1,0, missing = 0),
      asian = if_else(asian == 'Y',1,0, missing = 0),
      black = if_else(black == 'Y',1,0, missing = 0),
      native_hawaiian = if_else(native_hawaiian == 'Y',1,0, missing = 0),
      white = if_else(white == 'Y',1,0, missing = 0),
      other = if_else(other == 'Y',1,0, missing = 0),
      race_no_response = if_else(race_no_response == 'Y',1,0, missing = 0),      
    ) %>% summarize(
      american_indian = mean(american_indian),
      asian = mean(asian),
      black = mean(black),
      native_hawaiian = mean(native_hawaiian),
      white = mean(white),
      other = mean(other),
      race_no_response = mean(race_no_response)
    ) 
    
    # non_hispanic origin category == 'Y'
      lists_df %>% filter(univ_id %in% c('228431','224545'), non_hispanic=='Y') %>% mutate(
      american_indian = if_else(american_indian == 'Y',1,0, missing = 0),
      asian = if_else(asian == 'Y',1,0, missing = 0),
      black = if_else(black == 'Y',1,0, missing = 0),
      native_hawaiian = if_else(native_hawaiian == 'Y',1,0, missing = 0),
      white = if_else(white == 'Y',1,0, missing = 0),
      other = if_else(other == 'Y',1,0, missing = 0),
      race_no_response = if_else(race_no_response == 'Y',1,0, missing = 0),      
    ) %>% summarize(
      american_indian = mean(american_indian),
      asian = mean(asian),
      black = mean(black),
      native_hawaiian = mean(native_hawaiian),
      white = mean(white),
      other = mean(other),
      race_no_response = mean(race_no_response)
    )       




















  




lists_df %>% select(univ_id,student_id,city,state,zip,zip_code,country,geomarket,race,is_hispanic_origin,hs_code,order_no,order_date,update_date,county_code,post_del,post_corr,
                    gender,cuban,mexican,puerto_rican,other_hispanic,non_hispanic,american_indian,asian,black,native_hawaiian,white,other,race_no_response,ethnicity_no_response,
                    grad_year,major_1,major_2,major_3,name_source,homeschool,hs_cluster,en_cluster,nhrp,first_gen,score_range) %>%
  rename(id = student_id) %>% rename_with(.fn = function(x){paste0("stu_", x)}, .cols = !(order_no|univ_id)) %>% glimpse()





orders_df %>% select(contains('date')) %>% glimpse()
lists_df %>% select(contains('date')) %>% glimpse()


lists_df %>% count(univ_name,name_source) %>% print(n=500)
lists_df %>% count(univ_name,update_date) %>% print(n=500)





lists_df %>% glimpse()



orders_df %>% rename_with(.fn = function(x){paste0("ord_", x)}, .cols = !(starts_with('univ')|starts_with('order'))) %>% glimpse()

mtcars %>% rename_with(.cols = hp:wt, function(x){paste0("cars.", x)}) # v.1.0.4.

orders_df %>% select(starts_with('univ'),order_num) %>% 
  left_join(lists_df %>% select(univ_id,order_no,student_id), by = c('univ_id', 'order_num' = 'order_no'))


c("a" = "b", "c" = "d")

      lists_df <- univ_data %>% select(univ_id, univ_name, state_code, zip_code, sector, c15basic) %>% rename(univ_state = state_code, univ_zip = zip_code, univ_sector = sector, univ_c15basic = c15basic) %>%
  right_join(lists_df, by = 'univ_id') %>% select(-univ_sector)





## -----------------------------------------------------------------------------
## RESEARCH QUESTION 3: WHAT ARE THE CHARACTERISTICS OF PROSPECTS/SCHOOLS/COMMUNITIES INCLUDED VERSUS EXCLUDED FROM STUDENT LIST PURCHASES?
## -----------------------------------------------------------------------------

#load(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/145637_orders.RData'))
# Contains: IL_orders, OOS_orders, OOS_eng_orders, OOS_noneng_orders, intl_orders,
#           lists_df_pivot, lists_df_sat, lists_df_act, df_sat_ca_20, df_sat_ca_19, hs_tract_ca


# read in data
lists_df <- read_csv(file.path(data_dir, '145637_lists.csv'), col_types = cols(.default = 'c'))
lists_df %>% glimpse() # note: the order number is embedded in the variable "source"
  #lists_df %>% count(Source) %>% print(n=100)

# Checks
str_detect(lists_df$Source, '^(?:\\w+ \\d+, \\d{4} [SACT]{3} Search \\d+;?\\s*)+$') %>% table() # WHAT IS THIS CHECKING?
str_count(lists_df$Source, 'SAT|ACT') %>% sum()  # 465231 matches number of rows in lists_df_pivot

# 
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

lists_df_pivot %>% glimpse()

# create data frame for ACT orders
  #lists_df_act <- lists_df_pivot %>% filter(test_type == 'act')
  # Missing order summary for most of lists_df_act (only have 3/22/19 & 6/27/19 order summary, which doesn't match any of the student list dates)
  #View(lists_df_act %>% select(order_num, order_date) %>% distinct())

# SAT orders
lists_df_sat <- lists_df_pivot %>% filter(test_type == 'sat')
lists_df_sat %>% glimpse()

lists_df_sat %>% count(order_num) %>% print(n=100)
  # NOTE: there appears to be like 92 different order numbers; this is more than the 80 orders on orders_df

  # Missing order summary for 107713 of 415689 rows in lists_df_sat (15 distinct orders)
  anti_join(lists_df_sat, orders_df, by = 'order_num') %>% nrow()
  View(anti_join(lists_df_sat, orders_df, by = 'order_num') %>% select(order_num, order_date) %>% distinct())

# 3 order summaries w/ no lists entries, but these look like draft orders that weren't actually placed (i.e., 'Edit name')
View(anti_join(orders_df, lists_df_sat, by = 'order_num'))

# Explore remaining matched rows (rows may be duplicates if they belong to multiple orders)
merged_df_sat <- inner_join(lists_df_sat, orders_df, by = 'order_num')


## -----------------------------------------------------------------------------
## RQ1: WHAT ARE THE CHARACTERISTICS OF STUDENT LIST PURCHASES
## -----------------------------------------------------------------------------

######### structure of orders data frames

names(IL_orders)
names(intl_orders)
OOS_eng_orders
OOS_noneng_orders
OOS_orders


############### IN-STATE ORDERS


IL_orders


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------


