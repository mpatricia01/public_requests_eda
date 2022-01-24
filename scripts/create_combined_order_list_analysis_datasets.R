################################################################################
##
## [ PROJ ] < student list project, EDA >
## [ FILE ] < create_combined_order_list_analysis_datasets.R >
## [ AUTH ] < Ozan Jaquette / ozanj >
## [ INIT ] < 8/31/2021 >
## [ DESC ] < clean and create common variables for datasets that combine purchases across universities. three types of analysies datasets created:>
              # order data (only data from order summaries)
              # list data (only data from deidentified student list purchases)
              # list-order data (merge order data to prosptect-level list data by order number)
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

#rm(list = ls())
options(max.print=100)

#options(tibble.width = Inf, width = 10000, scipen = 999) # does this work for scripts or just rmd?
options(scipen = 999)

#?memory.limit
memory.size()
memory.limit()
memory.limit(size = 40000) # set max memory allocation to 40000; I think this is 40 gigabytes; not sure if this is actually possible
memory.limit()

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


## -----------------------------------------------------------------------------
## LOAD ORDER DATA AND LIST DATA
## -----------------------------------------------------------------------------

# Create vector of state codes for 50 states + DC + territories; used in creation of variables for country and others
  state_codes <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY','AS','FM','GU','MH','MP','PR','PW','VI','UM')  
    #AS – American Samoa
    #FM – Federated States of Micronesia
    #GU – Guam
    #MH – Marshall Islands
    #MP – Northern Mariana Islands
    #PR – Puerto Rico
    #PW – Palau
    #VI – U.S. Virgin Islands
    #UM – U.S. Minor Outlying Island

load(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/combined_data.RData'))



# variables names in orders_df and in lists_df
#order_names <- names(orders_df) %>% as.tibble() %>% rename(var_name=value) %>% filter(!var_name %in% c('univ_name','univ_state','univ_zip','univ_c15basic'))
  #order_names %>% print(n=50)

# VARIABLES THAT ARE FORMATTED DIFFERENTLY ACROSS UNIVERSITIES [AS OF 8/31/2021] (must create common version before you can include in cross-university analyses)

  # state_name
  # intl_region
  # race_ethnicity
  # ?? should create variables that equates SAT and PSAT min/max??
    # e.g., sat_psat_score_min: equals value of sat_score_min if they filtered on sat score and equals value of psat_score_min if they filtered on psat
    
## -----------------------------------------------------------------------------
## INVESTIGATE/LABEL ORDER DATA
## -----------------------------------------------------------------------------

orders_df <- univ_data %>% select(univ_id, univ_name, state_code, zip_code, sector, c15basic) %>% rename(univ_state = state_code, univ_zip = zip_code, univ_sector = sector, univ_c15basic = c15basic) %>%
  right_join(orders_df, by = 'univ_id') %>% select(-univ_sector) %>%
  # drop order from U Illinois-Urbana that seems like it was not executed (order name is OOS ENG Female PSAT Catch-Up; 1,377 students available; but "name license status" was "saved" rather than "fulfilled" and "maximum volume = 0)
  filter(order_num != '374945') %>% arrange(univ_id,order_num)

  orders_df %>% group_by(order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # unique
  
  orders_df %>% count(univ_id)

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
var_label(orders_df[['geomarket']]) <- 'College Board geomarket(s) specified in filter for student list purchase' # var always NA for urbana
var_label(orders_df[['county']]) <- 'Counties specified in filter for student list purchase'

var_label(orders_df[['citizenship']]) <- 'Citizenship categories (e.g., U.S. Citizen|Alien, Refugee, or Permanent Resident) specified in filter for student list purchase'


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

var_label(orders_df[['ap_scores']]) <- 'AP test(s) and score ranges specified in filter for student list purchase'


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

# check that vars have variable labels
orders_df %>% var_label()


## -----------------------------------------------------------------------------
## LIST DATA TO INCORPORATE/PROCESS 10/18/2021
## -----------------------------------------------------------------------------


#lists_df_145637 %>% glimpse() # University of Illinois at Urbana-Champaign (145637)

#lists_df_110644 %>% glimpse() # UC-davis (110644)
#lists_df_145600 %>% glimpse() # UI-Chicago (145600)
#lists_df_104151 %>% glimpse() # ASU (104151)
#lists_df_228723 %>% glimpse() # Texas A&M-College Station (228723)
#lists_df_228529 %>% glimpse() # Tarleton State University (228529)



## -----------------------------------------------------------------------------
## INVESTIGATE LIST DATA
## -----------------------------------------------------------------------------

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


# FIX STATE, COUNTRY, HS GRAD YEAR
  # merge by zip code to add state code variable for institutions that did not put state code on list data: UC-davis (110644); Texas A&M-College Station (228723) 
  lists_df <- lists_df %>% left_join(y=zip_to_state_v2, by=c('zip_code'='zip_code')) %>% 
    mutate(
      state = if_else(state %in% state_codes==0,state_code,state),
      country = tolower(country),
      country = if_else(state %in% state_codes & is.na(country),"united states",country) # replace country w/ united states if state in US state or territory
    ) %>% select(-state_code) %>%
    # create grad_year variable for texas A&M college station
    mutate(
      entry_season = tolower(str_extract(string = entry_term, pattern = '\\w+')),
      entry_year = as.numeric(str_extract(string = entry_term, pattern = '\\d+')),
      grad_year = case_when(
        univ_id == '228723' & entry_season %in% c('fall','summer') ~ entry_year, # if entry_term for college is fall or summer, assume graduated high school in same calendar year
        univ_id == '228723' & entry_season %in% c('spring') ~ entry_year-1, # if entry_term for college is spring, assume graduated high school in previous calendar year
        univ_id != '228723' ~ as.numeric(grad_year)
      ),
      entry_seaon = NULL,
      entry_year = NULL
    )


  #lists_df %>% filter(univ_id %in% c('228723','110644')) %>% count(univ_id,state) %>% print(n=200)
  #lists_df %>% filter(univ_id %in% c('228723','110644'),is.na(state)) %>% View()
  #lists_df %>% filter(univ_id %in% c('228723','110644'),is.na(state)) %>% count(zip_code) %>% print(n=400)
  
# merge in IPEDS vars
lists_df <- univ_data %>% select(univ_id, univ_name, state_code, zip_code, sector, c15basic) %>% rename(univ_state = state_code, univ_zip = zip_code, univ_sector = sector, univ_c15basic = c15basic) %>%
  right_join(lists_df, by = 'univ_id') %>% select(-univ_sector) %>%
  mutate(gpa = as.numeric(gpa),first_gen = as.numeric(first_gen))  %>% 
  set_value_labels(
    # add value labels to variable GPA
    gpa = c(
      # labels for each value found from the file: xls_sat-esr-data-file-layout-crosswalk-fixed-width.xls
      # NOTE: UC-SD has like 4K observations where value of gpa == 0; above codebook does not have a value label for this value
        # could these be students who have GPA of greater than 100% (which is quite common in CA)?
      'A+ (97-100)' = 1,
      'A (93-96)' = 2,
      'A- (90-92)' = 3,
      'B+ (87-89)' = 4,
      'B (83-86)' = 5,
      'B- (80-82)' = 6,
      'C+ (77-79)' = 7,
      'C (73-76)' = 8,
      'C- (70-72)' = 9,
      'D+ (67-69)' = 20,
      'D (65-66)' = 11,
      'E or F (below 65)' = 12
    ),
    first_gen = c(
      'no college' = 1,
      'some college' = 2,
      'not first gen' = 3,
      'no response' = 4
    )
  ) %>%
  mutate(
    # create race var for Minnesota State University-Moorhead (174358)
    race_moorhead = if_else(univ_id=='174358',tolower(race),NA_character_,missing = NULL),
    # original race var should be missing for Moorhead
    race = if_else(univ_id=='174358',NA_character_,race,missing = NULL),
    # fix state variable for Minnesota State University-Moorhead (174358)    
    state_moorhead = case_when(
      univ_id == '174358' & str_length(state)==2 ~ state, # case when value of variable state has a string length of 2
      univ_id == '174358' & state == 'California' ~ 'CA',
      univ_id == '174358' & state == 'Minnesota' ~ 'MN',
      univ_id == '174358' & state == 'North Dakota' ~ 'ND',
      univ_id == '174358' & state == 'South Dakota' ~ 'SD',
      univ_id == '174358' & state == 'Virginia' ~ 'VA',
      univ_id == '174358' & state == 'Wisconsin' ~ 'WI',
    ),
    state = if_else(univ_id == '174358',state_moorhead,state, missing = NULL),
  ) %>%
  select(-state_moorhead)


#### LABEL ALL VARIABLES


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

var_label(lists_df[['gpa']]) <- 'high school cumulative GPA, categorical' # 

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
    univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & (cuban == 'Y' | mexican == 'Y' | puerto_rican == 'Y' | other_hispanic == 'Y') ~ 1, # == 1 if at least one of the four categories (cuban, mexican, puerto_rican, other_hispanic) == 'Y'
    univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & (non_hispanic=='Y' & is.na(cuban)==1 & is.na(mexican)==1 & is.na(puerto_rican)==1 & is.na(other_hispanic)==1) ~ 0,   # == 0 if non_hispanic == 1 and all four categories == NA
    # Note: variable is_hisp_common is NA for 12893 obs for following reasons:
      # all ethnicity variables [including ethnicity_no_respone] == NA ; 8391 obs
      # ethnicity_no_response== 'Y'; 4502 obs
      # u minnesota moorhead
    univ_id == '174358' & race_moorhead == 'hispanic or latino' ~ 1,
    univ_id == '174358' & race_moorhead != 'hispanic or latino' & !is.na(race_moorhead) ~ 0
    # note: 10/19/2021: probably need to add code for texas A&M college station
  ))

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
  
  rm(race_groups_list,race_groups_list_unique)
###################### 0/1 CREATE RACE-SPECIFIC INDICATORS, COMMON ACROSS ALL UNIVERSITIES


  lists_df <- lists_df %>% mutate(
    american_indian_common = case_when(
      univ_id == '145637'  ~ american_indian_urbana,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & american_indian =='Y' ~ 1,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & is.na(american_indian) & is.na(race_no_response) ~ 0,
      # u minnesota moorhead
      univ_id == '174358' & race_moorhead == 'american indian or alaska native' ~ 1,
      univ_id == '174358' & race_moorhead != 'american indian or alaska native' & !is.na(race_moorhead) ~ 0
    ),
    asian_common = case_when(
      univ_id == '145637'  ~ asian_urbana,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & asian =='Y' ~ 1,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & is.na(asian) & is.na(race_no_response) ~ 0,
      # u minnesota moorhead
      univ_id == '174358' & race_moorhead == 'asian' ~ 1,
      univ_id == '174358' & race_moorhead != 'asian' & !is.na(race_moorhead) ~ 0      
    ),
    black_common = case_when(
      univ_id == '145637'  ~ black_urbana,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & black =='Y' ~ 1,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & is.na(black) & is.na(race_no_response) ~ 0,
      # u minnesota moorhead
      univ_id == '174358' & race_moorhead == 'black or african american' ~ 1,
      univ_id == '174358' & race_moorhead != 'black or african american' & !is.na(race_moorhead) ~ 0      
    ),
    native_hawaiian_common = case_when(
      univ_id == '145637'  ~ native_hawaiian_urbana,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & native_hawaiian =='Y' ~ 1,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & is.na(native_hawaiian) & is.na(race_no_response) ~ 0,
      # u minnesota moorhead
      univ_id == '174358' & race_moorhead == 'native hawaiian/other pacific islander' ~ 1,
      univ_id == '174358' & race_moorhead != 'native hawaiian/other pacific islander' & !is.na(race_moorhead) ~ 0      
    ),
    white_common = case_when(
      univ_id == '145637'  ~ white_urbana,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & white =='Y' ~ 1,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & is.na(white) & is.na(race_no_response) ~ 0,
      # u minnesota moorhead
      univ_id == '174358' & race_moorhead == 'white' ~ 1,
      univ_id == '174358' & race_moorhead != 'white' & !is.na(race_moorhead) ~ 0      
    ),
    race_no_response_common = case_when(
      univ_id == '145637'  ~ race_no_response_urbana,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & race_no_response =='Y' ~ 1,
      univ_id %in% c('228431','224545','110680','174075','110644','145600','104151','228529') & is.na(race_no_response) ~ 0,
    ),
    other_common = case_when( # confused about this variable; not sure which boxes respondent could have checked to get this; 
      # no relevant input var for urbana; for Texarkana/SF Austin there are 152 total cases; not sure where they come from
        # note from xls_sat-esr-data-file-layout-crosswalk-fixed-width.xls:
          # "Note, "other" will be maintained until all students have responded to the new question."
          # note [10/19/2021]: UC-davis (110644) data does not have "other" variable
      univ_id %in% c('145637','110644')  ~ 0,
      univ_id %in% c('228431','224545','110680','174075','145600','104151','228529') & other =='Y' ~ 1,
      univ_id %in% c('228431','224545','110680','174075','145600','104151','228529') & is.na(other) & is.na(race_no_response) ~ 0,
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
      univ_id %in% c('174358','228723')==0 & (is.na(is_hisp_common)==1 | (is_hisp_common==0 & race_no_response_common==1)) ~ 0, #0    No Response [ethnicity/Hispanic is NA; OR hispanic==0 AND race_no_response indicator indicates they chose not to respond]
      univ_id %in% c('174358','228723')==0 & (american_indian_common==1 & multi_race_common == 0 & is_hisp_common == 0) ~ 1, #1    American Indian/Alaska Native [american_indian_common ==1; AND multi_race_common == 0; AND is_hisp_common == 0 ]
      univ_id %in% c('174358','228723')==0 & (asian_common==1 & multi_race_common == 0 & is_hisp_common == 0) ~ 2, #2    Asian
      univ_id %in% c('174358','228723')==0 & (black_common==1 & multi_race_common == 0 & is_hisp_common == 0) ~ 3, #3    Black/African American
      univ_id %in% c('174358','228723')==0 & (is_hisp_common ==1) ~ 4, #4    Hispanic/Latino
      univ_id %in% c('174358','228723')==0 & (native_hawaiian_common==1 & multi_race_common == 0 & is_hisp_common == 0) ~ 8, #8    Native Hawaiian or Other Pacific Islander
      univ_id %in% c('174358','228723')==0 & (white_common==1 & multi_race_common == 0 & is_hisp_common == 0) ~ 9, #9    White
      univ_id %in% c('174358','228723')==0 & (other_common==1 & multi_race_common == 0 & is_hisp_common == 0) ~ 10, #10   Other
      univ_id %in% c('174358','228723')==0 & (multi_race_common == 1 & is_hisp_common == 0) ~ 12 #12   Two Or More Races, Non-Hispanic      
    ),
    # for U Minnesota Moorhead
    race_cb = case_when(
      univ_id != '174358' ~ race_cb,
      univ_id == '174358' & race_moorhead == 'american indian or alaska native' ~ 1, # 
      univ_id == '174358' & race_moorhead == 'asian' ~ 2, # 
      univ_id == '174358' & race_moorhead == 'black or african american' ~ 3, # 
      univ_id == '174358' & race_moorhead == 'hispanic or latino' ~ 4, # 
      univ_id == '174358' & race_moorhead == 'native hawaiian/other pacific islander' ~ 8, # 
      univ_id == '174358' & race_moorhead == 'white' ~ 9, # 
    ),
    race_cb = case_when(
      univ_id != '228723' ~ race_cb,
      univ_id == '228723' & race == 'AO - Asian' ~ 2, # AO - Asian                                     39115
      univ_id == '228723' & race == 'BA - Black or Multi w/Black' ~ 3, # BA - Black or Multi w/Black                     5078
      univ_id == '228723' & race == 'HA - Hispanic' ~ 4, # HA - Hispanic                                  41762
      univ_id == '228723' & race == 'IN - International' ~ 10, # IN - International; code as "other"
      univ_id == '228723' & race == 'IO - American Indian or Alaskan Native' ~ 1, # IO - American Indian or Alaskan Native           136
      univ_id == '228723' & race == 'MX - Multi w/o Black' ~ 12, # MX - Multi w/o Black                            4250
      univ_id == '228723' & race == 'PO - Native Hawaiian or Other Pacific Islander' ~ 8, # PO - Native Hawaiian or Other Pacific Islander    73
      univ_id == '228723' & race == 'UK - Not Reported' ~ 0, # UK - Not Reported                               3782
      univ_id == '228723' & race == 'WO - White' ~ 9, # WO - White                                     49045    
    ),
  ) %>%
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

  #lists_df %>% count(univ_name,race_cb) %>% print(n=100)

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
  #lists_df <- lists_df %>% select(-contains('urbana'),-is_hispanic_origin,-race,-race2,-cuban,-mexican,-puerto_rican,-other_hispanic,-non_hispanic,-american_indian,-asian,-black,-native_hawaiian,-white,-other,-race_no_response,-ethnicity_no_response,-race_moorhead) %>% glimpse()
  lists_df <- lists_df %>% select(-contains('urbana'),-is_hispanic_origin,-race,-race2,-race_moorhead,-ct_race_groups_common)
    # -cuban,-mexican,-puerto_rican,-other_hispanic,-non_hispanic,-american_indian,-asian,-black,-native_hawaiian,-white,-other,-race_no_response,-ethnicity_no_response
  lists_df %>% glimpse()
  
# delete other variables no longer needed
  lists_df <- lists_df %>% select(-entry_season,-entry_term,
    -major_1_text,-major_2_text,-major_3_text,-interest_me,-score_range,-family_income,
    -hs_name,-order_date,-source_file)
  
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
                                      cuban,mexican,puerto_rican,other_hispanic,non_hispanic,american_indian,asian,black,native_hawaiian,white,other,race_no_response,ethnicity_no_response,
                    is_hisp_common,american_indian_common,asian_common,black_common,native_hawaiian_common,white_common,race_no_response_common,other_common,multi_race_common,race_cb,
                    grad_year,major_1,major_2,major_3,name_source,homeschool,hs_cluster,en_cluster,nhrp,first_gen) %>% # deleted these vars order_date,update_date, note that "order_date" is specific to Urbana, taken from the "source" column in raw data
  rename(id = student_id) %>% rename_with(.fn = function(x){paste0("stu_", x)}, .cols = !(order_no|starts_with('univ')))), by = c('univ_id', 'ord_num' = 'order_no')) %>% # note: same result of you merge just by order number
  # create indicator of whether order summary data missing
  mutate(na_ord_summ = if_else(is.na(one),1,0)) %>% select(-one) %>%
  # other variables used later
  mutate(
    # indicator variable for whether student is in 50 US states + DC [note: variable stu_country sometimes missing/unreliable]
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
 
###################### MERGE IN SECONDARY DATA


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
  left_join(y=acs_race_zipcodev3 %>% select(-zipcode,-msa_name) %>% rename_with(.fn = function(x){paste0("zip_", x)}, .cols = !(starts_with('zip'))) %>% mutate(one=1),by = c('stu_zip_code' = 'zip_code')) %>%
  mutate(na_zip_acs = if_else(is.na(one),1,0)) %>% select(-one)

  # INVESTIGATE MERGE
    #lists_orders_zip_df %>% count(na_zip_acs)
    #lists_orders_zip_df %>% filter(stu_country == 'united states') %>% count(na_zip_acs)
  
    lists_orders_zip_anti <- lists_orders_df %>% filter(stu_country == 'united states') %>% anti_join(acs_race_zipcodev3, by = c('stu_zip_code' = 'zip_code'))
    
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
  ) %>%
  # create additional variables for analysis
    mutate(
      # student-level in-state vs. out-of-state purchase
      #stu_out_st = if_else(stu_state != univ_state,1,0, missing = NULL),
      stu_out_st = case_when(
        !is.na(stu_state) & stu_state != univ_state ~ 1,
        !is.na(stu_state) & stu_state == univ_state ~ 0,
        is.na(stu_state) & !is.na(stu_country) & stu_country != 'united states' ~ 1
        # should be missing if state is missing and country is missing
        # should be missing if state is missing and country is 'united states'
      ),
      # student-level dichotomous race vars from categorical CB race var
      stu_white_01 = if_else(stu_race_cb==9,1,0,missing=NULL),
      stu_asian_01 = if_else(stu_race_cb==2,1,0,missing=NULL),
      stu_black_01 = if_else(stu_race_cb==3,1,0,missing=NULL),
      stu_hispanic_01 = if_else(stu_race_cb==4,1,0,missing=NULL),
      stu_amerindian_01 = if_else(stu_race_cb==1,1,0,missing=NULL),
      stu_nativehawaii_01 = if_else(stu_race_cb==8,1,0,missing=NULL),
      stu_native_01 = if_else(stu_race_cb %in% c(1,8),1,0,missing=NULL), # ametican indian, alaska native, native hawwaiian or pacific islander
      stu_tworaces_01 = if_else(stu_race_cb==12,1,0,missing=NULL),
      stu_unknown_01 = if_else(stu_race_cb==0,1,0,missing=NULL),
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
  #ceeb_hs_old %>% rename_with(.fn = function(x){paste0("hs_", x)}, .cols = !(starts_with('ceeb'))) %>% mutate(one=1) %>% glimpse()
  #ceeb_hs %>% rename_with(.fn = function(x){paste0("hs_", x)}, .cols = !(starts_with('ceeb'))) %>% mutate(one=1) %>% glimpse()
  
  lists_orders_zip_hs_df <- lists_orders_zip_df %>% 
    left_join(y= (ceeb_hs %>% rename_with(.fn = function(x){paste0("hs_", x)}, .cols = !(starts_with('ceeb'))) %>% mutate(one=1)), by = c('stu_ceeb' = 'ceeb')) %>%
    mutate(na_hs = if_else(is.na(one),1,0)) %>% select(-one) 

    lists_orders_zip_hs_df %>% glimpse()


  # INVESTIGATE MERGE
    # NOTE: 8/20/2021: THE NUMBERS IN COMMENTS ON BELOW CHECKS ARE BASED ON USING HIGH SCHOOL DATA FRAME hs_data, WHICH WE ARE NOT USING ANYMORE
    # SUMMARY OF INVESTIGATION
      # prospects w/ missing hs_level data somewhat more likely to be hispanic/black, somewhat less likely to be white
      # don't see other huge differences
  
    #lists_orders_zip_hs_df %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 89.6 of students merge
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 93.8% of students merge
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,!is.na(stu_ceeb)) %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 94.2% of students merge
    
  # anti-merge
    # lists_orders_zip_hs_anti <- lists_orders_zip_df %>% anti_join(ceeb_hs, by = c('stu_ceeb' = 'ceeb')) #80K obs
    # lists_orders_zip_hs_anti <- lists_orders_zip_hs_anti %>% filter(!is.na(stu_ceeb)) # 70K obs that have a 6 digit ceeb code
    
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
    
    # rm(lists_orders_zip_hs_anti)

