################################################################################
##
## [ PROJ ] < student list project, EDA >
## [ FILE ] < 145637_urbana_oj.R >
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
hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c'))

ceeb_nces <- read_csv(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/ceeb_nces_crosswalk.csv'))
cds_nces <- readr::with_edition(1, read_csv(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/CDS_NCES_crosswalk.csv'))) %>% mutate(ncessch = str_c(NCESDist, NCESSchool))
  # note: because using readr version 2.0, must use readr::with_edition() to overcome this error: The size of the connection buffer (131072) was not large enough to fit a complete line: * Increase it by setting `Sys.setenv("VROOM_CONNECTION_SIZE")`

# TO DO: ADD SECONDARY DATA FOR UNIVERSITY CHARACTERISTICS FROM IPEDS
  # PULL THIS FROM PRIVATE SCHOOL CHAPTER


## -----------------------------------------------------------------------------
## LOAD/INVESTIGATE ORDER SUMMARY DATA
## -----------------------------------------------------------------------------

# ORDER SUMMARY DATA
orders_df <- read_csv(file.path(data_dir, '145637_orders.csv'), col_types = c('univ_id' = 'c', 'order_num' = 'c')) # this one seems to only have College Board orders
  # this is the original data frame with all orders; the data frames for different kinds of orders is created from this one in Crystal's script 145637_urbana_ch.R around lines 80 through 150

orders_df %>% glimpse()

var_label(orders_df[['univ_id']]) <- 'University IPEDS/unitid'

var_label(orders_df[['po_num']]) <- 'Purchase order number (multiple student list purchases per purchase order number)'
  #orders_df %>% count(order_num)
  # each value of order_num -- which uniquely identifies obs - is associated with a purchase order number
  # orders_df %>% count(po_num,order_num) %>% print(n=100)

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
  orders_df %>% count(sat_score_max) %>% print(n=100)
var_label(orders_df[['sat_score_old_min']]) <- 'minimum SAT score (old scoring) specified in filter for student list (sometimes combined w/ PSAT score as an OR filter)'
  orders_df %>% count(sat_score_old_min) %>% print(n=100)
var_label(orders_df[['sat_score_old_max']]) <- 'maximum SAT score (old scoring) specified in filter for student list (sometimes combined w/ PSAT score as an OR filter)'
  orders_df %>% count(sat_score_old_max) %>% print(n=100)
var_label(orders_df[['psat_score_min']]) <- 'minimum PSAT score specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  orders_df %>% count(psat_score_min) %>% print(n=100)
var_label(orders_df[['psat_score_max']]) <- 'maximum PSAT score specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  orders_df %>% count(psat_score_max) %>% print(n=100)
var_label(orders_df[['psat_score_old_min']]) <- 'minimum PSAT score (old scoring) specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  orders_df %>% count(psat_score_old_min) %>% print(n=100)  
var_label(orders_df[['psat_score_old_max']]) <- 'minimum PSAT score (old scoring) specified in filter for student list (sometimes combined w/ SAT score as an OR filter)'
  orders_df %>% count(psat_score_old_max) %>% print(n=100)  
var_label(orders_df[['gpa_high']]) <- 'highest HS GPA specified in filter for student list purchase'
  orders_df %>% count(gpa_high) %>% print(n=100)
var_label(orders_df[['gpa_low']]) <- 'lowest HS GPA specified in filter for student list purchase'
  orders_df %>% count(gpa_low) %>% print(n=100)
var_label(orders_df[['rank_high']]) <- 'highest high school rank specified in filter for student list purchase'
  orders_df %>% count(rank_high) %>% print(n=100)  
var_label(orders_df[['rank_low']]) <- 'lowest high school rank specified in filter for student list purchase'
  orders_df %>% count(rank_low) %>% print(n=100)

var_label(orders_df[['race_ethnicity']]) <- 'race/ethnicity categories (character var) specified in filter for student list purchase'
  orders_df %>% count(race_ethnicity) %>% print(n=100)
var_label(orders_df[['gender']]) <- 'gender categories (character var) specified in filter for student list purchase'
  orders_df %>% count(gender) %>% print(n=100)
    
# VARIABLES YOU ARE NOT REALLY SURE ABOUT
var_label(orders_df[['num_runs']]) <- 'Number of runs? DO NOT KNOW WHAT THIS VARIABLE REALLY REFERS TO'
  #orders_df %>% count(num_runs) %>% print(n=100)
var_label(orders_df[['date_start']]) <- '????'
var_label(orders_df[['date_end']]) <- '????'
  #orders_df %>% count(date_start) %>% print(n=100)
  #orders_df %>% count(date_end) %>% print(n=100)
var_label(orders_df[['zip_code_file']]) <- '????' # var always NA for urbana
var_label(orders_df[['zip_code']]) <- '????' # var always NA for urbana
  #orders_df %>% count(zip_code_file) %>% print(n=100)
  #orders_df %>% count(zip_code) %>% print(n=100)


#### Investigating data quality


# three orders with most variables missing
  orders_na <- orders_df %>% filter(is.na(num_students)==1)
  orders_na # three purchases that have NA values for most variables; was this a parsing issue? or were we given incomplete data for these three student list purcahses
  # looking at the associated order summaries; these purchases do not have "download details" and "file output actuals" at the bottom of the order summary
orders_df %>% count(zip_code)
  # CHECK TO MAKE: LOOK AT STUDENT LIST DATA AND TRY TO IDENTIFY WHETHER THESE THREE ORDER NUMBERS ARE ASSOCIATED WITH ANY PROSPECTS


## -----------------------------------------------------------------------------
## LOAD INVESTIGATE STUDENT LIST DATA
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


