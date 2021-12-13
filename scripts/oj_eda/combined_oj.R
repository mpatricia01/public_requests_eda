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
## OZAN THINGS TO DO, SECONDARY DATA
## -----------------------------------------------------------------------------

# high school data
  # load updated public high school data; run script to clean
    # revise race/ethnicity
  # load updated private high school data; run script to clean
    # revise race/ethnicity
  # append public and private high school data
    
# Census ACS data
  # document definition of race categories; make sure race categories add up to 100

## -----------------------------------------------------------------------------
## SECONDARY DATA, CENSUS ACS DATA
## -----------------------------------------------------------------------------

###### LOAD CENSUS ZIP-CODE LEVEL DATA
  
  # load ACS data w/ zipcode-level data on population and median household income; one obs per zip-code 
  acs_race_zipcode <- read_csv(file.path(data_dir, 'acs_race_zipcode.csv')) %>% arrange(zipcode)
    # based on 2019 5-year ACS
    # script: C:\Users\ozanj\Documents\public_requests_eda\scripts\acs_collect_via_API.py
    # definitions for race
      # 2016 ACS: https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr&table=B03002
        # detailed info on ethnicity/race: https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr&table=B03002 
      # 2019 ACS: https://www.socialexplorer.com/data/ACS2019_5yr/metadata/?ds=ACS19_5yr
  
  
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
      pop_hispanic_15_19_pct = as.numeric(pop_hispanic_15_19_pct),
      # create number and percent variable that combines native american, alaska native, native hawaiian and pacific islander
      pop_native_15_19 = pop_amerindian_15_19 + pop_nativehawaii_15_19,
      pop_native_15_19_pct = (pop_native_15_19/pop_total_15_19)*100
      
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
  
  # check on race/ethnicity vars
    #acs_race_zipcodev2 %>% count(is.na(pop_white_15_19))
    #acs_race_zipcodev2 %>% count(is.na(pop_amerindian_15_19))
    #acs_race_zipcodev2 %>% count(is.na(pop_nativehawaii_15_19))
    #acs_race_zipcodev2 %>% count(is.na(pop_native_15_19))
    
    #acs_race_zipcodev2 %>% count(is.na(pop_white_15_19_pct))
    #acs_race_zipcodev2 %>% count(is.na(pop_amerindian_15_19_pct))
    #acs_race_zipcodev2 %>% count(is.na(pop_nativehawaii_15_19_pct))
    #acs_race_zipcodev2 %>% count(is.na(pop_native_15_19_pct))
    
    #acs_race_zipcodev2 %>% filter(is.na(pop_white_15_19_pct)) %>% count(pop_total_15_19)
  
  # check what race/ethnicity variables add up to [**** basically, always = 100!*****]
  acs_race_zipcodev2 %>% select(-pop_native_15_19_pct) %>% mutate(tot_pct = rowSums(dplyr::across(.cols = contains('pct'), na.rm = TRUE))) %>%
    select(state_code,zip_code,contains('pct')) %>% count(tot_pct)
  
  acs_race_zipcodev2 %>% select(-pop_amerindian_15_19_pct,-pop_nativehawaii_15_19_pct) %>% mutate(tot_pct = rowSums(dplyr::across(.cols = contains('pct'), na.rm = TRUE))) %>%
    select(state_code,zip_code,contains('pct')) %>% count(tot_pct)  
  
  #acs_race_zipcodev2 %>% glimpse()


# data frame with code of CBSA associated with each zip-code
zip_cbsa_data <- read_csv(url('https://raw.githubusercontent.com/cyouh95/third-way-report/master/assets/data/zip_code_cbsa.csv'))
  
  zip_cbsa_data %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
  zip_cbsa_data %>% glimpse() # one observation per zip-code; for each zip-code indicates which CBSA(s) that zip code belongs to

###### crosswalk of cbsa to csa from NBER
  cbsa_name <- read_csv(file.path(data_dir, 'cbsa2fipsxw.csv'), col_names = TRUE, col_types = c('cbsacode' = 'c','csacode' = 'c'), skip_empty_rows = TRUE) %>% 
  # to deal w/ blank row betwen col_names row and actual data
  filter(!(is.na(cbsacode)))
  
  cbsa_name %>% glimpse()
  #cbsa_name %>% mutate(cbsacode_len = str_length(cbsacode)) %>% count(cbsacode_len) # always 5
  
  # count number of obs per cbsa code and cbsa name
    #cbsa_name %>% group_by(cbsacode) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key)  %>% print(n=30) # does not uniquely identifies obs
    #cbsa_name %>% group_by(cbsacode) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key)  %>% print(n=30) # does not uniquely identifies obs

  # keep one obs per cbsa
    cbsa_name <- cbsa_name %>% select(cbsacode,cbsatitle,metropolitanmicropolitanstatis,csacode,csatitle) %>% group_by(cbsacode) %>% filter(row_number()==1) %>% ungroup()
    
    cbsa_name %>% group_by(cbsacode) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key)  # TRUE
    
# add cbsa name to cbsa code
    # assign cbsa_1 as cbsa associated with zip code, even if part if zip-code in another CBSA; because cbsa_1 is the cbsa with highest percentage of zip-code in that cbsa
    #zip_cbsa_data %>% count(cbsa_1_ratio)
    #zip_cbsa_data %>% count(cbsa_2_ratio)
    

    zip_cbsa_name_data <- zip_cbsa_data %>% select(zip_code,cbsa_1,cbsa_1_ratio) %>% left_join((y=cbsa_name %>% mutate(one=1)) %>% select(-metropolitanmicropolitanstatis),by = c('cbsa_1'='cbsacode')) %>% rename(cbsatitle_1 = cbsatitle) %>%
      mutate(na_cbsa_name = if_else(is.na(one),1,0)) %>% select(-one)
    
    zip_cbsa_name_data %>% glimpse()
    zip_cbsa_name_data %>% count(na_cbsa_name)
    
    
#### names associated with CBSA codes
  # relevant links
    # Census "delineation" files: 
      #https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
    # NBER page with "Census Core-Based Statistical Area (CBSA) to Federal Information Processing Series (FIPS) County Crosswalk"
      # https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk
  # Core based statistical areas (CBSA)
    # definition
      # wikipedia: "A core-based statistical area (CBSA) is a U.S. geographic area defined by the Office of Management and Budget (OMB) that consists of one or more counties (or equivalents) anchored by an urban center of at least 10,000 people plus adjacent counties that are socioeconomically tied to the urban center by commuting. Areas defined on the basis of these standards applied to Census 2000 data were announced by OMB in June 2003. These standards are used to replace the definitions of metropolitan areas that were defined in 1990. The OMB released new standards based on the 2010 Census on July 15, 2015.[1][2][3]"
    # wikipedia link: https://en.wikipedia.org/wiki/Core-based_statistical_area
    # link; https://www.census.gov/topics/housing/housing-patterns/about/core-based-statistical-areas.html
  # Combined statistical areas (CSA)
    # definition
      # wikipedia: 
        # Combined statistical area (CSA) is a United States Office of Management and Budget (OMB) term for a combination of adjacent metropolitan (MSA) and micropolitan statistical areas (µSA) across the 50 US states and the territory of Puerto Rico that can demonstrate economic or social linkage. The OMB defines a CSA as consisting of various combinations of adjacent metropolitan and micropolitan areas with economic ties measured by commuting patterns. These areas that combine retain their own designations as metropolitan or micropolitan statistical areas within the larger combined statistical area.
        # The primary distinguishing factor between a CSA and an MSA/µSA is that the social and economic ties between the individual MSAs/µSAs within a CSA are at lower levels than between the counties within an MSA.[1] CSAs represent multiple metropolitan or micropolitan areas that have an employment interchange of at least 15%.[1] CSAs often represent regions with overlapping labor and media markets.
        # As of March 2020, there are 172 combined statistical areas across the United States, plus another three in the territory of Puerto Rico.[1]
      # link: https://en.wikipedia.org/wiki/Combined_statistical_area
  # example
    # cbsa = 14460; cbsa title = Boston-Cambridge-Newton, MA-NH; csa code = 148; csa title = Boston-Worcester-Providence, MA-RI-NH-CT
    # cbsa = 49340; cbsa title = Worcester, MA-CT; csa code = 148; csa title = Boston-Worcester-Providence, MA-RI-NH-CT


# merge zip code data to cbsa name
    
acs_race_zipcodev2

zip_cbsa_name_data %>% glimpse()

acs_race_zipcodev2 %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
zip_cbsa_name_data %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs

acs_race_zipcodev3 <- acs_race_zipcodev2 %>% 
  left_join(y= zip_cbsa_name_data %>% select(-na_cbsa_name),by = c('zip_code'))

rm(acs_race_zipcodev2,cbsa_name)
## -----------------------------------------------------------------------------
## SECONDARY DATA, HIGH SCHOOL DATA
## -----------------------------------------------------------------------------

# PUBLIC HIGH SCHOOL DATA
    
  pubhs_data_1718 <- readRDS(file.path(data_dir, 'ccd_1718.RDS'))

# public high school data used for the private school chapter    
      # CCD script: https://github.com/cyouh95/recruiting-chapter/blob/master/scripts/ccd_data_saving.R#L176-L193

# public high school race categories
  #$ g11_white           <int> 0, 180, 0, 0, 0, 0, 0, 35, 0, 0, 94, 0, 99, 0, 0, 0, 0, NA, 69, 0, 0, 0, 0, 0, 419, 1, 0, 0, 245, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 270, 0,~
  #$ g12_amerindian      <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0,~
  #$ g12_asian           <int> 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 43, 0, 0, 0, 26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 38, 0, 0, 0, 0,~
  #$ g12_black           <int> 0, 9, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 1, 0, 0, 0, 0, NA, 4, 0, 0, 0, 0, 0, 201, 0, 0, 0, 96, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 110, 0, 0, 0, ~
  #$ g12_hispanic        <int> 0, 126, 0, 0, 0, 0, 0, 19, 0, 0, 34, 0, 1, 0, 0, 0, 0, NA, 4, 0, 0, 0, 0, 0, 44, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0~
  #$ g12_nativehawaii    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0,~
  #$ g12_tworaces        <int> 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0, 14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0,~
  #$ g12_unknown         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~

  # investigating data
    # at present, no latitude or longitude variable on public hs data frame

  # PATRICIA: ULTIMATELY THIS DATA FRAME WILL APPEND MULTIPLE YEARS OF CCD DATA AND VARIABLE 'YEAR' WILL NOT ALWAYS BE '1718'
  pubhs_data <- pubhs_data_1718 %>% filter(!(is.na(total_students)),total_students>0) %>%
    select(ncessch,sch_name,state_code,lzip,contains('total'),contains('pct'),starts_with('g09'),starts_with('g10'),starts_with('g11'),starts_with('g12'),sch_type,titlei_status_text,magnet_text,charter_text,school_year) %>% 
    mutate(school_control = 'public',
      # to be consistent w/ how this variable formatted in private school data
      school_year = str_replace_all(school_year,'20|-','')
    ) %>%  rename(name = sch_name,zip_code = lzip,pub_sch_type = sch_type,total_09 = g09,total_10=g10,total_11 = g11, total_12 = g12, data_year = school_year)
  

   pubhs_data %>% glimpse()

# PRIVATE HIGH SCHOOL DATA
privhs_data_1718 <- readRDS(file.path(data_dir, 'pss_1718.RDS'))

  # private high school data used for the private school chapter
    #private school script: https://github.com/cyouh95/recruiting-chapter/blob/master/scripts/pss_data_saving.R#L529-L535

  # notes on question/coding of ethnicity/race
    # question wording for ethnicity/race questions can be found in pss_codebook_1717.docx
      # enrollment by race includes all students in K-12; not separate by grade, which is available for the public school data
    # private high school race vars
      #6A Hispanic or Latino Students
      #6B White Students
      #6C Black Students
      #6D Asian Students
      #6E Native Hawaiian/Pacific Islander Students
      #6F American Indian/Alaska Native Students
      #6G Students of Two or More Races

  # investigating privhs_data 
    #privhs_data_1718 %>% select(contains('pct')) %>% glimpse()

    # total students always equalls sum of students for each race group
      #privhs_data_1718 %>% mutate(total_generated = rowSums(dplyr::across(.cols = c(total_white,total_black,total_hispanic,total_asian,total_amerindian,total_nativehawaii,total_tworaces), na.rm = TRUE))) %>% select(total_white,total_black,total_hispanic,total_asian,total_amerindian,total_nativehawaii,total_tworaces,total_generated,total_students) %>% mutate(gen_eq_total = if_else(total_students == total_generated,1,0, missing = NULL)) %>% count(gen_eq_total)
  
  # create data frame w/ selected variables
    # PATRICIA: ULTIMATELY THIS DATA FRAME WILL APPEND MULTIPLE YEARS OF PSS DATA 
    privhs_data <- privhs_data_1718 %>% select(ncessch,name,state_code,zip_code,latitude,longitude,contains('total'),-total_male,contains('pct'),-pct_to_4yr,religion,religion_5,school_type,year) %>%
      rename(priv_sch_type = school_type, data_year = year) %>% mutate(school_control= 'private')
    

    privhs_data %>% glimpse()
    
  
## APPEND PUBLIC AND PRIVATE HIGH SCHOOL DATA

  # Combine data
    pubhs_privhs_data <- dplyr::bind_rows(
      pubhs_data,privhs_data
    ) %>% filter(!is.na(pct_white)) %>% # 2 obs in 17-18 data for publics that have 0s for all enrollment by race vars and NAs for all pct enrollment by race vars
    # create number and percent variable that combines native american, alaska native, native hawaiian and pacific islander
    mutate(
      total_native = total_nativehawaii + total_amerindian,
      pct_native = (total_native/total_students)*100
    )
    #pubhs_privhs_data %>% select(total_amerindian,total_nativehawaii,total_native,total_students,pct_amerindian,pct_nativehawaii,pct_native) %>% View()
    
  # checks
    #pubhs_privhs_data %>% glimpse()
    #pubhs_privhs_data %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs      
    #pubhs_privhs_data %>% count(school_control)
    #pubhs_privhs_data %>% group_by(school_control) %>% count(is.na(pct_unknown))
    
    #pubhs_privhs_data %>% group_by(school_control) %>% count(is.na(total_students))
    #pubhs_privhs_data %>% group_by(school_control) %>% count(is.na(pct_white))
    #pubhs_privhs_data %>% group_by(school_control) %>% count(is.na(pct_amerindian))
    #pubhs_privhs_data %>% group_by(school_control) %>% count(is.na(pct_nativehawaii))
    #pubhs_privhs_data %>% group_by(school_control) %>% count(is.na(pct_native))
    
    #pubhs_privhs_data %>% glimpse()
  
    # check that enrollment count race vars sum to total enrollment both public and private; looks good
    #pubhs_privhs_data %>% select(contains('total')) %>% glimpse()  
    
    pubhs_privhs_data %>% mutate(
      total_unknown = if_else(school_control=='public',total_unknown,0L, missing = NULL),
      total_generated = rowSums(dplyr::across(.cols = c(contains('total'),-total_09,-total_10,-total_11,-total_12,-total_students), na.rm = TRUE))
    ) %>% select(ncessch,name,school_control,state_code,zip_code,total_generated,contains('total'),-total_09,-total_10,-total_11,-total_12) %>%
      mutate(gen_eq_tot = if_else(total_generated == total_students,1,0,missing = NULL)) %>% count(school_control,gen_eq_tot)
      #%>% count(school_control,tot_pct) %>% print(n=100)
    
    
    # check that pct race vars sum to 100 for both public and private; looks good
    pubhs_privhs_data %>% select(contains('pct')) %>% glimpse()  
    pubhs_privhs_data %>% select(-pct_native) %>% mutate(
      pct_unknown = if_else(school_control=='public',pct_unknown,0, missing = NULL),
      tot_pct = rowSums(dplyr::across(.cols = contains('pct'), na.rm = TRUE))
    ) %>% select(ncessch,name,school_control,state_code,zip_code,contains('pct')) %>% count(school_control,tot_pct) %>% print(n=100)
      # %>% filter(school_control == 'private') %>% View()
      # %>% count(school_control,tot_pct)
    
# merge cbsa code/name and csa code/name to high school data
    zip_cbsa_name_data %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
    
    pubhs_privhs_data <- pubhs_privhs_data %>% left_join(y=zip_cbsa_name_data %>% mutate(one=1), by = c('zip_code')) %>% 
      mutate(na_hs_cbsa_file = if_else(is.na(one),1,0)) %>% select(-one,-na_hs_cbsa_file)

    
########### HIGH SCHOOL DATA USED FOR OFF-CAMPUS RECRUITING PROJECT; TOO DATED
    
  #hs_data <- read_csv(url('https://github.com/cyouh95/third-way-report/blob/master/assets/data/hs_data.csv?raw=true'), col_types = c('zip_code' = 'c'))
  # data file located here: C:\Users\ozanj\Documents\third-way-report\assets\data
  # script to create file: https://github.com/ksalazar3/recruiting-static-visuals#data-source
  # years of data:
    # 2014-15 NCES Common Core of Data (CCD) [x]
    # 2015-16 NCES Private School Universe Survey (PSS) [x]:  If 2015-16 data is not available, 2013-14 data was used.
  # 

  
  #hs_data %>% glimpse() # 
  #hs_data %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # ncessch uniquely identifies obs
  #hs_data %>% count(school_type) # public and private

## -----------------------------------------------------------------------------
## SECONDARY DATA, CEEB CODE
## -----------------------------------------------------------------------------

ceeb_nces <- read_csv(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/ceeb_nces_crosswalk.csv'))

  # this dataset is a crosswalk between nces school code and ceeb code (college board school code)
  #ceeb_nces %>% glimpse() # two variables: ceeb code (college board school code); ncessch code
  #ceeb_nces %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs
  #ceeb_nces %>% group_by(ceeb) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # does not uniquely identify obs

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
  
  #ceeb_hs_old <- ceeb_nces %>% inner_join(hs_data, by = 'ncessch')  # get rid of rows w/o NCES data too
  #ceeb_hs_old <- ceeb_hs_old %>% arrange(ceeb,desc(total_students)) %>% group_by(ceeb) %>% filter(row_number()==1) %>% ungroup()
  
  ceeb_hs <- ceeb_nces %>% inner_join(pubhs_privhs_data, by = 'ncessch')  # get rid of rows w/o NCES data too
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
  right_join(orders_df, by = 'univ_id') %>% select(-univ_sector) %>%
  # drop order from U Illinois-Urbana that seems like it was not executed (order name is OOS ENG Female PSAT Catch-Up; 1,377 students available; but "name license status" was "saved" rather than "fulfilled" and "maximum volume = 0)
  filter(order_num != '374945')

  orders_df %>% group_by(order_num) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # unique



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
      stu_out_st = if_else(stu_state != univ_state,1,0, missing = NULL),
      # student-level dichotomous race vars from categorical CB race var
      stu_white = if_else(stu_race_cb==9,1,0,missing=NULL),
      stu_asian = if_else(stu_race_cb==2,1,0,missing=NULL),
      stu_black = if_else(stu_race_cb==3,1,0,missing=NULL),
      stu_hispanic = if_else(stu_race_cb==4,1,0,missing=NULL),
      stu_amerindian = if_else(stu_race_cb==1,1,0,missing=NULL),
      stu_nativehawaii = if_else(stu_race_cb==8,1,0,missing=NULL),
      stu_native = if_else(stu_race_cb %in% c(1,8),1,0,missing=NULL), # ametican indian, alaska native, native hawwaiian or pacific islander
      stu_tworaces = if_else(stu_race_cb==12,1,0,missing=NULL),
      stu_unknown = if_else(stu_race_cb==0,1,0,missing=NULL),
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
    mutate(na_hs = if_else(is.na(one),1,0)) %>% select(-one) %>%
    # create 0/1 indicator for student attends a private high school
    mutate(
      hs_private = if_else(hs_school_control=='private',1,0,missing=NULL)
    )    
      
      
  # 8/23/2021: exclude Minnesota State University Moorhead for now because missing prospect-level race vars
    #lists_orders_zip_df %>% filter(univ_id == '174358') %>% count()
    #lists_orders_zip_df %>% filter(univ_id == '174358') %>% count(stu_in_us)
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% filter(univ_id != '174358')
    lists_orders_zip_hs_df %>% count()

    
  # INVESTIGATE MERGE
    # NOTE: 8/20/2021: THE NUMBERS IN COMMENTS ON BELOW CHECKS ARE BASED ON USING HIGH SCHOOL DATA FRAME hs_data, WHICH WE ARE NOT USING ANYMORE
    # SUMMARY OF INVESTIGATION
      # prospects w/ missing hs_level data somewhat more likely to be hispanic/black, somewhat less likely to be white
      # don't see other huge differences
  
    #lists_orders_zip_hs_df %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 89.6 of students merge
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 93.8% of students merge
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,!is.na(stu_ceeb)) %>% count(na_hs) %>% mutate(freq = (n / sum(n)) * 100) # 94.2% of students merge
    
  # anti-merge
    lists_orders_zip_hs_anti <- lists_orders_zip_df %>% anti_join(ceeb_hs, by = c('stu_ceeb' = 'ceeb')) #80K obs
    lists_orders_zip_hs_anti <- lists_orders_zip_hs_anti %>% filter(!is.na(stu_ceeb)) # 70K obs that have a 6 digit ceeb code
    
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
## RQ2A: WHAT ARE THE CHARACTERISTICS OF PROSPECTS PURCHASED BY STUDENT LISTS? HOW DO THESE CHARACTERISTICS DIFFER ACROSS UNIVERSITY TYPE, GEOGRAPHIC FOCUS, AND ACROSS FILTER CRITERIA
## -----------------------------------------------------------------------------

# EDA GOOGLE DOCS
  # 'exploratory data analysis': https://docs.google.com/document/d/17XGsoYYmqODmdUik-5q5-0GuBc0LNb6KOrs_lhAYUpU/edit# 
  # 'OJ and KS analysis plans' https://docs.google.com/document/d/17lpSfaXgKCc3bRBsuiqKEPMhLSL6agyv2lWQxeGf9a4/edit
    
  
#############      
######### PROSPECT CHARACTERISTICS OF INTEREST; BY ALL AND IN-STATE VS. OUT-OF-STATE PROSPECTS
#############    
    
# number of students
    lists_orders_zip_hs_df %>% filter(stu_in_us==1) %>% count()
    
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

  
# number/percent of prospects by race
  
  # all
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100)  
  # same
  #lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(stu_race_cb) %>% summarise (n = n()) %>%  mutate(pct = 100*(n / sum(n)))
  
  # create from 0/1 variables defined by the categorical variable 
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>%    
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
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(stu_out_st) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100)  
  
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(stu_out_st) %>% 
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
  

# prospect attends public vs. private school
  
  # all
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)  
  
  # by 0/1 prospect is out of state
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0) %>% group_by(stu_out_st) %>% count(hs_private) %>% mutate(pct = (n / sum(n)) * 100)  
  
# Racial composition of prospects by public/private high school
  
  # all
    
    #lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(hs_private) %>% count(stu_race_cb) %>% mutate(pct = (n / sum(n)) * 100)  
  
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(hs_private) %>% 
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

  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(stu_out_st,hs_private) %>% 
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

  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>%    
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
  
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,!(is.na(stu_race_cb))) %>% group_by(univ_c15basic,stu_out_st) %>% 
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
  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(univ_c15basic,hs_private) %>% 
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

  lists_orders_zip_hs_df %>% filter(stu_in_us==1,na_hs ==0,!(is.na(stu_race_cb))) %>% group_by(univ_c15basic,stu_out_st,hs_private) %>% 
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

          # student-level racial composition of prospects
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>%
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
          gt0_stu = if_else(n_stu>0,1,0, missing = NULL),
          n_stu_cat5 = cut(n_stu, breaks=c(-Inf, 0, 10, 40, 100, +Inf),labels = lbls_cat5),
          n_stu_cat4 = cut(n_stu, breaks=c(-Inf, 0, 10, 100, +Inf),labels = lbls_cat4),
          n_stu_cat3 = cut(n_stu, breaks=c(-Inf, 0, 10, +Inf),labels = lbls_cat3),
          )


      # average racial composition of public schools in the CBSA, with 0 vs. gt0 prospects purchased
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

          # student-level racial composition of prospects
          lists_orders_zip_hs_df %>% filter(univ_id == '145637', zip_cbsa_1 == '12060', ord_num %in% c('483724','470283','371629','456737','386335','500590','567376','483751')) %>%
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
