################################################################################
##
## [ PROJ ] < student list project, EDA >
## [ FILE ] < create_secondary_datasets.R >
## [ AUTH ] < Ozan Jaquette / ozanj >
## [ INIT ] < 8/31/2021 >
## [ DESC ] < Create datasets from secondary data sources (e.g., ACS, NCES school-level, IPEDS) >
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

## ---------------------------
## system settings
## ---------------------------

#rm(list = ls())
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

## -----------------------------------------------------------------------------
## SECONDARY DATA, CENSUS ACS DATA
## -----------------------------------------------------------------------------

####### LOAD DATA (FROM HUD) THAT IS USED TO CREATE CROSS-WALK OF ZIP-CODE TO STATE
  library(readxl)
  #zip_tract_032018 <- read_excel(path = file.path(data_dir, 'ZIP_TRACT_032018.xlsx'))
  zip_tract_092021 <- read_excel(path = file.path(data_dir, 'ZIP_TRACT_092021.xlsx'))
  names(zip_tract_092021) <- tolower(names(zip_tract_092021))
  
  zip_tract_092021 <- zip_tract_092021 %>% distinct(zip,usps_zip_pref_state) %>% select(zip,usps_zip_pref_state) %>% rename(zip_code = zip, state_code = usps_zip_pref_state)
  
  
  zip_tract_092021 %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
  
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
  
    zip_to_state %>% glimpse()
    #zip_to_state %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
    zip_to_state <- zip_to_state %>% select(state_code, zip_code)
    zip_to_state %>% count(state_code) %>% print(n=70)
  
    
    # create data frame that has two-digit state code for every zip code
    zip_to_state_v2 <- dplyr::bind_rows(zip_to_state,zip_tract_092021) %>% distinct(zip_code,state_code) %>% arrange(zip_code)
    
    zip_to_state_v2 %>% group_by(zip_code) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
    rm(zip_tract_092021)
    
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


rm(acs_race_zipcode)

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
    select(ncessch,sch_name,state_code,lea_name,state_agency_no,union,st_leaid,leaid,st_schid,lzip,contains('total'),contains('pct'),starts_with('g09'),starts_with('g10'),starts_with('g11'),starts_with('g12'),sch_type,titlei_status_text,magnet_text,charter_text,school_year) %>% 
    mutate(school_control = 'public',
      # to be consistent w/ how this variable formatted in private school data
      school_year = str_replace_all(school_year,'20|-','')
    ) %>%  rename(name = sch_name,zip_code = lzip,pub_sch_type = sch_type,total_09 = g09,total_10=g10,total_11 = g11, total_12 = g12, data_year = school_year)
  

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
    
  # Unclass haven_labelled columns
    pubhs_data <- pubhs_data %>% mutate(
      pub_sch_type = unclass(pub_sch_type)
    )
    
    privhs_data <- privhs_data %>% mutate(
      priv_sch_type = unclass(priv_sch_type)
    )

  # Combine data
    pubhs_privhs_data <- dplyr::bind_rows(
      pubhs_data,privhs_data
    ) %>% filter(!is.na(pct_white)) %>% # 2 obs in 17-18 data for publics that have 0s for all enrollment by race vars and NAs for all pct enrollment by race vars
    # create number and percent variable that combines native american, alaska native, native hawaiian and pacific islander
    mutate(
      total_native = total_nativehawaii + total_amerindian,
      pct_native = (total_native/total_students)*100,
      private = if_else(school_control=='private',1,0,missing=NULL)
    )

    #pubhs_privhs_data %>% select(total_amerindian,total_nativehawaii,total_native,total_students,pct_amerindian,pct_nativehawaii,pct_native) %>% View()
    
  # checks
    #pubhs_privhs_data %>% glimpse()
    #pubhs_privhs_data %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs      
    #pubhs_privhs_data %>% count(school_control)
    #pubhs_privhs_data %>% count(private)
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
    
    rm(pubhs_data_1718,privhs_data_1718)
    
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
    


# load 2017-18 University data from IPEDS
  # NOTE: I THINK THIS MAY HAVE BEEN CREATED FROM SUBSET OF STATA IPEDS PANEL WHICH WAS THEN USED IN PRIVATE HIGH SCHOOL RECRUITING CHAPTER; 
    # MAY WANT TO CHANGE THIS LATER TO PULL FROM RAW IPEDS DATA AND FOR MORE RECENT YEAR(S)
  # ./data/ipeds_1718.RDS has over 400 variables

## -----------------------------------------------------------------------------
## IPEDS DATA
## -----------------------------------------------------------------------------
      
univ_data <- readRDS('./data/ipeds_1718.RDS') %>% 
  select(univ_id,opeid,univ_name,ialias,addr,city,state_code,zip_code,fips_state_code,obereg,region,sector,locale,starts_with('c15'),
         ccbasic,carnegie,landgrnt,instsize,cbsa,csa,countycd,countynm,cngdstcd,longitude,latitude,room,board,roomamt,boardamt,rmbrdamt,
         starts_with('pct_freshman'),starts_with('applcn'),starts_with('admssn'),enrlt,enrlft,enrlpt,starts_with('sat'),starts_with('act'),
         -act,tuition1,fee1,tuit_fees_instate,tuit_fees_outstate) %>% 
  rename(tuit_indist = tuition1, fee_indist = fee1, tuit_fee_instate = tuit_fees_instate, tuit_fee_outstate = tuit_fees_outstate) %>%
  mutate(tuit_fee_indist = tuit_indist + fee_indist)

## -----------------------------------------------------------------------------
## CALIFORNIA DEPARTMENT OF EDUCATION DATA
## -----------------------------------------------------------------------------

# cds_nces <- readr::with_edition(1, read_csv(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/CDS_NCES_crosswalk.csv'))) %>% mutate(ncessch = str_c(NCESDist, NCESSchool))
  # note: because using readr version 2.0, must use readr::with_edition() to overcome this error: The size of the connection buffer (131072) was not large enough to fit a complete line: * Increase it by setting `Sys.setenv("VROOM_CONNECTION_SIZE")`

  # This seems to be data about California public schools
  # cds_nces %>% glimpse()
  # cds_nces %>% group_by(CDSCode) %>% summarise(n_per_key=n()) %>% ungroup() %>% count(n_per_key) # uniquely identifies obs
  # cds_nces %>% count(SOCType)
  # cds_nces %>% count(State) # california or missing
