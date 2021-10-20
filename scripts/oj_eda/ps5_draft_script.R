
# purpose of this dataset is to give students practice processing across rows
  # dataset we use is student list data from UI-Chicago; 
    # particularly one student list purchase, where they bought all american indian/alaskan native names in the nation
  # thought it would be interesting to check these data out
  # here is google doc to notes I have
    # https://docs.google.com/document/d/1-kpTmgDrNhv298Q3fNJBy0ot8-THYujBsFBlinhuHzU/edit 
  # task: 
    # can you work on turning this R script into a .Rmd solutions file w/ directions and stuff
  # the dropbox folder we are using for rclass1 problem sets is here
    # C:\Users\ozanj\Dropbox\rclass1_ps
  # also, you have absolute autonomy to change questions add new ones/ cut other ones if too redundant
    # e.g., I haven't done anything w/ helper functions like first(), last(), nth()

library(tidyverse)
library(labelled)


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




# create data frame of prospect-level data from U Illinois-Chicago, order '487927'
list_native_df <- lists_orders_zip_hs_df %>% filter(univ_id == '145600', ord_num =='487927')  %>%
  select(univ_id,ord_num,univ_state,univ_zip,
    stu_state,stu_city,stu_zip_code,stu_geomarket,stu_country,stu_in_us,stu_hs_code,stu_county_code,stu_gender,
    stu_cuban,stu_mexican,stu_puerto_rican,stu_other_hispanic,stu_non_hispanic,stu_ethnicity_no_response,
    stu_american_indian,stu_asian,stu_black,stu_native_hawaiian,stu_white,stu_race_no_response,
    stu_major_1,
    na_zip_acs,zip_cbsa_1,zip_cbsatitle_1,zip_csacode,zip_csatitle,zip_median_household_income,
    #zip_pop_total,zip_pop_white,zip_pop_black,zip_pop_amerindian,zip_pop_asian,zip_pop_nativehawaii,zip_pop_otherrace,zip_pop_tworaces,zip_pop_hispanic,
    na_hs,hs_private,hs_cbsa_1,hs_cbsatitle_1,hs_csacode,hs_csatitle,hs_name,hs_ncessch,hs_state_code,hs_zip_code,
    hs_total_students,hs_total_amerindian,hs_total_asian,hs_total_black,hs_total_hispanic,hs_total_nativehawaii,hs_total_tworaces,hs_total_unknown,hs_total_white,
    hs_pct_amerindian,hs_pct_asian,hs_pct_black,hs_pct_hispanic,hs_pct_nativehawaii,hs_pct_tworaces,hs_pct_unknown,hs_pct_white,    
  ) %>% 
  rename(zip_cbsa = zip_cbsa_1,zip_cbsatitle = zip_cbsatitle_1,hs_cbsa = hs_cbsa_1,hs_cbsatitle = hs_cbsatitle_1)

# Save dataframes
save(list_native_df, file = file.path(data_dir, 'list_native_df.RData'))
rm(list_native_df)

# load
  # CRYSTAL: SAVE THIS TO CLOUD (MAYBE PROBLEM SET DROP BOX) AND 
load(file = file.path(data_dir, 'list_native_df.RData')) 

list_native_df %>% glimpse()

# create ethnicity and race variables
list_native_df %>% mutate(
  # create 0/1 variable for identifies as hispanic
  stu_hispanic_01 = case_when(
    (stu_cuban == 'Y' | stu_mexican == 'Y' | stu_puerto_rican == 'Y' | stu_other_hispanic == 'Y') ~ 1, # any of the hispanic categories equal 'Y'
    (stu_non_hispanic=='Y' & is.na(stu_cuban)==1 & is.na(stu_mexican)==1 & is.na(stu_puerto_rican)==1 & is.na(stu_other_hispanic)==1) ~ 0,   # == 0 if non_hispanic == 1 and all four categories == NA; sometimes students select 'non_hispanic' and also mexican or cuban, etc.
  ),
  # create 0/1 variables for each ethnicity group
  stu_cuban_01 = case_when(stu_cuban == 'Y' ~ 1,is.na(stu_cuban) & is.na(stu_ethnicity_no_response) ~ 0),
  stu_mexican_01 = case_when(stu_mexican == 'Y' ~ 1,is.na(stu_mexican) & is.na(stu_ethnicity_no_response) ~ 0),
  stu_puerto_rican_01 = case_when(stu_puerto_rican == 'Y' ~ 1,is.na(stu_puerto_rican) & is.na(stu_ethnicity_no_response) ~ 0),
  stu_other_hispanic_01 = case_when(stu_other_hispanic == 'Y' ~ 1,is.na(stu_other_hispanic) & is.na(stu_ethnicity_no_response) ~ 0),
  #stu_non_hispanic_01 = case_when(stu_non_hispanic == 'Y' ~ 1,is.na(stu_non_hispanic) & is.na(stu_ethnicity_no_response) ~ 0),
  # create 0/1 variables for each race group
  stu_american_indian_01 = case_when(stu_american_indian == 'Y' ~ 1,is.na(stu_american_indian) & is.na(stu_race_no_response) ~ 0),
  stu_asian_01 = case_when(stu_asian == 'Y' ~ 1,is.na(stu_asian) & is.na(stu_race_no_response) ~ 0),
  stu_black_01 = case_when(stu_black == 'Y' ~ 1,is.na(stu_black) & is.na(stu_race_no_response) ~ 0),
  stu_native_hawaiian_01 = case_when(stu_native_hawaiian == 'Y' ~ 1,is.na(stu_native_hawaiian) & is.na(stu_race_no_response) ~ 0),
  stu_white_01 = case_when(stu_white == 'Y' ~ 1,is.na(stu_white) & is.na(stu_race_no_response) ~ 0),
  # create ct of number of race groups
  race_ct = rowSums(dplyr::across(c(stu_american_indian_01,stu_asian_01,stu_black_01,stu_native_hawaiian_01,stu_white_01), na.rm = TRUE)),
  # create 0/1 measure of multi-race
  multi_race_01 = if_else(race_ct >=2,1,0, missing = NULL),
  # create college board categorical ethnicity race variable
  race_cb = case_when(
    (is.na(stu_hispanic_01)==1 | (stu_hispanic_01==0 & stu_race_no_response=='Y')) ~ 'no_response', #0    No Response [ethnicity/Hispanic is NA; OR hispanic==0 AND race_no_response indicator indicates they chose not to respond]
    (stu_american_indian_01==1 & multi_race_01 == 0 & stu_hispanic_01 == 0) ~ 'ai_an', #1    American Indian/Alaska Native [american_indian_common ==1; AND multi_race_common == 0; AND is_hisp_common == 0 ]
    (stu_asian_01==1 & multi_race_01 == 0 & stu_hispanic_01 == 0) ~ 'asian', #2    Asian
    (stu_black_01==1 & multi_race_01 == 0 & stu_hispanic_01 == 0) ~ 'black', #3    Black/African American
    (stu_hispanic_01 ==1) ~ 'hispanic', #4    Hispanic/Latino
    (stu_native_hawaiian_01==1 & multi_race_01 == 0 & stu_hispanic_01 == 0) ~ 'nh_pi', #8    Native Hawaiian or Other Pacific Islander
    (stu_white_01==1 & multi_race_01 == 0 & stu_hispanic_01 == 0) ~ 'white', #9    White
    (multi_race_01 == 1 & stu_hispanic_01 == 0) ~ 'multi_race' #12   Two Or More Races, Non-Hispanic      
  )
) %>%
  # drop input ethnicity/race vars
  select(-stu_cuban,-stu_mexican,-stu_puerto_rican,-stu_other_hispanic,-stu_non_hispanic,-stu_american_indian,-stu_asian,stu_black,-stu_native_hawaiian,-stu_white)
############# PART X. GET TO KNOW DATASET; 

# glimpse
list_native_df %>% glimpse()

# frequency count of College Board aggregate ethnicity/race variable
list_native_df %>% count(race_cb)
list_native_df %>% count(race_cb) %>% mutate(pct = (n / sum(n)) * 100) %>% arrange(desc(pct))

# frequency count of gender variable
list_native_df %>% count(stu_gender)

# frequency count of the variable
list_native_df %>% count(na_hs)


############# PART X. SUMMARIZE

# use summarize to calculate:
  # number of observations n()
  # number of non-missing obs for variable stu_hispanic_01 sum()
  # number of non-missing obs for 
list_native_df %>% summarize(
  n_obs = n(),
  n_nonmiss_hisp = sum(!is.na(stu_hispanic_01))
)

# using summarize with the sum function, calculate number of students who identify as:
  # stu_ethnicity_no_response
  # cuban, mexican, puerto_rican, other_hispanic, non_hispanic
  
list_native_df %>% summarize(
  n_obs = n(),
  n_eth_no_response = sum(stu_ethnicity_no_response == 'Y', na.rm=TRUE),
  n_cuban = sum(stu_cuban_01 == 1, na.rm = TRUE),
  n_mexican = sum(stu_mexican_01 == 1, na.rm = TRUE),
  n_puerto_rican = sum(stu_puerto_rican_01 == 1, na.rm = TRUE),
  n_other_hispanic = sum(stu_other_hispanic_01 == 1, na.rm = TRUE),
  n_non_hispanic = sum(stu_non_hispanic_01 == 1, na.rm = TRUE),
)
  

# using summarize with the mean() function, calculate percentage of students who identify as:
  # hispanic () stu_hispanic_01
  # american indian or alaska native, stu_american_indian_01
  # asian stu_asian_01
  # black stu_black_01
  # native hawaiian or pacific islander stu_native_hawaiian_01
  # white stu_white_01
# how do these percentages differ from the categorical race_cb variable in which each student can only be in one group?

list_native_df %>% summarize(
  n_obs = n(),
  pct_hispanic = mean(stu_hispanic_01, na.rm = TRUE)*100,
  pct_american_indian = mean(stu_american_indian_01, na.rm = TRUE)*100,
  pct_asian = mean(stu_asian_01, na.rm = TRUE)*100,
  pct_black = mean(stu_black_01, na.rm = TRUE)*100,
  pct_native_hawaiian = mean(stu_native_hawaiian_01, na.rm = TRUE)*100,
  pct_white = mean(stu_white_01, na.rm = TRUE)*100,
)

# calculate 
  # number of observations where median household income is missing
  # mean of median household income
list_native_df %>% summarize(
  n_obs = n(),
  n_miss_inc = sum(is.na(zip_median_household_income)),
  mean_zip_inc = mean(zip_median_household_income, na.rm = TRUE)
)

# calculate racial composition of high schools these students attend
list_native_df %>% summarize(
  n_obs = n(),
  n_miss_hs = sum(is.na(hs_total_students)),
  pct_amerindian = sum(hs_total_amerindian, na.rm = TRUE)/sum(hs_total_students, na.rm = TRUE)*100,
  pct_asian = sum(hs_total_asian, na.rm = TRUE)/sum(hs_total_students, na.rm = TRUE)*100,
  pct_black = sum(hs_total_black, na.rm = TRUE)/sum(hs_total_students, na.rm = TRUE)*100,
  pct_hispanic = sum(hs_total_hispanic, na.rm = TRUE)/sum(hs_total_students, na.rm = TRUE)*100,
  pct_nativehawaii = sum(hs_total_nativehawaii, na.rm = TRUE)/sum(hs_total_students, na.rm = TRUE)*100,
  pct_tworaces = sum(hs_total_tworaces, na.rm = TRUE)/sum(hs_total_students, na.rm = TRUE)*100,
  pct_white = sum(hs_total_white, na.rm = TRUE)/sum(hs_total_students, na.rm = TRUE)*100,
)


############# PART X. GROUP_BY AND SUMMARIZE

# group by state and calculate number of AIAN by state; sort descending

list_native_df %>% group_by(stu_state) %>% summarize(
 n_obs = n(),
) %>% arrange(desc(n_obs))
  
list_native_df %>% glimpse()


# group by state and calculate the following by state:
  # number of AIAN by state; 
  # average of median income in zip code they live
  # max median income in zip code they live in
  # min median income in zip code they live in
  # calculate the percentage of people who identify as:
    # cuban, mexican, puerto_rican, other_hispanic, non_hispanic
  # sort descending by number of AIAN in state

list_native_df %>% filter(!is.na(stu_state)) %>% group_by(stu_state) %>% summarize(
  n_obs = n(),
  mean_zip_inc = mean(zip_median_household_income, na.rm = TRUE),
  max_zip_inc = max(zip_median_household_income, na.rm = TRUE),
  min_zip_inc = min(zip_median_household_income, na.rm = TRUE),
) %>% arrange(desc(n_obs)) %>% print(n=55)

list_native_df %>% glimpse()

# group by state and calculate the following by state:
  # number of AIAN by state; 
  # calculate the percentage of people who identify as:
    # cuban, mexican, puerto_rican, other_hispanic, non_hispanic
  # sort descending by number of AIAN in state

list_native_df %>% group_by(stu_state) %>% summarize(
  n_obs = n(),
  pct_cuban = mean(stu_cuban_01 ==1, na.rm = TRUE)*100,
  pct_mexican = mean(stu_mexican_01 == 1, na.rm = TRUE)*100,
  pct_puerto_rican = mean(stu_puerto_rican_01 == 1, na.rm = TRUE)*100,
  pct_other_hispanic = mean(stu_other_hispanic_01 == 1, na.rm = TRUE)*100,
) %>% arrange(desc(n_obs)) %>% print(n=55)


# grouping by state and summarize with the mean() function, calculate percentage of students who identify as:
  # hispanic () stu_hispanic_01
  # american indian or alaska native, stu_american_indian_01
  # asian stu_asian_01
  # black stu_black_01
  # native hawaiian or pacific islander stu_native_hawaiian_01
  # white stu_white_01
# sort descending by number of AIAN in state

list_native_df %>% group_by(stu_state) %>% summarize(
  n_obs = n(),
  pct_hispanic = mean(stu_hispanic_01, na.rm = TRUE)*100,
  pct_american_indian = mean(stu_american_indian_01, na.rm = TRUE)*100,
  pct_asian = mean(stu_asian_01, na.rm = TRUE)*100,
  pct_black = mean(stu_black_01, na.rm = TRUE)*100,
  pct_native_hawaiian = mean(stu_native_hawaiian_01, na.rm = TRUE)*100,
  pct_white = mean(stu_white_01, na.rm = TRUE)*100,
) %>% arrange(desc(n_obs)) %>% print(n=55)


# this time group by zip_cbsatitle )essentially metro area) and calculate the same as above
  # sort descending by number of AIAN in metro area

list_native_df %>% group_by(zip_cbsatitle) %>% summarize(
  n_obs = n(),
  pct_hispanic = mean(stu_hispanic_01, na.rm = TRUE)*100,
  pct_american_indian = mean(stu_american_indian_01, na.rm = TRUE)*100,
  pct_asian = mean(stu_asian_01, na.rm = TRUE)*100,
  pct_black = mean(stu_black_01, na.rm = TRUE)*100,
  pct_native_hawaiian = mean(stu_native_hawaiian_01, na.rm = TRUE)*100,
  pct_white = mean(stu_white_01, na.rm = TRUE)*100,
) %>% arrange(desc(n_obs)) %>% print(n=55)