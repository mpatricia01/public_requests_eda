library(tidyverse)


data_dir <- file.path('.', 'data')


# UI-Urbana data
load(file.path(data_dir, '145637_data.RData'))

lists_df_145637 %>% count(is.na(race))  # Missing race: 24855 / 415432 = 0.06
  #lists_df_145637 %>% count(race) %>% print(n=50)
lists_df_145637 %>% count(is.na(is_hispanic_origin))  # Missing ethnicity: 308826 / 415432 = 0.74
table(lists_df_145637$is_hispanic_origin, useNA = 'always')


# UI-Chicago data (original College Board race vars)
load(file.path(data_dir, '145600_data.RData'))

table(lists_df_145600$race_no_response, useNA = 'always')  # Answered Y: 3273 / 246785 = 0.01
table(lists_df_145600$ethnicity_no_response, useNA = 'always')  # Answered Y: 4737 / 246785 = 0.02

# Besides only counting "Y" for the no response vars, below also includes rows where it is NA for all race/ethnicity vars

lists_df_145600 %>% group_by(american_indian, asian, black, native_hawaiian, white, other, race_no_response) %>% count() %>% View()

# 12117 rows NA for all race vars
# 3271 + 1 + 1 = 3273 rows Y for race_no_response
# (12117 + 3273) / 246785 = 0.06  <-- close to Urbana's missing race

lists_df_145600 %>% group_by(cuban, mexican, puerto_rican, other_hispanic, non_hispanic, ethnicity_no_response) %>% count() %>% View()

# 3143 rows NA for all ethnicity vars
# 4737 rows Y for ethnicity_no_response
# (3143 + 4737) / 246785 = 0.03  <-- still much lower than Urbana's missing ethnicity


# Check only in-state (IL) for ethnicity

lists_df_145637 %>% filter(state == 'IL') %>% count(is.na(is_hispanic_origin))  # 80640 / 155186 = 0.52

lists_df_145600 %>% filter(state == 'IL') %>% group_by(cuban, mexican, puerto_rican, other_hispanic, non_hispanic, ethnicity_no_response) %>% count() %>% View()
lists_df_145600 %>% filter(state == 'IL') %>% nrow()

# 1465 rows NA for all ethnicity vars
# 2185 rows Y for ethnicity_no_response
# (1465 + 2185) / 115662 = 0.03  <-- still much lower than Urbana's missing ethnicity


# Illinois State University data (another example of original College Board race vars)
load(file.path(data_dir, '145813_data.RData'))

table(lists_df_145813$race_no_response, useNA = 'always')  # Answered Y: 1030 / 154040 = 0.0067
table(lists_df_145813$ethnicity_no_response, useNA = 'always')  # Answered Y: 3265 / 154040 = 0.02

# Besides only counting "Y" for the no response vars, below also includes rows where it is NA for all race/ethnicity vars

lists_df_145813 %>% group_by(american_indian, asian, black, native_hawaiian, white, other, race_no_response) %>% count() %>% View()

# 12820 rows NA for all race vars
# 1029 + 1 = 1030 rows Y for race_no_response
# (12820 + 1030) / 154040 = 0.09  <-- greater than Urbana's missing race

lists_df_145813 %>% group_by(cuban, mexican, puerto_rican, other_hispanic, non_hispanic, ethnicity_no_response) %>% count() %>% View()

# 2025 rows NA for all ethnicity vars
# 3265 rows Y for ethnicity_no_response
# (2025 + 3265) / 154040 = 0.03  <-- still much lower than Urbana's missing ethnicity



#############################

lists_df %>% filter(univ_id=='145637') %>% count(race_cb)

# urbana in state
lists_df %>% filter(univ_id=='145637',state=='IL') %>% count()
lists_df %>% filter(univ_id=='145637',state=='IL') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)

# ui-chicago in state
lists_df %>% filter(univ_id=='145600',state=='IL') %>% count()
lists_df %>% filter(univ_id=='145600',state=='IL') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)


#### Urbana, order = 371076, ordered feb 2018
  # in state, PSAT/SAT = 1160 - 1270; race = asian/white/other/native and NOT hispanic
  # results = 677 people who identify as hispanic origin in variable is_hispanic_origin

  lists_df %>% filter(univ_id=='145637',state=='IL',order_no=='371076') %>% count()
  
  lists_df %>% filter(univ_id=='145637',order_no=='371076') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  lists_df %>% filter(univ_id=='145637',order_no=='371076') %>% count(is_hispanic_origin) %>% mutate(freq = (n / sum(n)) * 100)
  
  lists_df %>% filter(univ_id=='145637',order_no=='371076',is_hispanic_origin=='Yes') %>% count(race) %>% print(n=50)
  
  
#### Urbana, order = 456700, ordered feb 2019  
  lists_df %>% filter(univ_id=='145637',order_no=='456700') %>% count()
  
  lists_df %>% filter(univ_id=='145637',order_no=='456700') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  lists_df %>% filter(univ_id=='145637',order_no=='456700') %>% count(is_hispanic_origin) %>% mutate(freq = (n / sum(n)) * 100)

  lists_df %>% filter(univ_id=='145637',order_no=='456700',is_hispanic_origin=='Yes') %>% count(race) %>% print(n=50)
  
  
#### urbana IL URM 1030-1190 August 2018
  
  lists_df %>% filter(univ_id=='145637',order_no=='403330') %>% count()
  
  lists_df %>% filter(univ_id=='145637',order_no=='403330') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  lists_df %>% filter(univ_id=='145637',order_no=='403330') %>% count(is_hispanic_origin) %>% mutate(freq = (n / sum(n)) * 100)
  
#############################
#############################

##### UI-CHICAGO, 392835, ordered 7/9/18
  # SAT 1310-1600, B- to A+, 2019 and 2020 HS grad class
  
  lists_df %>% filter(univ_id=='145600',order_no=='392835') %>% count()
  
  
  lists_df %>% filter(univ_id=='145600',order_no=='392835') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
####### UI-CHICAGO, 488035, ordered 7/19/2019
  # SAT 1160-1300, B- to A+, 2020 and 2021 HS grad class
  
  lists_df %>% filter(univ_id=='145600',order_no=='488035') %>% count()
  
  lists_df %>% filter(univ_id=='145600',order_no=='488035') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  
##### urbana   IL 1160-1270 Feb 2019
  # in state, PSAT/SAT = 1160 - 1270; race = asian/white/other/native and NOT hispanic, 2021,2020, 2022 HS grad class
  lists_df %>% filter(univ_id=='145637',order_no=='456700') %>% count()
  
  lists_df %>% filter(univ_id=='145637',order_no=='456700') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  lists_df %>% filter(univ_id=='145637',order_no=='456700') %>% count(is_hispanic_origin) %>% mutate(freq = (n / sum(n)) * 100)

  lists_df %>% filter(univ_id=='145637',order_no=='456700',is_hispanic_origin=='Yes') %>% count(race) %>% print(n=50)  
  
  
# Illinois State University data (another example of original College Board race vars)
load(file.path(data_dir, '145813_data.RData'))

lists_df %>% filter(univ_id=='145813') %>% count(order_no)


orders_df_145813 %>% filter(state_name=='IL') %>% select(univ_id,order_num,order_title,num_students,date_start,date_end,hs_grad_class,psat_score_min,psat_score_max,sat_score_min,sat_score_max,gpa_low,gpa_high,race_ethnicity) %>% View()
  

# Illinois state, 
  # 387264 SAT Redbird Fall 2019, HS class 2019, SAT 1200-1380, B- to A+


  lists_df %>% filter(univ_id=='145813',order_no=='387264') %>% count()
  lists_df %>% filter(univ_id=='145813',order_no=='387264') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  
#ILLINOIS STATE. ORDER= 320627, Redbird Scholarship 2018, HS class 2018, SAT 1200-1380, B- to A+  
  lists_df %>% filter(univ_id=='145813',order_no=='320627') %>% count()
  lists_df %>% filter(univ_id=='145813',order_no=='320627') %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
#ILLINOIS STATE. ORDER= 483384, SAT University 2020, HS class 2020, SAT 1200-1380, B- to A+  
  #American Indian or Alaska Native| Asian (including Indian subcontinent and Philippines origin)|Cuban| Black or African American| Hispanic or Latino (including Spanish origin)| Mexican| Puerto Rican| Other Hispanic or Latino| Native Hawaiian or Other Pacific Islander
  
  lists_df %>% filter(univ_id=='145813',order_no=='483384') %>% count()
  lists_df %>% filter(univ_id=='145813',order_no=='483384',race_cb %in% c(0,2)==0) %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)

  
  # urbana, order no = 500488, PAP URM 1200-1380 2020
  
  lists_df %>% filter(univ_id=='145637',order_no=='500488',race_cb %in% c(0)==0) %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  lists_df %>% filter(univ_id=='145637',order_no=='500488') %>% count(grad_year)
  
  
    lists_df %>% filter(univ_id=='145637') %>% count(grad_year)
    
    
    
# ILLINOIS STATE: HS class of 2018,2019,2020, SAT 1200-1380, B- to A+  
    
  lists_df %>% filter(univ_id=='145813',order_no %in% c('320627','387264','483384')) %>% count(order_no)
  
  lists_df %>% filter(univ_id=='145813',order_no %in% c('320627','387264','483384')) %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
  
  lists_df %>% filter(univ_id=='145813',order_no %in% c('320627','387264','483384'),race_cb %in% c(0,2,9,NA)==0) %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
#                                  race_cb     n   freq
#1  1 [American Indian/Alaska Native]       37  0.660
#2  3 [Black/African American]             978 17.5  
#3  4 [Hispanic/Latino]                   3564 63.6  
#4  8 [Native Hawaiian/Pacific Islander]    12  0.214
#5 12 [two or more races, non-Hispanic]   1012 18.1
  
# URBANA, HS class 2018, 2019, 2021,2020,2023,2022, PSAT/SAT 1200-1380, B- to A+; 2nd 5th to highest 10th class rank
  
  lists_df %>% filter(univ_id=='145637',order_no %in% c('371093','327700','383138','500488','541012','567373','456650','470081')) %>% count(order_no)
  
  
  lists_df %>% filter(univ_id=='145637',order_no %in% c('371093','327700','383138','500488','541012','567373','456650','470081'),race_cb %in% c(0,2,9)==0) %>% count(race_cb) %>% mutate(freq = (n / sum(n)) * 100)
    
#                                race_cb     n   freq
#1  1 [American Indian/Alaska Native]       26  0.972
#2  3 [Black/African American]             458 17.1  
#3  4 [Hispanic/Latino]                   1943 72.7  
#4  8 [Native Hawaiian/Pacific Islander]    13  0.486
#5 12 [two or more races, non-Hispanic]    234  8.75   
  
# summary
  # IL state has lower % hispanic and higher two or more races non-hispanic
  
# what is the potential probelm
  # we assumed propsects from urbana w/ missing hispanic_origin are not hispanic, but some may be, so maybe our numbers of pct hispanic are too low
  # but in above table %hispanic is for urbana is higher than it is for Illinois State
  