library(tidyverse)


# Order summary fields

# 'univ_id', 'po_num', 'order_num', 'order_title', 'created_by', 'date_updated',
# 'order_cost', 'num_runs', 'num_students',
# 'date_start', 'date_end', 'hs_grad_class', 'zip_code_file', 'zip_code',
# 'state_name', 'cbsa_name', 'county', 'geomarket', 'intl_region', 'segment', 'race_ethnicity', 'gender',
# 'sat_score_min', 'sat_score_max', 'sat_score_old_min', 'sat_score_old_max',
# 'psat_score_min', 'psat_score_max', 'psat_score_old_min', 'psat_score_old_max',
# 'sat_score_reading_min', 'sat_score_reading_max',
# 'gpa_high', 'gpa_low', 'rank_high', 'rank_low', 'ap_scores',
# 'college_type', 'college_size', 'college_location', 'college_setting', 'college_control', 'college_student_body', 'college_living_plans',
# 'edu_aspirations', 'rotc_plans', 'major', 'low_ses', 'financial_aid', 'citizenship', 'national_recognition_programs'
# 'source_file', 'market'  (user-created)

# Order list fields

# 'univ_id', 'order_no', 'run_no', 'student_id',
# 'city', 'state', 'zip', 'country', 'province', 'county_code', 'post_del', 'post_corr',
# 'gender', 'grad_year', 'hs_code', 'geomarket', 'score_range',
# 'cuban', 'mexican', 'puerto_rican', 'other_hispanic', 'non_hispanic', 'american_indian', 'asian', 'black', 'native_hawaiian', 'white', 'other', 'race_no_response', 'ethnicity_no_response',
# 'major_1', 'major_2', 'major_3', 'ap1', 'ap2', 'ap3', 'ap4', 'ap5', 'ap6', 'ap7', 'ap8', 'ap9', 'ap10', 'satsub1', 'satsub2', 'satsub3', 'satsub4', 'satsub5', 'satsub6', 'satsub7', 'satsub8', 'satsub9', 'satsub10',
# 'name_source', 'update_date', 'homeschool', 'low_ses', 'hs_cluster', 'en_cluster', 'nhrp', 'first_gen', 'pltw', 'interest_me', 'pref_inst1', 'pref_inst2', 'pref_inst3', 'pref_inst4', 'pref_inst5', 'nrp_afam', 'nrp_hisp', 'nrp_indg', 'nrp_rurl'
# 'source', (urbana/college station/springfield specific)
# 'order_date', 'is_hispanic_origin' (urbana specific)
# 'race', (urbana/moorhead/college station/springfield specific)
# 'entry_term', 'family_income', (college station specific)
# 'hs_name', (college station/springfield specific)
# 'list_name', 'letter_code', 'hs_address', 'hs_city', 'hs_state', 'hs_country', 'hs_zip', (springfield specific)
# 'source_file', 'zip_code', 'market', 'grade_level' (user-created)


data_dir <- file.path('.', 'data')

# variable names (all variables, across all universities)
  # comment this out because uses dfs orders_df and lists_df, which aren't created until bottom of this script
  # order_names <- names(orders_df) %>% as.tibble() %>% rename(var_name=value) %>% filter(!var_name %in% c('univ_name','univ_state','univ_zip','univ_c15basic'))
    #order_names %>% print(n=50)

  # list_names <- names(lists_df)  %>% as.tibble() %>% rename(var_name=value) %>% filter(!var_name %in% c('race','is_hispanic_origin','source','student_id','score_range','order_date','family_income','hs_name','entry_term','zip_code'))


# University of Illinois at Urbana-Champaign (145637)
load(file = file.path(data_dir, '145637_data.RData'))

n_distinct(orders_df_145637$order_num)  # 80 order summaries
n_distinct(lists_df_145637$order_no)  # 92 lists

# 3 order summaries w/ no lists, but these look like draft orders that weren't actually placed (i.e., 'Edit name' in the title)
View(anti_join(orders_df_145637, lists_df_145637, by = c('order_num' = 'order_no')))

# 15 orders where we have list but no order summary (mostly from early 2017 - currently requesting)
View(anti_join(lists_df_145637, orders_df_145637, by = c('order_no' = 'order_num')) %>% select(order_no, order_date) %>% distinct())


# Texas A & M University-Texarkana (224545)
load(file = file.path(data_dir, '224545_data.RData'))

n_distinct(orders_df_224545$order_num)  # 91 order summaries
n_distinct(lists_df_224545$order_no)  # 91 lists

# Originally provided us 93 pairs of order summaries/lists (i.e., source_file column in orders_df_224545 & lists_df_224545)
# But Order59/List59 are exact duplicates of Order58/List58 so were excluded from above dataframes
# Order93 was excluded from orders_df_224545 because it is a duplicate of Order62 instead of the correct summary for List93
# List60 was excluded from lists_df_224545 because it is a duplicate of List72 instead of the correct list for Order60
# Currently following up on these errors

# potentially missing variables (not all important) [ozan 10/18/2021]
   #list_names %>% anti_join(y=(names(lists_df_224545) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=50)
    # lists df: 


# Stephen F Austin State University (228431)
load(file = file.path(data_dir, '228431_data.RData'))

n_distinct(orders_df_228431$order_num)  # 16 order summaries
n_distinct(lists_df_228431$order_no)  # 15 lists

# 1 order summary w/o list (does say "duplicated" in title so maybe that's why? currently following up)
View(anti_join(orders_df_228431, lists_df_228431, by = c('order_num' = 'order_no')))


# Minnesota State University-Moorhead (174358)
load(file = file.path(data_dir, '174358_data.RData'))

n_distinct(orders_df_174358$order_num)  # 2 order summaries
n_distinct(lists_df_174358$order_no)  # 2 lists

# Note: Only received 2 College Board PSAT orders for 2017 and 2018, both have 7 runs


# University of Minnesota-Crookston (174075)
load(file = file.path(data_dir, '174075_data.RData'))

n_distinct(orders_df_174075$order_num)  # 1 order summary
n_distinct(lists_df_174075$order_no)  # 1 list

# Note: Only received 1 College Board order from 2019
# Order summary's geography criteria was cut off (i.e., 'View all' not expanded) so need to update state_name in orders_df_174075 once we request expanded version


# University of California-San Diego (110680)
load(file = file.path(data_dir, '110680_data.RData'))

n_distinct(orders_df_110680$order_num)  # 48 order summaries
n_distinct(lists_df_110680$order_no)  # 104 lists

# The last 3 order summaries look like draft orders that weren't actually placed (i.e., 'Edit name' in the title)
orders_df_110680$order_title %>% tail(3)

# Also 3 other orders that resulted in 0 students, but other than that, we did receive the corresponding lists for the remaining 42 orders
orders_df_110680 %>% select(order_num, order_title, num_students) %>% left_join(lists_df_110680 %>% group_by(order_no) %>% summarise(num_students_in_list = n()), by = c('order_num' = 'order_no')) %>% View()

# This leaves 62 of 104 lists that we did not receive order summaries for
orders_df_110680 %>% select(order_num, order_title, num_students) %>% right_join(lists_df_110680 %>% group_by(order_no) %>% summarise(num_students_in_list = n()), by = c('order_num' = 'order_no')) %>% View()

# Note: Currently requesting order summaries for the remaining lists and any other order summaries/lists we are missing for the full timeframe


# Tarleton State University (228529)
load(file = file.path(data_dir, '228529_data.RData'))

n_distinct(orders_df_228529$order_num)  # 9 order summaries
n_distinct(lists_df_228529$order_no)  # 3 lists

# Only 1 order overlap between received order summaries and lists
base::intersect(orders_df_228529$order_num, lists_df_228529$order_no)

# Order summary's geography criteria was cut off (i.e., 'View all' not expanded) so need to update orders_df_228529 once we request expanded version
# Also need to request those corresponding order summaries/lists that we're missing
# 10/19/2021: grad_year and many other variables NA for about 40,000 obs

# potentially missing variables (not all important) [ozan 10/18/2021]
  #order_names %>% anti_join(y=(names(orders_df_228529) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=50)
    # orders df: 
  #list_names %>% anti_join(y=(names(lists_df_228723) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=80)
    # lists df: 


# Arizona State University (104151)
load(file = file.path(data_dir, '104151_data.RData'))

n_distinct(orders_df_104151$order_num)  # 131 order summaries
n_distinct(lists_df_104151$order_no)  # 193 lists

# potentially missing variables (not all important) [ozan 10/18/2021]
   #list_names %>% anti_join(y=(names(lists_df_104151) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=50)
    # lists df: hs_code, 


# Texas A & M University-College Station (228723)
load(file = file.path(data_dir, '228723_data.RData'))

n_distinct(orders_df_228723$order_num)  # 28 order summaries
n_distinct(lists_df_228723$order_no)  # 48 lists

# 29 lists w/o order summaries
lists_df_228723 %>% select(source, order_no) %>% distinct() %>% anti_join(orders_df_228723 %>% select(order_num, order_title, date_start), by = c('order_no' = 'order_num')) %>% View()

# 9 order summaries w/o lists
orders_df_228723 %>% select(order_num, order_title, date_start) %>% anti_join(lists_df_228723 %>% select(source, order_no) %>% distinct(), by = c('order_num' = 'order_no')) %>% View()

# potentially missing variables (not all important) [ozan 10/18/2021]
  #order_names %>% anti_join(y=(names(orders_df_228723) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=50)
    # orders df: date_updated, source_file
  #list_names %>% anti_join(y=(names(lists_df_228723) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=80)
    # lists df: gender, cuban, mexican, puerto_rican, other_hispanic, non_hispanic, american_indian, asian, black, native_hawaiian, white, other, race_no_response, ethnicity_no_response, grad_year, homeschool, low_ses
  # race variable for college station is way different
  #lists_df_228723 %>% glimpse()
  #lists_df_228723 %>% count(race)
  #lists_df_228723 %>% count(entry_term)
    # use var entry_term to create grad_year


# University of California-Davis (110644)
load(file = file.path(data_dir, '110644_data.RData'))

n_distinct(orders_df_110644$order_num)  # 5 order summaries
  # potential variables missing on order summary data [ozan 10/18/2021]
    # num_runs, date_end, created_by, date_updated, source_file

n_distinct(lists_df_110644$order_no)  # 5 lists

# Only have 5 College Board order summaries from Oct 2020 so far

# potentially missing variables (not all important) [ozan 10/18/2021]
  #order_names %>% anti_join(y=(names(orders_df_110644) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=50)
    # orders df: # num_runs, date_end, created_by, date_updated, source_file
   #list_names %>% anti_join(y=(names(lists_df_110644) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=50)
    # lists df: gender, geomarket, city, state, low_ses, post_del, post_corr, other (race category), name_source, update_date, low_ses
    # 


# University of Illinois at Chicago (145600)
load(file = file.path(data_dir, '145600_data.RData'))

n_distinct(orders_df_145600$order_num)  # 18 order summaries
n_distinct(lists_df_145600$order_no)  # 25 lists

# potentially missing variables (not all important) [ozan 10/18/2021]
   #list_names %>% anti_join(y=(names(lists_df_145600) %>% as.tibble() %>% rename(var_name=value)), by = 'var_name') %>% print(n=50)
    # lists df: non important missing
    # note: grad_year is two-digit rather than four-digit
lists_df_145600 %>% glimpse()


# University of Illinois at Springfield (148654)
load(file = file.path(data_dir, '148654_data.RData'))

n_distinct(orders_df_148654$order_num)  # 188 order summaries
# n_distinct(lists_df_148654$order_no)  # no order number on lists

# Some orders used zip code files in filters - can infer what the zips are for the last 3 files that are for the same markets
View(orders_df_148654 %>% select(zip_code_file, zip_code, market) %>% distinct())


# Northeastern Illinois University (147776)
load(file = file.path(data_dir, '147776_data.RData'))

n_distinct(orders_df_147776$order_num)  # 1 order summary
n_distinct(lists_df_147776$order_no)  # 1 list

# Only received a single 2018 CB order - note: made up the order number just to be able to link them
orders_df_147776$order_num


# Illinois State University (145813)
load(file = file.path(data_dir, '145813_data.RData'))

n_distinct(orders_df_145813$order_num)  # 17 order summaries
n_distinct(lists_df_145813$order_no)  # 17 lists


# Combine data
orders_df <- dplyr::bind_rows(
  orders_df_145637, orders_df_224545, orders_df_228431, orders_df_174358, orders_df_174075, orders_df_110680, orders_df_228529, orders_df_104151, orders_df_228723, orders_df_110644, orders_df_145600, orders_df_148654, orders_df_147776, orders_df_145813
)

lists_df <- dplyr::bind_rows(
  lists_df_145637, lists_df_224545, lists_df_228431, lists_df_174358, lists_df_174075, lists_df_110680, lists_df_228529, lists_df_104151, lists_df_228723, lists_df_110644, lists_df_145600, lists_df_148654, lists_df_147776, lists_df_145813
)

names(orders_df)
names(lists_df)
table(orders_df$univ_id, useNA = 'always')
table(lists_df$univ_id, useNA = 'always')
table(lists_df$update_date, useNA = 'always')
table(lists_df$grad_year, useNA = 'always')


# Add zip_code variable extracting 5-digit zip code
lists_df <- lists_df %>% 
  mutate(
    zip_code = case_when(
      str_to_lower(country) == 'united states' | is.na(country) ~ str_pad(str_sub(zip, end = 5), width = 5, side = 'left', pad = '0'),
      TRUE ~ NA_character_
    ),
    grad_year = if_else(nchar(grad_year) == 2, str_c('20', grad_year), grad_year)
  )

table(lists_df$grad_year, useNA = 'always')


# Save dataframes
save(orders_df, lists_df, file = file.path(data_dir, 'combined_data.RData'))
