library(tidyverse)


# Order summary fields

# 'univ_id', 'po_num', 'order_num', 'order_title', 'created_by', 'date_updated',
# 'order_cost', 'num_runs', 'num_students',
# 'date_start', 'date_end', 'hs_grad_class', 'zip_code_file', 'zip_code',
# 'state_name', 'cbsa_name', 'county', 'geomarket', 'intl_region', 'segment', 'race_ethnicity', 'gender',
# 'sat_score_min', 'sat_score_max', 'sat_score_old_min', 'sat_score_old_max',
# 'psat_score_min', 'psat_score_max', 'psat_score_old_min', 'psat_score_old_max',
# 'gpa_high', 'gpa_low', 'rank_high', 'rank_low', 'ap_scores',
# 'college_type', 'edu_aspirations', 'rotc_plans', 'major', 'citizenship', 'national_recognition_programs'
# 'source_file', 'market'  (user-created)

# Order list fields

# 'univ_id', 'order_no', 'run_no', 'student_id',
# 'city', 'state', 'zip', 'country', 'province', 'county_code', 'post_del', 'post_corr',
# 'gender', 'grad_year', 'hs_code', 'geomarket', 'score_range',
# 'cuban', 'mexican', 'puerto_rican', 'other_hispanic', 'non_hispanic', 'american_indian', 'asian', 'black', 'native_hawaiian', 'white', 'other', 'race_no_response', 'ethnicity_no_response',
# 'major_1', 'major_2', 'major_3', 'ap1', 'ap2', 'ap3', 'ap4', 'ap5', 'ap6', 'ap7', 'ap8', 'ap9', 'ap10', 'satsub1', 'satsub2', 'satsub3', 'satsub4', 'satsub5', 'satsub6', 'satsub7', 'satsub8', 'satsub9', 'satsub10',
# 'name_source', 'update_date', 'homeschool', 'low_ses', 'hs_cluster', 'en_cluster', 'nhrp', 'first_gen', 'pltw', 'interest_me', 'pref_inst1', 'pref_inst2', 'pref_inst3', 'pref_inst4', 'pref_inst5', 'nrp_afam', 'nrp_hisp', 'nrp_indg', 'nrp_rurl'
# 'source', (urbana/college station specific)
# 'order_date', 'is_hispanic_origin' (urbana specific)
# 'race', (urbana/moorhead/college station specific)
# 'entry_term', 'hs_name', 'family_income', (college station specific)
# 'source_file', 'zip_code' (user-created)


data_dir <- file.path('.', 'data')


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


# Arizona State University (104151)
load(file = file.path(data_dir, '104151_data.RData'))


# Texas A & M University-College Station (228723)
load(file = file.path(data_dir, '228723_data.RData'))

n_distinct(orders_df_228723$order_num)  # 28 order summaries
n_distinct(lists_df_228723$order_no)  # 48 lists

# 29 lists w/o order summaries
lists_df_228723 %>% select(source, order_no) %>% distinct() %>% anti_join(orders_df_228723 %>% select(order_num, order_title, date_start), by = c('order_no' = 'order_num')) %>% View()

# 9 order summaries w/o lists
orders_df_228723 %>% select(order_num, order_title, date_start) %>% anti_join(lists_df_228723 %>% select(source, order_no) %>% distinct(), by = c('order_num' = 'order_no')) %>% View()


# Combine data
orders_df <- dplyr::bind_rows(
  orders_df_145637, orders_df_224545, orders_df_228431, orders_df_174358, orders_df_174075, orders_df_110680, orders_df_228529, orders_df_228723
)

lists_df <- dplyr::bind_rows(
  lists_df_145637, lists_df_224545, lists_df_228431, lists_df_174358, lists_df_174075, lists_df_110680, lists_df_228529, lists_df_104151, lists_df_228723
)

names(orders_df)
names(lists_df)
table(orders_df$univ_id, useNA = 'always')
table(lists_df$univ_id, useNA = 'always')
table(lists_df$update_date, useNA = 'always')


# Add zip_code variable extracting 5-digit zip code
lists_df <- lists_df %>% 
mutate(
  zip_code = case_when(
    str_to_lower(country) == 'united states' | is.na(country) ~ str_pad(str_sub(zip, end = 5), width = 5, side = 'left', pad = '0'),
    TRUE ~ NA_character_
  )
)


# Save dataframes
save(orders_df, lists_df, file = file.path(data_dir, 'combined_data.RData'))
