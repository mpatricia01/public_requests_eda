library(tidyverse)


# Order summary fields

# 'univ_id', 'po_num', 'order_num', 'order_title', 'created_by', 'date_updated',
# 'order_cost', 'num_runs', 'num_students',
# 'date_start', 'date_end', 'hs_grad_class', 'zip_code_file', 'zip_code', 'county',
# 'state_name', 'cbsa_name', 'intl_region', 'segment', 'race_ethnicity', 'gender',
# 'sat_score_min', 'sat_score_max', 'sat_score_old_min', 'sat_score_old_max',
# 'psat_score_min', 'psat_score_max', 'psat_score_old_min', 'psat_score_old_max',
# 'gpa_high', 'gpa_low', 'rank_high', 'rank_low',
# 'source_file', 'market'  (user-created)

# Order list fields

# 'univ_id', 'order_no', 'run_no', 'student_id',
# 'city', 'state', 'zip', 'country', 'province', 'county_code', 'post_del', 'post_corr',
# 'gender', 'grad_year', 'hs_code', 'geomarket', 'score_range',
# 'cuban', 'mexican', 'puerto_rican', 'other_hispanic', 'non_hispanic', 'american_indian', 'asian', 'black', 'native_hawaiian', 'white', 'other', 'race_no_response', 'ethnicity_no_response',
# 'major_1', 'major_2', 'major_3', 'ap1', 'ap2', 'ap3', 'ap4', 'ap5', 'ap6', 'ap7', 'ap8', 'ap9', 'ap10', 'satsub1', 'satsub2', 'satsub3', 'satsub4', 'satsub5', 'satsub6', 'satsub7', 'satsub8', 'satsub9', 'satsub10',
# 'name_source', 'update_date', 'homeschool', 'low_ses', 'hs_cluster', 'en_cluster', 'nhrp', 'first_gen', 'pltw', 'interest_me', 'pref_inst1', 'pref_inst2', 'pref_inst3', 'pref_inst4', 'pref_inst5',
# 'source', 'order_date', 'race', 'is_hispanic_origin' (urbana specific)
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


# Combine data
orders_df <- dplyr::bind_rows(
  orders_df_145637, orders_df_224545, orders_df_228431
)

lists_df <- dplyr::bind_rows(
  lists_df_145637, lists_df_224545, lists_df_228431
) 


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
