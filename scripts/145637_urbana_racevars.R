library(tidyverse)


data_dir <- file.path('.', 'data')


# UI-Urbana data
load(file.path(data_dir, '145637_data.RData'))

lists_df_145637 %>% count(is.na(race))  # Missing race: 24855 / 415432 = 0.06
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
