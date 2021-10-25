library(tidyverse)


univ_id <- '148654'

# Directory paths
data_dir <- file.path('.', 'data')

# Read in data
lists_df <- read_csv(file.path(data_dir, '148654_lists.csv'), col_types = cols(.default = 'c'))
orders_df <- read_csv(file.path(data_dir, '148654_orders.csv'), col_types = c('univ_id' = 'c', 'order_num' = 'c', 'hs_grad_class' = 'c'))
markets_df <- read_csv(file.path(data_dir, '148654_markets.csv'))

# Inspect orders
unique(orders_df$zip_code_file)
unique(orders_df$zip_code)

# Primary: '622|620|627|626|625'
# Chicagoland: '601|600|606|605|604|603|602'
# Rest of IL: '623|610|612|611|614|609|613|608|616'
# Secondary: '637|636|651|633|634|631|476|475|478'

remove_NA_cols <- function(data_df) {
  data_df[!sapply(data_df, function(x) all(is.na(x)))]
}

orders_df_148654 <- orders_df %>% mutate(
  market = case_when(
    zip_code == '622|620|627|626|625' ~ 'primary',
    zip_code == '601|600|606|605|604|603|602' | (zip_code_file == 'Chicagoland.txt') ~ 'chicagoland',
    zip_code == '623|610|612|611|614|609|613|608|616' | (zip_code_file == 'Rest of IL.txt') ~ 'rest_of_il',
    zip_code == '637|636|651|633|634|631|476|475|478' | (zip_code_file == 'Secondary.txt') ~ 'secondary'
  )
) %>%
  arrange(date_start, hs_grad_class, sat_score_max, psat_score_min) %>%
  select(-created_by, -date_updated) %>% 
  remove_NA_cols()

# Split orders by market
# primary_orders <- orders_df %>% filter(market == 'primary') %>% remove_NA_cols()
# chicagoland_orders <- orders_df %>% filter(market == 'chicagoland') %>% remove_NA_cols()
# rest_of_IL_orders <- orders_df %>% filter(market == 'rest_of_IL') %>% remove_NA_cols()
# secondary_orders <- orders_df %>% filter(market == 'secondary') %>% remove_NA_cols()
# other_orders <- orders_df %>% filter(is.na(market)) %>% remove_NA_cols()

# save(primary_orders, chicagoland_orders, rest_of_IL_orders, secondary_orders, other_orders, file = file.path(data_dir, '148654_uis_orders.RData'))

# Inspect lists
View(lists_df %>% count(`Segment Description`))

lists_df <- lists_df %>%  # IL purchases from 4 markets above
  mutate(
    market = str_extract(`Segment Description`, 'primary|chicagoland|rest of il|secondary') %>% str_replace_all(' ', '_'),
    grade_level = str_extract(`Segment Description`, 'seniors|juniors|sophomores'),
    test_type = case_when(
      str_detect(`Segment Description`, '[^\\d]+\\d{3}') ~ 'sat',
      str_detect(`Segment Description`, '[^\\d]+\\d{2}') ~ 'act'
    )
  )

View(lists_df %>% select(`Segment Description`, market, grade_level, test_type) %>% distinct())

View(lists_df %>% filter(State == 'CA'))  # CA purchases mostly come from '2019 application - pre names from client' or '2019 application - inquiry pool d1', etc. - unknown source + race/ethnicity unknown for those

# Known SAT students
lists_df_148654 <- lists_df %>% filter(test_type == 'sat') %>% select(-test_type) %>% remove_NA_cols()

View(lists_df_148654 %>% count(State) %>% arrange(-n))  # should all be from the 4 IL/surrounding markets, but not always

names(lists_df_148654) <- names(lists_df_148654) %>% str_to_lower() %>% str_replace_all(' |-', '_')

lists_df_148654 <- lists_df_148654 %>% 
  rename(
    'student_id' = 'rc_id',
    'source' = 'segment_description',
    'country' = 'country_name',
    'hs_code' = 'hs_ceeb_code',
    'hs_address' = 'hs_address_1',
    'race' = 'ethnicity'
  ) %>% 
  mutate(
    univ_id = univ_id
  )

names(lists_df_148654)

table(lists_df_148654$list_name, useNA = 'always')
table(lists_df_148654$letter_code, useNA = 'always')
table(lists_df_148654$country, useNA = 'always')
table(lists_df_148654$race, useNA = 'always')
table(lists_df_148654$market, useNA = 'always')
table(lists_df_148654$grade_level, useNA = 'always')
table(lists_df_148654$univ_id, useNA = 'always')

# Save data
save(lists_df_148654, orders_df_148654, file = file.path(data_dir, str_c(univ_id, '_data.RData')))
