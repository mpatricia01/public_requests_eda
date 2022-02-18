library(tidyverse)


data_dir <- file.path('.', 'data')


ceeb_nces <- read_csv(file.path(data_dir, 'ceeb_nces_crosswalk.csv'))


# Arizona State University (104151)
load(file = file.path(data_dir, '104151_data.RData'))

lists_df_104151 %>% count(nchar(hs_code))

lists_df_104151 %>%  # When pad 5 digits, about 0.99 are 6 digits in length (1557770 rows)
  mutate(hs_code = case_when(
    nchar(hs_code) == 5 ~ str_pad(hs_code, width = 6, side = 'left', pad = '0'),
    TRUE ~ hs_code
  )) %>% 
  semi_join(ceeb_nces, by = c('hs_code' = 'ceeb')) %>% 
  nrow()  # 1511244 / 1557770 = 0.97 of the 6-digit codes merged


# University of Illinois at Chicago (145600)
load(file = file.path(data_dir, '145600_data.RData'))

lists_df_145600 %>% count(nchar(hs_code))

lists_df_145600 %>%  # When pad 5 digits, about 0.97 are 6 digits in length (239705 rows)
  mutate(hs_code = case_when(
    nchar(hs_code) == 5 ~ str_pad(hs_code, width = 6, side = 'left', pad = '0'),
    TRUE ~ hs_code
  )) %>% 
  semi_join(ceeb_nces, by = c('hs_code' = 'ceeb')) %>% 
  nrow()  # 211216 / 239705 = 0.88 of the 6-digit codes merged


# Illinois State University (145813)
load(file = file.path(data_dir, '145813_data.RData'))

lists_df_145813 %>% count(nchar(hs_code))

lists_df_145813 %>%  # When pad 5 digits, about 0.999 are 6 digits in length (153946 rows)
  mutate(hs_code = case_when(
    nchar(hs_code) == 5 ~ str_pad(hs_code, width = 6, side = 'left', pad = '0'),
    TRUE ~ hs_code
  )) %>% 
  semi_join(ceeb_nces, by = c('hs_code' = 'ceeb')) %>% 
  nrow()  # 150836 / 153946 = 0.98 of the 6-digit codes merged


# Texas A & M University-Texarkana (224545)
load(file = file.path(data_dir, '224545_data.RData'))

lists_df_224545 %>% count(nchar(hs_code))

lists_df_224545 %>%  # When pad 5 digits, about 0.999 are 6 digits in length (171496 rows)
  mutate(hs_code = case_when(
    nchar(hs_code) == 5 ~ str_pad(hs_code, width = 6, side = 'left', pad = '0'),
    TRUE ~ hs_code
  )) %>% 
  semi_join(ceeb_nces, by = c('hs_code' = 'ceeb')) %>% 
  nrow()  # 160429 / 171496 = 0.94 of the 6-digit codes merged
