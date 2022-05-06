library(tidyverse)
library(usmap)
library(haven)


data_dir <- file.path('.', 'data')
scripts_dir <- file.path('.', 'scripts')


source(file = file.path(scripts_dir, 'create_secondary_datasets.R'))
source(file = file.path(scripts_dir, 'create_combined_order_list_analysis_datasets.R'))


acs_income_zip <- read_csv(file.path(data_dir, 'acs_income_zip.csv'), col_types = c('state_fips_code' = 'c', 'zip_code' = 'c'), na = c('-666666666'))
acs_income_zip$medincome_2564 <- rowMeans(acs_income_zip[, c('medincome_2544', 'medincome_4564')], na.rm = T)
acs_income_zip$medincome_2564[is.nan(acs_income_zip$medincome_2564)] <- NA

acs_income_metro <- read_csv(file.path(data_dir, 'acs_income_metro.csv'), col_types = c('cbsa_code' = 'c'), na = c('-666666666'))
acs_income_metro$medincome_2564 <- rowMeans(acs_income_metro[, c('medincome_2544', 'medincome_4564')], na.rm = T)
acs_income_metro$medincome_2564[is.nan(acs_income_metro$medincome_2564)] <- NA

zip_locale <- read_sas(file.path(data_dir, 'EDGE_ZCTALOCALE_2021_LOCALE.sas7bdat'))

ccd <- readRDS(file.path(data_dir, 'ccd_membership_1718.RDS')) %>% 
  select(-total_students) %>% 
  left_join(readRDS(file.path(data_dir, 'ccd_1718.RDS')) %>% select(ncessch, matches('g\\d{2}_|total_')), by = 'ncessch') %>% 
  left_join(zip_cbsa_name_data, by = c('lzip' = 'zip_code'))

pss <- readRDS(file.path(data_dir, 'pss_1718.RDS')) %>% 
  left_join(zip_cbsa_name_data, by = 'zip_code')


# --------------------
# Prepare univ sample
# --------------------

rm(lists_orders_df, lists_orders_zip_df)

research_univs <- c(
  '104151',  # Arizona State University-Tempe
  '228723',  # Texas A & M University-College Station
  '110644',  # University of California-Davis
  '110653',  # University of California-Irvine
  '110680',  # University of California-San Diego
  '145600',  # University of Illinois at Chicago
  '145637'   # University of Illinois at Urbana-Champaign
)

regional_univs <- c(
  '145813',  # Illinois State University
  '147776',  # Northeastern Illinois University
  '105330',  # Northern Arizona University
  '228431',  # Stephen F Austin State University
  '228529',  # Tarleton State University
  '224545',  # Texas A&M University-Texarkana
  '148654'   # University of Illinois at Springfield
)

orders_df <- orders_df %>% 
  filter(univ_id %in% c(research_univs, regional_univs), !str_detect(order_title, 'Edit name')) %>% 
  mutate_if(is.character, list(~na_if(., ''))) %>% 
  mutate(
    univ_type = if_else(univ_id %in% research_univs, 'research', 'regional'),
    univ_label = recode_factor(
      univ_type,
      'research' = 'Research',
      'regional' = 'MA/doctoral'
    ),
    filter_hsgrad_class = if_else(!is.na(hs_grad_class), 1, 0),
    filter_zip = if_else(!is.na(zip_code) | !is.na(zip_code_file), 1, 0),
    filter_states_fil = if_else(!is.na(state_name), 1, 0), 
    filter_cbsa = if_else(!is.na(cbsa_name), 1, 0), 
    filter_intl = if_else(!is.na(intl_region), 1, 0), 
    filter_segment = if_else(!is.na(segment), 1, 0), 
    filter_race = if_else(!is.na(race_ethnicity), 1, 0), 
    filter_gender = if_else(!is.na(gender), 1, 0), 
    filter_sat = if_else(!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max) | !is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max) | !is.na(sat_score_reading_writing_min) | !is.na(sat_score_reading_writing_max) | !is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max) | !is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max), 1, 0), 
    filter_psat = if_else(!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max), 1, 0), 
    filter_gpa = if_else(!is.na(gpa_low) | !is.na(gpa_high), 1, 0), 
    filter_rank = if_else(!is.na(rank_low) | !is.na(rank_high), 1, 0), 
    filter_geomarket = if_else(!is.na(geomarket), 1, 0), 
    filter_ap_score = if_else(!is.na(ap_scores), 1, 0)
  )

lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% 
  filter(univ_id %in% c(research_univs, regional_univs)) %>%
  mutate_if(is.character, list(~na_if(., ''))) %>% 
  mutate(
    univ_type = if_else(univ_id %in% research_univs, 'research', 'regional'),
    univ_label = recode_factor(
      univ_type,
      'research' = 'Research',
      'regional' = 'MA/doctoral'
    ),
    stu_nonres = case_when(
      stu_in_us == 0 ~ NA_real_,
      univ_state == stu_state ~ 0,
      univ_state != stu_state ~ 1
    ),
    region = if_else(stu_in_us == 0, 'international', 'domestic'),
    locale = if_else(stu_in_us == 0 | stu_nonres == 1, 'outofstate', 'instate'),
    stu_women_dummy = case_when(
      stu_gender %in% c('F', 'Female') ~ 1,
      stu_gender %in% c('M', 'Male') ~ 0,
      TRUE ~ NA_real_
    )
  )


# ----------------------------------------------------------------------
# Figure 7 - Orders and prospects purchased by research vs. ma/doctoral
# ----------------------------------------------------------------------

orders_prospects_purchased <- orders_df %>% 
  group_by(univ_id, univ_type, univ_label) %>% 
  summarise(
    total_orders = n(),
    total_students = sum(num_students, na.rm = T)
  ) %>% 
  ungroup()


# -----------------------------------------------------------------------
# Figure 8 - Filters used in order purchases by research vs. ma/doctoral
# -----------------------------------------------------------------------

orders_filters <- orders_df %>% 
  select(univ_type, univ_label, starts_with('filter_')) %>% 
  group_by(univ_type, univ_label) %>% 
  summarize_if(is.numeric, sum) %>% 
  pivot_longer(
    cols = -c(univ_type, univ_label),
    names_to = 'filters',
    names_prefix = 'filter_',
    values_to = 'num'
  ) %>% 
  left_join(
    orders_df %>% group_by(univ_type) %>% summarise(total = n()),
    by = 'univ_type'
  ) %>% 
  mutate(
    pct = num / total
  ) %>% 
  ungroup()


# -------------------------------------------------------
# Figure 9 - GPA filter used by research vs. ma/doctoral
# -------------------------------------------------------

orders_gpa <- orders_df %>% 
  filter(!is.na(gpa_low) | !is.na(gpa_high)) %>% 
  group_by(univ_type, univ_label, gpa_low) %>% 
  summarise(
    n_low = n()
  ) %>% 
  mutate(
    pct_low = n_low / sum(n_low)
  ) %>% 
  ungroup() %>% 
  complete(nesting(univ_type, univ_label), gpa_low, fill = list(n_low = 0, pct_low = 0))


# --------------------------------------------------------
# Figure 10 - SAT filter used by research vs. ma/doctoral
# --------------------------------------------------------

test_scores_breaks <- c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620)
test_scores_labels <- c('<=1000', '1010-1100', '1110-1200', '1210-1300', '1310-1400', '1410-1500', '1510+')

orders_sat <- orders_df %>% 
  filter(!is.na(sat_score_min) | !is.na(sat_score_max)) %>% 
  mutate(
    sat_min_brks = cut(
      sat_score_min,
      breaks = test_scores_breaks, 
      labels = test_scores_labels
    ),
    sat_max_brks = cut(
      sat_score_max,
      breaks = test_scores_breaks, 
      labels = test_scores_labels
    )
  ) %>% 
  select(univ_type, univ_label, sat_min_brks, sat_max_brks) %>% 
  pivot_longer(
    cols = -c(univ_type, univ_label),
    names_pattern = '(sat_\\w+)_brks',
    names_to = 'test_range',
    values_to = 'brks'
  ) %>% 
  group_by(univ_type, univ_label, test_range, brks) %>% 
  summarise(
    num = n()
  ) %>% 
  mutate(
    pct = num / sum(num)
  ) %>% 
  ungroup() %>% 
  complete(nesting(univ_type, univ_label), test_range, brks, fill = list(num = 0, pct = 0)) 


# ---------------------------------------------------------
# Figure 11 - PSAT filter used by research vs. ma/doctoral
# ---------------------------------------------------------

orders_psat <- orders_df %>% 
  filter(!is.na(psat_score_min) | !is.na(psat_score_max)) %>% 
  mutate(
    psat_min_brks = cut(
      psat_score_min,
      breaks = test_scores_breaks, 
      labels = test_scores_labels
    ),
    psat_max_brks = cut(
      psat_score_max,
      breaks = test_scores_breaks, 
      labels = test_scores_labels
    )
  ) %>% 
  select(univ_type, univ_label, psat_min_brks, psat_max_brks) %>% 
  pivot_longer(
    cols = -c(univ_type, univ_label),
    names_pattern = '(psat_\\w+)_brks',
    names_to = 'test_range',
    values_to = 'brks'
  ) %>% 
  group_by(univ_type, univ_label, test_range, brks) %>% 
  summarise(
    num = n()
  ) %>% 
  mutate(
    pct = num / sum(num)
  ) %>% 
  ungroup() %>% 
  complete(nesting(univ_type, univ_label), test_range, brks, fill = list(num = 0, pct = 0))


# ----------------------------------------------------------------------------------
# Figure 12 - State filter used by research universities, in-state vs. out-of-state
# ----------------------------------------------------------------------------------

get_state_abb <- Vectorize(
  function(x) {
    if (is.na(x)) return(NA_character_)
    if (nchar(x) == 2) return(x)
    if (x == 'District of Columbia') return('DC')
    a <- state.abb[grep(str_c('^', x, '$'), state.name)]
    ifelse(length(a) == 0, NA_character_, a)
  }
)

orders_state_research_num <- orders_df %>% 
  filter(univ_type == 'research') %>% 
  group_by(univ_state, state_name) %>% 
  summarise(n = n()) %>% 
  mutate(name = strsplit(state_name, split = '|', fixed = T)) %>% 
  unnest(name) %>% 
  ungroup() %>% 
  mutate(
    abb = get_state_abb(name)
  ) %>%
  mutate(locale = if_else(univ_state == name, 'instate', 'outofstate')) %>% 
  filter(!is.na(abb)) %>% 
  group_by(abb, locale) %>% 
  summarise(frequency = sum(n)) %>% 
  ungroup()

orders_state_research <- us_map(regions = 'states') %>% 
  group_by(fips, abbr, full) %>% 
  count(fips) %>%
  ungroup() %>% 
  left_join(data.frame(locale = c('instate', 'outofstate')), by = character()) %>% 
  left_join(orders_state_research_num, by = c('abbr' = 'abb', 'locale' = 'locale')) %>% 
  mutate(
    frequency = if_else(is.na(frequency), as.double(0), as.double(frequency))
  )


# ---------------------------------------------------------
# Figure 13 - Race filter used by research vs. ma/doctoral
# ---------------------------------------------------------

orders_race <- orders_df %>%
  filter(filter_race == 1) %>% 
  mutate(
    race_ethnicity_group = recode(
      race_ethnicity,
      'American Indian or Alaska Native' = 'Native American',
      'American Indian or Alaska Native|\rAsian (including Indian subcontinent and Philippines origin)|Cuban|\rBlack or African American|\rHispanic or Latino (including Spanish origin)|\rMexican|\rPuerto Rican|\rOther Hispanic or Latino|\rNative Hawaiian or Other Pacific Islander' = 'Latinx, Black, Asian, Native American',
      'American Indian or Alaska Native|Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino' = 'Latinx, Native American',
      'American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander' = 'Native American, Native Hawaii/PI',
      'Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)' = 'Asian, White',
      'Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)|Native Hawaiian or Other Pacific Islander' = 'Asian, White, NativeHawaii/PI',
      'Asian (including Indian subcontinent and Philippines origin)|White (including Middle Eastern origin)|Other' = 'Asian, White',
      'Black or African American' = 'Black',
      'Black or African American|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander' = 'Black, Native American, NativeHawaii/PI',
      'Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban' = 'Latinx, Black, Native American',
      'Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban' = 'Latinx, Black, Native American, NativeHawaii/PI',
      'Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino' = 'Latinx, Black',
      'Cuban|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino' = 'Latinx',
      'Native Hawaiian or Other Pacific Islander' = 'NativeHawaii/PI',
      'Other|Asian (including Indian subcontinent and Philippines origin)|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)' = 'Asian, White',
      'Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban' = 'Latinx',
      .default = NA_character_
    )
  ) %>% 
  filter(!is.na(race_ethnicity_group)) %>% 
  group_by(univ_type, univ_label, race_ethnicity_group) %>% 
  summarise(
    count = n()
  ) %>% 
  ungroup()


# --------------------------------------------------------------------------
# Figure 14 - Number of prospects purchased by university type and location
# --------------------------------------------------------------------------

rq2_counts <- lists_orders_zip_hs_df %>%
  count(univ_type, univ_label, region, locale)


# ----------------------------------------------------------------------------------
# Figure 15 - Racial composition of prospects purchased by research universities
# +
# Figure 18 - Racial composition of prospects purchased by ma/doctoral universities
# ----------------------------------------------------------------------------------

rq2_race <- lists_orders_zip_hs_df %>% 
  group_by(univ_type, univ_label, region, locale, stu_race_cb) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    pct = count / sum(count)
  ) %>% 
  ungroup()


# ---------------------------------------------------------------------------------------
# Figure 16 - Median household income of prospects purchased by research universities
# +
# Figure 19 - Median household income of prospects purchased by ma/doctoral universities
# ---------------------------------------------------------------------------------------

rq2_income <- lists_orders_zip_hs_df %>% 
  left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
  group_by(univ_type, univ_label, region, locale) %>% 
  summarise(
    total = n(),
    count = sum(!is.na(medincome_total)),
    income = mean(medincome_total, na.rm = T),
    count_2564 = sum(!is.na(medincome_2564)),
    income_2564 = mean(medincome_2564, na.rm = T)
  ) %>% 
  ungroup()


# ----------------------------------------------------------------------
# Figure 17 - Locale of prospects purchased by research universities
# +
# Figure 20 - Locale of prospects purchased by ma/doctoral universities
# ----------------------------------------------------------------------

rq2_locale <- lists_orders_zip_hs_df %>% 
  left_join(zip_locale, by = c('stu_zip_code' = 'ZCTA5CE20')) %>% 
  mutate(
    locale_group = case_when(
      str_sub(LOCALE, 1, 1) %in% c('1', '2') ~ str_sub(LOCALE, 1, 1),
      str_sub(LOCALE, 2, 2) == '1' ~ '3',
      str_sub(LOCALE, 2, 2) == '2' ~ '4',
      str_sub(LOCALE, 2, 2) == '3' ~ '5',
      TRUE ~ '6'
    ),
    locale_text = recode_factor(
      locale_group,
      '6' = 'Missing',
      '5' = 'Rural - Remote',
      '4' = 'Rural - Distant',
      '3' = 'Rural - Fringe',
      '2' = 'Suburban',
      '1' = 'City'
    )
  ) %>% 
  group_by(univ_type, univ_label, region, locale, locale_text) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    pct = count / sum(count)
  ) %>% 
  ungroup()


# --------------------------------------------------------------------------
# Figure 21 - Zip code deep dive by Arizona State University in Los Angeles
# --------------------------------------------------------------------------

la_zips <- acs_race_zipcodev3 %>%  # 378 (total) zip codes in LA
  filter(cbsa_1 == '31080') %>% 
  select(zip_code) %>% 
  left_join(acs_income_zip, by = 'zip_code') %>% 
  arrange(-medincome_2564)

la_zips %>% count(is.na(medincome_2564))  # 14 zip codes missing income data

la_zips_top10pct <- la_zips %>%  # 38 (top 10% income) zip codes in LA
  head(n = 38)

asu_la <- lists_orders_zip_hs_df %>%
  filter(univ_id == '104151', zip_cbsa_1 == '31080', ord_num %in% c('366935', '448420', '448374', '394956')) %>% 
  mutate(
    ord_type = recode(
      ord_num,
      '366935' = 'psat_low',   # PSAT score from 1110-1210
      '448420' = 'psat_med',   # PSAT score from 1190-1260
      '448374' = 'psat_high',  # PSAT score from 1270-1520
      '394956' = 'sat_med'     # SAT score from 1140-1260
    ),
    in_zip_top10pct = if_else(stu_zip_code %in% la_zips_top10pct$zip_code, 1, 0)
  ) %>% 
  group_by(ord_num, ord_type, in_zip_top10pct, stu_race_cb) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    pct = count / sum(count)
  ) %>% 
  ungroup()


# ---------------------------------------------------------------------------
# Figure 22 - Women in STEM deep dive by University of California, San Diego
# ---------------------------------------------------------------------------

ccd %>% count(is_virtual, virtual, virtual_text)
ccd %>% count(updated_status, updated_status_text)

ucsd_metros <- c('16980', '42660', '35620', '12060')

ucsd_metro_race <- ccd %>%
  filter(g12_f >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8'), cbsa_1 %in% ucsd_metros) %>% 
  select(cbsa_1, cbsatitle_1, ncessch, matches('g12_\\w+_f')) %>%
  pivot_longer(
    cols = -c(cbsa_1, cbsatitle_1, ncessch),
    names_pattern = 'g12_(\\w+)_f',
    names_to = 'race',
    values_to = 'count'
  ) %>%
  mutate(
    count = if_else(is.na(count), 0L, count)
  ) %>% 
  group_by(cbsa_1, cbsatitle_1, ncessch) %>%
  mutate(
    pct = count / sum(count, na.rm = T),
    pct = if_else(is.nan(pct), NA_real_, pct)
  ) %>%
  ungroup() %>%
  group_by(cbsa_1, cbsatitle_1, race) %>%
  summarise(
    count = sum(count, na.rm = T), 
    pct = mean(pct, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    ord_type = 'metro'
  ) %>% 
  rename(
    cbsa_code = cbsa_1,
    cbsa_name = cbsatitle_1
  ) %>% 
  select(cbsa_code, cbsa_name, ord_type, race, count, pct)

ucsd_metro_income <- acs_income_metro %>% 
  filter(cbsa_code %in% ucsd_metros) %>% 
  mutate(
    ord_type = 'metro'
  ) %>% 
  rename(
    income_2564 = medincome_2564
  ) %>% 
  select(cbsa_code, cbsa_name, ord_type, income_2564)

ucsd_orders <- orders_df %>%
  filter(gender == 'Female', univ_id == '110680') %>% 
  select(order_num, order_title, num_students, hs_grad_class, gender, ap_scores, sat_score_min, sat_score_max)

ucsd_all <- lists_orders_zip_hs_df %>%
  filter(univ_id == '110680', ord_num %in% ucsd_orders$order_num) %>% 
  mutate(
    stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
    race = recode(
      stu_race_cb,
      `0` = 'noresponse',
      `1` = 'amerindian',
      `2` = 'asian',
      `3` = 'black',
      `4` = 'hispanic',
      `8` = 'nativehawaii',
      `9` = 'white',
      `12` = 'tworaces',
      `999` = 'missing'
    )
  ) %>% 
  select(ord_num, stu_race_cb, race, stu_zip_code) %>% 
  left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code'))

ucsd <- lists_orders_zip_hs_df %>%
  filter(univ_id == '110680', zip_cbsa_1 %in% ucsd_metros, ord_num %in% ucsd_orders$order_num) %>% 
  mutate(
    ord_type = case_when(
      ord_num %in% (ucsd_orders %>% filter(!is.na(ap_scores)))$order_num ~ 'ap',
      ord_num %in% (ucsd_orders %>% filter(!is.na(sat_score_min)))$order_num ~ 'sat'
    ),
    stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
    race = recode(
      stu_race_cb,
      `0` = 'noresponse',
      `1` = 'amerindian',
      `2` = 'asian',
      `3` = 'black',
      `4` = 'hispanic',
      `8` = 'nativehawaii',
      `9` = 'white',
      `12` = 'tworaces',
      `999` = 'missing'
    )
  ) %>% 
  select(zip_cbsa_1, zip_cbsatitle_1, ord_num, ord_type, stu_race_cb, race, stu_zip_code) %>% 
  left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
  rename(
    cbsa_code = zip_cbsa_1,
    cbsa_name = zip_cbsatitle_1
  )

ucsd %>% 
  count(ord_num, ord_type)

ucsd %>% 
  count(cbsa_code, ord_type)

ucsd_race <- ucsd %>% 
  group_by(cbsa_code, cbsa_name, ord_type, race) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    pct = count / sum(count)
  ) %>% 
  ungroup() %>% 
  bind_rows(ucsd_metro_race)

ucsd_income <- ucsd %>% 
  group_by(cbsa_code, cbsa_name, ord_type) %>% 
  summarise(
    income_2564 = mean(medincome_2564, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(ucsd_metro_income)


# ----------------------------------------------------------------------------
# Figure 23 - Segment deep dive by University of Illinois at Urbana-Champaign
# ----------------------------------------------------------------------------

uiuc_metros <- c('35620', '31080', '37980', '47900')

uiuc_metro_race <- ccd %>%
  filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8'), cbsa_1 %in% uiuc_metros) %>% 
  select(cbsa_1, cbsatitle_1, ncessch, matches('g12_[a-z]{2,}$')) %>%
  pivot_longer(
    cols = -c(cbsa_1, cbsatitle_1, ncessch),
    names_pattern = 'g12_(\\w+)',
    names_to = 'race',
    values_to = 'count'
  ) %>%
  mutate(
    count = if_else(is.na(count), 0L, count)
  ) %>% 
  group_by(cbsa_1, cbsatitle_1, ncessch) %>%
  mutate(
    pct = count / sum(count, na.rm = T),
    pct = if_else(is.nan(pct), NA_real_, pct)
  ) %>%
  ungroup() %>%
  group_by(cbsa_1, cbsatitle_1, race) %>%
  summarise(
    count = sum(count, na.rm = T), 
    pct = mean(pct, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    ord_type = 'metro'
  ) %>% 
  rename(
    cbsa_code = cbsa_1,
    cbsa_name = cbsatitle_1
  ) %>% 
  select(cbsa_code, cbsa_name, ord_type, race, count, pct)

uiuc_metro_income <- acs_income_metro %>% 
  filter(cbsa_code %in% uiuc_metros) %>% 
  mutate(
    ord_type = 'metro'
  ) %>% 
  rename(
    income_2564 = medincome_2564
  ) %>% 
  select(cbsa_code, cbsa_name, ord_type, income_2564)

uiuc_orders <- orders_df %>%
  filter(univ_id == '145637', filter_segment == 1, filter_gender == 0) %>% 
  select(order_num, order_title, num_students, hs_grad_class, segment, state_name, cbsa_name, sat_score_min, sat_score_max, psat_score_min, psat_score_max, gpa_low, gpa_high)

uiuc <- lists_orders_zip_hs_df %>%
  filter(univ_id == '145637', zip_cbsa_1 %in% uiuc_metros, ord_num %in% uiuc_orders$order_num) %>% 
  mutate(
    ord_type = 'prospect',
    stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
    race = recode(
      stu_race_cb,
      `0` = 'noresponse',
      `1` = 'amerindian',
      `2` = 'asian',
      `3` = 'black',
      `4` = 'hispanic',
      `8` = 'nativehawaii',
      `9` = 'white',
      `12` = 'tworaces',
      `999` = 'unknown'
    )
  ) %>% 
  select(zip_cbsa_1, zip_cbsatitle_1, ord_num, ord_type, stu_race_cb, race, stu_zip_code) %>% 
  left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
  rename(
    cbsa_code = zip_cbsa_1,
    cbsa_name = zip_cbsatitle_1
  )

uiuc %>% 
  count(ord_num)

uiuc %>% 
  count(cbsa_code)

uiuc_race <- uiuc %>% 
  group_by(cbsa_code, cbsa_name, ord_type, race) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    pct = count / sum(count)
  ) %>% 
  ungroup() %>% 
  bind_rows(uiuc_metro_race)

uiuc_income <- uiuc %>% 
  group_by(cbsa_code, cbsa_name, ord_type) %>% 
  summarise(
    income_2564 = mean(medincome_2564, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(uiuc_metro_income)


# ---------------------------------------------------------
# Figure 24 - Targeting students of color, race categories
# ---------------------------------------------------------

poc_order <- '560119'
poc_metros <- c('35620', '33100', '26420')

poc <- lists_orders_zip_hs_df %>%
  filter(univ_id == '110680', zip_cbsa_1 %in% poc_metros, ord_num == poc_order) %>% 
  left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
  rename(
    cbsa_code = zip_cbsa_1,
    cbsa_name = zip_cbsatitle_1,
    control = hs_school_control
  )

poc %>% 
  count(cbsa_code)

poc %>% 
  count(cbsa_code, stu_race_cb)

poc_cb <- poc %>% 
  group_by(cbsa_code, cbsa_name, stu_race_cb) %>% 
  summarise(
    count = n()
  ) %>% 
  ungroup() %>% 
  group_by(cbsa_code, cbsa_name) %>% 
  mutate(
    pct = count / sum(count, na.rm = T)
  )

poc_common <- poc %>% 
  select(cbsa_code, cbsa_name, stu_white_common, stu_asian_common, stu_black_common, stu_is_hisp_common, stu_american_indian_common, stu_native_hawaiian_common) %>% 
  pivot_longer(
    cols = -c(cbsa_code, cbsa_name),
    names_pattern = 'stu_(\\w+)_common',
    names_to = 'race',
    values_to = 'count'
  ) %>% 
  group_by(cbsa_code, cbsa_name, race) %>% 
  summarise(
    count = sum(count, na.rm = T)
  ) %>% 
  ungroup() %>% 
  left_join(
    poc %>% count(cbsa_code),
    by = 'cbsa_code'
  ) %>% 
  mutate(
    pct = count / n
  )


# -------------------------------------------------------------
# Figure 25 - Targeting students of color, purchased prospects
# -------------------------------------------------------------

poc_metro_income <- acs_income_metro %>% 
  filter(cbsa_code %in% poc_metros) %>% 
  mutate(
    ord_type = 'metro'
  ) %>% 
  rename(
    income_2564 = medincome_2564
  ) %>% 
  select(cbsa_code, cbsa_name, ord_type, income_2564)

poc_metro_pubhs <- ccd %>%
  filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8'), cbsa_1 %in% poc_metros) %>% 
  select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown, lzip) %>% 
  left_join(acs_income_zip, by = c('lzip' = 'zip_code')) %>% 
  left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
  mutate(
    n_cat = case_when(
      is.na(n) ~ 'zero',
      n <= 5 ~ '1-5',
      T ~ '6+'
    ),
    control = 'public'
  ) %>% 
  select(-lzip)

poc_metro_privhs <- pss %>% 
  filter(total_12 >= 10, cbsa_1 %in% poc_metros) %>% 
  select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, zip_code) %>% 
  left_join(acs_income_zip, by = 'zip_code') %>% 
  left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
  mutate(
    n_cat = case_when(
      is.na(n) ~ 'zero',
      T ~ '1+'
    ),
    control = 'private'
  ) %>% 
  select(-zip_code)

poc_metro_pubprivhs <- poc_metro_pubhs %>% 
  bind_rows(poc_metro_privhs) %>% 
  rename(
    cbsa_code = cbsa_1,
    cbsa_name = cbsatitle_1
  )

poc_metro_hs <- poc_metro_pubprivhs %>% 
  select(cbsa_code, cbsa_name, control, ncessch, total_students) %>% 
  mutate(
    ord_type = 'metro'
  ) %>% 
  group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
  summarise(
    count = sum(total_students)
  ) %>% 
  mutate(
    pct = count / sum(count, na.rm = T)
  ) %>% 
  ungroup() 

poc_hs <- poc %>% 
  filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
  mutate(
    ord_type = 'prospect'
  ) %>% 
  group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
  summarise(
    count = n()
  ) %>% 
  filter(!is.na(control)) %>% 
  mutate(
    pct = count / sum(count, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(poc_metro_hs)

poc_race <- poc_metro_pubprivhs %>% 
  select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
  mutate(
    ord_type = str_c(control, n_cat, sep = '_')
  ) %>% 
  select(-control, -n_cat) %>% 
  pivot_longer(
    cols = starts_with('total_'),
    names_prefix = 'total_',
    names_to = 'race',
    values_to = 'count'
  ) %>% 
  mutate(
    n = if_else(is.na(n), 0L, n),
    count = if_else(is.na(count), 0L, count)
  ) %>% 
  group_by(cbsa_code, cbsa_name, ord_type, ncessch) %>%
  mutate(
    pct = count / sum(count, na.rm = T),
    pct = if_else(is.nan(pct), NA_real_, pct)
  ) %>%
  ungroup() %>%
  group_by(cbsa_code, cbsa_name, ord_type, race) %>%
  summarise(
    num_hs = n(),
    num_prospects = sum(n, na.rm = T),
    count = sum(count, na.rm = T), 
    pct = mean(pct, na.rm = T)
  ) %>% 
  ungroup()

poc_income_hs <- poc_metro_pubprivhs %>% 
  select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, medincome_2564) %>% 
  group_by(cbsa_code, cbsa_name, control, n_cat) %>% 
  summarise(
    num_hs = n(),
    num_prospects = sum(n, na.rm = T),
    income_2564 = mean(medincome_2564, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    ord_type = str_c(control, n_cat, sep = '_')
  ) %>% 
  select(cbsa_code, cbsa_name, ord_type, num_hs, num_prospects, income_2564)

poc_income <- poc %>% 
  filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
  mutate(
    ord_type = 'prospect'
  ) %>% 
  group_by(cbsa_code, cbsa_name, ord_type) %>% 
  summarise(
    income_2564 = mean(medincome_2564, na.rm = T)
  ) %>% 
  ungroup() %>% 
  bind_rows(poc_metro_income) %>% 
  bind_rows(poc_income_hs)

# Check totals for income and race are consistent
poc_race %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs) / 8, sum(num_prospects / 8))
poc_income %>% filter(!is.na(num_hs)) %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs), sum(num_prospects))


# ----------------------------------------------------------------------------------------
# Figure A1 - School type of prospects purchased by research vs. ma/doctoral universities
# ----------------------------------------------------------------------------------------

rq2_school <- lists_orders_zip_hs_df %>% 
  group_by(univ_type, univ_label, region, locale, hs_school_control) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    pct = count / sum(count)
  ) %>% 
  ungroup()


# ----------------------------------------------------------------------------
# Table 7 - Filter combos used in order purchases by research vs. ma/doctoral
# ----------------------------------------------------------------------------

orders_filters_combo <- orders_df %>% 
  mutate(
    hsgrad_class = if_else(filter_hsgrad_class == 1, 'HS grad class', NA_character_),
    zip = if_else(filter_zip == 1, 'Zip code', NA_character_),
    states_fil = if_else(filter_states_fil == 1, 'State', NA_character_), 
    cbsa = if_else(filter_cbsa == 1, 'CBSA', NA_character_), 
    intl = if_else(filter_intl == 1, 'International', NA_character_), 
    segment = if_else(filter_segment == 1, 'Segment', NA_character_), 
    race = if_else(filter_race == 1, 'Race', NA_character_), 
    gender = if_else(filter_gender == 1, 'Gender', NA_character_), 
    sat = if_else(filter_sat == 1, 'SAT', NA_character_), 
    psat = if_else(filter_psat == 1, 'PSAT', NA_character_), 
    gpa = if_else(filter_gpa == 1, 'GPA', NA_character_), 
    rank = if_else(filter_rank == 1, 'Rank', NA_character_), 
    geomarket = if_else(filter_geomarket == 1, 'Geomarket', NA_character_), 
    ap_score = if_else(filter_ap_score == 1, 'AP score', NA_character_)
  ) %>% 
  group_by(univ_type, univ_label, hsgrad_class, gpa, sat, psat, rank, ap_score, states_fil, zip, geomarket, segment, cbsa, intl, race, gender) %>% 
  count() %>% 
  unite('filter_combos', c(hsgrad_class, gpa, sat, psat, rank, ap_score, states_fil, zip, geomarket, segment, cbsa, intl, race, gender), sep = ', ', remove = T, na.rm = T) %>% 
  arrange(univ_type, -n) %>% 
  mutate(
    pct = n / sum(n)
  ) %>% 
  ungroup()


# --------------------------------------------------
# Table 8 - Prospect characteristics by filter used
# --------------------------------------------------

rq3_prospects <- lists_orders_zip_hs_df %>% 
  filter(stu_in_us == 1) %>% 
  select(univ_id, ord_num, stu_in_us, stu_nonres, stu_race_cb, stu_gender, stu_zip_code) %>% 
  left_join(orders_df %>% select(order_num, starts_with('filter_')), by = c('ord_num' = 'order_num')) %>% 
  left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
  left_join(zip_locale, by = c('stu_zip_code' = 'ZCTA5CE20'))

get_rq3_data <- function(filter_var) {
  rq3_df <- rq3_prospects %>% 
    filter(get(filter_var) == 1)
  
  rq3_counts <- rq3_df %>% 
    count(stu_nonres) %>% 
    mutate(
      row_subj = recode(
        stu_nonres,
        `0` = 'pct_instate',
        `1` = 'pct_outofstate'
      ),
      val = n / sum(n)
    ) %>%
    select(row_subj, val) %>%
    add_row(row_subj = 'n', val = nrow(rq3_df), .before = 1)
  
  rq3_race <- rq3_df %>% 
    count(stu_race_cb) %>% 
    mutate(
      stu_race_cb = if_else(is.na(stu_race_cb), 999, as.numeric(stu_race_cb)),
      row_subj = recode(
        stu_race_cb,
        `0` = 'pct_noresponse',
        `1` = 'pct_aian',
        `2` = 'pct_asian',
        `3` = 'pct_black',
        `4` = 'pct_latinx',
        `8` = 'pct_nhpi',
        `9` = 'pct_white',
        `10` = 'pct_raceother',
        `12` = 'pct_multiracial',
        `999` = 'pct_racemissing'
      ),
      val = n / sum(n)
    ) %>%
    select(row_subj, val)
  
  rq3_gender <- rq3_df %>% 
    mutate(
      row_subj = case_when(
        stu_gender == 'F' ~ 'pct_female',
        stu_gender == 'M' ~ 'pct_male',
        is.na(stu_gender) ~ 'pct_gendermissing',
        T ~ 'pct_genderother'
      )
    ) %>% 
    count(row_subj) %>% 
    mutate(
      val = n / sum(n)
    ) %>%
    select(row_subj, val)
  
  rq3_income <- rq3_df %>% 
    summarise(
      val = mean(medincome_2564, na.rm = T)
    ) %>% 
    mutate(
      row_subj = 'med_income'
    ) %>%
    select(row_subj, val)
  
  rq3_locale <- rq3_df %>% 
    mutate(
      locale_group = case_when(
        str_sub(LOCALE, 1, 1) %in% c('1', '2') ~ str_sub(LOCALE, 1, 1),
        str_sub(LOCALE, 2, 2) == '1' ~ '3',
        str_sub(LOCALE, 2, 2) == '2' ~ '4',
        str_sub(LOCALE, 2, 2) == '3' ~ '5',
        TRUE ~ '6'
      )
    ) %>% 
    count(locale_group) %>% 
    mutate(
      row_subj = recode(
        locale_group,
        '1' = 'pct_city',
        '2' = 'pct_suburban',
        '3' = 'pct_fringe',
        '4' = 'pct_distant',
        '5' = 'pct_remote',
        '6' = 'pct_localemissing'
      ),
      val = n / sum(n)
    ) %>%
    select(row_subj, val)
  
  bind_rows(rq3_counts, rq3_race, rq3_gender, rq3_income, rq3_locale) %>% 
    setNames(c('row_subj', filter_var))
}

rq3 <- c('stu_in_us', 'filter_gpa', 'filter_psat', 'filter_sat', 'filter_rank', 'filter_ap_score', 'filter_zip', 'filter_states_fil', 'filter_geomarket', 'filter_segment', 'filter_cbsa', 'filter_race', 'filter_gender') %>% 
  lapply(get_rq3_data) %>% 
  reduce(left_join, by = 'row_subj') %>% 
  mutate_all(~replace(., is.na(.), 0))


# --------------
# Save datasets
# --------------

save(orders_prospects_purchased, orders_filters, orders_gpa, orders_sat, orders_psat, orders_state_research, orders_race, orders_filters_combo, rq2_counts, rq2_race, rq2_income, rq2_locale, rq2_school, rq3, asu_la, ucsd_all, ucsd_race, ucsd_income, uiuc_race, uiuc_income, poc_cb, poc_common, poc_hs, poc_race, poc_income, file = file.path(data_dir, 'tbl_fig_data_final.RData'))
