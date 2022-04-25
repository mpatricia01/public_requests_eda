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
  mutate(
    univ_type = if_else(univ_id %in% research_univs, 'research', 'regional'),
    univ_label = recode_factor(
      univ_type,
      'research' = 'Research',
      'regional' = 'MA/doctoral'
    )
  ) %>% 
  mutate_if(is.character, list(~na_if(., '')))

lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% 
  filter(univ_id %in% c(research_univs, regional_univs)) %>%
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
  mutate(
    hsgrad_class = if_else(!is.na(hs_grad_class), 1, 0),
    zip = if_else(!is.na(zip_code) | !is.na(zip_code_file), 1, 0),
    states_fil = if_else(!is.na(state_name), 1, 0), 
    cbsa = if_else(!is.na(cbsa_name), 1, 0), 
    intl = if_else(!is.na(intl_region), 1, 0), 
    segment = if_else(!is.na(segment), 1, 0), 
    race = if_else(!is.na(race_ethnicity), 1, 0), 
    gender = if_else(!is.na(gender), 1, 0), 
    sat = if_else(!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max) | !is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max) | !is.na(sat_score_reading_writing_min) | !is.na(sat_score_reading_writing_max) | !is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max) | !is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max), 1, 0), 
    psat = if_else(!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max), 1, 0), 
    gpa = if_else(!is.na(gpa_low) | !is.na(gpa_high), 1, 0), 
    rank = if_else(!is.na(rank_low) | !is.na(rank_high), 1, 0), 
    geomarket = if_else(!is.na(geomarket), 1, 0), 
    ap_score = if_else(!is.na(ap_scores), 1, 0)
  ) %>% 
  select(univ_type, univ_label, hsgrad_class, zip, states_fil, cbsa, intl, segment, race, gender, sat, psat, gpa, rank, geomarket, ap_score) %>% 
  group_by(univ_type, univ_label) %>% 
  summarize_if(is.numeric, sum) %>% 
  pivot_longer(
    cols = -c(univ_type, univ_label),
    names_to = 'filters',
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
    hsgrad_class = if_else(!is.na(hs_grad_class), 'HS grad class', NA_character_),
    zip = if_else(!is.na(zip_code) | !is.na(zip_code_file), 'Zip code', NA_character_),
    states_fil = if_else(!is.na(state_name), 'State', NA_character_), 
    cbsa = if_else(!is.na(cbsa_name), 'CBSA', NA_character_), 
    intl = if_else(!is.na(intl_region), 'International', NA_character_), 
    segment = if_else(!is.na(segment), 'Segment', NA_character_), 
    race = if_else(!is.na(race_ethnicity), 'Race', NA_character_), 
    gender = if_else(!is.na(gender), 'Gender', NA_character_), 
    sat = if_else(!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max) | !is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max) | !is.na(sat_score_reading_writing_min) | !is.na(sat_score_reading_writing_max) | !is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max) | !is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max), 'SAT', NA_character_), 
    psat = if_else(!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max), 'PSAT', NA_character_), 
    gpa = if_else(!is.na(gpa_low) | !is.na(gpa_high), 'GPA', NA_character_), 
    rank = if_else(!is.na(rank_low) | !is.na(rank_high), 'Rank', NA_character_), 
    geomarket = if_else(!is.na(geomarket), 'Geomarket', NA_character_), 
    ap_score = if_else(!is.na(ap_scores), 'AP score', NA_character_)
  ) %>% 
  group_by(univ_type, univ_label, hsgrad_class, gpa, sat, psat, rank, ap_score, states_fil, zip, geomarket, segment, cbsa, intl, race, gender) %>% 
  count() %>% 
  unite('filter_combos', c(hsgrad_class, gpa, sat, psat, rank, ap_score, states_fil, zip, geomarket, segment, cbsa, intl, race, gender), sep = ', ', remove = T, na.rm = T) %>% 
  arrange(univ_type, -n) %>% 
  mutate(
    pct = n / sum(n)
  ) %>% 
  ungroup()


# --------------
# Save datasets
# --------------

save(orders_prospects_purchased, orders_filters, orders_gpa, orders_sat, orders_psat, orders_state_research, orders_filters_combo, rq2_counts, rq2_race, rq2_income, rq2_locale, rq2_school, file = file.path(data_dir, 'tbl_fig_data_final.RData'))
