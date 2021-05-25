### CREATE PERCENT RACE/ETHNICITY VARIABLES FOR STUDENTS AGES 15-19

#=======================================================================
# Load libraries
#=======================================================================
library(tidyverse)

#=======================================================================
# Directory file paths
#=======================================================================
data_dir <- file.path('.', 'data')


#=======================================================================
# Read in census data
#=======================================================================

acs <- read_csv(str_c(file.path(data_dir, 'msa_raw.csv')))

#var names
names(acs)

glimpse(acs)

# zipcode level
acs %>%
  group_by(zipcode) %>%
  summarise(n_per_grp = n()) %>%
  ungroup() %>%
  count(n_per_grp)

# check missing values in every column
sapply(acs, function(x) sum(is.na(x)))


# create vars that combine ages 15-17 & 18-19 & sex for each race/ethnicity
acs <- acs %>%
  mutate(pop_white_15_19 = `pop_white_m_15-17` + `pop_white_m_18-19` + `pop_white_f_15-17` + `pop_white_f_18-19`,
         pop_black_15_19 = `pop_black_m_15-17` + `pop_black_m_18-19` + `pop_black_f_15-17` + `pop_black_f_18-19`,
         pop_asian_15_19 = `pop_asian_m_15-17` + `pop_asian_m_18-19` + `pop_asian_f_15-17` + `pop_asian_f_18-19`,
         pop_amerindian_15_19 = `pop_amerindian_m_15-17` + `pop_amerindian_m_18-19` + `pop_amerindian_f_15-17` + `pop_amerindian_f_18-19`,
         pop_nativehawaii_15_19 = `pop_nativehawaii_m_15-17` + `pop_nativehawaii_m_18-19` + `pop_nativehawaii_f_15-17` + `pop_nativehawaii_f_18-19`,
         pop_otherrace_15_19 = `pop_otherrace_m_15-17` + `pop_otherrace_m_18-19` + `pop_otherrace_f_15-17` + `pop_otherrace_f_18-19`,
         pop_tworaces_15_19 = `pop_tworaces_m_15-17` + `pop_tworaces_m_18-19` + `pop_tworaces_f_15-17` + `pop_tworaces_f_18-19`,
         pop_hispanic_15_19 = `pop_hispanic_m_15-17` + `pop_hispanic_m_18-19` + `pop_hispanic_f_15-17` + `pop_hispanic_f_18-19`,
         pop_total_15_19 = pop_white_15_19 + pop_black_15_19 + pop_asian_15_19 + pop_amerindian_15_19 + pop_nativehawaii_15_19 + pop_otherrace_15_19 + pop_tworaces_15_19 + pop_hispanic_15_19)

# some checks 
acs %>%
  filter(pop_white_15_19 != `pop_white_m_15-17` + `pop_white_m_18-19` + `pop_white_f_15-17` + `pop_white_f_18-19`)

acs %>%
  filter(pop_asian_15_19 != `pop_asian_m_15-17` + `pop_asian_m_18-19` + `pop_asian_f_15-17` + `pop_asian_f_18-19`)

acs %>%
  filter(pop_total_15_19 != pop_white_15_19 + pop_black_15_19 + pop_asian_15_19 + pop_amerindian_15_19 + pop_nativehawaii_15_19 + pop_otherrace_15_19 + pop_tworaces_15_19 + pop_hispanic_15_19)

View(acs %>% select(pop_hispanic_15_19, `pop_hispanic_m_15-17`, `pop_hispanic_m_18-19`, `pop_hispanic_f_15-17`, `pop_hispanic_f_18-19`))

View(acs %>% select(pop_total_15_19, pop_white_15_19, pop_black_15_19, pop_asian_15_19, pop_amerindian_15_19, pop_nativehawaii_15_19, pop_otherrace_15_19, pop_tworaces_15_19, pop_hispanic_15_19))

# create pct vars

acs <- acs %>%
  mutate(pop_white_15_19_pct = (pop_white_15_19/pop_total_15_19)*100,
         pop_black_15_19_pct = (pop_black_15_19/pop_total_15_19)*100,
         pop_asian_15_19_pct = (pop_asian_15_19/pop_total_15_19)*100,
         pop_amerindian_15_19_pct = (pop_amerindian_15_19/pop_total_15_19)*100,
         pop_nativehawaii_15_19_pct = (pop_nativehawaii_15_19/pop_total_15_19)*100,
         pop_otherrace_15_19_pct = (pop_otherrace_15_19/pop_total_15_19)*100,
         pop_tworaces_15_19_pct = (pop_tworaces_15_19/pop_total_15_19)*100,
         pop_hispanic_15_19_pct = (pop_hispanic_15_19/pop_total_15_19)*100)

# some checks
acs %>%
  filter(pop_white_15_19_pct != (pop_white_15_19/pop_total_15_19)*100)

acs %>%
  filter(pop_tworaces_15_19_pct != (pop_tworaces_15_19/pop_total_15_19)*100)

View(acs %>% select(pop_asian_15_19_pct, pop_asian_15_19, pop_total_15_19))

View(acs %>% select(pop_black_15_19_pct, pop_black_15_19, pop_total_15_19))

# keep certain variables
acs <- acs %>% select(-contains('_f_'), -contains('_m_'))

# save csv
write_csv(acs, str_c(file.path(data_dir), "/","acs_race_zipcode.csv"))
