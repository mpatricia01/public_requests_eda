library(tidyverse)
library(labelled)


data_dir <- file.path('.', 'data')


# Read in directory data and dictionary (102337 obs. of 65 variables)
ccd_directory <- read.csv(file.path(data_dir, 'ccd_directory_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'MZIP' = 'character', 'MZIP4' = 'character', 'LZIP' = 'character', 'LZIP4' = 'character', 'SY_STATUS' = 'character', 'UPDATED_STATUS' = 'character', 'SCH_TYPE' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))
ccd_directory_dictionary <- read.csv(file.path(data_dir, 'ccd_directory_dictionary_1718.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

# Turn variables to lowercase
names(ccd_directory) <- tolower(names(ccd_directory))
names(ccd_directory_dictionary) <- tolower(names(ccd_directory_dictionary))
ccd_directory_dictionary <- ccd_directory_dictionary %>% mutate(names_lc = tolower(variable.name))

# Label variables
for (i in names(ccd_directory)) { 
  description <- (ccd_directory_dictionary %>% filter(names_lc == i))$description
  var_label(ccd_directory[[i]]) <- description
}

# Label values
ccd_directory <- ccd_directory %>% set_value_labels( 
  sy_status = c('open' = '1', 
                'closed' = '2',
                'new' = '3',
                'added' = '4',
                'changed boundary/agency' = '5',
                'inactive' = '6',
                'future' = '7',
                'reopened' = '8'),
  updated_status = c('open' = '1',
                     'closed' = '2',
                     'new' = '3',
                     'added' = '4',
                     'changed boundary/agency' = '5',
                     'inactive' = '6',
                     'future' = '7',
                     'reopened' = '8'),
  sch_type = c('regular school' = '1',
               'special education school' = '2',
               'career and technical school' = '3',
               'alternative education school' = '4'),
  gslo = c('1st grade students' = '1',
           '2nd grade students' = '2',
           '3rd grade students' = '3',
           '4th grade students' = '4',
           '5th grade students' = '5',
           '6th grade students' = '6',
           '7th grade students' = '7',
           '8th grade students' = '8',
           '9th grade students' = '9',
           '10th grade students' = '10',
           '11th grade students' = '11',
           '12th grade students' = '12',
           '13th grade students' = '13',
           'adult education students' = 'AE',
           'kindergarten students' = 'KG',
           'not applicable' = 'N',
           'prekindergarten students' = 'PK',
           'students in ungraded classes' = 'UG',
           'missing' = 'M'),
  gshi = c('1st grade students' = '1',
           '2nd grade students' = '2',
           '3rd grade students' = '3',
           '4th grade students' = '4',
           '5th grade students' = '5',
           '6th grade students' = '6',
           '7th grade students' = '7',
           '8th grade students' = '8',
           '9th grade students' = '9',
           '10th grade students' = '10',
           '11th grade students' = '11',
           '12th grade students' = '12',
           '13th grade students' = '13',
           'adult education students' ='AE',
           'kindergarten students' = 'KG',
           'not applicable' = 'N',
           'prekindergarten students' = 'PK',
           'students in ungraded classes' = 'UG',
           'missing' = 'M')
)


# Read in characteristics data and dictionary (99899 obs. of 20 variables)
ccd_characteristics <- read.csv(file.path(data_dir, 'ccd_characteristics_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))
ccd_characteristics_dictionary <- read.csv(file.path(data_dir, 'ccd_characteristics_dictionary_1718.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

# Turn variables to lowercase
names(ccd_characteristics) <- tolower(names(ccd_characteristics))
names(ccd_characteristics_dictionary) <- tolower(names(ccd_characteristics_dictionary))
ccd_characteristics_dictionary <- ccd_characteristics_dictionary %>% mutate(names_lc = tolower(variable.name))

# Label variables
for (i in names(ccd_characteristics)) { 
  description <- (ccd_characteristics_dictionary %>% filter(names_lc == i))$description
  var_label(ccd_characteristics[[i]]) <- description
}

# Add virtual variable
table(ccd_characteristics$virtual)
table(ccd_characteristics$virtual_text)

ccd_characteristics <- ccd_characteristics %>%
  mutate(is_virtual = case_when(
    virtual == 'NOTVIRTUAL' ~ 0,
    virtual == 'Not reported' ~ NA_real_,
    TRUE ~ 1  # FULLVIRTUAL, FACEVIRTUAL, SUPPVIRTUAL
  ))

table(ccd_characteristics$is_virtual, useNA = 'always')

# Filter duplicate variables from directory (10 remaining variables)
ccd_characteristics <- ccd_characteristics %>% 
  select(ncessch, setdiff(names(ccd_characteristics), names(ccd_directory)))


# Read in characteristics data and dictionary (12112911 obs. of 18 variables)
ccd_membership <- read.csv(file.path(data_dir, 'ccd_membership_1718.csv'), header = TRUE, na.strings = c('', 'NA'), stringsAsFactors = F,  colClasses = c('FIPST' = 'character', 'LEAID' = 'character', 'NCESSCH' = 'character', 'SCHID' = 'character', 'STATE_AGENCY_NO' = 'character', 'UNION' = 'character'))
ccd_membership_dictionary <- read.csv(file.path(data_dir, 'ccd_membership_dictionary_1718.csv'), header = TRUE, na.strings=c('', 'NA'), stringsAsFactors = F)

# Turn variables to lowercase
names(ccd_membership) <- tolower(names(ccd_membership))
names(ccd_membership_dictionary) <- tolower(names(ccd_membership_dictionary))
ccd_membership_dictionary <- ccd_membership_dictionary %>% mutate(names_lc = tolower(variable.name))

# Label variables
for (i in names(ccd_membership)) { 
  description <- (ccd_membership_dictionary %>% filter(names_lc == i))$description
  var_label(ccd_membership[[i]]) <- description
}

# Pivot table (33369 obs.)
ccd_membership_by_grade <- ccd_membership %>%
  filter(grade %in% c('Grade 9', 'Grade 10', 'Grade 11', 'Grade 12', 'Grade 13'), total_indicator == 'Subtotal 4 - By Grade') %>%
  select(ncessch, grade, student_count) %>%
  mutate(
    grade = recode(grade,
                   'Grade 9' = 'g09',
                   'Grade 10' = 'g10',
                   'Grade 11' = 'g11',
                   'Grade 12' = 'g12',
                   'Grade 13' = 'g13')
    ) %>% 
  pivot_wider(names_from = grade, values_from = student_count) %>%
  rowwise() %>% 
  mutate(total_students = sum(g09, g10, g11, g12, g13, na.rm = T))

# Pivot table (33369 obs.)
ccd_membership_by_race <- ccd_membership %>%
  filter(grade %in% c('Grade 9', 'Grade 10', 'Grade 11', 'Grade 12', 'Grade 13'), total_indicator == 'Category Set A - By Race/Ethnicity; Sex; Grade') %>%
  mutate(
    grade = recode(grade,
                   'Grade 9' = 'g09',
                   'Grade 10' = 'g10',
                   'Grade 11' = 'g11',
                   'Grade 12' = 'g12',
                   'Grade 13' = 'g13'),
    race_ethnicity = recode(race_ethnicity,
                            'American Indian or Alaska Native' = 'amerindian',
                            'Asian' = 'asian',
                            'Black or African American' = 'black',
                            'Hispanic/Latino' = 'hispanic',
                            'Native Hawaiian or Other Pacific Islander' = 'nativehawaii',
                            'Not Specified' = 'unknown',
                            'Two or more races' = 'tworaces',
                            'White' = 'white'),
    grade_race = str_c(grade, race_ethnicity, sep = '_')
  ) %>% 
  group_by(ncessch, grade_race) %>% 
  summarise(count = sum(student_count, na.rm = T)) %>% 
  pivot_wider(names_from = grade_race, values_from = count) %>%
  rowwise() %>% 
  mutate(
    total_amerindian = sum(g09_amerindian, g10_amerindian, g11_amerindian, g12_amerindian, g13_amerindian, na.rm = T),
    total_asian = sum(g09_asian, g10_asian, g11_asian, g12_asian, g13_asian, na.rm = T),
    total_black = sum(g09_black, g10_black, g11_black, g12_black, g13_black, na.rm = T),
    total_hispanic = sum(g09_hispanic, g10_hispanic, g11_hispanic, g12_hispanic, g13_hispanic, na.rm = T),
    total_nativehawaii = sum(g09_nativehawaii, g10_nativehawaii, g11_nativehawaii, g12_nativehawaii, g13_nativehawaii, na.rm = T),
    total_tworaces = sum(g09_tworaces, g10_tworaces, g11_tworaces, g12_tworaces, g13_tworaces, na.rm = T),
    total_unknown = sum(g09_unknown, g10_unknown, g11_unknown, g12_unknown, g13_unknown, na.rm = T),
    total_white = sum(g09_white, g10_white, g11_white, g12_white, g13_white, na.rm = T),
    sum_students = sum(total_amerindian, total_asian, total_black, total_hispanic, total_nativehawaii, total_tworaces, total_unknown, total_white, na.rm = T),
    total_students = if_else(sum_students == 0, NA_integer_, sum_students),
    pct_amerindian = total_amerindian / total_students * 100,
    pct_asian = total_asian / total_students * 100,
    pct_black = total_black / total_students * 100,
    pct_hispanic = total_hispanic / total_students * 100,
    pct_nativehawaii = total_nativehawaii / total_students * 100,
    pct_tworaces = total_tworaces / total_students * 100,
    pct_unknown = total_unknown / total_students * 100,
    pct_white = total_white / total_students * 100
  )

# Total should be the same
ccd_membership_by_grade %>% full_join(ccd_membership_by_race, by = 'ncessch') %>% filter(total_students.x != total_students.y) %>% nrow()

# Combine by grade and by race
ccd_membership_by_grade_race <- ccd_membership_by_grade %>% 
  full_join(ccd_membership_by_race %>% select(-sum_students, -total_students), by = 'ncessch')


# Join tables
ccd <- left_join(ccd_directory, ccd_characteristics, by = 'ncessch') %>% 
  left_join(ccd_membership_by_grade_race, by = 'ncessch')

# Rename variables
ccd <- ccd %>% rename(state_code = 'st')

# Check variables
unique(ccd$g_12_offered)
summary(ccd$g12)
table(ccd$is_virtual, useNA = 'always')
val_labels(ccd$sch_type)
val_labels(ccd$updated_status)
all(nchar(ccd$mzip) == 5)
all(nchar(ccd$lzip) == 5)
all(nchar(ccd$state_code) == 2)
all(nchar(ccd$ncessch) == 12)

# Export data
ccd <- as_tibble(x = ccd)
saveRDS(ccd, file = file.path(data_dir, 'ccd_1718.RDS'))
