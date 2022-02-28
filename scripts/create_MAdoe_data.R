### Settings
rm(list = ls())
options(max.print=1000)

### Libraries

library(tidyverse)
library(lubridate)
library(labelled)
library(tidyr)
library(stringr)
library(eatATA)
library(readxl)


################### OPENING MA SCIENCE & TECH AP DATA

# MA Function For OPENING & RENAMING

open_rename_MA <- function(datafile, abbrev) { 

  #read in datafile
    df <- read_excel(datafile, skip = 1) 

  # convert to numerical
    cols.num <- c("Tests Taken","Score=1","Score=2","Score=3","Score=4","Score=5","% Score 1-2","% Score 3-5")
    df[cols.num] <- sapply(df[cols.num],as.numeric)
    sapply(df, class)
    
  #rename vars
    df <- df %>% rename(sch_name="School Name",
                        ma_doe_id="School Code",
                        !!paste0("tot_taken_", abbrev) := "Tests Taken",
                        !!paste0("score1_", abbrev) := "Score=1",
                        !!paste0("score2_", abbrev) := "Score=2",
                        !!paste0("score3_", abbrev) := "Score=3",
                        !!paste0("score4_", abbrev) := "Score=4",
                        !!paste0("score5_", abbrev) := "Score=5",
                        !!paste0("pct_score1to2_", abbrev) := "% Score 1-2",
                        !!paste0("pct_score3to5_", abbrev) := "% Score 3-5")}
                 

#function calls
    
    #Science and Tech Tests = Biology, Chemistry, Environmental Science, Physics B/C:E/C:EM/C:Mech/1/2
      
      #science & tech tests, all students
      scitech_all <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech.xlsx", "all")
    
      #science & tech tests, female students
      scitech_female <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (1).xlsx", "female")
      
      #science & tech tests, Native students
      scitech_native <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (2).xlsx", "native")
      
      #science & tech tests, Asian students
      scitech_asian <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (3).xlsx", "asian")
      
      #science & tech tests, Black students
      scitech_black <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (4).xlsx", "black")
      
      #science & tech tests, Latinx students
      scitech_latinx <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (5).xlsx", "latinx")
      
      #science & tech tests, Multiracial students
      scitech_multirace <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (6).xlsx", "multirace")
      
      #science & tech tests, Native Hawaiian Pacific Islander students
      scitech_nhpi <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (7).xlsx", "nhpi")
      
      #science & tech tests, White
      scitech_white <- open_rename_MA("data/achievement data/MA DOE/ap_performance_sciencetech (8).xlsx", "white")
  
      
    #Math and Computer Science = Biology, Chemistry, Environmental Science, Physics B/C:E/C:EM/C:Mech/1/2
      
      #math & computer science, all students
      mathCS_all <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience.xlsx", "all")
      
      #math & computer science, female students
      mathCS_female <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (1).xlsx", "female")
      
      #math & computer science, Native students
      mathCS_native <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (2).xlsx", "native")
      
      #math & computer science, Asian students
      mathCS_asian <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (3).xlsx", "asian")
      
      #math & computer science, Black students
      mathCS_black <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (4).xlsx", "black")
      
      #math & computer science, Latinx students
      mathCS_latinx <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (5).xlsx", "latinx")
      
      #math & computer science, Multiracial students
      mathCS_multirace <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (6).xlsx", "multirace")
      
      #math & computer science, Native Hawaiian Pacific Islander students
      mathCS_nhpi <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (7).xlsx", "nhpi")
      
      #math & computer science, White
      mathCS_white <- open_rename_MA("data/achievement data/MA DOE/ap_performance_mathcompscience (8).xlsx", "white")
      
      
##### MERGE ACROSS TEST SUBJECTS
      
      scitech <- merge(scitech_all,scitech_female, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      scitech <- merge(scitech,scitech_native, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      scitech <- merge(scitech,scitech_asian, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      scitech <- merge(scitech,scitech_black, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      scitech <- merge(scitech,scitech_latinx, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      scitech <- merge(scitech,scitech_multirace, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      scitech <- merge(scitech,scitech_nhpi, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      scitech <- merge(scitech,scitech_white, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      
          #checks
          scitech %>% group_by(sch_name) %>% count(tot_taken_all)
          scitech<- scitech %>% mutate(tot_taken_allv2 = select(.,tot_taken_native, tot_taken_asian,tot_taken_black,tot_taken_latinx,tot_taken_multirace,tot_taken_nhpi,tot_taken_white) %>% rowSums(na.rm = T))
          all(scitech$tot_taken_all == scitech$tot_taken_allv2)
          scitech <- scitech %>% select(-tot_taken_allv2)
          scitech <- scitech %>% mutate(test_subj ="science and technology")
          
          
      mathCS <- merge(mathCS_all,mathCS_female, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      mathCS <- merge(mathCS,mathCS_native, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      mathCS <- merge(mathCS,mathCS_asian, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      mathCS <- merge(mathCS,mathCS_black, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      mathCS <- merge(mathCS,mathCS_latinx, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      mathCS <- merge(mathCS,mathCS_multirace, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
      mathCS <- merge(mathCS,mathCS_nhpi, by=c("ma_doe_id", "sch_name"), all.x = TRUE)        
      mathCS <- merge(mathCS,mathCS_white, by=c("ma_doe_id", "sch_name"), all.x = TRUE)
          
          #checks
          mathCS %>% group_by(sch_name) %>% count(tot_taken_all)
          mathCS<- mathCS %>% mutate(tot_taken_allv2 = select(.,tot_taken_native, tot_taken_asian,tot_taken_black,tot_taken_latinx,tot_taken_multirace,tot_taken_nhpi,tot_taken_white) %>% rowSums(na.rm = T))
          all(mathCS$tot_taken_all == mathCS$tot_taken_allv2)
          mathCS <- mathCS %>% select(-tot_taken_allv2)    
          mathCS <- mathCS %>% mutate(test_subj ="math and computer science")
          
          
  MA_APscores <- rbind(scitech, mathCS)
  MA_APscores <- MA_APscores %>%
                      relocate(ma_doe_id, sch_name,test_subj) %>% arrange(ma_doe_id)

  
  save(MA_APscores,file="data/MA_DOE_apscores.RData")
  