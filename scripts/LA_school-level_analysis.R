# CLEAR MEMORY
rm(list = ls())


# LIBRARIES 
options(max.print=999999)
library(tidyverse) 
library(assertthat)
library(stringr)


#############################################
#OPEN DATA

  # Open Student List Data (Urbana Champaign)
  df_sl <- read.csv("data/145637_lists.csv", na.strings="")

    #keep only domestic purchases
    df_sl <- df_sl %>% filter(Country=="United States")

  # Open CEEB to NCES Crosswalk
  ceeb_nces <- read.csv("data/ceeb_nces_crosswalk.csv")
  
  # Open CDS to NCES Crosswalk
  cds_nces <- read.csv("data/cds_nces_crosswalk.csv", na.strings="")
  
  
# MERGING DATA
  
  # Merge in NCES to Student List DF
    
    #ceeb does not uniquely identify rows in CEEB to NCES crosswalk
    ceeb_nces %>% group_by(ceeb) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)
    
    #ceeb + ncessch uniquely identifies rows [Are SOME CEEBS assigned to multiple NCESSCH?]
    ceeb_nces %>% group_by(ceeb, ncessch) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)
    
    #add counter to investigate 
    ceeb_nces <- ceeb_nces %>%
      group_by(ceeb) %>%
      mutate(counter = row_number())
    
    ceeb_nces %>%
      group_by(counter) %>% count()
    
    ceeb_dups <- ceeb_nces %>% filter(counter>1) %>% print(n=1500)
    
    #check to see if all duplicate CBS have the same NCES [KS investigation: first NCES ID is false/no school match; 2nd ID matches via NCES look up tool]
    ceeb_nces %>% arrange(ceeb) %>%
      filter(ceeb %in% ceeb_dups$ceeb)  %>% print(n=5000)
    
    #remove duplicate CEEB except for the last instance (i.e, 2nd obs)
    ceeb_nces <- ceeb_nces %>%
      group_by(ceeb) %>%
      slice(which.max(counter))
    
    #ceeb now uniquely identifies rows in CEEB to NCES crosswalk
    ceeb_nces %>% group_by(ceeb) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)
    
    #drop counter var
    ceeb_nces <- ceeb_nces %>% select(-counter)
    
    #check num of missing CEEBS in Student List Data
    df_sl %>% count(is.na(SchoolCode)) #717 missing CEEB
    
    # Student List Data CEEB missing leading zeros! [KS: need to investigate this better]
    df_sl <- df_sl %>% mutate(SchoolCodeV2= str_pad(SchoolCode, 6, pad = "0"))
    
    # merge NCES to student list data
    df_sl <- df_sl %>% rename(ceeb=SchoolCodeV2) %>% left_join(ceeb_nces, by = "ceeb")
    
    # check for obs that didnt merge
    anti_df_sl <- df_sl %>% anti_join(ceeb_nces, by = "ceeb") #before leading zero fix this was 70k, now only 9k 
    anti_df_sl %>% filter(str_detect(ceeb, "^E")) %>% count() #2050 ceebs that did not merge start with "E" (no such ceeb codes in crosswalk)
    
      #KS investigation Notes
      "some of the anti join results are due to CEEB codes that do not have matching NCES codes bc
        1) CEEB crosswalk doesnt contain that CEEB record. 
            For example, lookup school name for CEEB=050599 via college board search tool https://collegereadiness.collegeboard.org/k-12-school-code-search
            Which results in Vivian Webb School, and in NCES lookup tool NCES=00083484"
    
        anti_df_sl %>% filter(ceeb=="050599") %>% count() #56 obs are due to this specific problem 

    
  # Merge in CDS (CA DOE ID) to Student List DF 
    
    #Cds does uniquely identify rows in CEEB to NCES crosswalk
    cds_nces %>% group_by(CDSCode) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)

    #replace "no data" to missing in school code
    cds_nces <- cds_nces %>% 
              mutate(NCESSchool= ifelse(NCESSchool=="No Data", NA, NCESSchool))
    
    #need to concatenate district + school code = NCESsch ID
    cds_nces <- cds_nces %>% mutate(ncessch = str_c(NCESDist, NCESSchool))
    cds_nces %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)
  
    #keep only ncees and cds columns and filter out missing to avoid merging errors for now
    cds_nces <- cds_nces %>% select(ncessch, CDSCode) %>% filter(!is.na(ncessch))
    
    #add counter to investigate duplicate ncessch
    cds_nces %>% group_by(ncessch) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)
    
    cds_nces <- cds_nces %>%
      group_by(ncessch) %>%
      mutate(counter = row_number())
    
    cds_nces %>%
      group_by(counter) %>% count()
    
    cds_dups <- cds_nces %>% filter(counter>1)
    
    #check to see if all duplicate ncessch have the same CDS [KS investigation: 54 obs with duplicate CDS dont seem to have ANY real nccesch]
    cds_nces %>% arrange(ncessch) %>%
      filter(ncessch %in% cds_dups$ncessch)  %>% print(n=5000)
    
    #remove duplicate cds
    cds_nces <- cds_nces %>%
      filter(!ncessch %in% cds_dups$ncessch)
    
    #keep only CA obs in student list data
    df_sl_CA <- df_sl %>% filter(State=="CA")
    
    # merge CDS to CA student list data [only keep matches for now]
    df_sl_CA <- df_sl_CA %>% left_join(cds_nces, by = "ncessch")
    
    df_sl_CA %>% count(is.na(ncessch))
    df_sl_CA %>% count(is.na(CDSCode))
    
    df_sl_CA %>% filter(is.na(CDSCode)) %>% group_by(ncessch) %>% count() %>% print(n=400)

    #save csv file
    #write_csv(df_sl_CA, "data/145637_list_ca.csv")
