

# create data frame of prospect-level data from U Illinois-Chicago, order '487927'


list_native_df <- lists_orders_zip_hs_df %>% filter(univ_id == '145600', ord_num =='487927')


list_native_df <- list_native_df %>% 
  select(univ_id,ord_num,univ_state,univ_zip,
    stu_state,stu_city,stu_zip_code,stu_geomarket,stu_country,stu_in_us,stu_hs_code,stu_county_code,stu_gender,
    stu_cuban,stu_mexican,stu_puerto_rican,stu_other_hispanic,stu_non_hispanic,stu_ethnicity_no_response,
    stu_american_indian,stu_asian,stu_black,stu_native_hawaiian,stu_white,stu_race_no_response,
    stu_major_1,
    na_zip_acs,zip_cbsa_1,zip_cbsatitle_1,zip_csacode,zip_csatitle,zip_median_household_income,
    zip_pop_total,zip_pop_white,zip_pop_black,zip_pop_amerindian,zip_pop_asian,zip_pop_nativehawaii,zip_pop_otherrace,zip_pop_tworaces,zip_pop_hispanic,
    na_hs,hs_private,hs_cbsa_1,hs_cbsatitle_1,hs_csacode,hs_csatitle,hs_name,hs_ncessch,hs_state_code,hs_zip_code,
    hs_total_students,hs_total_amerindian,hs_total_asian,hs_total_black,hs_total_hispanic,hs_total_nativehawaii,hs_total_tworaces,hs_total_unknown,hs_total_white,
    hs_pct_amerindian,hs_pct_asian,hs_pct_black,hs_pct_hispanic,hs_pct_nativehawaii,hs_pct_tworaces,hs_pct_unknown,hs_pct_white,    
  )

list_native_df %>% count(stu_asian)
list_native_df %>% count(stu_white,stu_race_no_response)

# create ethnicity and race variables
list_native_df %>% mutate(
  stu_hispanic_01 = case_when(
    (stu_cuban == 'Y' | stu_mexican == 'Y' | stu_puerto_rican == 'Y' | stu_other_hispanic == 'Y') ~ 1, # any of the hispanic categories equal 'Y'
    (stu_non_hispanic=='Y' & is.na(stu_cuban)==1 & is.na(stu_mexican)==1 & is.na(stu_puerto_rican)==1 & is.na(stu_other_hispanic)==1) ~ 0,   # == 0 if non_hispanic == 1 and all four categories == NA; sometimes students select 'non_hispanic' and also mexican or cuban, etc.
  ),
  stu_american_indian_01 = case_when(stu_american_indian == 'Y' ~ 1,is.na(stu_american_indian) & is.na(stu_race_no_response) ~ 0),
  stu_asian_01 = case_when(stu_asian == 'Y' ~ 1,is.na(stu_asian) & is.na(stu_race_no_response) ~ 0),
  stu_black_01 = case_when(stu_black == 'Y' ~ 1,is.na(stu_black) & is.na(stu_race_no_response) ~ 0),
  stu_native_hawaiian_01 = case_when(stu_native_hawaiian == 'Y' ~ 1,is.na(stu_native_hawaiian) & is.na(stu_race_no_response) ~ 0),
  stu_white_01 = case_when(stu_white == 'Y' ~ 1,is.na(stu_white) & is.na(stu_race_no_response) ~ 0),
)
stu_race_no_response,
# %>% filter(is.na(stu_hispanic_01)) %>% count(stu_ethnicity_no_response)
# %>% filter(stu_ethnicity_no_response == 'Y') %>% count(stu_hispanic_01)
# select(stu_cuban,stu_mexican,stu_puerto_rican,stu_other_hispanic,stu_non_hispanic,stu_ethnicity_no_response,stu_hispanic_01) %>% View()

stu_ethnicity_no_response

list_native_df %>% count(stu_ethnicity_no_response) %>% print(n=50)
list_native_df %>% filter(stu_ethnicity_no_response == 'Y') %>% count(stu_non_hispanic)

list_native_df %>% filter(stu_non_hispanic == 'Y') %>% count(stu_other_hispanic)





list_native_df %>% glimpse()


list_native_df %>% count(stu_country) %>% print(n=50)

list_native_df %>% count(stu_ethnicity_no_response) %>% print(n=50)
list_native_df %>% count(stu_race_no_response) %>% print(n=50)
list_native_df %>% count(stu_grad_year) 
list_native_df %>% count(stu_major_1) %>% print(n=350)
list_native_df %>% count(hs_private) %>% print(n=350)
list_native_df %>% count(na_hs) %>% print(n=350)
list_native_df %>% count(hs_total_amerindian) %>% print(n=350)




$ stu_is_hisp_common             <dbl> 1, 1, 1, 1, 0, NA, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1,~
$ stu_american_indian_common     <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
$ stu_asian_common               <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, ~
$ stu_black_common               <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, ~
$ stu_native_hawaiian_common     <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, ~
$ stu_white_common               <dbl> 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, ~
$ stu_race_no_response_common    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ stu_other_common               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
$ stu_multi_race_common          <dbl> 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, ~
$ stu_race_cb                    <dbl+lbl>  4,  4,  4,  4,  1,  0, 12,  1, 12,  4,  4, 12, 12~

lists_df_na
lists_df_145600 %>% filter(order_no == '487927')

lists_df %>% glimpse()

lists_orders_zip_hs_df %>% glimpse()

# 
lists_orders_zip_hs_df %>% filter(univ_id == '145600', ord_num =='487927') %>%
  count(stu_race_cb)

lists_orders_zip_hs_df %>% filter(univ_id == '145600', ord_num =='487927') %>%
  count(stu_american_indian_common)

lists_orders_zip_hs_df %>% filter(univ_id == '145600', ord_num =='487927') %>%
  count(stu_american_indian)

lists_orders_zip_hs_df %>% filter(univ_id == '145600', ord_num =='487927') %>%
  count(stu_american_indian_common)

lists_orders_zip_hs_df %>% filter(univ_id == '145600', ord_num =='487927') %>%
  count(stu_state) %>% print(n=55)


