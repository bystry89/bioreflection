library(tidyverse)

#session 1
part1 <- read.csv("data/02a_raw_data.csv")

#code politics
part1 <- part1[3:nrow(part1),] %>% 
  mutate(RL = case_when(STUDY_ID == '610fa0817bba062b1fb158c5'~ "Right", 
                        STUDY_ID == "610fa2dc06d18e9f8a5da6e9"~"Left")) 

#session 2 data
part2 <- read.csv("data/02b_raw_data.csv")
part2 <- part2[3:nrow(part2),]

tdata = part1 %>%
  select(Finished, 
         #first-order
         ab_f_1, ab_n_1,
         su_f_1, su_n_1,
         va_f_1, va_n_1, 
         gm_f_1, gm_n_1,
         sa_f_1, sa_n_1, 
         br_f_1, br_n_1, 
         pb_f_1, pb_n_1, 
         #second-order
         ab_c_f_1, ab_c_n_1,
         su_c_f_1, su_c_n_1,
         va_c_f_1, va_c_n_1, 
         gm_c_f_1, gm_c_n_1,
         sa_c_f_1, sa_c_n_1, 
         br_c_f_1, br_c_n_1, 
         pb_c_f_1, pb_c_n_1,PROLIFIC_PID) %>% 
  filter(!is.na(PROLIFIC_PID), Finished == 1) %>%
  select(-Finished)


tlong = tdata %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'resp')

tlong = tlong %>%
  mutate(
    type = case_when(str_detect(key, '_n') ~ "Normative", 
                     str_detect(key, '_f') ~ "Factual"),
    order = case_when(str_detect(key, '[a-z]{2}_c_') ~ "Second-order",
                      str_detect(key, '[a-z]{2}_[fn]_') ~ "First-order"),
    issue = case_when(
                      str_detect(key, 'ab_') ~ "Abortion",
                      str_detect(key, 'br_') ~ "Animals in Research",
                      str_detect(key, 'gm_') ~ "GM Crops",
                      str_detect(key, 'pb_') ~ "Transgender Teen",
                      str_detect(key, 'sa_') ~ "Sexual Assistance",
                      str_detect(key, 'su_') ~ "Surrogacy",
                      str_detect(key, 'va_') ~ "Vaccines"))

tlong <- tlong %>% 
  inner_join(select(part1, gender, age, religion, 
                   education, politics_1, politics_2, RL, PROLIFIC_PID), 
            by="PROLIFIC_PID") 
tlong <- tlong %>% filter(resp != "")


tdata2 = part2 %>%
  select(Finished, 
         #first-order
         ab_f_1, ab_n_1,
         su_f_1, su_n_1,
         va_f_1, va_n_1, 
         gm_f_1, gm_n_1,
         sa_f_1, sa_n_1, 
         br_f_1, br_n_1, 
         pb_f_1, pb_n_1, 
         #second-order
         ab_c_f_1, ab_c_n_1,
         su_c_f_1, su_c_n_1,
         va_c_f_1, va_c_n_1, 
         gm_c_f_1, gm_c_n_1,
         sa_c_f_1, sa_c_n_1, 
         br_c_f_1, br_c_n_1, 
         pb_c_f_1, pb_c_n_1,PROLIFIC_PID) %>% 
  filter(!is.na(PROLIFIC_PID), Finished == 1) %>%
  select(-Finished)


tlong2 = tdata2 %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'resp')%>% filter(resp != "") %>% 
  dplyr::rename(resp2 = resp)

tlong_full <- tlong %>% right_join(tlong2, by = c("PROLIFIC_PID", "key"))

reflection <- part2 %>% select(ab_f_ref_1, su_f_ref_1,
                                    va_f_ref_1, gm_f_ref_1,
                                    sa_f_ref_1, br_f_ref_1, 
                                    pb_f_ref_1, PROLIFIC_PID) %>% 
  gather(key = 'key', value = 'resp', -PROLIFIC_PID) %>% filter(resp != '') %>% 
  left_join(select(part2, issue, Q75_Page.Submit, explain, PROLIFIC_PID, c_ref_1), 
            by = 'PROLIFIC_PID') %>% 
  dplyr::rename(reflection_topic = issue, reflection_time = Q75_Page.Submit, 
         reflection_resp_O1 = resp, reflection_resp_O2 = c_ref_1) %>% 
  mutate(reflection_topic = case_when(
    str_detect(key, 'ab_') ~ "Abortion",
    str_detect(key, 'br_') ~ "Animals in Research",
    str_detect(key, 'gm_') ~ "GM Crops",
    str_detect(key, 'pb_') ~ "Transgender Teen",
    str_detect(key, 'sa_') ~ "Sexual Assistance",
    str_detect(key, 'su_') ~ "Surrogacy",
    str_detect(key, 'va_') ~ "Vaccines")) %>% select(-key)

tlong_full <- tlong_full %>% left_join(reflection, by = 'PROLIFIC_PID')
write.csv(tlong_full, 'data/02_long_data.csv')
