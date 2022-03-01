library(readr)
library(tidyverse)

survey <- read_csv("data/03_raw_data.csv")
survey <- survey[3:nrow(survey),] %>% filter(Finished==1)

ex_long <- survey[,c(9,19,20:33)] %>% gather(-ResponseId, -competence, key='key',value='resp')%>%
  mutate(order = case_when(str_detect(key, '[a-z]{2}_c_') ~ "Second-order",
                           str_detect(key, '[a-z]{2}_1') ~ "First-order"),
         issue = case_when(
           str_detect(key, 'ab_') ~ "Abortion",
           str_detect(key, 'br_') ~ "Animals in Research",
           str_detect(key, 'gm_') ~ "GM Crops",
           str_detect(key, 'pb_') ~ "Transgender Teen",
           str_detect(key, 'sa_') ~ "Sexual Assistance",
           str_detect(key, 'su_') ~ "Surrogacy",
           str_detect(key, 'va_') ~ "Vaccines"),
         #is the issue in respondent's AOCs?
         expert = case_when(
           str_detect(key, 'ab_') & str_detect(competence, '1') ~ "1",
           str_detect(key, 'br_') & str_detect(competence, '7') ~ "1",
           str_detect(key, 'gm_') & str_detect(competence, '3') ~ "1",
           str_detect(key, 'pb_') & str_detect(competence, '2') ~ "1",
           str_detect(key, 'sa_') & str_detect(competence, '5') ~ "1",
           str_detect(key, 'su_') & str_detect(competence, '4') ~ "1",
           str_detect(key, 'va_') & str_detect(competence, '6') ~ "1"))

write.csv(ex_long, "data/03_long_data.csv")
