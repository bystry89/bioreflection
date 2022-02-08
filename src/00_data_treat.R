library(tidyverse)
library(readr)
library(stringr)

int <- read_csv("data/00_raw_data.csv") %>% 
  slice(3:n())%>%
  rename(gender = Q48,
         read = Q73,
         ab_n2_1 = Q53_1) %>%
  filter(!is.na(prolific))

scenlet <- c("eu", "as", "ab", "su", "va", "or", "gm", "sa", "br", "en", "pb")

tdata <- int %>% select(starts_with(scenlet, 
                                    ignore.case = F), PROLIFIC_PID)

tlong <- tdata %>% 
  gather(-PROLIFIC_PID, key = 'key', value = 'resp')

tlong = tlong %>%
  mutate(
    type = case_when(str_detect(key, '_n_') ~ "Normative", 
                     str_detect(key, '_f_') ~ "Factual",
                     str_detect(key, '_n2') ~ "Normative2", 
                     str_detect(key, '_f2') ~ "Factual2"),
    issue = case_when(
      str_detect(key, 'ab_') ~ "Abortion",
      str_detect(key, 'as_') ~ "Assisted Suicide",
      str_detect(key, 'br_') ~ "Animals in Research",
      str_detect(key, 'en_') ~ "Enhancement",
      str_detect(key, 'eu_') ~ "Euthanasia",
      str_detect(key, 'gm_') ~ "GM Crops",
      str_detect(key, 'or_') ~ "Organ Markets",
      str_detect(key, 'pb_') ~ "Transgender Teen",
      str_detect(key, 'sa_') ~ "Sexual Assistance",
      str_detect(key, 'su_') ~ "Surrogacy",
      str_detect(key, 'va_') ~ "Vaccines"))

tlong %>% left_join(select(int, PROLIFIC_PID, gender, age, 
                           religion, education, politics_1, politics_2,
                           read)) %>% 

write.csv("data/00_long_data.csv")



