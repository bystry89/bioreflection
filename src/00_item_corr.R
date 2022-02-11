library(tidyverse)

pretest <- read.csv("data/00_long_data.csv")

selected_keys <- c('ab_f_', 'ab_n_', 'su_n_','su_f2','va','gm_f_','gm_n_',
                   'sa', 'br_f2', 'br_n2', 'pb')

pretest <- pretest %>% 
  mutate(type = case_when(
    type == "Normative2" ~ "Normative",
    type == "Factual2" ~ "Factual",
    type == "Normative" ~ "Normative",
    type == "Factual" ~ "Factual"
  ))

pretest %>%  filter(type=='Normative', grepl(paste(selected_keys,collapse="|"), .$key)) %>% 
  group_by(issue, type) %>% 
      dplyr::summarise(cor=cor(politics_1, resp, use='pairwise.complete.obs'))


# recode normative statements that negatively correlate with politics
pretest <- pretest %>% mutate(
  resp = if_else(str_detect(key, "sa_n|su_n"),
                 abs(100-as.numeric(resp)), as.numeric(resp)))

# now, which factual statements negatively correlate with normative statements?
pretest %>% filter(grepl(paste(selected_keys,collapse="|"), .$key)) %>%
  select(issue, type, resp, PROLIFIC_PID) %>% 
  spread(type, resp) %>% 
  group_by(issue) %>%
  dplyr::summarise(cor=cor(Factual, Normative, use="pairwise.complete.obs"))
