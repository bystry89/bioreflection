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

# recode factual statements that negatively correlate with the factual ones

pretest <- pretest %>% mutate(
  resp = if_else(str_detect(key, "ab_f|gm_f|sa_f"),
                 abs(100-as.numeric(resp)), as.numeric(resp)))

# reliability
reliability <- pretest %>% 
  filter(type=='Normative', grepl(paste(selected_keys,collapse="|"), .$key)) %>% 
  select(PROLIFIC_PID, resp, issue) %>% 
  pivot_wider(names_from = issue, values_from = resp, id_cols = PROLIFIC_PID) %>% 
  select(-PROLIFIC_PID) %>% 
  psych::alpha()


# FA
#EFA--factual
fact <- pretest %>% filter(type=='Factual', grepl(paste(selected_keys,collapse="|"), .$key)) %>% 
  select(issue, resp, PROLIFIC_PID) %>% 
  pivot_wider(id_cols = PROLIFIC_PID, names_from = issue, values_from = resp) %>% 
  select(-PROLIFIC_PID) 
fact%>% psych::fa.parallel()

fact2_EFA <- psych::fa(fact, nfactors = 2, rotate='oblimin')
fact2_EFA$loadings
fact2_EFA$e.values
fact2_EFA$score.cor

#EFA--normative
norm <- pretest %>% filter(type=='Normative', grepl(paste(selected_keys,collapse="|"), .$key)) %>% 
  select(issue, resp, PROLIFIC_PID) %>% 
  pivot_wider(id_cols = PROLIFIC_PID, names_from = issue, values_from = resp) %>% 
  select(-PROLIFIC_PID) 
norm%>% psych::fa.parallel()

norm2_EFA <- psych::fa(norm, nfactors = 2, rotate='oblimin')
norm2_EFA$loadings
norm2_EFA$e.values
norm2_EFA$score.cor