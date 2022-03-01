library(tidyverse)
library(lme4)

selected_keys <- c('ab_f_', 'ab_n_', 'su_n_','su_f2','va','gm_f_','gm_n_',
                   'sa', 'br_f2', 'br_n2', 'pb')

pretest <- read.csv("data/00_long_data.csv") %>% 
  mutate(type = case_when(
    type == "Normative2" ~ "Normative",
    type == "Factual2" ~ "Factual",
    type == "Normative" ~ "Normative",
    type == "Factual" ~ "Factual"
  )) %>% mutate(
    resp = if_else(str_detect(key, "sa_n|su_n"),
                   abs(100-as.numeric(resp)), as.numeric(resp)))%>% 
  mutate(
                     resp = if_else(str_detect(key, "ab_f|gm_f|sa_f"),
                                    abs(100-as.numeric(resp)), as.numeric(resp)),
                     sample = "lay") %>% 
  filter(grepl(paste(selected_keys,collapse="|"), .$key))
  

# expert <- read.csv("../ex_long.csv") %>% 
#   filter(order == "First-order") %>% 
#   mutate(
#     resp = if_else(str_detect(key, "ab|gm|sa"),
#                    abs(100-as.numeric(resp)), as.numeric(resp)),
#     sample = "expert") %>% 
#   rename(PROLIFIC_PID = ResponseId)
# 
# 
# mRight <- pretest %>% 
#   filter(RL == "R",
#          type == "Factual") %>% 
#   bind_rows(expert) %>% 
#   lmer(resp ~ sample + (1 | issue) + (1 | PROLIFIC_PID), data = .)
# 
# mLeft <- pretest %>% 
#   filter(RL == "L",
#          type == "Factual") %>% 
#   bind_rows(expert) %>% 
#   lmer(resp ~ sample + (1 | issue) + (1 | PROLIFIC_PID), data = .)
# 
# jtools::summ(mRight)
# jtools::summ(mLeft)


m0_f <- pretest %>% 
  filter(type == "Factual") %>% 
  lmer(resp ~ RL + (1 | issue) + (1 | PROLIFIC_PID), data = .)

m0_n <- pretest %>% 
  filter(type == "Normative") %>% 
  lmer(resp ~ RL + (1 | issue) + (1 | PROLIFIC_PID), data = .)

m0_pol <- pretest %>% 
  pivot_wider(id_cols = c(PROLIFIC_PID, issue), names_from = type, values_from = resp) %>% 
  left_join(select(pretest,PROLIFIC_PID, issue, politics_1)) %>% 
  lmer(Normative ~ Factual + politics_1 + (1 | issue) + (1 | PROLIFIC_PID), data = .) 

# pretest %>% 
#   filter(type == "Factual") %>% 
#   bind_rows(expert) %>% 
#   ggplot(aes(x=resp))+
#   geom_density()+
#   facet_grid(sample~issue)
