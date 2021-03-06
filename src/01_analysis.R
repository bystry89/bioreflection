library(tidyverse)
library(lme4)

# ---- 01_read-data -------
source("src/functions/polit_recode.R")
source("src/functions/scale_this.R")
study1 <- read.csv('data/01_long_data.csv') %>% 
  mutate(reflect = if_else(
    issue == reflection_topic, "Reflected", "Unreflected"
  )) %>% 
  polit_recode()
  

# ---- 01_bf-test
bf_norm <- data.frame()
# for (i in c("Reflected", "Unreflected")) {
#   for (j in unique(study1$issue)[1:7]) {
#     obj <- study1 %>% 
#       filter(order=='First-order', type=="Normative", reflect==i, issue==j) %>% 
#       select(reflect, issue, resp, resp2) %>% 
#       gather('time', 'Resp', -reflect, -issue) %>% 
#       onewaytests::bf.test(Resp ~ time, data = .)
#     bf_norm <- bind_rows(bf_norm, data.frame(Reflect=i,
#                                              Issue =j, 
#                                              Statistic = round(obj$statistic,2),
#                                              DF = paste(obj$parameter[1],round(obj$parameter[2],0), sep="/"),
#                                              p.value=round(obj$p.value,2)))
#     
#   }}

study1 %>% filter(reflect == "Reflected", type=="Normative", order=='First-order') %>% 
  lmer(resp2 ~ resp + reflection_resp_O1  + (1 | issue), data = .) %>% 
  summary()


longer <- study1 %>% dplyr::select(PROLIFIC_PID, key, resp, resp2) %>% 
  gather(-PROLIFIC_PID, -key, key='time', value='Resp') %>% 
  left_join(dplyr::select(
    study1, -resp, -resp2
  ), by=c('PROLIFIC_PID', 'key'))

s1_scaled <-  longer %>% 
  filter(order=='First-order') %>% 
  select(issue, type, Resp, PROLIFIC_PID,time, reflect, RL) %>% 
  group_by(issue, type) %>%
  mutate(z_resp=scale_this(Resp)) %>% select(-Resp) %>% 
  spread(time, z_resp) %>% 
  mutate(diff=resp2-resp)


## reflection models
m1_ref_0 <- s1_scaled %>% 
  filter(type=='Normative') %>% 
  lmer(diff ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) 

m1_ref_1 <- s1_scaled %>% 
  filter(type=='Normative') %>% 
  lmer(diff ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .)

m1_rat_0 <- s1_scaled %>% 
  filter(type=='Factual') %>% 
  lmer(diff ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) 

m1_rat_1 <- s1_scaled %>% 
  filter(type=='Factual') %>% 
  lmer(diff ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .)

m1_ref_4 <- study1 %>% 
  filter(order=="First-order", reflect == "Reflected") %>% 
  group_by(issue, type) %>% 
  mutate(z_reflected=scale_this(reflection_resp_O1)) %>%
  select(PROLIFIC_PID, key, z_reflected) %>% 
  left_join(s1_scaled) %>% 
  filter(type == "Normative") %>% 
  lmer(resp2 ~ resp + z_reflected + (1 | issue), data = .)

# m1_rat_4 <- study1 %>% 
#   filter(order=="First-order", reflect == "Reflected") %>% 
#   group_by(issue, type) %>% 
#   mutate(z_reflected=scale_this(reflection_resp_O1)) %>%
#   select(PROLIFIC_PID, key, z_reflected) %>% 
#   left_join(s1_scaled) %>% 
#   filter(type == "Factual") %>% 
#   lmer(resp2 ~ resp + z_reflected + (1 | issue), data = .)


  

s1_devs <-
  s1_scaled %>% 
  mutate(absdev1 = abs(resp),
        absdev2 = abs(resp2),
        diff_absdebv=absdev2-absdev1)


m1_ref_2 <- s1_devs %>% 
  filter(type=='Normative') %>% 
  lmer(diff_absdebv ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) 

m1_ref_3 <- s1_devs %>% 
  filter(type=='Normative') %>% 
  lmer(diff_absdebv ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .)

m1_rat_2 <- s1_devs %>% 
  filter(type=='Factual') %>% 
  lmer(diff_absdebv ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) 

m1_rat_3 <- s1_devs %>% 
  filter(type=='Factual') %>% 
  lmer(diff_absdebv ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .)
# 
# s1_conf <- study1 %>% 
#   filter(order == "Second-order") %>% 
#   mutate(diff = resp2 - resp)
# 
# s1_conf %>% 
#   filter(type == "Normative") %>% 
#   lmer(diff ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) %>% 
#   summ()
# 
# s1_conf %>% 
#   filter(type == "Normative") %>% 
#   lmer(diff ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .) %>% 
#   summ()
# 
# s1_conf %>% 
#   filter(type == "Factual") %>% 
#   lmer(diff ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) %>% 
#   summ()
# 
# s1_conf %>% 
#   filter(type == "Factual") %>% 
#   lmer(diff ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .) %>% 
#   summ()

m1_ref_4 <- s1_scaled %>% 
  filter(reflect == "Reflected") %>% 
  left_join(filter(study1, order == "First-order"), by = c("issue", "type", "PROLIFIC_PID", "reflect", "RL")) %>% 
  filter(type == "Normative") %>% 
  group_by(issue) %>% 
  mutate(reflect_fact = scale_this(reflection_resp_O1)) %>% 
  pivot_longer(cols=c(resp.x, resp2.x), names_to = "time", values_to = "resp") %>% 
  lmer(resp ~ reflect_fact*time + (1 | issue) + (1 | PROLIFIC_PID), data=.) 

