library(tidyverse)
library(lme4)
library(lmerTest)

# lay sample
source("src/functions/polit_recode.R")
source("src/functions/scale_this.R")
study2 <- read.csv("data/02_long_data.csv") %>% 
  filter(!is.na(resp), !is.na(issue))%>% polit_recode()
  
dupl <- study2 %>% count(issue, type, order, PROLIFIC_PID) %>% filter(n>1) %>% .$PROLIFIC_PID %>% unique()

study2 <- study2 %>% filter(!PROLIFIC_PID %in% dupl)


#expert sample
#ex_long <- read.csv("ex_long.csv")

#identify reflected/filler issues
study2$reflect <- if_else(study2$issue == study2$reflection_topic, 
                         "Target issue", "Non-target issue")




#stacked set--DV
longer <- study2 %>% dplyr::select(PROLIFIC_PID, key, resp, resp2) %>% 
  gather(-PROLIFIC_PID, -key, key='time', value='Resp') %>% left_join(dplyr::select(
    study2, -resp, -resp2
  ), by=c('PROLIFIC_PID', 'key'))

s2_scaled <-  longer %>% 
  filter(order=='First-order') %>% 
  select(issue, type, Resp, PROLIFIC_PID,time, reflect) %>% 
  group_by(issue, type) %>%
  mutate(z_resp=scale_this(Resp)) %>% select(-Resp) %>% 
  spread(time, z_resp) %>% 
  mutate(diff=resp2-resp)

# # normalize DVs within each issue/type
# normal <- study2er_resp %>% filter(order=='First-order') %>% 
#   mutate(sample='lay') %>% bind_rows(dplyr::rename(mutate(ex_long, sample='expert', type='Factual'), Resp=resp)) %>% 
#   select(issue, type, Resp, PROLIFIC_PID, sample, time, reflect, politics_1) %>% 
#   group_by(issue, type) %>%
#   mutate(z_resp=scale_this(Resp)) 

# calculate delta_f for each layperson/issue
fact_shift <- s2_scaled %>%ungroup() %>% filter(type == "Factual") %>% 
  # filter(
  # #sample=='lay', 
  # type=='Factual') %>% select(PROLIFIC_PID, issue, time, z_resp) %>% 
  # spread(time, z_resp) %>%select(PROLIFIC_PID, issue, resp, resp2) %>%
  # mutate(delta_f=resp2-resp) %>% 
  select(PROLIFIC_PID, issue, diff) 

# wide format for normative target statements
normal_n <- s2_scaled %>%  
  #pivot_longer(cols=c(resp, resp2), names_to = "time") %>% 
  filter(
    #sample=='lay', 
    type=='Normative', reflect=='Target issue') %>% 
  select(PROLIFIC_PID, type, issue, resp, resp2) %>% 
   left_join(fact_shift, by=c('PROLIFIC_PID', 'issue'))

# normative shift model -- without random slope
m2_norm_shift <- normal_n %>% 
  lmer(resp2~resp+diff + (1|issue), data=.) 

# normative shift model -- without random slope; per issue
normative_coef=data.frame()
for (i in levels(as.factor(normal_n$issue)))  {
  estimate=normal_n%>% filter(issue==i) %>% 
    lm(resp2~resp+diff, data=.) %>% summary() %>%broom::tidy()
  normative_coef=bind_rows(normative_coef,data.frame(issue=i, prior=estimate$estimate[2], pvalue1=estimate$p.value[2],factual_shift=estimate$estimate[3], pvalue2=estimate$p.value[3]))
}
#normative_coef

# normative shift model -- with random slope
m2_norm_rs <- normal_n %>% 
  lmer(resp2~resp+diff + (1+diff|issue), data=.) %>% summary()

# does the correlation with politics decrease following reflection? not really
# study2 %>% filter(order=='First-order', type=='Normative', reflect=='Target issue') %>% group_by(issue, type) %>% 
#   dplyr::summarise(cor=cor(politics_1, resp), cor2=cor(politics_1, resp2)) %>% mutate(diff=cor2-cor)

# is there an interaction between politics and time? not at all
# s2_scaled %>% filter(type=='Normative', reflect=='Target issue') %>% 
#   pivot_longer(cols=c(resp, resp2), names_to = "time", values_to = "resp") %>% 
#   left_join(select(filter(study2, order=="First-order"), PROLIFIC_PID, politics_1, issue, type)) %>% 
#   lmer(resp~politics_1*time + (1|issue), data=.) %>% summary()

# expert mean opinions per issue
# expert <- normal %>% filter(sample=='expert') %>% dplyr::group_by(issue) %>% 
#   dplyr::summarize(expert_mean=mean(z_resp))

# does the lay factual distance from experts decrease following reflection? the opposite if anything
# normal %>% filter(sample=='lay', type=='Factual') %>% 
#   left_join(expert) %>% mutate(dist=abs(expert_mean-z_resp)) %>% filter(reflect=='Target issue') %>% 
#   lmer(dist~time + (1|issue), data=.) %>% summary()
# 
# 
# # do expert beliefs and time predict lay factual responses? It doesn't seem so...
# normal %>% filter(sample=='lay', type=='Factual', reflect=='Target issue') %>%
#   left_join(expert) %>% 
#   lmer(z_resp~expert_mean*time + (1|issue), data=.) %>% 
#   car::Anova( type = 2, test.statistic = 'F')



##### replication
m2_ref_0 <- s2_scaled %>% 
  filter(type=='Normative') %>% 
  lmer(diff ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) 

m2_ref_1 <- s2_scaled %>% 
  filter(type=='Normative') %>% 
  lmer(diff ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .)


s2_devs <- longer %>% 
  filter(order=='First-order') %>% 
  group_by(type, issue) %>% 
  dplyr::summarize(median=median(Resp), sd=sd(Resp)) %>% 
  right_join(longer, by=c('type', 'issue')) %>% 
  select(PROLIFIC_PID, order, type, issue, Resp, median, sd, time, reflect, RL) %>% 
  spread(time, Resp) %>% 
  mutate(absdev1 = abs(resp-median)/sd,
         absdev2 = abs(resp2-median)/sd,
         diff_absdebv=absdev2-absdev1)

m2_ref_2 <- s2_devs %>% 
  filter(type=='Normative') %>% 
  lmer(diff_absdebv ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) 

m2_ref_3 <- s2_devs %>% 
  filter(type=='Normative') %>% 
  lmer(diff_absdebv ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .)

m1_rat_2 <- s2_devs %>% 
  filter(type=='Factual') %>% 
  lmer(diff_absdebv ~ 1 + (1 | PROLIFIC_PID) + (1 | issue), data = .) 

m1_rat_3 <- s2_devs %>% 
  filter(type=='Factual') %>% 
  lmer(diff_absdebv ~ reflect + (1 | PROLIFIC_PID) + (1 | issue), data = .)


m2_f_dev <- s2_devs %>% 
  pivot_longer(cols=c(absdev1:absdev2), names_to = "time", values_to = "abs_dev") %>% 
  filter(type == "Factual", reflect == "Target issue", order == "First-order") %>% 
  lmer(abs_dev ~ time + (1 | issue), data = .)
