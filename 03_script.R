library(readr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)

survey <- read_csv("expert+survey_September+27,+2021_15.44.csv")
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

#recode so that experts' views are alway on the right-hand side
# ex_long <- ex_long %>% mutate(
#   resp = if_else(str_detect(key, "va"), 
#                  abs(100-as.numeric(resp)), as.numeric(resp)))

ex_long <- read.csv("../ex_long.csv")

ex_long <- ex_long%>% 
  left_join(select(survey, Q30, gender, age, politics, ResponseId), by='ResponseId') %>% 
  mutate(expert=replace_na(expert,'0'))

ex_long$expert <- as.factor(ex_long$expert)
ex_long$resp <- as.numeric(ex_long$resp)


#differences between expert experts and non-expert experts
pilot_exp$expert <- as.factor(pilot_exp$expert)
pilot_exp %>% group_by(issue,expert) %>%
  dplyr::summarise(mean=mean(resp, na.rm=T), sd=sd(resp, na.rm=T), n=n()) %>% 
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  ggplot(aes(x=mean, y=expert))+
  geom_point()+
  geom_linerange(aes(xmin=lower.ci, xmax=upper.ci))+
  geom_point(data=pilot_exp, aes(x=resp, y=expert), alpha=0.1, 
             position = position_jitter(height=.1))+
  geom_vline(aes(xintercept=mean), linetype='dashed', alpha=.4)+
  facet_grid(.~issue)+
  theme_minimal()


write.csv(ex_long, file='ex_long.csv')

ex_long <- read.csv("../ex_long.csv")

##### validate
library(dplyr)
pilot_exp <- pretest %>% 
  filter(type=='Factual', grepl(paste(selected_keys,collapse="|"), .$key)) %>% 
  mutate(expert=0) %>% bind_rows(mutate(filter(ex_long,
                                               order == 'First-order'),
                                        expert=1,
                                        PROLIFIC_PID=ResponseId,
                                        type="Factual")) %>% mutate(
                                          resp = if_else(str_detect(key, "ab|gm|sa"), 
                                                         abs(100-as.numeric(resp)), 
                                                         as.numeric(resp))) 

lmer(resp ~ expert + (1 | PROLIFIC_PID) + (1 | issue), data = pilot_exp) %>% 
  car::Anova()

pilot_exp %>% 
  group_by(issue, expert) %>% 
  summarize(median=median(resp, na.rm=T)) %>% 
  right_join(pilot_exp) %>% 
  mutate(absdev = abs(median - resp)) %>% 
  lmer(absdev  ~ expert + (1 | PROLIFIC_PID) + (1 | issue), data = .) %>% 
  car::Anova()
  emmeans::emmeans(pairwise ~ expert)
