library(tidyverse)
library(lme4)

ex <- read.csv("data/03_long_data.csv") %>% 
  mutate(resp = if_else(order == "First-order" & str_detect(key, "ab|gm|sa"),
                 abs(100-as.numeric(resp)), as.numeric(resp)),
                 sample = "expert") %>% 
  rename(PROLIFIC_PID = ResponseId)

pretest <- read.csv("data/00_filt_data.csv") %>% mutate(sample = "lay") 

mRight <- pretest %>%
  filter(RL == "R",
         type == "Factual") %>%
  bind_rows(filter(ex, order == "First-order")) %>%
  lmer(resp ~ sample + (1 | issue) + (1 | PROLIFIC_PID), data = .)

mLeft <- pretest %>%
  filter(RL == "L",
         type == "Factual") %>%
  bind_rows(filter(ex, order == "First-order")) %>%
  lmer(resp ~ sample + (1 | issue) + (1 | PROLIFIC_PID), data = .)

jtools::summ(mRight)
jtools::summ(mLeft)

pretest_exp <- ex %>% 
  filter(order == "First-order") %>% 
  bind_rows(filter(pretest, type == "Factual"))

m3_dv <- lmer(resp ~ sample + (1 | issue) + (1 | PROLIFIC_PID), data = pretest_exp) 

s1_devs <- pretest_exp %>% 
  filter(!is.na(resp)) %>% 
  group_by(sample, issue) %>% 
  dplyr::summarize(median=median(resp), sd=sd(resp)) %>% 
  right_join(pretest_exp, by=c('sample', 'issue')) %>% 
  select(PROLIFIC_PID, order, issue, resp, median, sd, sample) %>% 
  mutate(absdev = abs(resp-median)/sd)

m3_dev <- lmer(absdev ~ sample + (1 | issue) + (1 | PROLIFIC_PID), data = s1_devs)

exp_per_issue <- function(x) {
  pretest_exp %>% 
    filter(issue == x) %>% 
    t.test(resp ~ sample, data = .)
}
        