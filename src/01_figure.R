library(ggplot2)
library(tidyverse)


source("src/functions/polit_recode.R")
longer <- read.csv("data/01_long_data.csv") %>%
  mutate(reflect = if_else(
    issue == reflection_topic, "Reflected", "Unreflected"
  )) %>%
  polit_recode() %>%
  #gather('time', 'Resp', -c(1:3, 5:14, 16:21))

dplyr::select(PROLIFIC_PID, key, resp, resp2) %>% 
  gather(-PROLIFIC_PID, -key, key='time', value='Resp') %>% 
  left_join(dplyr::select(
    study1, -resp, -resp2
  ), by=c('PROLIFIC_PID', 'key'))  %>%  filter(order=='First-order') %>% 
  select(issue, type, Resp, PROLIFIC_PID,time, reflect, RL) %>% 
  group_by(issue, type) %>%
  mutate(z_resp=scale_this(Resp)) %>% select(-Resp) %>% 
  spread(time, z_resp) %>% 
  mutate(diff=resp2-resp)

devs <- 
  longer %>% 
  mutate(absdev1 = abs(resp),
         absdev2 = abs(resp2),
         diff_absdebv=absdev2-absdev1) %>% 
  group_by(reflect, issue, type) %>% 
  summarize(resp=mean(resp2)-mean(resp), 
            absdev=mean(absdev2)-mean(absdev1)) %>% 
  mutate(reflect=recode(reflect, '1'="Reflected", '0'='Unreflected'))

fig_ref <- devs %>% filter(type=="Normative") %>% ggplot()+
  geom_point(aes(x=resp, y=absdev, color=issue, shape=reflect), 
             size=4, stroke=1)+
  geom_vline(xintercept = 0, linetype='dashed')+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_shape_manual(values=c(15,6))+
  theme_minimal() +
  xlab("Mean attitude shift")+
  ylab("Change in polarization")+
  ylim(-0.2,0.2)+xlim(-0.4, 0.4)

fig_rat <- devs %>% filter(type=="Factual") %>% ggplot()+
  geom_point(aes(x=resp, y=absdev, color=issue, shape=reflect), 
             size=4, stroke=1)+
  geom_vline(xintercept = 0, linetype='dashed')+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_shape_manual(values=c(15,6))+
  theme_minimal() +
  xlab("Mean attitude shift")+
  ylab("Change in polarization")+
  ylim(-0.2,0.2)+xlim(-0.4, 0.4)
  
  
  