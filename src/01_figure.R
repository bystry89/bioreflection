library(ggplot2)
library(tidyverse)


source("src/functions/polit_recode.R")
longer <- read.csv("data/01_long_data.csv") %>% 
  mutate(reflect = if_else(
    issue == reflection_topic, "Reflected", "Unreflected"
  )) %>% 
  polit_recode() %>% 
  gather('time', 'Resp', -c(1:3, 5:14, 16:21))

devs <- 
  longer %>% 
  filter(order=='First-order') %>% 
  group_by(type, issue) %>% 
  dplyr::summarize(median=median(Resp)) %>% 
  right_join(filter(longer, order=='First-order'), by=c('type', 'issue')) %>% 
  select(PROLIFIC_PID, order, type, issue, Resp, median, time, reflect, RL) %>% 
  spread(time, Resp) %>% 
  mutate(absdev1 = abs(resp-median),
         absdev2 = abs(resp2-median),
         diff_absdebv=absdev2-absdev1) %>% 
  group_by(reflect, issue, type) %>% 
  #summarize(resp=mean(resp), resp2=mean(resp2), absdev1=mean(absdev1)
  #          , absdev2=mean(absdev2)) %>% 
  summarize(resp=mean(resp2)-mean(resp), 
            absdev=mean(absdev2)-mean(absdev1)) %>% 
  mutate(reflect=recode(reflect, '1'="Reflected", '0'='Unreflected'))

fig_ref <- devs %>% filter(type=="Normative") %>% ggplot()+
  #geom_segment(aes(x=resp, xend=resp2, y=absdev1, yend=absdev2, color=issue),
  #              arrow=arrow(angle=15, length = unit(0.10, "inches")))+
  #facet_wrap(reflect~type)+
  geom_point(aes(x=resp, y=absdev, color=issue, shape=reflect), 
             size=4, stroke=1)+
  geom_vline(xintercept = 0, linetype='dashed')+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_shape_manual(values=c(15,6))+
  theme_minimal() +
  xlab("Mean attitude shift")+
  ylab("Change in polarization")+
  ylim(-5,5)+xlim(-10, 10)

fig_rat <- devs %>% filter(type=="Factual") %>% ggplot()+
  geom_point(aes(x=resp, y=absdev, color=issue, shape=reflect), 
             size=4, stroke=1)+
  geom_vline(xintercept = 0, linetype='dashed')+
  geom_hline(yintercept = 0, linetype='dashed')+
  scale_shape_manual(values=c(15,6))+
  theme_minimal() +
  xlab("Mean attitude shift")+
  ylab("Change in polarization")+
  ylim(-5,5)+xlim(-10, 10)
  
  
  