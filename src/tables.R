
table1 <- function(pretest) {
  selected_keys <- c('ab_f_', 'ab_n_', 'su_n_','su_f2','va','gm_f_','gm_n_',
                     'sa', 'br_f2', 'br_n2', 'pb')
  selected <- pretest %>% 
    filter(grepl(paste(selected_keys,collapse="|"), .$key)) %>%
    mutate(type=recode(type,Normative2='Normative',
                       Factual2='Factual'))
  
  tbl <- selected %>% rename(Issue=issue, Type=type) %>% 
    group_by(Issue, Type) %>% 
    summarize(Mean=round(mean(resp, na.rm=T), 2), 
              SD=round(sd(resp, na.rm=T), 2))
  
  tbl$`Fact-Norm correlation` <- sapply(tbl$Issue, function(x) {
    cols <- selected %>% filter(issue==x) %>% select(PROLIFIC_PID,
                                                     type,resp) %>% 
      spread(type, resp) 
    round(cor.test(cols$Factual, cols$Normative)$estimate, 2)
  })
  
  tbl$`Shapiro-Wilk test` <- apply(tbl,1, function(x) {
    selected %>% filter(issue==x[1], type==x[2]) %>% .$resp %>% 
      shapiro.test() %>% .$statistic %>% round(2)
  })
  
  tbl$`Correlation with politics` <- apply(tbl,1, function(x) {
    cols <- selected %>% filter(issue==x[1], type==x[2]) 
    round(cor.test(cols$resp, cols$politics_1)$estimate,2)
  })
  
  return(tbl)
}
