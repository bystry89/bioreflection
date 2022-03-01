library(ggplot2)
library(tidyverse)

pretest <- read.csv('data/00_long_data.csv')

  selected_keys <- c('ab_f_', 'ab_n_', 'su_n_','su_f2','va','gm_f_','gm_n_',
                     'sa', 'br_f2', 'br_n2', 'pb')
  
  # ---- 00_table_1 -------
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
  

  # ---- 00_table_2 -------
  
tbl2 <- data.frame(
  Issue = c("Abortion",
            "Animals in Research",
            "GM Crops",
            "Sexual Assistance",
            "Surrogacy",
            "Transgender Teens",
            "Vaccines"),
  Normative =c("The human fetus has a right to life.",
               "Animal testing (e.g., with mice, rabits, guinea pigs) in biomedical research is morally acceptable.",
               "Governments should ban all genetically modified crops developed for human consumption.",
               "Governments shoud not subsidize sexual assistance to meet the needs of disabled individuals.",
               "Surrogate motherhood should be illegal.",
               "Parents should be allowed to veto transgender teens' choice to undergo certain medical procedures (e.g., to receive puberty blocking drugs",
               "If a parent is concerned about vaccinating their children, they should have the right to refuse to vaccinate them."),
  Factual = c("The human fetus is capable of experiencing pain [by the second trimester or earlier].",
              "Animal testing is necessary to guarantee the safety of biomedical developments.",
              "Genetically modified crops are unfit for human consumption.",
              "Paid sexual assistance would not help meet the needs of disabled individuals",
              "It is likely that surrogate mothers are being exploited.",
              "The average teenager is mature enough to decide whether to undergo irreversible surgeries [at 15 years of age or later].",
              "The side-effects caused by modern vaccines are frequent and severe.")
)

