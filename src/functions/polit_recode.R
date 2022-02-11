

polit_recode <- function(x) {
  x %>% 
    mutate(
      resp = if_else(str_detect(key, "ab_f|gm_f|sa_f|sa_n|su_n"), 
                     abs(100-as.numeric(resp)), as.numeric(resp)),
      resp2 = if_else(str_detect(key, "ab_f|gm_f|sa_f|sa_n|su_n"), 
                      abs(100-as.numeric(resp2)), as.numeric(resp2)),
      reflection_resp_O1 = if_else(type=='Factual'&reflection_topic%in%
                                     c('Abortion', 'GM Crops', 'Sexual Assistance') |
                                     type=='Normative'&reflection_topic%in%
                                     c('Sexual Assistance', "Surrogacy"),
                                   abs(100-as.numeric(reflection_resp_O1)), as.numeric(reflection_resp_O1))
    )
}


