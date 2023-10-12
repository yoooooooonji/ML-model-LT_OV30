# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "MatchIt", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS", "pROC", "Epi") # nolint
ipak(pkg)
##########################################################################################################################################################

data <- read.csv('prj-ML-model-LT_OV30/on_boarding_data.csv', encoding = 'utf-8', stringsAsFactors = FALSE)
data <- data  %>% filter(!is.na(실제수행기간))

data$실제수행기간 <- as.integer(as.character(data$실제수행기간))

df <- subset(data, select = -c(brms_rider_id, first_available_date, business_start_day, recommender_rider_account_id, rider_no, last_working_day))

#df <- df  %>% filter(주요수행method == 'BIKE' & is_recom == 0)
#df <- df  %>% filter(주요수행method == 'BIKE')

vars_to_convert <- setdiff(names(df), c('수행건수', '실제수행기간', '기본배달비_중간값', '총배달비_중간값', 'not_working', '첫운행이후'))
df[vars_to_convert] <- lapply(df[vars_to_convert], as.factor)
str(df)

df %>% 
  tbl_summary(
    by = outcome,
   type = list(
    실제수행기간 ~ "continuous2",
    수행건수 ~ "continuous2",
    기본배달비_중간값 ~ "continuous2",
    총배달비_중간값 ~ "continuous2",
    not_working ~ "continuous2", 
    첫운행이후 ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()



test <- data  %>% filter(주요수행method=='BIKE' & is_recom == 1)



