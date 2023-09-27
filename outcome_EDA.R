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

data <- read_excel('prj-ML-model-LT_OV30/cluster_add_new_data.xlsx')
head(data)
colSums(is.na(data))
str(data)
dim(data)

data <- data %>% mutate(is_holiday = ifelse(is_holiday == TRUE, 1,0))


df <- subset(data, select = c(ord_price, 전체배차시간, actual_dlvry_distance,기온, is_holiday,
pick_floor, reg_hour, pick_category, pick_건물용도, dlvry_지상층수,  dlvry_건물용도,y_pred_new, KMeansCluster ))

df$KMeansCluster <- factor(df$KMeansCluster, level = c(0,2,1,3))
df$reg_hour <- factor(df$reg_hour)
df %>%
  tbl_summary(
    by = KMeansCluster,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})", "{min}, {max}")
    ),
    missing_text = "(Missing value)",
    digits = list(
      all_continuous() ~ c(2, 1),  # mean 값을 2자리까지 표시
      all_categorical() ~ c(0, 1)
    )
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~ style_pvalue(., digits = 3)) %>%
  bold_labels()



