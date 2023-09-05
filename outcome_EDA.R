# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "MatchIt", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS") # nolint
ipak(pkg)
##########################################################################################################################################################

#data <- read.csv("/Users/yj.noh/Desktop/psm_data.csv", fileEncoding = "cp949")
data <- read_excel("/Users/yj.noh/Desktop/final_df.xls")
head(data)
str(data)
colSums(is.na(data))

summary(data)

# 미발생비율 >0 
data <- data  %>% filter(data$미발생비율 > 0)

# boxplot 
# 장미배 기준 : 총배차소요시간 / 고안시 20분 초과 간의 상관관계? 
# 라이더 기피 기준 : 전체 노출 시간 
