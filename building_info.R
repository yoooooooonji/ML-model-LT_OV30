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

# 1. 건물 정보 데이터 load 
data <- read.csv("/Users/yj.noh/Desktop/address_info.csv", fileEncoding = "cp949")
#data <- read_excel("/Users/yj.noh/Desktop/final_df.xls")
head(data)
str(data)
colSums(is.na(data))

data <- data[c("법정동명", "지번", "세부용도명", "지상층수", "지하층수")]
data <- data %>% mutate(address = paste(data$법정동명, data$지번, sep = " "))
data <- subset(data, select = -c(법정동명, 지번))

# 지상층수 max, 지하층수 min
data <- data %>% group_by(address)  %>% mutate(지하층수_min = min(지하층수), 지상층수_max = max(지상층수))
data_filter  <- data[!duplicated(data[,c("세부용도명", "address", "지하층수_min" ,"지상층수_max")]), ]
dim(data_filter)

data_filter <- data_filter %>% group_by(address)  %>% mutate(n = n())
summary(data_filter)
table(data_filter$n)

p_vtls_pr2 <-p_vtls_pr2[!duplicated(p_vtls_pr2[,c("연구번호","임신추정일","임신종결일")]),]
# 중복제거
data <- 


shop <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/shop_result.csv", fileEncoding = "cp949")
head(shop)

dim(shop) #1534 픽업지 위경도 지번주소 
dim(data) #14982 건물 특성 데이터

# join
join_data <- shop %>%
  left_join(data[c("address", "세부용도명", "지상층수", "지하층수")], by = c("Address" = "address"))
dim(join_data) #1709



write.csv(join_data, file = "join_data.csv", fileEncoding= "cp949", row.names = FALSE)
write.csv(data, file = "address_info_filter.csv", fileEncoding= "cp949", row.names = FALSE)
