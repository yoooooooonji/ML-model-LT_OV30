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
# 서초, 강남, 동작, 관악, 과천 
sc <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_서초.csv", fileEncoding = "cp949")
gn <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_강남.csv", fileEncoding = "cp949")
dj <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_동작.csv", fileEncoding = "cp949")
gw <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_관악.csv", fileEncoding = "cp949")
gc <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_과천.csv", fileEncoding = "cp949")

build_df <- rbind(sc,gn,dj,gw,gc)
build_df <- subset(build_df, select = c(법정동명, 지번, 주요용도명, 세부용도명, 지상층수, 지하층수))
build_df <- build_df %>% mutate(address = paste(build_df$법정동명, build_df$지번, sep = " "))
build_df <- subset(build_df, select = -c(법정동명, 지번))

#write.csv(build_df, "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_전체.csv", fileEncoding = "cp949", row.names = FALSE)

# 건물 세부용도명 정리
build_name <- read_excel("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건물_용도명.xlsx")
head(build_name)
head(build_df)

# join 
build_df <- build_df %>% left_join(build_name, by = c("주요용도명", "세부용도명"))
colSums(is.na(build_df)) #na 160

# NA값 처리 
# test <- build_df  %>% filter(is.na(건물용도명))
# table(test$주요용도명)

build_df <- build_df  %>% mutate(건물용도명 = case_when (!is.na(건물용도명)~ 건물용도명, 
                                                is.na(건물용도명) & 주요용도명 %in% c("공동주택", "숙박시설", "업무시설") ~ 주요용도명,     
                                                is.na(건물용도명) & 주요용도명 %in% c("제1종근린생활시설", "제2종근린생활시설") ~ "근린생활시설",
                                                TRUE ~ "공공시설"))
colSums(is.na(build_df))

# 지상층수 max, 지하층수 min
head(build_df)
build_df <- build_df  %>% group_by(address, 건물용도명)  %>% mutate(지상층수_max = max(지상층수), 지하층수_min = min(지하층수))
build_df$지하층수_min[is.na(build_df$지하층수_min)] <- 0 
build_df <- subset(build_df, select = -c(주요용도명, 세부용도명, 지상층수, 지하층수))

#중복제거
build_fin <- build_df[!duplicated(build_df[,c("건물용도명", "address", "지하층수_min" ,"지상층수_max")]), ]

dim(build_df) # 88542
dim(build_fin) # 84293

build_fin <- build_fin  %>%  group_by(address)  %>% mutate(n = n())
summary(build_fin)
table(build_fin$n)

write.csv(build_fin, file = "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_final.csv", fileEncoding = "cp949", row.names = FALSE)


# 정리된 건물 정보 데이터 reload
build_data<- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/building_final.csv", fileEncoding = "cp949")
head(build_data)
colSums(is.na(build_data))

build_pick <- build_data  %>% filter(pick == 1)
build_dlvry <- build_data  %>% filter(dlvry==1)

# 픽업지 위경도/지번주소 -> 건물타입 join 
shop <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/shop_result.csv", fileEncoding = "cp949")
head(shop)
dim(shop) #1534 

# join
shop_df <- shop  %>% left_join(build_pick[c("address", "건물용도명", "지상층수_max", "지하층수_min")], by = c("Address" = "address"))
dim(shop_df) #1534
colSums(is.na(shop_df)) #212

# 전달지 위경도/지번주소->건물타입 join
dlvry <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/dlvry_result.csv", fileEncoding = "cp949")
dim(dlvry) #47012

#join 
dlvry_df <- dlvry  %>% left_join(build_dlvry[c("address", "건물용도명", "지상층수_max", "지하층수_min")], by = c("Address" = "address"))
dim(dlvry_df) #47012
colSums(is.na(dlvry_df)) #13673 



write.csv(shop_df, file = "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/shop_final.csv", fileEncoding = "cp949", row.names = FALSE)
write.csv(dlvry_df, file = "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/dlvry_final.csv", fileEncoding = "cp949", row.names = FALSE)


