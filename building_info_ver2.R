# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "MatchIt", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS", "data.table")
ipak(pkg)
##########################################################################################################################################################

# 1. data load -> 구별로 묶기
# 서초구, 강남구, 동작구, 관악구, 과천시 
directory <- "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장/과천시"
file_names <- list.files(directory, pattern = ".csv", full.names = TRUE)
data_list <- lapply(file_names, function(file) {
  raw_data <- readLines(file, encoding = "cp949")
  utf8_data <- iconv(raw_data, from = "cp949", to = "UTF-8", sub = "byte")
  data <- read.csv(text = utf8_data, header = TRUE)
  return(data)
})

combined_data <- do.call(rbind, data_list)
output_filename <- "과천시_건축물대장.csv"
write.csv(combined_data, file.path("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장", output_filename), row.names = FALSE, fileEncoding = "cp949")

# 2. 구별 -> 전체로 묶기 
data1 <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장/서초구_건축물대장.csv", fileEncoding = "cp949")
data2 <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장/강남구_건축물대장.csv", fileEncoding = "cp949")
data3 <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장/관악구_건축물대장.csv", fileEncoding = "cp949")
data4 <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장/동작구_건축물대장.csv", fileEncoding = "cp949")
data5 <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장/과천시_건축물대장.csv", fileEncoding = "cp949")

total_data <- rbind(data1, data2, data3, data4, data5)
write.csv(total_data, "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장_all.csv", row.names = FALSE, fileEncoding = "cp949")


# 3. 픽업지/전달지 na 값 -> 채워넣기
total_data <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/건축물대장_all.csv", fileEncoding= "cp949")
str(total_data)
total_data <- total_data[(c("주용도코드명", "대지위치"))]
head(total_data)

shop_df <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/shop_final.csv", fileEncoding= "cp949")
dlvry_df <- read.csv("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/dlvry_final.csv", fileEncoding= "cp949")

colSums(is.na(shop_df)) #212 / 1482
colSums(is.na(dlvry_df)) #13668

shop_df <- shop_df  %>% left_join(total_data, by = c("Address" = "대지위치"))
dlvry_df <- dlvry_df  %>% left_join(total_data, by = c("Address" = "대지위치"))


shop_df <- shop_df  %>% mutate(건물용도명_2 = ifelse(is.na(건물용도명), 주용도코드명, 건물용도명))
dlvry_df <- dlvry_df  %>% mutate(건물용도명_2 = ifelse(is.na(건물용도명), 주용도코드명, 건물용도명))

colSums(is.na(shop_df))
colSums(is.na(dlvry_df))

write.csv(shop_df, file = "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/shop_final.csv", fileEncoding = "cp949", row.names = FALSE)
write.csv(dlvry_df, file = "/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/dlvry_final.csv", fileEncoding = "cp949", row.names = FALSE)
