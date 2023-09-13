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
# 1. pick_floor data + 건물용도명 join
pick_floor <- read_excel("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/pick_floor_data.xls")
pick_info <- read_excel("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/shop_final.xls")

# data_join 
pick_floor <- pick_floor %>% left_join(pick_info[c("Latitude", "Longitude", "Address" ,"건물용도명_2")],by = c("latitude" = "Latitude", "longitude" = "Longitude"))
colSums(is.na(pick_floor)) #784개 

#write.csv(pick_floor, "prj-ML-model-LT_OV30/pick_floor_data_2.csv", row.names = FALSE, fileEncoding = "cp949")

# 2. raw_data + dlvry / pick 건물용도명 join
data <- read_excel("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/raw_data_time.xlsx")
dim(data) #115823
colSums(is.na(data))

# 2.1 픽업지 join
data <- data  %>% left_join(pick_floor[c("shop_no", "floor", "rgn2_nm", "rgn3_nm", "shop_category","건물용도명_2", "Address")], by  = "shop_no")
data <- data  %>% rename("pick_floor" = "floor",
                         "pick_rgn2_nm" = "rgn2_nm", 
                         "pick_rgn3_nm" = "rgn3_nm",
                         "pick_건물용도" = "건물용도명_2",
                         "pick_category" = "shop_category",
                         "pick_address" = "Address")
str(data)

# 2.2 전달지 join
dlvry <- read_excel("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/address/dlvry_final.xls")
#dlvry <- dlvry[!duplicated(dlvry[,c("Latitude", "Longitude","Address","건물용도명", "지상층수_max","지하층수_min","주용도코드명","건물용도명_2")]), ]

str(dlvry)
str(data)

dlvry$Latitude <- sprintf("%.7f", dlvry$Latitude)
dlvry$Longitude <- sprintf("%.7f", dlvry$Longitude)
data$dlvry_loc_pnt_lat <- sprintf("%.7f", data$dlvry_loc_pnt_lat)
data$dlvry_loc_pnt_lon <- sprintf("%.7f", data$dlvry_loc_pnt_lon)


data <- data  %>% left_join(dlvry[c("Latitude", "Longitude","Address","지상층수_max", "지하층수_min", "건물용도명_2")], by = c("dlvry_loc_pnt_lat" = "Latitude" , "dlvry_loc_pnt_lon" = "Longitude"))
data <- data  %>% rename("dlvry_address" = "Address",
                          "dlvry_지상층수" = "지상층수_max",
                          "dlvry_지하층수" = "지하층수_min",
                          "dlvry_건물용도" = "건물용도명_2")



cols_to_replace_na <- c("pick_floor", "pick_rgn2_nm", "pick_rgn3_nm", "pick_category", "pick_건물용도", "pick_address", "dlvry_address", "dlvry_지상층수", "dlvry_지하층수", "dlvry_건물용도")
data[cols_to_replace_na][data[cols_to_replace_na] == "NA"] <- NA

colSums(is.na(data)) # 13010, 30375
dim(data)

write.csv(data, "prj-ML-model-LT_OV30/raw_data_final.csv", fileEncoding = "utf-8", row.names = FALSE, na= "")





