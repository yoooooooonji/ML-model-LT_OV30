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
# function
optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}

optimal_cutpoint=function(x){
   y=optimal_lr.eta(x)
   b0=unname(x$lr$coeff[1])
   b1=unname(x$lr$coeff[2])
   result=(-log(1/y-1)-b0)/b1
   result
} 

holiday_list = ymd(c("2022-01-01", "2022-01-31", "2022-02-01", "2022-03-01", "2022-03-09",  "2022-05-05", "2022-05-08", "2022-06-01", "2022-06-06", "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11", "2022-09-12", 
"2022-10-03",  "2022-10-09", "2022-10-10", "2022-12-25", "2023-01-01", "2023-01-21","2023-01-22", "2023-01-23", "2023-01-24", "2023-03-01", "2023-05-01", "2023-05-05","2023-05-27", "2023-05-29", "2023-06-06", "2023-08-15", "2023-09-28", "2023-09-29",
"2023-09-30", "2023-10-03", "2023-10-09", "2023-12-25"))

par(family ="AppleGothic")

##########################################################################################################################################################
# 1. data load
data <- read_excel("prj-ML-model-LT_OV30/raw_data_final.xlsx")
#data <- read_csv("prj-ML-model-LT_OV30/raw_data_final.csv", fileEncoding = "utf-8")

dim(data) #115,823
colSums(is.na(data)) # 13010, 30375

# cols_to_replace_na <- c("pick_floor", "pick_rgn2_nm", "pick_rgn3_nm", "pick_category", "pick_건물용도", "pick_address", "dlvry_address", "dlvry_지상층수", "dlvry_지하층수", "dlvry_건물용도")
# data[cols_to_replace_na][data[cols_to_replace_na] == "NA"] <- NA

#write.csv(data,"prj-ML-model-LT_OV30/raw_data_final2.csv", fileEncoding = "utf-8", row.names = FALSE, na="")

##########################################################################################################################################################
# 2. 데이터 정제
# 추천미발생시간유 > 0 
data <- data  %>% filter(추천미발생시간유 >= 0.0)
dim(data) #115,803

# sum < 전체배차시간
data <- data %>% mutate(chk = ifelse((추천노출시간_AI + 추천노출시간_일반 + 추천미발생시간유 + 추천미발생시간무 + 배차후취소시간) < 전체배차시간, 1,0))   

table(data$chk) # 90,620, 25,183

data_filter <- data  %>% filter(chk == 0)
dim(data_filter) #90,620

##########################################################################################################################################################
# 3. 요일, 공휴일 여부 추가 
data_filter <- data_filter  %>% mutate(day_of_week = substr(weekdays(as.Date(business_day)),1,3)) 
data_filter <- data_filter  %>% mutate(is_holiday = ifelse(business_day %in% holiday_list | day_of_week %in% c("금요일", "토요일", "일요일"),1,0))

table(data_filter$is_holiday) #50592, 40028

##########################################################################################################################################################
# 4. 라이더 노출 시간 boxplot
# 알뜰 : 묶음 대기시간 존재 
data_filter$reg_hour <- as.factor(data_filter$reg_hour)

group_data <- data_filter %>% group_by(reg_hour)

group_data_summary <- group_data %>%
  summarize(q1 = quantile(per_no_recomm, 0.25),
            q3 = quantile(per_no_recomm, 0.75))

b_plot <- ggplot(group_data, aes(x = reg_hour, y = per_no_recomm)) +
  geom_boxplot() +
  labs(x = "reg_hour", y = "미노출시간비율") +
  ggtitle("reg_hour별 미노출시간비율 boxplot") +
 geom_text(data = group_data_summary, aes(label = paste("q1:", q1, "\nq3:", q3), x = reg_hour, y = q3), vjust = -1, hjust = -1)

#ggsave("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/graph/boxplot_시간대별_미노출비율.png")

data_rm <- data_filter  %>% left_join(group_data_summary, by = "reg_hour")
head(data_rm)

data_rm <- data_rm %>% filter(per_no_recomm <= q3)
dim(data_rm) # 68,027
colSums(is.na(data_rm))  # 7634, 17651 

##########################################################################################################################################################
#data_final <- data_rm %>% filter(!is.na(dlvry_건물용도) & !is.na(pick_건물용도))
#dim(data_final) # 44776

#write.csv(data_final, "prj-ML-model-LT_OV30/modeling_data.csv", fileEncoding = "utf-8", na = "")

##########################################################################################################################################################
# 5. 전체배차시간 - 고안시 roc 

data_rm <- data_rm %>% mutate(output_10 = ifelse(notiOver_min_max >= 10,1,0),
                         output_20 = ifelse(notiOver_min_max >= 20, 1,0),
                         output_5 = ifelse(notiOver_min_max >=5,1,0))

table(data_rm$output_5) # 4226
table(data_rm$output_10) # 1932
table(data_rm$output_20) # 453

data_rm$notiOver <- as.factor(data_rm$notiOver)
data_rm$output_5 <- as.factor(data_rm$output_5)
data_rm$output_10 <- as.factor(data_rm$output_10)
data_rm$output_20 <- as.factor(data_rm$output_20)

##########################################################################################################################################################
# 5-1. 고안시 < 배달시간 
b_plot <- boxplot(전체배차시간 ~ notiOver, data = data_rm, col = "orange")
boxplot_stats <- data.frame(
  Min = b_plot$stats[1, ],
  Q1 = b_plot$stats[2, ],
  Median = b_plot$stats[3, ],
  Q3 = b_plot$stats[4, ],
  Max = b_plot$stats[5, ]
)
print(boxplot_stats)
max_0 <- as.numeric(b_plot$stats[5,][1])
max_1 <- as.numeric(b_plot$stats[5,][2])

roc_df <- data_rm  %>% filter((notiOver == 0 & 전체배차시간 <= max_0) | (notiOver == 1 & 전체배차시간 <= max_1))
roc_1 <- ROC(form = notiOver ~ 전체배차시간, data= roc_df, plot= "ROC")
roc_2 <- roc(roc_df$notiOver, roc_df$전체배차시간)

tiff("prj-ML-model-LT_OV30/graph/ROC_over.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  
optimal_lr.eta(roc_1)
optimal_cutpoint(roc_1) # 2.58

##########################################################################################################################################################
# 5.2. 고안시 초과 5분 
b_plot <- boxplot(전체배차시간 ~ output_5, data = data_rm, col = "orange")
boxplot_stats <- data.frame(
  Min = b_plot$stats[1, ],
  Q1 = b_plot$stats[2, ],
  Median = b_plot$stats[3, ],
  Q3 = b_plot$stats[4, ],
  Max = b_plot$stats[5, ]
)
print(boxplot_stats)
max_0 <- as.numeric(b_plot$stats[5,][1])
max_1 <- as.numeric(b_plot$stats[5,][2])

roc_df<- data_rm  %>% filter((output_5 == 0 & 전체배차시간 <= max_0) | (output_5 == 1 & 전체배차시간 <= max_1))
roc_1 <- ROC(form = output_5 ~ 전체배차시간, data= roc_df, plot= "ROC")
roc_2 <- roc(roc_df$output_5, roc_df$전체배차시간)

tiff("prj-ML-model-LT_OV30/graph/ROC_over5.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  
optimal_lr.eta(roc_1)
optimal_cutpoint(roc_1) # 4.4

##########################################################################################################################################################
# 5.3. 고안시 초과 10분 
b_plot <- boxplot(전체배차시간 ~ output_10, data = data_rm, col = "orange")
boxplot_stats <- data.frame(
  Min = b_plot$stats[1, ],
  Q1 = b_plot$stats[2, ],
  Median = b_plot$stats[3, ],
  Q3 = b_plot$stats[4, ],
  Max = b_plot$stats[5, ]
)
print(boxplot_stats)
max_0 <- as.numeric(b_plot$stats[5,][1])
max_1 <- as.numeric(b_plot$stats[5,][2])

roc_df<- data_rm  %>% filter((output_10 == 0 & 전체배차시간 <= max_0) | (output_10 == 1 & 전체배차시간 <= max_1))
roc_1 <- ROC(form = output_10 ~ 전체배차시간, data= roc_df, plot= "ROC")
roc_2 <- roc(roc_df$output_10, roc_df$전체배차시간)

tiff("prj-ML-model-LT_OV30/graph/ROC_over10.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  
optimal_lr.eta(roc_1)
optimal_cutpoint(roc_1) # 4.95

##########################################################################################################################################################
# 5.4. 고안시 초과 20분 
b_plot <- boxplot(전체배차시간 ~ output_20, data = data_rm, col = "orange")
boxplot_stats <- data.frame(
  Min = b_plot$stats[1, ],
  Q1 = b_plot$stats[2, ],
  Median = b_plot$stats[3, ],
  Q3 = b_plot$stats[4, ],
  Max = b_plot$stats[5, ]
)
print(boxplot_stats)
max_0 <- as.numeric(b_plot$stats[5,][1])
max_1 <- as.numeric(b_plot$stats[5,][2])

roc_df<- data_rm  %>% filter((output_20 == 0 & 전체배차시간 <= max_0) | (output_20 == 1 & 전체배차시간 <= max_1))
roc_1 <- ROC(form = output_20 ~ 전체배차시간, data= roc_df_1, plot= "ROC")
roc_2 <- roc(roc_df$output_20, roc_df$전체배차시간)

tiff("prj-ML-model-LT_OV30/graph/ROC_over20.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  
optimal_lr.eta(roc_1)
optimal_cutpoint(roc_1) # 5.25

##########################################################################################################################################################
# 6. output 결정짓기 -> 고안시 초과 10분 결정짓는 값보다 크면 장미배
value <- optimal_cutpoint(roc_1) # 4.95
value
dim(data_rm)
data_rm <- data_rm  %>% mutate(outcome = ifelse(전체배차시간 >= value ,1, 0))
table(data_rm$outcome) # 58981, 9046

input_val <- c("dlvry_id","reg_hour", "ord_price", "actual_dlvry_distance", "pick_floor", "pick_rgn3_nm","pick_category","pick_건물용도","dlvry_address",
"dlvry_지상층수", "dlvry_지하층수", "dlvry_건물용도", "day_of_week", "is_holiday", "outcome")
model_df <- data_rm[input_val] 

name_list <- c("pick_건물용도", "dlvry_건물용도")
model_df[name_list][model_df[name_list]=="NA"] <- NA
colSums(is.na(model_df))

#write.csv(model_df, "modeling_data.csv", fileEncoding = "utf-8", row.names = FALSE ,na = "")

##########################################################################################################################################################
# 7. modeling 

model_df <- read_excel("prj-ML-model-LT_OV30/modeling_data.xlsx")

dim(model_df) # 44776
colSums(is.na(model_df))
str(model_df)
table(model_df$outcome) #38843, 5933

val_name = c("pick_floor", "pick_rgn3_nm", "pick_category", "pick_건물용도", "dlvry_지상층수", "dlvry_지하층수", "dlvry_건물용도", "day_of_week", "is_holiday", "outcome")
model_df[val_name] <- lapply(model_df[val_name], as.factor)
str(model_df)

model_df <- subset(model_df, select = -c(dlvry_id, dlvry_address))

# 변수 전처리 
model1 <- glm(outcome ~., data = model_df, family = binomial)

# Predicting the Test set results
prob_pred = predict(, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)

