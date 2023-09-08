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
# 함수
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
data <- read_excel("prj-ML-model-LT_OV30/raw_data_time.xlsx")
dim(data) #115,823
colSums(is.na(data))

# 2. 데이터 정제
summary(data)

data <- data  %>% filter(추천미발생시간유 >= 0.0)
dim(data) #115,803

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
data_filter$reg_hour <- as.factor(data_filter$reg_hour)

group_data <- data_filter %>% group_by(reg_hour)

ggplot(group_data, aes(x = reg_hour, y = per_no_recomm)) +
  geom_boxplot() +
  labs(x = "reg_hour", y = "미노출시간비율") +
  ggtitle("reg_hour별 미노출시간비율 boxplot")


a_plot  <- boxplot(data_filter$per_display) #노출시간 
a_plot$stats

plot(data_filter$per_display) #노출
plot(data_filter$per_no_recomm) #미노출

summary(data_filter)
test<- data_filter %>% filter(per_no_recomm == 100.0)
dim(test) #913건 

data_filter$per_no_recomm <- as.numeric(data_filter$per_no_recomm)
data_2 <- data_filter %>% filter(data_filter$per_display > 0.0)
dim(data_2)
summary(data_2$per_no_recomm)

a_plot  <- boxplot(data_2$per_no_recomm) #미노출시간 
a_plot$stats

b <- boxplot(data_2$per_display) #노출시간 
b$stats

# 2-1 
data_filter <- data_filter  %>% mutate(output_10 = ifelse(notiOver_min_max >= 10,1,0),
                         output_20 = ifelse(notiOver_min_max >= 20, 1,0),
                         output_5 = ifelse(notiOver_min_max >=5,1,0))

table(data_filter$output_5) #5991
table(data_filter$output_10) # 2720
table(data_filter$output_20) #600

# 3. 전체배차시간 - 고안시 roc
png("boxplot_전처리전.png")
boxplot(전체배차시간 ~ notiOver, data = data_filter, col = "orange")
dev.off()

b_plot <- boxplot(전체배차시간 ~ notiOver, data = data_filter, col = "orange")
boxplot_stats <- data.frame(
  Min = b_plot$stats[1, ],
  Q1 = b_plot$stats[2, ],
  Median = b_plot$stats[3, ],
  Q3 = b_plot$stats[4, ],
  Max = b_plot$stats[5, ]
)
print(boxplot_stats)
max <- b_plot$stats[5,]
max_0 <- as.numeric(max[1])
max_1 <- as.numeric(max[2])
max_0 
max_1

# 3-1. ouput_5 
f_plot <- boxplot(전체배차시간 ~ output_5, data = data_filter, col = "orange")
max <- f_plot$stats[5,]
max_0 <- as.numeric(max[1])
max_1 <- as.numeric(max[2])
max_0 #7.03
max_1 # 46.98

data_rm <- data_filter %>% filter((output_5 == 0 & 전체배차시간 <= max_0) | (output_5 == 1 & 전체배차시간 <= max_1))
dim(data_rm) # 81350

roc_1<- ROC(form = output_5 ~ 전체배차시간, data = data_rm, plot = "ROC")
roc_2<- roc(data_rm$output_5, data_rm$전체배차시간)

tiff("prj-ML-model-LT_OV30/graph/ROC_curve_ov5.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  


optimal_lr.eta(roc_1)
optimal_cutpoint(roc_1) # 6.51

# output_10으로 테스트
t_plot <- boxplot(전체배차시간 ~ output_10, data = data_filter, col = "orange")
max <- t_plot$stats[5,]
max_0 <- as.numeric(max[1])
max_1 <- as.numeric(max[2])
max_0 #7.7
max_1 # 51.7

data_rm <- data_filter %>% filter((output_10 == 0 & 전체배차시간 <= max_0) | (output_10== 1 & 전체배차시간 <= max_1))
dim(data_rm) # 80779

roc_1<- ROC(form = output_10 ~ 전체배차시간, data = data_rm, plot = "ROC")
roc_2<- roc(data_rm$output_10, data_rm$전체배차시간)

tiff("prj-ML-model-LT_OV30/graph/ROC_curve_ov10.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  


optimal_lr.eta(roc_1)
optimal_cutpoint(roc_1) # 7.66

# output_20으로 테스트
t_plot <- boxplot(전체배차시간 ~ output_20, data = data_filter, col = "orange")
max <- t_plot$stats[5,]
max_0 <- as.numeric(max[1])
max_1 <- as.numeric(max[2])
max_0 # 8.28
max_1 # 61.53

data_rm <- data_filter %>% filter((output_20 == 0 & 전체배차시간 <= max_0) | (output_20== 1 & 전체배차시간 <= max_1))
dim(data_rm) # 80026

roc_1<- ROC(form = output_20 ~ 전체배차시간, data = data_rm, plot = "ROC")
roc_2<- roc(data_rm$output_20, data_rm$전체배차시간)

tiff("prj-ML-model-LT_OV30/graph/ROC_curve_ov20.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  

optimal_lr.eta(roc_1)
optimal_cutpoint(roc_1) # 8.21










