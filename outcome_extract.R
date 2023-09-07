# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "MatchIt", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS", "pROC") # nolint
ipak(pkg)
##########################################################################################################################################################

# 1. data load
setwd("/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/")
#data <- read.csv("raw_data_time.csv", fileEncoding = "cp949")
data <- read_excel("raw_data_time.xlsx")
head(data)
colSums(is.na(data))

# 2. 데이터 정제
data <- data %>% mutate(chk = ifelse((추천노출시간_Ai + 추천노출시간_일반 + 추천미발생시간유 + 추천미발생시간무 + 배차후취소시간) < 전체배차시간, 1,0))
table(data$chk) # 90616, 25352 

data_filter <- data  %>% filter(chk == 0)
dim(data_filter) #90616 

# 3. 전체배차시간 - 고안시 초과 상관관계 
cor(data_filter$전체배차시간, data_filter$notiOver) #0.46

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

# 4. remove outlier
data_rm <- data_filter %>% filter((notiOver == 0 & 전체배차시간 <= max_0) | (notiOver == 1 & 전체배차시간 <= max_1))
dim(data_rm) # 81993 

test <- data_rm  %>% filter(notiOver == 0)
summary(test$전체배차시간)

test2 <- data_rm  %>% filter(notiOver == 1)
summary(test2$전체배차시간)

png("boxplot_전처리후.png")
boxplot(전체배차시간 ~ notiOver, data = data_rm, col = "orange")
dev.off()

boxplot_stats <- data.frame(
  Min = a_plot$stats[1, ],
  Q1 = a_plot$stats[2, ],
  Median = a_plot$stats[3, ],
  Q3 = a_plot$stats[4, ],
  Max = a_plot$stats[5, ]
)
print(boxplot_stats)


# 5. roc 
table(data_rm$notiOver) #0 : 67480, 1: 14513 

roc_1 <- ROC(form = notiOver ~ 전체배차시간, data = data_rm, plot = "ROC")
roc_2<- roc(data_rm$notiOver, data_rm$전체배차시간)

roc_1
roc_2 

tiff("graph/ROC_Curve1.png", width = 1200, height = 1200)   
plot.roc(roc_1,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  

tiff("graph/ROC_Curve2.png", width = 1200, height = 1200)   
plot.roc(roc_2,   
         col="red",   
         print.auc = TRUE,   
         max.auc.polygon = TRUE,   
         print.thres = TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon = TRUE, auc.polygon.col="#D1F2EB")   
dev.off()  

optimal_lr.eta=function(x){
  no=which.max(x$res$sens+x$res$spec)[1]
  result=x$res$lr.eta[no]
  result
}
optimal_lr.eta(roc_1)

optimal_cutpoint=function(x){
   y=optimal_lr.eta(x)
   b0=unname(x$lr$coeff[1])
   b1=unname(x$lr$coeff[2])
   result=(-log(1/y-1)-b0)/b1
   result
} 
optimal_cutpoint(roc_1) #4.316667


