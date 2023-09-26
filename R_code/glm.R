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

data <- read_excel('/Users/yj.noh/Documents/GitHub/prj-ML-model-LT_OV30/modeling_data_final.xlsx')
head(data) 
dim(data)
colSums(is.na(data))

data <- subset(data, select = -c(dlvry_id, is_holiday, 전체배차시간, dlvry_rgn2_nm))

#data <- data %>% mutate(is_holiday = ifelse(is_holiday == TRUE, 1,0))
cols <- c('reg_hour', 'pick_floor',  'pick_category', 'pick_건물용도', 'dlvry_지상층수', 'dlvry_건물용도', 'day_of_week',  'outcome')

for (col in cols) {
  data[[col]] <- as.factor(data[[col]])
}

str(data)

# # tbl_summary
#  data %>% 
# # tbl_strata(
# # #     strata = outcome,~.x  %>% 
# 
#        tbl_summary(
#          by = outcome,
#          type = all_continuous() ~ "continuous2",
#                statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
#                missing_text = "(Missing value)", 
#                digits = list(all_continuous() ~ c(0, 1), 
#                              all_categorical() ~ c(0, 1))) %>%
#    add_overall() %>%
#    add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
#    bold_labels()


# #scale
num_vars <- c('ord_price', 'actual_dlvry_distance', 'dlvry_지하층수', '기온')
data[, num_vars] <- scale(data[,num_vars])

head(data)

library(caret)
set.seed(123)  
train_ratio <- 0.75
splitIndex <- createDataPartition(data$outcome, p = train_ratio, list = FALSE, 
                                   times = 1)
train_set <- data[splitIndex, ]
test_set <- data[-splitIndex, ]

glm1 <- glm(outcome ~., data =  train_set, family = binomial)
summary(glm1)

library(car)
vif(glm1)

table(train_set$pick_floor)
table(test_set$pick_floor)

glm1_pred <- predict(glm1, newdata = test_set, type = "response")
glm1_pred_binary <- ifelse(glm1_pred > 0.5, 1, 0)
conf_matrix <- table(Actual = test_set$outcome, Predicted = glm1_pred_binary)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / (conf_matrix[2, 1] + conf_matrix[2, 2])
specificity <- conf_matrix[1, 1] / (conf_matrix[1, 1] + conf_matrix[1, 2])

conf_matrix
accuracy
sensitivity
specificity


result <- summary(glm1)
write.csv(result, "glm_result.csv", fileEncoding = "cp949", row.names =FALSE)
