setwd("D:/1091/行銷數據分析")
library(data.table)
library(vioplot)
library(ggplot2)
library(dplyr)

df = fread("oneAd_data.csv", encoding = "UTF-8") %>% as.data.frame()
df = df[,c(1:4,21)] # 留下需要欄位

df$spp[is.na(df$spp)] <- 0

percentage=c(0.3116996,0.1654135,0.2571179,0.2687747,0.127451)

maxmin <- function(x) (x - 0.1)/(max(x)-0.1)

new_perc = maxmin(percentage)


for(i in 1:nrow(df)){
  if(df$spp[i] > 0 & df$spp[i]<=17){
    df$new[i] = new_perc[1]
  }else if(df$spp[i]>=18 & df$spp[i]<=31){
    df$new[i] = new_perc[2]
  }else if(df$spp[i]>=32 & df$spp[i]<=44){
    df$new[i] = new_perc[3]
  }else if(df$spp[i]>=45 & df$spp[i]<=85){
    df$new[i] = new_perc[4]
  }else if(df$spp[i]>=86 & df$spp[i]<=100){
    df$new[i] = new_perc[5]
  } else{
    df$new[i] = 0
    print(round(i/nrow(df),3))
  }
}


# 77553 not 0
write.csv(df, "oneAd_data(SPP).csv")

# ===== above no dot read again ====== #
setwd("D:/1091/行銷數據分析")
library(data.table)
library(ggplot2)
library(dplyr)
library(e1071)

df = fread("combine_data.csv") %>% as.data.frame()
colnames(df)
attach(df)
x = subset(df, select = -view_50or_click) %>% model.matrix()
y = view_50or_click
model = svm(x, y)


# model <- svm(view_50or_click ~ ., data = df)

# ======= change data set  1228 ========= #

setwd("D:/1091/行銷數據分析/final data")
library(data.table)
library(dplyr)
test = fread("oneAd_data_test.csv", encoding = "UTF-8") %>% as.data.frame()

df = test[,c(1:4,21)]  # 留下需要欄位

df$spp[is.na(df$spp)] <- 0

percentage=c(0.3116996,0.1654135,0.2571179,0.2687747,0.127451)

maxmin <- function(x) (x - 0.1)/(max(x)-0.1)

new_perc = maxmin(percentage)


for(i in 1:nrow(df)){
  if(df$spp[i] > 0 & df$spp[i]<=17){
    df$new[i] = new_perc[1]
  }else if(df$spp[i]>=18 & df$spp[i]<=31){
    df$new[i] = new_perc[2]
  }else if(df$spp[i]>=32 & df$spp[i]<=44){
    df$new[i] = new_perc[3]
  }else if(df$spp[i]>=45 & df$spp[i]<=85){
    df$new[i] = new_perc[4]
  }else if(df$spp[i]>=86 & df$spp[i]<=100){
    df$new[i] = new_perc[5]
  } else{
    df$new[i] = 0
    print(round(i/nrow(df),3))
  }
}


# 77553 not 0
write.csv(df, "oneAd_data_test(SPP).csv")

# ==========  add hour column(1228) ======= #
test = fread("A_Pre-roll_train.csv", encoding = "UTF-8") %>% as.data.frame()

df = test[,c(1:4,25)]

df$hour = ymd_hms(test$dt_local) %>% hour()

write.csv(df, "verify(hour).csv")

# A
test = fread("A_Pre-roll_train.csv", encoding = "UTF-8") %>% as.data.frame()
table(test$hour, test$view_50or_click) %>% plot(main = "A_Pre-roll_train")
table(test$hour, test$view_50or_click) %>% chisq.test()


# B
test = fread("B_Mobile-Pre-roll_train.csv", encoding = "UTF-8") %>% as.data.frame()
table(test$hour, test$view_50or_click) %>% plot(main = "B_Mobile-Pre-roll_train")
table(test$hour, test$view_50or_click) %>% chisq.test()

# C
test = fread("C_Other_train.csv", encoding = "UTF-8") %>% as.data.frame()
table(test$hour, test$view_50or_click) %>% plot(main = "C_Other_train")
table(test$hour, test$view_50or_click) %>% chisq.test()

