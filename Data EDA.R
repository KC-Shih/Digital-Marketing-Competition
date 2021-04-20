setwd("D:/1091/行銷數據分析/期中報告")
library(data.table)
library(vioplot)
library(ggplot2)
library(dplyr)
#dat_small = fread("oneAd_data_small.csv", encoding = "UTF-8") %>% as.data.frame()
#colnames(dat_small)

dat = fread("oneAd_data1113.csv", encoding = "UTF-8") %>% as.data.frame()
dat = dat[,1:27]
dim(dat)

distinct_counts<- function(x){ 
  unique_values<- unique(x) 
  return(length(unique_values)) 
} 
var_number = apply(dat, 2, distinct_counts)
var_number

# spp 
# 時間區段的廣告分布  (先不做)

## ============================================ ##
## =============== spp 分群 =================== ##
## ============================================ ##


SPP.NAomit = which(dat$spp!="NA")
datSPP = dat[SPP.NAomit,]
plot(datSPP$spp)
boxplot(datSPP$spp)
boxplot(datSPP$spp[datSPP$spp<100])

fff = c()
for(i in 1:22639){
  if(datSPP$spp[i] <17){
    fff[i] = "G1"
  }else if(datSPP$spp[i]>=18 & datSPP$spp[i]<=31){
    fff[i] = "G2"
  }else if(datSPP$spp[i]>=32 & datSPP$spp[i]<=44){
    fff[i] = "G3"
  }else if(datSPP$spp[i]>=45 & datSPP$spp[i]<=85){
    fff[i] = "G4"
  }else if(datSPP$spp[i]>=85 & datSPP$spp[i]<=100){
    fff[i] = "G5"
  }else{
    fff[i] = "G6"
  }
}

datSPPm = datSPP %>% 
  mutate(spp_group = as.factor(fff))

ggplot(datSPPm, aes(x=fff)) +
  geom_bar()+facet_grid(~as.factor(play_mode)) +
  labs(title = "廣告格式",x = "廣告露出位置位於畫面高度" , y = "累積次數")+
  scale_x_discrete(labels = c("2" = "G2","4"="G4","6"="6"))
#廣告格式vs點擊+q50 可再新增比例欄位

ggplot(datSPPm, aes(x=fff)) +
  geom_bar()+facet_grid(~as.factor(userde)) +
  labs(title = "年齡性別",x = "廣告露出位置位於畫面高度" , y = "累積次數")
#年齡性別vs點擊+q50 可再新增比例欄位

ggplot(datSPPm, aes(x=fff)) +
  geom_bar()+facet_grid(~as.factor(view_50or_click)) +
  labs(title = "點擊或看超過一半的影片",x = "廣告露出位置位於畫面高度" , y = "累積次數")

summary(as.factor(datSPPm$view_50or_click)) # 點擊或看超過一半的影片占25%
# 看各自的比例 



############################
G1 = which(datSPP$spp<=17)                   # 5778
datSPP_G1 = datSPP[G1,]

G2 = which(datSPP$spp>=18 & datSPP$spp<=31)  # 5586
datSPP_G2 = datSPP[G2,]

G3 = which(datSPP$spp>=32 & datSPP$spp<=44)  # 5725
datSPP_G3 = datSPP[G3,]

G4 = which(datSPP$spp>=45 & datSPP$spp<=85)  # 4807
datSPP_G4 = datSPP[G4,]

G5 = which(datSPP$spp>=86 & datSPP$spp<=100) # 102
datSPP_G5 = datSPP[G5,]

G6 = which(datSPP$spp>100)                   # 641
datSPP_G6 = datSPP[G6,]


summary(as.factor(datSPP_G1$view_50or_click))
summary(as.factor(datSPP_G2$view_50or_click))
summary(as.factor(datSPP_G3$view_50or_click))
summary(as.factor(datSPP_G4$view_50or_click))
summary(as.factor(datSPP_G5$view_50or_click))
summary(as.factor(datSPP_G6$view_50or_click))
#=============
cq50 = cbind(dat$userde, dat$view_50or_click
             , dat$conntype, dat$play_mode) %>% as.data.frame()
colnames(cq50) = c("userde", "view_50or_click", 
                   "conntype", "play_mode")

w = which(cq50$userde==0)
cq50 = cq50[-w,]

ggplot(cq50, aes(x=as.factor(view_50or_click))) +
  geom_bar()+facet_grid(~as.factor(userde)) +
  labs(title = "年齡性別",x = "點擊或看超過一半的影片" , y = "累積次數")
#年齡性別vs點擊+q50 可再新增比例欄位
#=============
cq50 = cbind(dat$userde, dat$view_50or_click
             , dat$conntype, dat$play_mode) %>% as.data.frame()
colnames(cq50) = c("userde", "view_50or_click", 
                   "conntype", "play_mode")


ggplot(cq50, aes(x=as.factor(view_50or_click))) +
  geom_bar()+facet_grid(~as.factor(conntype)) +
  labs(title = "裝置連線方式",x = "點擊或看超過一半的影片" , y = "累積次數")
#裝置連線方式 可再新增比例欄位

#==============

cq50 = cbind(dat$userde, dat$view_50or_click
             , dat$conntype, dat$play_mode) %>% as.data.frame()
colnames(cq50) = c("userde", "view_50or_click", 
                   "conntype", "play_mode")

ggplot(cq50, aes(x=as.factor(view_50or_click))) +
  geom_bar()+facet_grid(~as.factor(play_mode)) +
  labs(title = "廣告格式",x = "點擊或看超過一半的影片" , y = "累積次數")
#廣告格式vs點擊+q50 可再新增比例欄位


#=========================


# 
# barplot(var_number)
# ## 可以分群敘述統計者：
# var_number[var_number<20 & var_number >1]
# 
# par(mfrow=c(2,2))
# barplot(table(dat$play_mode), main = "廣告格式")
# barplot(table(dat$conntype), main = "裝置連線方式")
# barplot(table(dat$bsn), main = "瀏覽器品牌")
# barplot(table(dat$the_dt), main = "廣告事件發生的日期")

##====比較影片觀看長度跟年齡性別的分布====##

q100 = dat[ grepl("(q100)", dat$event),]
q75 = dat[ grepl("(q75)", dat$event),]
q50  = dat[ grepl("(q50)", dat$event),]
q25 = dat[ grepl("(q25)", dat$event),]
q0   = dat[ grepl("(q0)", dat$event),]

#========排除0=====#

no0_100 = which(q100$userde!=0)
q100 = q100[no0_100,]

no0_75 = which(q75$userde!=0)
q75 = q75[no0_75,]

no0_50 = which(q50$userde!=0)
q50 = q50[no0_50,]

no0_25 = which(q25$userde!=0)
q25 = q25[no0_25,]

no0_0 = which(q0$userde!=0)
q0 = q0[no0_0,]

###======================##
## 看圖影片觀看與不同年齡性別沒有明顯差異，待檢定
par(mfrow=c(1,5))
vioplot(q100$userde, xlab = "q100" )
vioplot(q75$userde, xlab = "q75" )
vioplot(q50$userde, xlab = "q50" )
vioplot(q25$userde, xlab = "q25" )
vioplot(q0$userde, xlab = "q0" )

##=====================##