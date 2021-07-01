# Package
{
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(GGally)
  library(tidyverse)
}

read.csv("C://RRR/19bat.csv",sep = "\t") -> bat19  #csv檔用Tab分隔的話，以\t輸入
head(bat19)

clearbat19 <- bat19[,c(4,11,14,15,16,17,18,20)] #挑選要用的變數 #17(長打率),18(上壘率)可考慮拿掉
c2bat19    <- bat19[,c(4,11,14,15,16,      20)]

#建立敘述統計表格
{
  clearbat19.ds <- data.frame(min  = c(1:8) ,
                              max  = c(1:8) ,
                              mean = c(1:8) ,
                              sd   = c(1:8) )
  
  as.data.frame(apply(clearbat19,2,min ))              -> clearbat19.ds$min
  as.data.frame(apply(clearbat19,2,max ))              -> clearbat19.ds$max
  as.data.frame(apply(clearbat19,2,mean)) %>% round(2) -> clearbat19.ds$mean
  as.data.frame(apply(clearbat19,2,sd  )) %>% round(2) -> clearbat19.ds$sd
}

#觀察共變數
corrgram(clearbat19)
pairs(clearbat19) #///lower.panel = NULL
corrgram(c2bat19) #拿掉17,18看起來比較好

#用馬哈蘭距離(D^2)來偵測outlier
mh <- as.numeric(mahalanobis(clearbat19, colMeans(clearbat19), cov(clearbat19))) #以每個變數的平均數和共變矩陣為參數
mh19 <- mutate(clearbat19,mh)
mh19 %>%
  arrange(desc(mh)) %>% #由大到小列出mh的值,變數有8個，自由度為8，以3.5為閥值，3.5*8=28 (3.5*6=21)
  filter(mh>28)         #共有6個人，視為潛在離群值(99低打高長,371看不出,281盜壘最多,116看不出,315盜壘第二,424會轟會盜)


#依距離作區分原則

E.dist <- dist(clearbat19,method = "euclidean") #這邊用歐式距離當標準，也可以用manhattan等方法
h.cluster <- hclust(E.dist , method = "ward.D2") #也可以用其他方法，像是最小距離、平均距離等，這邊用華德D2
#///par(mfrow = c(1,2))
#///plot(h.cluster, xlab="歐式距離")

k3 <- as.character(cutree(h.cluster , k = 3))

k3data <- cbind(k3,clearbat19)
k3all  <- cbind(k3,bat19)

#///boxplot(k3data)

boxplot(home_run ~ k3 , data = k3data)

ggplot(k3data , aes(k3,home_run)) +
  geom_boxplot(aes(fill = k3),notch = FALSE ) + ggtitle("home_run") + xlab("group")

ggplot(k3data , aes(k3,total_stolen_base)) +
  geom_boxplot(aes(fill = k3),notch = FALSE ) + ggtitle("total_stolen_base") + xlab("group")

#可用來看各組變數分配的Cluster Profile
ggparcoord(k3data , columns = 2:9 , groupColumn = 1 ) #在資料數多的情況下不好用

#scatter plot matrix 整套
{
  lower.cor  <- function(x,y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits=3)
    
    txt <- paste0("", r)
    cex.cor <- 0.8/strwidth(txt)
    mark = 0.5
    threshold = 0.5
    if( abs(r)> threshold ){mark = 1.5}
    text(0.5, 0.5, txt, cex = mark)
    #text(0.5, 0.5, txt, cex = abs(cex.cor * r)) #字體大小會隨著數值而縮放
  }
  
  upper.plot <- function(x,y){
    points( x , y , pch = 19 ,col = c("red","navyblue","darkgreen")[k3data$k3])  
  }
  
  pairs(k3data, lower.panel = lower.cor , upper.panel = upper.plot )
  
}

k3data %>%
  group_by(k3) %>%
  summarise(HR = mean(home_run), HRs = sd(home_run) , OBP = mean(on_base_percent) ,SB = mean(total_stolen_base) ,SBs = sd(total_stolen_base))

k3data %>%
  group_by(k3) %>%
  tally()

#結論:第一組為普通人，第二組很會轟，第三組很會盜