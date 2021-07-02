#####Package#####
{
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(GGally)
  library(tidyverse)
  library(cluster)
  library(factoextra)
  library(corrgram)
  library(NbClust)
  library(stats)
  library(gridExtra)
}

read.csv("C://RRR/19bat.csv",sep = "\t") -> bat19  #csv檔用Tab分隔的話，以\t輸入
head(bat19)

clearbat19 <- bat19[,c(4,11,14,15,16,17,18,20)] #挑選要用的變數 #17(長打率),18(上壘率)可考慮拿掉
#///c2bat19    <- bat19[,c(4,11,14,15,16,      20)]

#####建立敘述統計表格#####
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

#####共變數與離群值#####
corrgram(clearbat19)
pairs(clearbat19) #///lower.panel = NULL
#///corrgram(c2bat19) #拿掉17,18看起來比較好
#可以考慮用因素分析來拿掉高度線性相關的變數

#用D^2來偵測outlier(會用到變異數和共變數)
mh <- as.numeric(mahalanobis(clearbat19, colMeans(clearbat19), cov(clearbat19))) #以每個變數的平均數和共變矩陣為參數
mh19 <- mutate(clearbat19,mh)
mh19 %>%
  arrange(desc(mh)) %>% #由大到小列出mh的值,變數有8個，自由度為8，以3.5為閥值，3.5*8=28 (3.5*6=21)
  filter(mh>28)         #共有6個人，視為潛在離群值(99低打高長,371看不出,281盜壘最多,116看不出,315盜壘第二,424會轟會盜)

df <- scale(clearbat19)

#####先驗集群的存在#####
#用Hopkins統計量來判斷集群是否明顯存在
#見https://www.datanovia.com/en/lessons/assessing-clustering-tendency/
#數值越大代表Data的分布跟Uniform分配相比，有較明顯的集群存在(若>0.75，則有90%的信心水準)

res = get_clust_tendency(df, n = 40, graph = TRUE) #n代表挑選的sample數量 
res$hopkins_stat #為0.73，高於閾值0.5，可接受集群存在

#圖示判斷法
bat.dist <- get_dist(clearbat19, stand = TRUE, method = "euclidean")
#或是用daisy(clearbat19 , stand = T), get_dist可調用參數比較多

fviz_dist(dist.obj = bat.dist , order = T ,    #order會使相近的data排在一起，紅色代表彼此的相似程度越高
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#並非隨機分布，代表集群有可能存在，但並不明顯

class(bat.dist)
order(bat.dist) %>%
  head()

head(bat.dist)


#####分層式集群分析#####
E.dist <- dist(clearbat19,method = "euclidean") #這邊用歐式距離當標準，也可以用manhattan等方法
h.cluster <- hclust(E.dist , method = "ward.D2") #也可以用其他方法，像是最小距離、平均距離等
#這邊用華德D2，會讓分群結果中，每一群的數量偏向更多
#///par(mfrow = c(1,2))
#///plot(h.cluster, xlab="歐式距離")

k3 <- as.character(cutree(h.cluster , k = 3))

k3data <- cbind(k3,clearbat19)
k3all  <- cbind(k3,bat19)

#agnes法
hc2 <- agnes(E.dist, method = "ward")
hc2$ac #agnes可以計算聚合係數(agglomerative coefficient)

#####分層式Stopping Rule#####
stop_num <- NbClust(clearbat19, distance = "euclidean", method = "ward.D2", max.nc = 6, index = "all")
stop_num
#根據NbClust的數種Stopping Rule的Index顯示，3個分群應該是最好的選擇

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
    points( x , y , pch = 19 ,col = c("red","navyblue","darkgreen")[k3data$k3])  #記得改
  }
  
  pairs(k3data, lower.panel = lower.cor , upper.panel = upper.plot )
  
}

#####後驗集群的存在(主成分分析)#####

#用主成分分析的頭兩個主成分，來判斷集群分析是否有效，並且驗證分群結果的好壞
fviz_pca_ind(prcomp(df), title = "PCA - clearbat19", palette = "jco",
             geom = "point", ggtheme = theme_classic(), habillage = k3data$k3, #habillage可用來標色
             legend = "bottom")

#接著用k-means來觀察分類狀況
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

#(要跑很久)
#///fviz_dend(hclust(dist(df)), k = 3, k_colors = "jco", as.ggplot = TRUE, show_labels = FALSE)

#####圖型判斷#####
#用散佈圖來判斷哪個變數可以畫盒鬚圖

boxplot(home_run ~ k3 , data = k3data)

ggplot(k3data , aes(k3,home_run)) +
  geom_boxplot(aes(fill = k3),notch = FALSE ) + ggtitle("home_run") + xlab("group")

ggplot(k3data , aes(k3,total_stolen_base)) +
  geom_boxplot(aes(fill = k3),notch = FALSE ) + ggtitle("total_stolen_base") + xlab("group")

#可用來看各組變數分配的Cluster Profile
ggparcoord(k3data , columns = 2:9 , groupColumn = 1 ) #在資料數多的情況下不好用

k3data %>%
  group_by(k3) %>%
  summarise(HR = mean(home_run), HRs = sd(home_run) , OBP = mean(on_base_percent) ,SB = mean(total_stolen_base) ,SBs = sd(total_stolen_base))

k3data %>%
  group_by(k3) %>%
  tally() #各組人數

#分層式結論:第一組為普通人，第二組很會轟，第三組很會盜

#####K-means#####
km.bat <-kmeans( clearbat19 , centers = 3 ,nstart = 25) #nstart可視為重新選中心點
km.bat #組間變異占總變異48.6%
km.bat$withinss #各組的組內變異
km.bat$centers  #各組的組內中心

fviz_cluster(km.bat,           
             data = clearbat19,              
             geom = c("point","text"), 
             frame.type = "norm") 

#####嘗試不同分組數並繪圖#####

{
km2 <- kmeans(clearbat19, centers = 2, nstart = 25)
km4 <- kmeans(clearbat19, centers = 4, nstart = 25)
km5 <- kmeans(clearbat19, centers = 5, nstart = 25)
#///km6 <- kmeans(clearbat19, centers = 6, nstart = 25)

kmp2 <- fviz_cluster(km2, geom = "point",  data = clearbat19) + ggtitle("k = 2")
kmp3 <- fviz_cluster(km.bat, geom = "point", data = clearbat19) + ggtitle("k = 3")
kmp4 <- fviz_cluster(km4, geom = "point",  data = clearbat19) + ggtitle("k = 4")
kmp5 <- fviz_cluster(km5, geom = "point",  data = clearbat19) + ggtitle("k = 5")
#///kmp6 <- fviz_cluster(km6, geom = "point",  data = clearbat19) + ggtitle("k = 6")

grid.arrange(kmp2,kmp3, kmp4, kmp5 , nrow = 2)
}
#就前兩項主成分來看，三組以上的分群並不直觀，
#而第一主成分佔了40%的變異，第二主成分只有17%，分兩組會更明瞭

#####K-means Stopping Rule#####
fviz_nbclust(kmdata, 
             FUNcluster = kmeans,   
             method = "wss", 
             k.max = 12) + 
  labs(title="Elbow Method for K-Means") +
  geom_vline(xintercept = 5,linetype = 2) #依照組內變異的法則，可以考慮分五群

fviz_nbclust(kmdata, kmeans, method = "silhouette")

#####調整組別(非必要)#####
change_group <-function(x){
  if (x==1){ x = 2}
  else if (x==2){ x = 1}
  else {x = x}
  return(x)
 }


km <- km.bat$cluster
km <- as.character(sapply(km,change_group))

kmdata <- cbind(km,clearbat19)
compare_data <-cbind(k3,kmdata)

groupdif <- compare_data[,1:2]
table(groupdif) #平庸組的人更平庸，巨砲組的標準更嚴格，速度組差距不明顯


upper.plot <- function(x,y){
  points( x , y , pch = 19 ,col = c("red","navyblue","darkgreen")[kmdata$km])  
}
pairs(kmdata, lower.panel = lower.cor , upper.panel = upper.plot )

#####kmeans圖型判斷#####
boxplot(home_run ~ km , data = kmdata)

#全壘打的組內差異更小了，代表分組的重要性變更大了
ggplot(kmdata , aes(km,home_run)) +
  geom_boxplot(aes(fill = km),notch = FALSE ) + ggtitle("home_run") + xlab("group")

ggplot(k3data , aes(k3,home_run)) +
  geom_boxplot(aes(fill = k3),notch = FALSE ) + ggtitle("home_run") + xlab("group")


#相較之下，第三組盜壘數的變異被放大了

ggplot(kmdata , aes(km,total_stolen_base)) +
  geom_boxplot(aes(fill = km),notch = FALSE ) + ggtitle("total_stolen_base") + xlab("group")

ggplot(k3data , aes(k3,total_stolen_base)) +
  geom_boxplot(aes(fill = k3),notch = FALSE ) + ggtitle("total_stolen_base") + xlab("group")

#另外，被三振率似乎也有關係，但相較之下不太重要
ggplot(kmdata , aes(km, k_percent)) +
  geom_boxplot(aes(fill = km),notch = FALSE ) + ggtitle("k_percent") + xlab("group")

#可用來看各組變數分配的Cluster Profile
ggparcoord(kmdata , columns = 2:9 , groupColumn = 1 ) #在資料數多的情況下不好用

kmdata %>%
  group_by(km) %>%
  summarise(HR = mean(home_run), HRs = sd(home_run) , OBP = mean(on_base_percent) ,SB = mean(total_stolen_base) ,SBs = sd(total_stolen_base))

kmdata %>%
  group_by(km) %>%
  tally() #各組人數
#####Manova檢驗各組的變數#####
kmmanova <- manova( cbind(age,home_run,k_percent,bb_percent,batting_avg,slg_percent,on_base_percent,total_stolen_base) ~ km ,
                    data = kmdata )
summary(kmmanova)     #整體F檢定顯著
summary.aov(kmmanova) #除了年紀之外，其他變數的F檢定也顯著


