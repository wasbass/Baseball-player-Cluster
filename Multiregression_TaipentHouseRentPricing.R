{
library(ggplot2)
library(car)
library(broom)
library(corrgram)
library(GGally)
library(tidyr)
library(AER)
library(MASS)
}

taipeirent <- read.csv("data_rent.csv")
colnames(taipeirent)[1] <- "rent"
colnames(taipeirent)[2] <- "egg"

#####EDA#####

densityPlot(taipeirent$rent)
densityPlot(log(taipeirent$rent))
#租金呈現右長尾，取對數後看起來像常態

#看連續變數的相關
tp <- taipeirent[c("rent","area","floor","height","parkingspace","neighbor")]
#plot(tp) 這個要跑很久 
plot(tp$neighbor,log(tp$rent))
plot(tp$neighbor,log(tp$area))
cor(log(tp$rent),log(tp$area))
cor(tp$area,tp$neighbor)

corrgram(tp)
#看起來area,rent,parkingspace要取對數比較好


#####變數變換#####
tpln <- cbind(log(taipeirent[c("rent","area")]),taipeirent[c("floor","height")])
tpln <- cbind(tpln,log(taipeirent$parkingspace+1),taipeirent["neighbor"])
colnames(tpln)[1:2] <- c("lnrent","lnarea")
colnames(tpln)[5]   <- "lnparking"
plot(tpln)
corrgram(tpln)
#確實比較好

newcol <- tpln[c(1,2,5)]
taipeirent[c(-1,-3,-8)]
tpnew  <- cbind(newcol,taipeirent[c(-1,-3,-8)])
#合併得到轉換過的資料


#看租金和坪數、蛋黃區之間的關係

qplot(lnarea,lnrent,data = tpnew  , col = egg , main = "蛋黃區房價明顯較高，坪數與租金正相關" ) + 
  geom_smooth(method = "lm" ) #淡藍色是蛋黃區的租金



#####總模型#####
lm (lnrent ~ . , data = tpnew) -> reg0
summary(reg0) #總模型R2為0.7655，調整R2為0.7617
cbind(tidy(reg0)[-1,],vif(reg0))
glance(reg0)
#其中area和egg的p-value最為顯著，與原本的猜想相同

vif(reg0) #共線性不明顯
corrgram(tpnew)
#有幾個特別要注意的高度線性相關
#1.蛋黃區和捷運高度正相關，代表蛋黃區內的房子基本上都有捷運，若已放入蛋黃區，則MRT的額外解釋力並不高
#2.樓層、建築物高、電梯和警衛呈現高度正相關，尤其是建築物高和警衛
#  代表大樓基本上都配有警衛和電梯，放入建築物高可能就足夠了
#3.坪數和警衛呈現正相關，可能大面積的房間(maybe高級住宅)會更傾向配警衛
#4.電源、電視、網路、第四台、冰箱高度正相關，可能對房東來說，一起安裝比較方便且有優惠
#5.可開伙跟其他附設配備為負相關，可能可開伙的比較多是沒裝潢的家庭式，但無從驗證

#####簡單模型#####
lm (lnrent ~ lnarea + egg , data = tpnew) ->reg1
summary(reg1)#R2為0.7246，調整R2為0.7243
tidy(reg1)
glance(reg1)
anova(reg1,reg0)

#####驗證交互作用模型#####

lm (lnrent ~ lnarea + egg + egg * lnarea , data = tpnew ) -> reg1i
summary(reg1i)
glance(reg1i)
tidy(reg1i)
anova(reg1,reg1i)
#R2為0.7252，調整R2為0.7247
#在初始模型中，交互作用項顯著，而主效果不顯著
#蛋黃區的係數估計降低非常多，在這個模型中不顯著
#lnarea的標準誤變小了，多放交互作用向能提升解釋力

#接著我把彼此可能具有交互作用的變數先放入模型中，分別是車位數量、樓層、建築高度和電梯，先不放入交互作用項
lm (lnrent ~ egg + lnarea + lnparking + floor + height + lift , data = tpnew) ->reg10
summary(reg10)

#加入egg * lnarea，檢驗蛋黃區的每坪單價是否比較貴
lm (lnrent ~ lnarea + egg + lnparking + floor + height + lift
            +lnarea * egg                                 , data = tpnew) -> reg11
summary(reg11)
anova(reg10,reg11)#與原模型無顯著不同，不放入egg * lnrent

#加入egg * parkingspace，檢驗蛋黃區的車位是否比較貴
lm (lnrent ~ egg + lnarea + lnparking + floor + height + lift
          +egg * lnarea + egg * lnparking                 , data = tpnew ) -> reg12 
summary(reg12)
anova(reg11,reg12)#與原模型並無顯著不同，不放入egg * parkingspace

#加入height * floor，檢驗住同樣樓層下，更高的建築物是否比較貴
lm (lnrent ~ egg + lnarea + lnparking + floor + height + lift
            +egg * lnarea + height * floor , data = tpnew ) -> reg13 
summary(reg13)
anova(reg11 ,reg13)#與原模型顯著不同

#加入lift * floor，檢驗住同樣樓層下，有電梯是否比較貴
lm (lnrent ~ egg + lnarea + lnparking + floor + height + lift
            +egg * lnarea + height * floor + lift * floor , data = tpnew ) -> reg14 
summary(reg14)
anova(reg13 ,reg14)#與上一個模型顯著不同

lm( lnrent ~ egg + lnarea + height + floor + lift +
             height * floor + lift * floor , data = tpnew ) -> regII
summary(regII)
glance(regII)

#regII是我們考慮所有交互作用項的模型，稱之為交互作用總模型

#####完整模型#####
#把以上獲得的交互作用項放入原本的總模型中
lm (lnrent ~ . + height * floor + lift * floor , data = tpnew ) -> reg2
summary(reg2) #完整模型的R2為0.706，調整R2為0.701
glance(reg2)
anova(reg2,reg0) #完整模型與總模型顯著不同

#####逐步迴歸法#####

#若從完整模型開始backward，以AIC作為模型篩選標準，依序放入變數，直到AIC不能再變小為止
reg3 = step(reg2,scope=list(lower=regII, upper=reg2),  #或是stepAIC
                  direction="backward")
glance(reg3)
AIC(reg3)
extractAIC(reg3,scale = 0)
summary(reg3)
glance(reg3S)
anova(reg3,reg2)#結果顯示簡化模型與完整模型解釋能力差不多，而簡化模型一口氣少了11項自變數

#從交互作用模型開始forward
forward.lm = step(regII, 
                  scope=list(lower=regII, upper=reg2), 
                  direction="forward")#結果與向後逐步相同，就是reg3

#####簡化模型#####
fit <- reg3
summary(fit)
glance(fit)
tidy(fit)
print(fit)
#R2為0.7725，調整R2為0.77，SER為0.405，SER蠻大的，不太適合預測

#####Gauss-Markov假設驗證#####

#殘差分析：常態、獨立、同質
res.fit <- resid(fit)
qplot(tpnew$lnrent,res.fit)
#租金越大(遠離平均11)，殘差越大變異，符合CI和PI的性質
#但有些微正相關

cor(tpnew$lnrent,res.fit)
yr <- lm(res.fit ~ tpnew$lnrent )
summary(yr)

resplot <- cbind(res.fit,tpln)
plot(resplot)
par(mfrow=c(2,2))  #一次顯示四張圖
plot(fit)
shapiro.test(res.fit)
par(mfrow=c(1,1))
densityPlot(res.fit)

shapiro.test(tpnew$lnrent)

#從QQ圖和檢定來看，殘差不服從常態分配
#但也合理，就算取了對數，租金的分佈本來就不像常態

#用Breusch-Pagan來檢定異質變異數
bptest(fit)

#結果為拒絕H0，代表確實存在異質變異數
#然而這也合理，就模型中的連續自變數為例
qplot(tpnew$lnarea,res.fit)
qplot(tpnew$height,res.fit)
qplot(tpnew$floor,res.fit)

lm(res.fit^2~tpnew$lnarea + tpnew$height + tpnew$floor) -> blm1
summary(blm1)

#15樓以上的房子較少見，成本結構會比較類似，變異較小
#樓層越低的房間，可用來做更多功能，租金變異較大

#因此需改用HC標準誤作統計推論
coeftest(fit,vcov = vcovHC(fit,type = "HC2"))
#結果大同小異
coeftest(reg1,vcov = vcovHC(reg1,type = "HC2"))
coeftest(regII,vcov = vcovHC(regII,type = "HC2"))


durbinWatsonTest(fit)
#本文並沒有時間序列資料，因此租金之間應該不存在自我相關
#DW顯著是因為資料的順序是照租金由大至小排序，應變數大小和殘差確實會有相關，因此忽略之

#最後的最後，殘差看起來跟自變數都沒有相關，應該符合外生性
#即使違反這麼多Gauss Markov的假設，至少係數估計仍然不偏
#如果沒有遺漏變數的話啦

qplot(fit$fitted.values,res.fit)
qplot(tpnew$lnrent,fit$fitted.values)

