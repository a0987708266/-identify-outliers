library(car)
data(Duncan)
plot(Duncan)
mod.duncan <- lm(prestige~income+education,data=Duncan)
summary(mod.duncan)
residualPlot(mod.duncan,quadratic=F)
qqPlot(mod.duncan)

influenceIndexPlot(mod.duncan,id=list(n=3))
#第二張圖可以看出有一個參數minister標準差在3，可能會是一個outlier
outlierTest(mod.duncan)
#但經過baffaroni調整後(第三張圖)他的p-value變成0.2了，雖然小，但不顯著為outlier

influencePlot(mod.duncan,id=list(n=1))
p=length(mod.duncan$coef)
n=nrow(Duncan)
limit1 = (2*p)/n #limit1 for Hat-Values of Bubble plot
limit2 = (3*p)/n #limit2 for Hat-Values of Bubble plot
#兩個limit即為泡泡圖中的直的虛線
#泡泡的半徑為cook distance，看有沒有超過兩個limit

#Check influential case using cook's distance
plot(cooks.distance(mod.duncan))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(mod.duncan), row.names(Duncan))

#Check influential case using dfbetas
dfbetasPlots(mod.duncan,id.n=2)
#minister會對income造成負影響、對education造成正影響(看DFBETAS)


#來處理minister的離群問題
Duncan[rownames(Duncan)=='minister',]
#先把minister拿掉
newd <- Duncan[!rownames(Duncan)%in%c('minister'),]
m1 <- lm(prestige~income+education,data=newd)
influenceIndexPlot(m1,id=list(n=2))
n=nrow(newd)
plot(cooks.distance(m1))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m1), row.names(newd))
dfbetasPlots(m1,id.n=2)
#反而是conductor出現離群問題

#比較去掉minister前後，兩係數的變化
compareCoefs(mod.duncan,m1,se=F)

#conductor變離群值，試試看把conductor拿掉
newd1 <- newd[!rownames(newd)%in%c('conductor'),]
m2 <- lm(prestige~income+education,data=newd1)
influenceIndexPlot(m2,id=list(n=2))
n=nrow(newd1)
plot(cooks.distance(m2))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m2), row.names(newd1))
#看出又有離群問題，這次是reporter
dfbetasPlots(m2,id.n=2)

#比較去掉conductor前後，兩係數的變化
compareCoefs(m1,m2,se=F)

#試試看把reporter拿掉
newd2 <- newd1[!rownames(newd1)%in%c('reporter'),]
m3 <- lm(prestige~income+education,data=newd2)
influenceIndexPlot(m3,id.n=3)
n=nrow(newd2)
plot(cooks.distance(m3))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m3), row.names(newd2))
dfbetaPlots(m3,id.n=3)

#比較去掉reporter前後，兩係數的變化
compareCoefs(m2,m3,se=F)
#前後的係數變化已經很小，可以停止

#最後要標註說明我們拿掉了三個樣本(minister, conductor, reporter)















