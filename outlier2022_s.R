library(car)
data(Duncan)
plot(Duncan)
mod.duncan <- lm(prestige~income+education,data=Duncan)
summary(mod.duncan)
residualPlot(mod.duncan,quadratic=F)
qqPlot(mod.duncan)

influenceIndexPlot(mod.duncan,id=list(n=3))
#第二張圖可以看出有一個參數minister標準差在3，可能會是一個outlier
#但經過baffaroni調整後(第三張圖)他的p-value變成0.2了，雖然小，但不顯著為outlier

outlierTest(mod.duncan)

influencePlot(mod.duncan,id=list(n=1))
p=length(mod.duncan$coef)
n=nrow(Duncan)
limit1 = (2*p)/n #limit1 for Hat-Values of Bubble plot
limit2 = (3*p)/n #limit2 for Hat-Values of Bubble plot

#Check influential case using cook's distance
plot(cooks.distance(mod.duncan))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(mod.duncan), row.names(Duncan))

#Check influential case using dfbetas
dfbetasPlots(mod.duncan,id.n=2)




Duncan[rownames(Duncan)=='minister',]

newd <- Duncan[!rownames(Duncan)%in%c('minister'),]
m1 <- lm(prestige~income+education,data=newd)
influenceIndexPlot(m1,id=list(n=2))
n=nrow(newd)
plot(cooks.distance(m1))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m1), row.names(newd))
dfbetasPlots(m1,id.n=2)

compareCoefs(mod.duncan,m1,se=F)

newd1 <- newd[!rownames(newd)%in%c('conductor'),]
m2 <- lm(prestige~income+education,data=newd1)
influenceIndexPlot(m2,id=list(n=2))
n=nrow(newd1)
plot(cooks.distance(m2))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m2), row.names(newd1))
dfbetasPlots(m2,id.n=2)

compareCoefs(m1,m2,se=F)

newd2 <- newd1[!rownames(newd1)%in%c('reporter'),]
m3 <- lm(prestige~income+education,data=newd2)
influenceIndexPlot(m3,id.n=3)
n=nrow(newd2)
plot(cooks.distance(m3))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m3), row.names(newd2))
dfbetaPlots(m3,id.n=3)

compareCoefs(m2,m3,se=F)















