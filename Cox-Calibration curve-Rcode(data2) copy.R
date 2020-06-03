library(rms) ##加载程序包
data <- read.csv("data2.csv", header = T)##加载生存数据
View(data)
head(data)
##申明为生存分析的数据，美元符号链接相应的变量名，需要对应修改。
s <- Surv(data$d.time,data$death,type = "right")
ddist <- datadist(data)
options(datadist='ddist')

##拟合COX回归模型，time.inc表示计算多长时间的生存率
f <- cph(s ~ age + sex, x=TRUE,y=TRUE,surv = TRUE,time.inc = 5,data=data)

##这里的u与time.inc保持一致。n/m决定曲线应该有几个数据点。
cal <-calibrate(f,u=5,cmethod = 'KM',m=100)

plot(cal,xlim=c(0.7,1),ylim=c(0.7,1),errbar.col=c(rgb(0,0,0,maxColorValue = 255)),col=c(rgb(255,0,0,maxColorValue = 255)))
abline(0,1,lty=2,lwd=3,col=c(rgb(123,34,23,maxColorValue = 255)))
##绘图的参数，xlim与ylim限定x和y轴的区间。errbar.col定义误差线的颜色，col定义校准曲线的颜色。
##添加y=x线并设置线型，粗细，颜色，可自定义。