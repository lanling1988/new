library(rms) ##���س����
data <- read.csv("data2.csv", header = T)##������������
View(data)
head(data)
##����Ϊ������������ݣ���Ԫ����������Ӧ�ı���������Ҫ��Ӧ�޸ġ�
s <- Surv(data$d.time,data$death,type = "right")
ddist <- datadist(data)
options(datadist='ddist')

##���COX�ع�ģ�ͣ�time.inc��ʾ����೤ʱ���������
f <- cph(s ~ age + sex, x=TRUE,y=TRUE,surv = TRUE,time.inc = 5,data=data)

##�����u��time.inc����һ�¡�n/m��������Ӧ���м������ݵ㡣
cal <-calibrate(f,u=5,cmethod = 'KM',m=100)

plot(cal,xlim=c(0.7,1),ylim=c(0.7,1),errbar.col=c(rgb(0,0,0,maxColorValue = 255)),col=c(rgb(255,0,0,maxColorValue = 255)))
abline(0,1,lty=2,lwd=3,col=c(rgb(123,34,23,maxColorValue = 255)))
##��ͼ�Ĳ�����xlim��ylim�޶�x��y������䡣errbar.col��������ߵ���ɫ��col����У׼���ߵ���ɫ��
##����y=x�߲��������ͣ���ϸ����ɫ�����Զ��塣