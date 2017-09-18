#importing and cleaning xdata
data<-read.csv("xdata",header=FALSE)
data<-data[,-4]
data
#impotring ydata
ydata<-read.table("ydata", header=FALSE)
data[4]<-ydata

#importing weights
wdata<-read.csv("wdata", header=FALSE)
wdata<-wdata[,-2]
wdata
dim(wdata)

#Normalizing dataset
data$V1<-(data$V1-mean(data$V1))/sd(data$V1)
data$V2<-(data$V2-mean(data$V2))/sd(data$V2)
data$V3<-(data$V3-mean(data$V3))/sd(data$V3)

#Scatterplots
par(mfrow=c(3,1))
plot(data$V1,data$V1.1)
plot(data$V2,data$V1.1)
plot(data$V3,data$V1.1)

#Question 2a
############
#calculating linear regression
model2a<-lm(V1.1~V1+V2+V3, data=data)
summary(model2a)

#plotting regression lines
plot(data$V1,data$V1.1)
lines(data$V1,coef(model2a)[1]+coef(model2a)[2]*data$V1)

plot(data$V2,data$V1.1)
lines(data$V2,coef(model2a)[1]+coef(model2a)[3]*data$V2)

plot(data$V3,data$V1.1)
lines(data$V3,coef(model2a)[1]+coef(model2a)[4]*data$V3)


#Question 2b
############
#calculating weighted linear regression
model2b<-lm(V1.1~V1+V2+V3, data=data, weights=wdata)
summary(model2b)

#plotting regression lines
plot(data$V1,data$V1.1)
lines(data$V1,coef(model2a)[1]+coef(model2a)[2]*data$V1)
-
plot(data$V2,data$V1.1)
lines(data$V2,coef(model2a)[1]+coef(model2a)[3]*data$V2)

plot(data$V3,data$V1.1)
lines(data$V3,coef(model2a)[1]+coef(model2a)[4]*data$V3)
