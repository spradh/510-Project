auto_mpg<-read.table('auto-mpg.data',header = FALSE,col.names = c("MPG","Cylinders","Displacement","HorsePower","Weight","Acceleration","ModelYear","Origin","CarName") )
auto_mpg <- auto_mpg[-c(33,127,331,337,355,375),]
rownames(auto_mpg) <- seq(length=nrow(auto_mpg))
auto_mpg$HorsePower = as.numeric(as.character(auto_mpg$HorsePower))
Training<-auto_mpg[1:300,]
Test<-auto_mpg[301:nrow(auto_mpg),]
View(Test)
View(Training)
summary(Training)


#1 variable Model

#Displacement vs MPG
plot(Training$Displacement,Training$MPG,xlab = "Displacement",ylab="MPG")
modelDisp<-lm(MPG~Displacement, data=Training)
summary(modelDisp)
lines(Training$Displacement,coef(modelDisp)[1]+coef(modelDisp)[2]* Training$Displacement)

#HorsePower vs MPG
plot(Training$HorsePower,Training$MPG,xlab = "Horsepower",ylab="MPG")
modelHp<-lm(MPG~HorsePower, data=Training)
summary(modelHp)
lines(Training$HorsePower,coef(modelHp)[1]+coef(modelHp)[2]* Training$HorsePower)

#Weight vs MPG
plot(Training$Weight,Training$MPG,xlab = "Weight",ylab="MPG")
modelWeight<-lm(MPG~Weight, data=Training)
summary(modelWeight)
lines(Training$Weight,coef(modelWeight)[1]+coef(modelWeight)[2]* Training$Weight)

#Acceleration vs MPG
plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
modelAcc<-lm(MPG~Acceleration, data=Training)
summary(modelAcc)
lines(Training$Acceleration,coef(modelAcc)[1]+coef(modelAcc)[2]* Training$Acceleration)


#2 variable Model

#2 Variable
#HW
model2HW<-lm(MPG ~ HorsePower+Weight, data = Training)
summary(model2HW)


plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HW)[1]+coef(model2HW)[2]*Training$HorsePower)

plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model2HW)[1]+coef(model2HW)[3]*Training$Weight)

#DH
model2DH<-lm(MPG ~ Displacement+HorsePower, data = Training)
summary(model2DH)


plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2DH)[1]+coef(model2DH)[3]*Training$HorsePower)

plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DH)[1]+coef(model2DH)[3]*Training$Displacement)


model2DH.res=resid(model2DH)

hist(model2DH.res)
plot(Training$Displacement,model2DH.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)

plot(Training$HorsePower,model2DH.res,ylab = 'Residuals',xlab = 'Horsepower')
abline(0,0)

#HA
model2HA<-lm(MPG ~ HorsePower+Acceleration, data = Training)
summary(model2HA)


plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HA)[1]+coef(model2HA)[2]*Training$HorsePower)

plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2HA)[1]+coef(model2HA)[3]*Training$Acceleration)
model2HA.res=resid(model2HA)

hist(model2HA.res)
plot(Training$HorsePower,model2HA.res,ylab = 'Residuals',xlab = 'Horsepower')
abline(0,0)

plot(Training$Acceleration,model2HA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)



#DA
model2DA<-lm(MPG ~ Displacement+Acceleration, data = Training)
summary(model2DA)


plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2DA)[1]+coef(model2DA)[3]*Training$Acceleration)

plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DA)[1]+coef(model2DA)[2]*Training$Displacement)

model2DA.res=resid(model2DA)

hist(model2DA.res)
plot(Training$Displacement,model2DA.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)

plot(Training$Acceleration,model2DA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)



#4variable Model

model4<-lm(MPG ~ Displacement+ HorsePower + Weight + Acceleration, data=Training)
summary(model4)


plot(Training$Displacement,Training$MPG,xlab = "Displacement",ylab="MPG")
lines(Training$Displacement,coef(model4)[1]+coef(model4)[2]* Training$Displacement)

plot(Training$HorsePower,Training$MPG,xlab = "HorsePower",ylab="MPG")
lines(Training$HorsePower,coef(model4)[1]+coef(model4)[3]* Training$HorsePower)

plot(Training$Weight,Training$MPG,xlab = "Weight",ylab="MPG")
lines(Training$Weight,coef(model4)[1]+coef(model4)[4]* Training$Weight)

plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
lines(Training$Acceleration,coef(model4)[1]+coef(model4)[5]* Training$Acceleration)

#3 variable

#Model for MPG Vs DHW


model3a <- lm(MPG ~ Displacement+HorsePower+Weight, data = Training)
summary(model3a)

plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model3a)[1]+coef(model3a)[2]*Training$Displacement)

plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3a)[1]+coef(model3a)[3]*Training$HorsePower)

plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3a)[1]+coef(model3a)[4]*Training$Weight)

#Model for MPG Vs HWA


#Model for MPG Vs HWA
model3HWA <- lm(MPG ~ HorsePower+Weight+Acceleration, data = Training)
summary(model3HWA )


plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3HWA )[1]+coef(model3HWA )[2]*Training$HorsePower)


plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3HWA )[1]+coef(model3HWA )[3]*Training$Weight)




plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3HWA )[1]+coef(model3HWA )[4]*Training$Acceleration)


