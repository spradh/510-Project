auto_mpg<-read.table('auto-mpg.data',header = FALSE,col.names = c("MPG","Cylinders","Displacement","HorsePower","Weight","Acceleration","ModelYear","Origin","CarName") )
auto_mpg <- auto_mpg[-c(33,127,331,337,355,375),]
rownames(auto_mpg) <- seq(length=nrow(auto_mpg))
auto_mpg$HorsePower = as.numeric(as.character(auto_mpg$HorsePower))
Training<-auto_mpg[1:300,]
Test<-auto_mpg[301:nrow(auto_mpg),]

##########################################################################################################################
#Normalizing Test Dataset
Test$Displacement<-(Test$Displacement-mean(Training$Displacement))/sd(Training$Displacement)
Test$HorsePower<-(Test$HorsePower-mean(Training$HorsePower))/sd(Training$HorsePower)
Test$Weight<-(Test$Weight-mean(Training$Weight))/sd(Training$Weight)
Test$Acceleration<-(Test$Acceleration-mean(Training$Acceleration))/sd(Training$Acceleration)


#Normalizing Training Dataset
Training$Displacement<-(Training$Displacement-mean(Training$Displacement))/sd(Training$Displacement)
Training$HorsePower<-(Training$HorsePower-mean(Training$HorsePower))/sd(Training$HorsePower)
Training$Weight<-(Training$Weight-mean(Training$Weight))/sd(Training$Weight)
Training$Acceleration<-(Training$Acceleration-mean(Training$Acceleration))/sd(Training$Acceleration)


##########################################################################################################################
##########################################################################################################################
#Studying the relation between the explanatory and response variables
par(mfrow=c(2,2))
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for ModelD",xlab = "Displacement",ylab="MPG")
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for ModelH",xlab = "Horsepower",ylab="MPG")
plot(Training$Weight,Training$MPG,main = "MPG vs Weight for ModelW",xlab = "Weight",ylab="MPG")
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for ModelA",xlab = "Acceleration",ylab="MPG")
##########################################################################################################################
##########################################################################################################################
#1 VARIABLE MODEL
##############################################
#1.MPG~Displacement
modelD<-lm(MPG~Displacement, data=Training)
summary(modelD)
par(mfrow=c(1,1))
#Plot of Displacement vs MPG for modelD
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for ModelD",xlab = "Displacement",ylab="MPG")
lines(Training$Displacement,coef(modelD)[1]+coef(modelD)[2]* Training$Displacement)
#Residuals
modelD.res=resid(modelD)  # For Residuals
par(mfrow=c(2,2))
hist(modelD.res ,breaks = 25,main = "Histogram of ModelD Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,modelD.res,main = "Residuals vs Displacement for ModelD",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
modelD.res.abs=abs(modelD.res)
plot(Training$Displacement,modelD.res.abs,main = "Absolute Residuals vs Displacement for ModelD",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
#Testing
predict.D<-coef(modelD)[1]+coef(modelD)[2]* Test$Displacement
e.D<-(Test$MPG-predict.D)^2
mean(e.D)
##############################################
#2.MPG~HorsePower
modelH<-lm(MPG~HorsePower, data=Training)
summary(modelH)
par(mfrow=c(1,1))
#Plot of Horsepower vs MPG for modelH
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for modelH",xlab = "Horsepower",ylab="MPG")
lines(Training$HorsePower,coef(modelH)[1]+coef(modelH)[2]* Training$HorsePower)
#Residuals
modelH.res=resid(modelH)  # For Residuals
par(mfrow=c(2,2))
hist(modelH.res ,breaks = 25,main = "Histogram of modelH Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$HorsePower,modelH.res,main = "Residuals vs HorsePower for modelH",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
modelH.res.abs=abs(modelH.res)
plot(Training$HorsePower,modelH.res.abs,main = "Absolute Residuals vs HorsePower for modelH",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
#Testing
predict.H<-coef(modelH)[1]+coef(modelH)[2]* Test$HorsePower
e.H<-(Test$MPG-predict.H)^2
mean(e.H)
##############################################
#3.MPG~Weight
modelW<-lm(MPG~Weight, data=Training)
summary(modelW)
par(mfrow=c(1,1))
#Plot of Weight vs MPG for modelW
plot(Training$Weight,Training$MPG,main = "MPG vs Weight for modelW",xlab = "Weight",ylab="MPG")
lines(Training$Weight,coef(modelW)[1]+coef(modelW)[2]* Training$Weight)
#Residuals
modelW.res=resid(modelW)  # For Residuals
par(mfrow=c(2,2))
hist(modelW.res,breaks = 25,main = "Histogram of modelW Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Weight,modelW.res,main = "Residuals vs Weight for modelW",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
modelW.res.abs=abs(modelW.res)
plot(Training$Weight,modelW.res.abs,main = "Absolute Residuals vs Weight for modelW",ylab = 'Absolute value of Residuals',xlab = 'Weight')
#Testing
predict.W<-coef(modelW)[1]+coef(modelW)[2]* Test$Weight
e.W<-(Test$MPG-predict.W)^2
mean(e.W)
##############################################
#4.MPG~Acceleration
modelA<-lm(MPG~Acceleration, data=Training)
summary(modelA)
par(mfrow=c(1,1))
#Plot of Acceleration vs MPG for modelA
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for modelA",xlab = "Acceleration",ylab="MPG")
lines(Training$Acceleration,coef(modelA)[1]+coef(modelA)[2]* Training$Acceleration)
#Residuals
modelA.res=resid(modelA)  # For Residuals
par(mfrow=c(2,2))
hist(modelA.res,breaks = 25,main = "Histogram of modelA Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Acceleration,modelA.res,main = "Residuals vs Acceleration for modelA",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
modelA.res.abs=abs(modelA.res)
plot(Training$Acceleration,modelA.res.abs,main = "Absolute Residuals vs Acceleration for modelA",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.A<-coef(modelA)[1]+coef(modelA)[2]* Test$Acceleration
e.A<-(Test$MPG-predict.A)^2
mean(e.A)
##########################################################################################################################
##########################################################################################################################
#2 VARIABLE MODELS
##############################################
#5.MPG ~ Displacement+HorsePower
model2DH<-lm(MPG ~ Displacement+HorsePower, data = Training)
summary(model2DH)
par(mfrow=c(1,2))
#Plot of Horsepower vs MPG for model2DH
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for model2DH",xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2DH)[1]+coef(model2DH)[3]*Training$HorsePower)
#Plot of Displacement vs MPG for model2DH
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for model2DH",xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DH)[1]+coef(model2DH)[2]*Training$Displacement)
#Residuals
model2DH.res=resid(model2DH)  # For Residuals
par(mfrow=c(2,3))
hist(model2DH.res,breaks = 25,main = "Histogram of model2DH Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,model2DH.res,main = "Residuals vs Displacement for model2DH",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model2DH.res,main = "Residuals vs HorsePower for model2DH",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
model2DH.res.abs=abs(model2DH.res)
plot(Training$Displacement,model2DH.res.abs,main = "Absolute Residuals vs Displacement for model2DH",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model2DH.res.abs,main = "Absolute Residuals vs HorsePower for model2DH",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
#Testing
predict.DH<-coef(model2DH)[1]+coef(model2DH)[2]* Test$Displacement+coef(model2DH)[3]*Test$HorsePower
e.DH<-(Test$MPG-predict.DH)^2
mean(e.DH)
##############################################
#6.MPG~ Displacement+Weight
model2DW <- lm(MPG ~ Displacement+Weight, data = Training)
summary(model2DW)
par(mfrow=c(1,2))
# Plot of Weight vs MPG for model2DW
plot(Training$Weight,Training$MPG,main = "MPG vs Weight for model2DW",xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model2DW)[1]+coef(model2DW)[3]*Training$Weight)
# Plot of Displacement vs MPG for model2DW
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for model2DW",xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DW)[1]+coef(model2DW)[2]*Training$Displacement)
#Residuals
model2DW.res=resid(model2DW)  # For Residuals
par(mfrow=c(2,3))
hist(model2DW.res,breaks = 25,main = "Histogram of model2DW Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,model2DW.res,main = "Residuals vs Displacement for model2DW",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$Weight,model2DW.res,main = "Residuals vs Weight for model2DW",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
model2DW.res.abs=abs(model2DW.res)
plot(Training$Displacement,model2DW.res.abs,main = "Absolute Residuals vs Displacement for model2DW",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$Weight,model2DW.res.abs,main = "Absolute Residuals vs Weight for model2DW",ylab = 'Absolute value of Residuals',xlab = 'Weight')
#Testing
predict.DW<-coef(model2DW)[1]+coef(model2DW)[2]* Test$Displacement+coef(model2DW)[3]*Test$Weight
e.DW<-(Test$MPG-predict.DW)^2
mean(e.DW)
##############################################
#7.MPG~Displacement+ Acceleration
model2DA<-lm(MPG ~ Displacement+Acceleration, data = Training)
summary(model2DA)
par(mfrow=c(1,2))
# Plot of Acceleration vs MPG for model2DA
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for model2DA",xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2DA)[1]+coef(model2DA)[3]*Training$Acceleration)
# Plot of Displacement vs MPG for model2DA
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for model2DA",xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DA)[1]+coef(model2DA)[2]*Training$Displacement)
#Residuals
model2DA.res=resid(model2DA)  # For Residuals
par(mfrow=c(2,3))
hist(model2DA.res,breaks = 25,main = "Histogram of model2DA Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,model2DA.res,main = "Residuals vs Displacement for model2DA",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$Acceleration,model2DA.res,main = "Residuals vs Acceleration for model2DA",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model2DA.res.abs=abs(model2DA.res)
plot(Training$Displacement,model2DA.res.abs,main = "Absolute Residuals vs Displacement for model2DA",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$Acceleration,model2DA.res.abs,main = "Absolute Residuals vs Acceleration for model2DA",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.DA<-coef(model2DA)[1]+coef(model2DA)[2]* Test$Displacement+coef(model2DA)[3]*Test$Acceleration
e.DA<-(Test$MPG-predict.DA)^2
mean(e.DA)
##############################################
#8.MPG ~ HorsePower+Weight 
model2HW<-lm(MPG ~ HorsePower+Weight, data = Training)
summary(model2HW)
par(mfrow=c(1,2))
# Plot of Horsepower vs MPG for model2HW
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for model2HW",xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HW)[1]+coef(model2HW)[2]*Training$HorsePower)
# Plot of Weight vs MPG for model2HW
plot(Training$Weight,Training$MPG,main = "MPG vs Weight for model2HW",xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model2HW)[1]+coef(model2HW)[3]*Training$Weight)
#Residuals
model2HW.res=resid(model2HW)  # For Residuals
par(mfrow=c(2,3))
hist(model2HW.res,breaks = 25,main = "Histogram of model2HW Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$HorsePower,model2HW.res,main = "Residuals vs HorsePower for model2HW",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model2HW.res,main = "Residuals vs Weight for model2HW",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
model2HW.res.abs=abs(model2HW.res)
plot(Training$Weight,model2HW.res.abs,main = "Absolute Residuals vs Weight for model2HW",ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$HorsePower,model2HW.res.abs,main = "Absolute Residuals vs HorsePower for model2HW",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
#Testing
predict.HW<-coef(model2HW)[1]+coef(model2HW)[2]* Test$HorsePower+coef(model2HW)[3]*Test$Weight
e.HW<-(Test$MPG-predict.HW)^2
mean(e.HW)
##############################################
#9.MPG ~ HorsePower+Acceleration 
model2HA<-lm(MPG ~ HorsePower+Acceleration, data = Training)
summary(model2HA)
par(mfrow=c(1,2))
# Plot of Horsepower vs MPG for model2HA
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for model2HA",xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HA)[1]+coef(model2HA)[2]*Training$HorsePower)    
# Plot of Acceleration vs MPG for model2HA
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for model2HA",xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2HA)[1]+coef(model2HA)[3]*Training$Acceleration)
#Residuals   
model2HA.res=resid(model2HA) # For Residuals
par(mfrow=c(2,3))
hist(model2HA.res,breaks = 25,main = "Histogram of Model2HA",xlab = 'Residuals') # For Histogram
plot(Training$HorsePower,model2HA.res,main = "Residuals vs HorsePower for model2HA",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Acceleration,model2HA.res,main = "Residuals vs Acceleration for model2HA",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model2HA.res.abs=abs(model2HA.res)
plot(Training$Acceleration,model2HA.res.abs,main = "Absolute Residuals vs Acceleration for model2HA",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
plot(Training$HorsePower,model2HA.res.abs,main = "Absolute Residuals vs HorsePower for model2HA",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
#Testing
predict.HA<-coef(model2HA)[1]+coef(model2HA)[2]* Test$HorsePower+coef(model2HA)[3]*Test$Acceleration
e.HA<-(Test$MPG-predict.HA)^2
mean(e.HA)
##############################################
#10.MPG ~ Acceleration+Weight 
model2WA <- lm(MPG ~ Weight+Acceleration, data = Training)
summary(model2WA)
coef(model2WA)
par(mfrow=c(1,2))
# Plot of Acceleration vs MPG for model2WA
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for model2WA",xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2WA)[1]+coef(model2WA)[3]*Training$Acceleration)
#Plot of Weight vs MPG for model2WA
plot(Training$Weight, Training$MPG,main = "MPG vs Weight for model2WA",xlab = 'Weight', ylab = 'MPG')   
lines(Training$Weight, coef(model2WA)[1]+coef(model2WA)[2]*Training$Weight)     
#Residuals
model2WA.res=resid(model2WA)  # For Residuals
par(mfrow=c(2,3))
hist(model2WA.res,breaks = 25,main = "Histogram of model2WA Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Weight,model2WA.res,main = "Residuals vs Weight for model2WA",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model2WA.res,main = "Residuals vs Acceleration for model2WA",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model2WA.res.abs=abs(model2WA.res)
plot(Training$Weight,model2WA.res.abs,main = "Absolute Residuals vs Weight for model2WA",ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model2WA.res.abs,main = "Absolute Residuals vs Acceleration for model2WA",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.WA<-coef(model2WA)[1]+coef(model2WA)[2]* Test$Weight+coef(model2WA)[3]*Test$Acceleration
e.WA<-(Test$MPG-predict.WA)^2
mean(e.WA)
##########################################################################################################################
##########################################################################################################################
#3 VARIABLE MODELS
##############################################
#11.MPG ~ Displacement+HorsePower+Weight 
model3DHW <- lm(MPG ~ Displacement+HorsePower+Weight, data = Training)
summary(model3DHW)
par(mfrow=c(2,2))
#Plot of Displacement vs MPG for model3DHW    
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for model3DHW",xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model3DHW)[1]+coef(model3DHW)[2]*Training$Displacement)     
#Plot of Horsepower vs MPG for model3DHW
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for model3DHW",xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3DHW)[1]+coef(model3DHW)[3]*Training$HorsePower)     
#Plot of Weight vs MPG for model3DHW
plot(Training$Weight,Training$MPG,main = "MPG vs Weight for model3DHW",xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3DHW)[1]+coef(model3DHW)[4]*Training$Weight)   
#Residuals
model3DHW.res=resid(model3DHW)  # For Residuals
par(mfrow=c(3,3))
hist(model3DHW.res,breaks = 25,main = "Histogram of model3DHW Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,model3DHW.res,main = "Residuals vs Displacement for model3DHW",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model3DHW.res,main = "Residuals vs HorsePower for model3DHW",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model3DHW.res,main = "Residuals vs Weight for model3DHW",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
model3DHW.res.abs=abs(model3DHW.res)
plot(Training$Displacement,model3DHW.res.abs,main = "Absolute Residuals vs Displacement for model3DHW",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model3DHW.res.abs,main = "Absolute Residuals vs HorsePower for model3DHW",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Weight,model3DHW.res.abs,main = "Absolute Residuals vs Weight for model3DHW",ylab = 'Absolute value of Residuals',xlab = 'Weight')
#Testing
predict.DHW <- coef(model3DHW)[1]+coef(model3DHW)[2]*Test$Displacement+coef(model3DHW)[3]*Test$HorsePower+coef(model3DHW)[4]*Test$Weight
e.DHW <- (Test$MPG-predict.DHW)^2
mean(e.DHW)
##############################################     
#12.MPG ~ Displacement+HorsePower+Acceleration
model3DHA <- lm(MPG ~ Displacement+HorsePower+Acceleration, data = Training)
summary(model3DHA)
par(mfrow=c(2,2))
# Plot for Acceleration vs MPG with coef of model3DHA (MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for model3DHA",xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3DHA)[1]+coef(model3DHA)[4]*Training$Acceleration)
# Plot for HorsePower vs MPG with coef of model3DHA (MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for model3DHA",xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3DHA)[1]+coef(model3DHA)[3]*Training$HorsePower)
# Plot for DISPLACEMENT vs MPG with coef of model3DHA (MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for model3DHA",xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model3DHA)[1]+coef(model3DHA)[2]*Training$Displacement)
#Residuals
model3DHA.res=resid(model3DHA)  # For Residuals
par(mfrow=c(3,3))
hist(model3DHA.res,breaks = 25,main = "Histogram of model3DHA Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,model3DHA.res,main = "Residuals vs Displacement for model3DHA",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model3DHA.res,main = "Residuals vs HorsePower for model3DHA",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Acceleration,model3DHA.res,main = "Residuals vs Acceleration for model3DHA",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model3DHA.res.abs=abs(model3DHA.res)
plot(Training$Displacement,model3DHA.res.abs,main = "Absolute Residuals vs Displacement for model3DHA",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model3DHA.res.abs,main = "Absolute Residuals vs HorsePower for model3DHA",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Acceleration,model3DHA.res.abs,main = "Absolute Residuals vs Acceleration for model3DHA",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
predict.DHA <- coef(model3DHA)[1]+coef(model3DHA)[2]*Test$Displacement+coef(model3DHA)[3]*Test$HorsePower+coef(model3DHA)[4]*Test$Acceleration
e.DHA <- (Test$MPG-predict.DHA)^2
mean(e.DHA)
##############################################
#13.MPG ~ Displacement+Weight+Acceleration
model3DWA<- lm(MPG ~ Displacement+Weight+Acceleration, data = Training)
summary(model3DWA)
par(mfrow=c(2,2))
# Plot of Acceleration vs MPG for model3DWA
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for model3DWA",xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3DWA)[1]+coef(model3DWA)[4]*Training$Acceleration)
# Plot of Weight vs MPG for model3DWA
plot(Training$Weight,Training$MPG,main = "MPG vs Weight for model3DWA",xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3DWA)[1]+coef(model3DWA)[3]*Training$Weight)
# Plot of Displacement vs MPG for model3DWA
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement for model3DWA",xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model3DWA)[1]+coef(model3DWA)[2]*Training$Displacement)
#Residuals
model3DWA.res=resid(model3DWA)  # For Residuals
par(mfrow=c(3,3))
hist(model3DWA.res,breaks = 25,main = "Histogram of model3DWA Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,model3DWA.res,main = "Residuals vs Displacement for model3DWA",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$Weight,model3DWA.res,main = "Residuals vs Weight for model3DWA",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model3DWA.res,main = "Residuals vs Acceleration for model3DWA",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)  
model3DWA.res.abs=abs(model3DWA.res)
plot(Training$Displacement,model3DWA.res.abs,main = "Absolute Residuals vs Displacement for model3DWA",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$Weight,model3DWA.res.abs,main = "Absolute Residuals vs Weight for model3DWA",ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model3DWA.res.abs,main = "Absolute Residuals vs Acceleration for model3DWA",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.DWA <- coef(model3DWA)[1]+coef(model3DWA)[2]*Test$Displacement+coef(model3DWA)[3]*Test$Weight+coef(model3DWA)[4]*Test$Acceleration
e.DWA <- (Test$MPG-predict.DWA)^2
mean(e.DWA)
##############################################
#14.Model for MPG Vs HWA
model3HWA <- lm(MPG ~ HorsePower+Weight+Acceleration, data = Training)
summary(model3HWA )
par(mfrow=c(2,2))
#Plot of Horsepower vs MPG for model3HWA
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower for model3HWA",xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3HWA)[1]+coef(model3HWA )[2]*Training$HorsePower)
#Plot of Weight vs MPG for model3HWA
plot(Training$Weight,Training$MPG,main = "MPG vs Weight for model3HWA",xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3HWA )[1]+coef(model3HWA )[3]*Training$Weight)
#Plot of Acceleration vs MPG for model3HWA
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration for model3HWA",xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3HWA )[1]+coef(model3HWA )[4]*Training$Acceleration)
#Residuals
model3HWA.res=resid(model3HWA)  # For Residuals
par(mfrow=c(3,3))
hist(model3HWA.res,breaks = 25,main = "Histogram of model3HWA Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$HorsePower,model3HWA.res,main = "Residuals vs HorsePower for model3HWA",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model3HWA.res,main = "Residuals vs Weight for model3HWA",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model3HWA.res,main = "Residuals vs Acceleration for model3HWA",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model3HWA.res.abs=abs(model3HWA.res)
plot(Training$HorsePower,model3HWA.res.abs,main = "Absolute Residuals vs HorsePower for model3HWA",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Weight,model3HWA.res.abs,main = "Absolute Residuals vs Weight for model3HWA",ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model3HWA.res.abs,main = "Absolute Residuals vs Acceleration for model3HWA",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.HWA <- coef(model3HWA)[1]+coef(model3HWA)[2]*Test$HorsePower+coef(model3HWA)[3]*Test$Weight+coef(model3HWA)[4]*Test$Acceleration
e.HWA <- (Test$MPG-predict.HWA)^2
mean(e.HWA)
##########################################################################################################################
##########################################################################################################################
#4 VARIABLE MODELS
############################################## 
#15.MPG ~ Displacement+ HorsePower + Weight + Acceleration
model4<-lm(MPG ~ Displacement+ HorsePower + Weight + Acceleration, data=Training)
summary(model4)
par(mfrow=c(2,2))
#Plot of Displacement vs MPG for model4
plot(Training$Displacement,Training$MPG,main = "MPG vs Displacement of Model4",xlab = "Displacement",ylab="MPG")
lines(Training$Displacement,coef(model4)[1]+coef(model4)[2]* Training$Displacement)
#Plot of Horsepower vs MPG for model4
plot(Training$HorsePower,Training$MPG,main = "MPG vs HorsePower of Model4",xlab = "HorsePower",ylab="MPG")
lines(Training$HorsePower,coef(model4)[1]+coef(model4)[3]* Training$HorsePower)
#Plot of Weight vs MPG for model4
plot(Training$Weight,Training$MPG,main = "MPG vs Weight of Model4",xlab = "Weight",ylab="MPG")
lines(Training$Weight,coef(model4)[1]+coef(model4)[4]* Training$Weight)
#Plot of Acceleration vs MPG for model4
plot(Training$Acceleration,Training$MPG,main = "MPG vs Acceleration of Model4",xlab = "Acceleration",ylab="MPG")
lines(Training$Acceleration,coef(model4)[1]+coef(model4)[5]* Training$Acceleration)
#Residuals
model4.res=resid(model4)  # For Residuals
par(mfrow=c(3,3))
hist(model4.res,breaks = 25,main = "Histogram of Model4 Residuals ",xlab = 'Residuals') # For Histogram
plot(Training$Displacement,model4.res,main = "Residuals vs Displacement for Model4",ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model4.res,main = "Residuals vs HorsePower for Model4",ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model4.res,main = "Residuals vs Weight for Model4",ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model4.res,main = "Residuals vs Acceleration for Model4",ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model4.res.abs=abs(model4.res)
plot(Training$Displacement,model4.res.abs,main = "Absolute Residuals vs Displacement for Model4",ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model4.res.abs,main = "Absolute Residuals vs HorsePower for Model4",ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Weight,model4.res.abs,main = "Absolute Residuals vs Weight for Model4",ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model4.res.abs,main = "Absolute Residuals vs Acceleration for Model4",ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.4 <- coef(model4)[1]+coef(model4)[2]*Test$Displacement+coef(model4)[3]*Test$HorsePower+coef(model4)[4]*Test$Weight+coef(model4)[5]*Test$Acceleration
e.4<-(Test$MPG-predict.4)^2
mean(e.4)
##########################################################################################################################
##########################################################################################################################
# THAT'S ALL FOLKS!
