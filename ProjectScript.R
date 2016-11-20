auto_mpg<-read.table('auto-mpg.data',header = FALSE,col.names = c("MPG","Cylinders","Displacement","HorsePower","Weight","Acceleration","ModelYear","Origin","CarName") )
auto_mpg <- auto_mpg[-c(33,127,331,337,355,375),]
rownames(auto_mpg) <- seq(length=nrow(auto_mpg))
auto_mpg$HorsePower = as.numeric(as.character(auto_mpg$HorsePower))
Training<-auto_mpg[1:300,]
Test<-auto_mpg[301:nrow(auto_mpg),]
View(Test)
View(Training)
summary(Training)
##########################################################################################################################
##########################################################################################################################
#Studying the relation between the explanatory and response variables
plot(Training$Displacement,Training$MPG,xlab = "Displacement",ylab="MPG")
plot(Training$HorsePower,Training$MPG,xlab = "Horsepower",ylab="MPG")
plot(Training$Weight,Training$MPG,xlab = "Weight",ylab="MPG")
plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
##########################################################################################################################
##########################################################################################################################
#1 VARIABLE MODEL
##############################################
#1.MPG~Displacement
modelD<-lm(MPG~Displacement, data=Training)
summary(modelD)
#Plot of Displacement vs MPG for modelD
plot(Training$Displacement,Training$MPG,xlab = "Displacement",ylab="MPG")
lines(Training$Displacement,coef(modelD)[1]+coef(modelD)[2]* Training$Displacement)
#Residuals
modelD.res=resid(modelD)  # For Residuals
hist(modelD.res ,breaks = 25,main = "Histogram of ModelD",xlab = 'Displacement') # For Histogram
plot(Training$Displacement,modelD.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
modelD.res.abs=abs(modelD.res)
plot(Training$Displacement,modelD.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
#Testing
predict.D<-coef(modelD)[1]+coef(modelD)[2]* Test$Displacement
e.D<-Test$MPG-predict.D
mean(e.D)
##############################################
#2.MPG~HorsePower
modelH<-lm(MPG~HorsePower, data=Training)
summary(modelH)
#Plot of Horsepower vs MPG for modelH
plot(Training$HorsePower,Training$MPG,xlab = "Horsepower",ylab="MPG")
lines(Training$HorsePower,coef(modelH)[1]+coef(modelH)[2]* Training$HorsePower)
#Residuals
modelH.res=resid(modelH)  # For Residuals
hist(modelH.res ,breaks = 25,main = "Histogram of ModelH",,xlab = 'HorsePower') # For Histogram
plot(Training$HorsePower,modelH.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
modelH.res.abs=abs(modelH.res)
plot(Training$HorsePower,modelH.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
#Testing
predict.H<-coef(modelH)[1]+coef(modelH)[2]* Test$HorsePower
e.H<-Test$MPG-predict.H
mean(e.H)
##############################################
#3.MPG~Weight
modelW<-lm(MPG~Weight, data=Training)
summary(modelW)
#Plot of Weight vs MPG for modelW
plot(Training$Weight,Training$MPG,xlab = "Weight",ylab="MPG")
lines(Training$Weight,coef(modelW)[1]+coef(modelW)[2]* Training$Weight)
#Residuals
modelW.res=resid(modelW)  # For Residuals
hist(modelW.res,breaks = 25,main = "Histogram of ModelW",xlab = 'Weight') # For Histogram
plot(Training$Weight,modelW.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
modelW.res.abs=abs(modelW.res)
plot(Training$Weight,modelW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
#Testing
predict.W<-coef(modelW)[1]+coef(modelW)[2]* Test$Weight
e.W<-Test$MPG-predict.W
mean(e.W)
##############################################
#4.MPG~Acceleration
modelA<-lm(MPG~Acceleration, data=Training)
summary(modelA)
#Plot of Acceleration vs MPG for modelA
plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
lines(Training$Acceleration,coef(modelA)[1]+coef(modelA)[2]* Training$Acceleration)
#Residuals
modelA.res=resid(modelA)  # For Residuals
hist(modelA.res,breaks = 25,main = "Histogram of ModelA",xlab = 'Acceleration') # For Histogram
plot(Training$Acceleration,modelA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
modelA.res.abs=abs(modelA.res)
plot(Training$Acceleration,modelA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.A<-coef(modelA)[1]+coef(modelA)[2]* Test$Accleration
e.A<-Test$MPG-predict.A
mean(e.A)
##########################################################################################################################
##########################################################################################################################
#2 VARIABLE MODELS
##############################################
#5.MPG ~ Displacement+HorsePower
model2DH<-lm(MPG ~ Displacement+HorsePower, data = Training)
summary(model2DH)
#Plot of Horsepower vs MPG for model2DH
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2DH)[1]+coef(model2DH)[3]*Training$HorsePower)
#Plot of Displacement vs MPG for model2DH
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DH)[1]+coef(model2DH)[3]*Training$Displacement)
#Residuals
model2DH.res=resid(model2DH)  # For Residuals
hist(model2DH.res,breaks = 25,main = "Histogram of Model2DH",xlab = 'Displacement') # For Histogram
plot(Training$Displacement,model2DH.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model2DH.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
model2DH.res.abs=abs(model2DH.res)
plot(Training$Displacement,model2DH.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model2DH.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
#Testing
predict.DH<-coef(model2DH)[1]+coef(model2DH)[2]* Test$Displacement+coef(model2DH)[3]*Test$HorsePower
e.DH<-Test$MPG-predict.DH
mean(e.DH)
##############################################
#6.MPG~ Displacement+Weight
model2DW <- lm(MPG ~ Displacement+Weight, data = Training)
summary(model2DW)
# Plot of Weight vs MPG for model2DW
plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model2DW)[1]+coef(model2DW)[3]*Training$Weight)
# Plot of Displacement vs MPG for model2DW
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DW)[1]+coef(model2DW)[2]*Training$Displacement)
#Residuals
model2DW.res=resid(model2DW)  # For Residuals
hist(model2DW.res,breaks = 25,main = "Histogram of Model2DW",xlab = 'Displacement') # For Histogram
plot(Training$Displacement,model2DW.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$Weight,model2DW.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
model2DW.res.abs=abs(model2DW.res)
plot(Training$Displacement,model2DW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$Weight,model2DW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
#Testing
predict.DW<-coef(model2DW)[1]+coef(model2DW)[2]* Test$Displacement+coef(model2DW)[3]*Test$Weight
e.DW<-Test$MPG-predict.DW
mean(e.DW)
##############################################
#7.MPG~Displacement+ Acceleration
model2DA<-lm(MPG ~ Displacement+Acceleration, data = Training)
summary(model2DA)
# Plot of Acceleration vs MPG for model2DA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2DA)[1]+coef(model2DA)[3]*Training$Acceleration)
# Plot of Displacement vs MPG for model2DA
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DA)[1]+coef(model2DA)[2]*Training$Displacement)
#Residuals
model2DA.res=resid(model2DA)  # For Residuals
hist(model2DA.res,breaks = 25,main = "Histogram of Model2DA",xlab = 'Displacement') # For Histogram
plot(Training$Displacement,model2DA.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$Acceleration,model2DA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model2DA.res.abs=abs(model2DA.res)
plot(Training$Displacement,model2DA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$Acceleration,model2DA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
#Testing
predict.DA<-coef(model2DA)[1]+coef(model2DA)[2]* Test$Displacement+coef(model2DA)[3]*Test$Acceleration
e.DA<-Test$MPG-predict.DA
mean(e.DA)
##############################################
#8.MPG ~ HorsePower+Weight 
model2HW<-lm(MPG ~ HorsePower+Weight, data = Training)
summary(model2HW)
# Plot of Horsepower vs MPG for model2HW
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HW)[1]+coef(model2HW)[2]*Training$HorsePower)
# Plot of Weight vs MPG for model2HW
plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model2HW)[1]+coef(model2HW)[3]*Training$Weight)
#Residuals
model2HW.res=resid(model2HW)  # For Residuals
hist(model2HW.res,breaks = 25,main = "Histogram of Model2HW",xlab = 'HorsePower') # For Histogram
plot(Training$HorsePower,model2HW.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model2HW.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
model2HW.res.abs=abs(model2HW.res)
plot(Training$Weight,model2HW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$HorsePower,model2HW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
##############################################
#9.MPG ~ HorsePower+Acceleration 
model2HA<-lm(MPG ~ HorsePower+Acceleration, data = Training)
summary(model2HA)
# Plot of Horsepower vs MPG for model2HA
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HA)[1]+coef(model2HA)[2]*Training$HorsePower)    
# Plot of Acceleration vs MPG for model2HA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2HA)[1]+coef(model2HA)[3]*Training$Acceleration)
#Residuals   
model2HA.res=resid(model2HA) # For Residuals
hist(model2HA.res,breaks = 25,main = "Histogram of Model2HA",xlab = 'HorsePower') # For Histogram
plot(Training$HorsePower,model2HA.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Acceleration,model2HA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
model2HA.res.abs=abs(model2HA.res)
plot(Training$Acceleration,model2HA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
plot(Training$HorsePower,model2HA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
#Testing
predict.DW<-coef(model2DW)[1]+coef(model2HA)[2]* Test$Displacement+coef(model2DW)[3]*Test$Weight
e.DW<-Test$MPG-predict.DW
mean(e.DW)
##############################################
#10.MPG ~ Acceleration+Weight 
model2WA <- lm(MPG ~ Acceleration+Weight, data = Training)
summary(model2WA)
coef(model2WA)
# Plot of Acceleration vs MPG for model2WA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2WA)[1]+coef(model2WA)[2]*Training$Acceleration)
#Plot of Weight vs MPG for model2WA
plot(Training$Weight, Training$MPG,xlab = 'Weight', ylab = 'MPG')   
lines(Training$Weight, coef(model2WA)[1]+coef(model2WA)[3]*Training$Weight)     
#Residuals
model2WA.res=resid(model2WA)  # For Residuals
hist(model2WA.res,breaks = 25,main = "Histogram of Model2WA",xlab = 'Weight') # For Histogram
plot(Training$Weight,model2WA.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model2WA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)

model2WA.res.abs=abs(model2WA.res)
plot(Training$Weight,model2WA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model2WA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
##########################################################################################################################
##########################################################################################################################
#3 VARIABLE MODELS
##############################################
#11.MPG ~ Displacement+HorsePower+Weight 
model3DHW <- lm(MPG ~ Displacement+HorsePower+Weight, data = Training)
summary(model3DHW)
#Plot of Displacement vs MPG for model3DHW    
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model3DHW)[1]+coef(model3DHW)[2]*Training$Displacement)     
#Plot of Horsepower vs MPG for model3DHW
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3DHW)[1]+coef(model3DHW)[3]*Training$HorsePower)     
#Plot of Weight vs MPG for model3DHW
plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3DHW)[1]+coef(model3DHW)[4]*Training$Weight)   
#Residuals
model3DHW.res=resid(model3DHW)  # For Residuals
plot(Training$Displacement,model3DHW.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model3DHW.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model3DHW.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)

model3DHW.res.abs=abs(model3DHW.res)
plot(Training$Displacement,model3DHW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model3DHW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Weight,model3DHW.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
##############################################     
#12.MPG ~ Displacement+HorsePower+Acceleration
model3DHA <- lm(MPG ~ Displacement+HorsePower+Acceleration, data = Training)
summary(model3DHA)
# Plot for Acceleration vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3DHA)[1]+coef(model3DHA)[4]*Training$Acceleration)
# Plot for HorsePower vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3DHA)[1]+coef(model3DHA)[3]*Training$HorsePower)
# Plot for DISPLACEMENT vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model3DHA)[1]+coef(model3DHA)[2]*Training$Displacement)
#Residuals
model3DHA.res=resid(model3DHA)  # For Residuals
plot(Training$Displacement,model3DHA.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model3DHA.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Acceleration,model3DHA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)

model3DHA.res.abs=abs(model3DHA.res)
plot(Training$Displacement,model3DHA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model3DHA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Acceleration,model3DHA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
##############################################
#13.MPG ~ Displacement+Weight+Acceleration
model3DWA<- lm(MPG ~ Displacement+Weight+Acceleration, data = Training)
summary(model3DWA)
# Plot of Acceleration vs MPG for model3DWA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3DWA)[1]+coef(model3DWA)[4]*Training$Acceleration)
# Plot of Weight vs MPG for model3DWA
plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3DWA)[1]+coef(model3DWA)[3]*Training$Weight)
# Plot of Displacement vs MPG for model3DWA
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model3DWA)[1]+coef(model3DWA)[2]*Training$Displacement)
#Residuals
model3DWA.res=resid(model3DWA)  # For Residuals
hist(model3DWA.res,breaks = 25,main = "Histogram of Model3DWA",xlab = 'Displacement') # For Histogram
plot(Training$Displacement,model3DWA.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$Weight,model3DWA.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model3DWA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)  

model3DWA.res.abs=abs(model3DWA.res)
plot(Training$Displacement,model3DWA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$Weight,model3DWA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model3DWA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
##############################################
#14.Model for MPG Vs HWA
model3HWA <- lm(MPG ~ HorsePower+Weight+Acceleration, data = Training)
summary(model3HWA )
#Plot of Horsepower vs MPG for model3HWA
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3HWA)[1]+coef(model3HWA )[2]*Training$HorsePower)
#Plot of Weight vs MPG for model3HWA
plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3HWA )[1]+coef(model3HWA )[3]*Training$Weight)
#Plot of Acceleration vs MPG for model3HWA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3HWA )[1]+coef(model3HWA )[4]*Training$Acceleration)
#Residuals
model3HWA.res=resid(model3HWA)  # For Residuals
hist(model3HWA.res,breaks = 25,main = "Histogram of Model3HWA",xlab = 'HorsePower') # For Histogram
plot(Training$HorsePower,model3HWA.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model3HWA.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model3HWA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)

model3HWA.res.abs=abs(model3HWA.res)
plot(Training$HorsePower,model3HWA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Weight,model3HWA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model3HWA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
##########################################################################################################################
##########################################################################################################################
#4 VARIABLE MODELS
############################################## 
#15.MPG ~ Displacement+ HorsePower + Weight + Acceleration
model4<-lm(MPG ~ Displacement+ HorsePower + Weight + Acceleration, data=Training)
summary(model4)
#Plot of Displacement vs MPG for model4
plot(Training$Displacement,Training$MPG,xlab = "Displacement",ylab="MPG")
lines(Training$Displacement,coef(model4)[1]+coef(model4)[2]* Training$Displacement)
#Plot of Horsepower vs MPG for model4
plot(Training$HorsePower,Training$MPG,xlab = "HorsePower",ylab="MPG")
lines(Training$HorsePower,coef(model4)[1]+coef(model4)[3]* Training$HorsePower)
#Plot of Weight vs MPG for model4
plot(Training$Weight,Training$MPG,xlab = "Weight",ylab="MPG")
lines(Training$Weight,coef(model4)[1]+coef(model4)[4]* Training$Weight)
#Plot of Acceleration vs MPG for model4
plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
lines(Training$Acceleration,coef(model4)[1]+coef(model4)[5]* Training$Acceleration)
#Residuals
model4.res=resid(model4)  # For Residuals
hist(model4.res,breaks = 25,main = "Histogram of Model4",xlab = 'Displacement') # For Histogram
plot(Training$Displacement,model4.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)
plot(Training$HorsePower,model4.res,ylab = 'Residuals',xlab = 'HorsePower')
abline(0,0)
plot(Training$Weight,model4.res,ylab = 'Residuals',xlab = 'Weight')
abline(0,0)
plot(Training$Acceleration,model4.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)

model4.res.abs=abs(model4.res)
plot(Training$Displacement,model4.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Displacement')
plot(Training$HorsePower,model4.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Horsepower')
plot(Training$Weight,model4.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Weight')
plot(Training$Acceleration,model3HWA.res.abs,ylab = 'Absolute value of Residuals',xlab = 'Acceleration')
##########################################################################################################################
##########################################################################################################################
# THAT'S ALL FOLKS!

