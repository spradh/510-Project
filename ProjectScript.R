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
##############################################
#2.MPG~HorsePower
modelH<-lm(MPG~HorsePower, data=Training)
summary(modelH)
#Plot of Horsepower vs MPG for modelH
plot(Training$HorsePower,Training$MPG,xlab = "Horsepower",ylab="MPG")
lines(Training$HorsePower,coef(modelH)[1]+coef(modelH)[2]* Training$HorsePower)
##############################################
#3.MPG~Weight
modelW<-lm(MPG~Weight, data=Training)
summary(modelW)
#Plot of Weight vs MPG for modelW
plot(Training$Weight,Training$MPG,xlab = "Weight",ylab="MPG")
lines(Training$Weight,coef(modelW)[1]+coef(modelW)[2]* Training$Weight)
##############################################
#4.MPG~Acceleration
modelA<-lm(MPG~Acceleration, data=Training)
summary(modelA)
#Plot of Acceleration vs MPG for modelA
plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
lines(Training$Acceleration,coef(modelA)[1]+coef(modelA)[2]* Training$Acceleration)
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
##############################################
#6.MPG~ Displacement+Weight
model2DW <- lm(MPG ~ Displacement+Weight, data = Training)
summary(model2DW)
# Plot of Weight vs MPG for model2DW
plot(Training$Weight,Training$MPG)
lines(Training$Weight,coef(model2DW)[1]+coef(model2DW)[3]*Training$Weight)
# Plot of Displacement vs MPG for model2DW
plot(Training$Displacement,Training$MPG)
lines(Training$Displacement,coef(model2DW)[1]+coef(model2DW)[2]*Training$Displacement)
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
##############################################
#10.MPG ~ Acceleration+Weight 
model2WA <- lm(MPG ~ Acceleration+Weight, data = Training)
summary(model2WA)
coef(model2WA)
# Plot of Acceleration vs MPG for model2WA
plot(Training$Acceleration,Training$MPG)
lines(Training$Acceleration,coef(model2WA)[1]+coef(model2WA)[2]*Training$Acceleration)
#Plot of Weight vs MPG for model2WA
plot(Training$Weight, Training$MPG)   
lines(Training$Weight, coef(model2WA)[1]+coef(model2WA)[3]*Training$Weight)     
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
##############################################     
#12.MPG ~ Displacement+HorsePower+Acceleration
model3DHA <- lm(MPG ~ Displacement+HorsePower+Acceleration, data = Training)
summary(model3DHA)
# Plot for Acceleration vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Acceleration,Training$MPG)
lines(Training$Acceleration,coef(model3DHA)[1]+coef(model3DHA)[4]*Training$Acceleration)
# Plot for HorsePower vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$HorsePower,Training$MPG)
lines(Training$HorsePower,coef(model3DHA)[1]+coef(model3DHA)[3]*Training$HorsePower)
# Plot for DISPLACEMENT vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Displacement,Training$MPG)
lines(Training$Displacement,coef(model3DHA)[1]+coef(model3DHA)[2]*Training$Displacement)
##############################################
#13.MPG ~ Displacement+Weight+Acceleration
model3DWA<- lm(MPG ~ Displacement+Weight+Acceleration, data = Training)
summary(model3DWA)
# Plot of Acceleration vs MPG for model3DWA
plot(Training$Acceleration,Training$MPG)
lines(Training$Acceleration,coef(model3DWA)[1]+coef(model3DWA)[4]*Training$Acceleration)
# Plot of Weight vs MPG for model3DWA
plot(Training$Weight,Training$MPG)
lines(Training$Weight,coef(model3DWA)[1]+coef(model3DWA)[3]*Training$Weight)
# Plot of Displacement vs MPG for model3DWA
plot(Training$Displacement,Training$MPG)
lines(Training$Displacement,coef(model3DWA)[1]+coef(model3DWA)[2]*Training$Displacement)
##############################################
#14.Model for MPG Vs HWA
model3HWA <- lm(MPG ~ HorsePower+Weight+Acceleration, data = Training)
summary(model3HWA )
#Plot of Horsepower vs MPG for model3HWA
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model3HWA )[1]+coef(model3HWA )[2]*Training$HorsePower)
#Plot of Weight vs MPG for model3HWA
plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model3HWA )[1]+coef(model3HWA )[3]*Training$Weight)
#Plot of Acceleration vs MPG for model3HWA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model3HWA )[1]+coef(model3HWA )[4]*Training$Acceleration)
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

##########################################################################################################################
##########################################################################################################################
# THAT'S ALL FOLKS!

