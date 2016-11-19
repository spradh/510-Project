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
#1 variable Model
##############################################
#MPG~Displacement(1)
plot(Training$Displacement,Training$MPG,xlab = "Displacement",ylab="MPG")
modelDisp<-lm(MPG~Displacement, data=Training)
summary(modelDisp)
lines(Training$Displacement,coef(modelDisp)[1]+coef(modelDisp)[2]* Training$Displacement)
##############################################
#MPG~HorsePower(2)
plot(Training$HorsePower,Training$MPG,xlab = "Horsepower",ylab="MPG")
modelHp<-lm(MPG~HorsePower, data=Training)
summary(modelHp)
lines(Training$HorsePower,coef(modelHp)[1]+coef(modelHp)[2]* Training$HorsePower)
##############################################
#MPG~Weight(3)
plot(Training$Weight,Training$MPG,xlab = "Weight",ylab="MPG")
modelWeight<-lm(MPG~Weight, data=Training)
summary(modelWeight)
lines(Training$Weight,coef(modelWeight)[1]+coef(modelWeight)[2]* Training$Weight)
##############################################
#MPG~Acceleration(4)
plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
modelAcc<-lm(MPG~Acceleration, data=Training)
summary(modelAcc)
lines(Training$Acceleration,coef(modelAcc)[1]+coef(modelAcc)[2]* Training$Acceleration)
##########################################################################################################################
##########################################################################################################################
#2 variable Model
##############################################
#MPG ~ Displacement+HorsePower(5)
model2DH<-lm(MPG ~ Displacement+HorsePower, data = Training)
summary(model2DH)

#Plot of Horsepower vs MPG for model2DH
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2DH)[1]+coef(model2DH)[3]*Training$HorsePower)
#Plot of Displacement vs MPG for model2DH
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DH)[1]+coef(model2DH)[3]*Training$Displacement)


model2DH.res=resid(model2DH)

hist(model2DH.res)
plot(Training$Displacement,model2DH.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)

plot(Training$HorsePower,model2DH.res,ylab = 'Residuals',xlab = 'Horsepower')
abline(0,0)
##############################################

#MPG~ Displacement+Weight(6)

#Plot for MPG VS Weight & Displacement
model2DW <- lm(MPG ~ Displacement+Weight, data = Training)
summary(model2DW)

# Plot of Weight vs MPG for model2DW
plot(Training$Weight,Training$MPG)
lines(Training$Weight,coef(model2DW)[1]+coef(model2DW)[3]*Training$Weight)


# Plot of Displacement vs MPG for model2DW
plot(Training$Displacement,Training$MPG)
lines(Training$Displacement,coef(model2DW)[1]+coef(model2DW)[2]*Training$Displacement)

##############################################
#MPG~Displacement+ Acceleration (7)
model2DA<-lm(MPG ~ Displacement+Acceleration, data = Training)
summary(model2DA)

# Plot of Acceleration vs MPG for model2DA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2DA)[1]+coef(model2DA)[3]*Training$Acceleration)

# Plot of Displacement vs MPG for model2DA
plot(Training$Displacement,Training$MPG,xlab = 'Displacement', ylab = 'MPG')
lines(Training$Displacement,coef(model2DA)[1]+coef(model2DA)[2]*Training$Displacement)

#Plotting Residuals
model2DA.res=resid(model2DA)

hist(model2DA.res)
plot(Training$Displacement,model2DA.res,ylab = 'Residuals',xlab = 'Displacement')
abline(0,0)

plot(Training$Acceleration,model2DA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
##############################################

#MPG ~ HorsePower+Weight (8)
model2HW<-lm(MPG ~ HorsePower+Weight, data = Training)
summary(model2HW)

# Plot of Horsepower vs MPG for model2HW
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HW)[1]+coef(model2HW)[2]*Training$HorsePower)

# Plot of Weight vs MPG for model2HW
plot(Training$Weight,Training$MPG,xlab = 'Weight', ylab = 'MPG')
lines(Training$Weight,coef(model2HW)[1]+coef(model2HW)[3]*Training$Weight)
##############################################
#MPG ~ HorsePower+Acceleration (9)
model2HA<-lm(MPG ~ HorsePower+Acceleration, data = Training)
summary(model2HA)

# Plot of Horsepower vs MPG for model2HA
plot(Training$HorsePower,Training$MPG,xlab = 'Horsepower', ylab = 'MPG')
lines(Training$HorsePower,coef(model2HA)[1]+coef(model2HA)[2]*Training$HorsePower
      
# Plot of Acceleration vs MPG for model2HA
plot(Training$Acceleration,Training$MPG,xlab = 'Acceleration', ylab = 'MPG')
lines(Training$Acceleration,coef(model2HA)[1]+coef(model2HA)[3]*Training$Acceleration)
      
#Plotting Residuals      
model2HA.res=resid(model2HA)

hist(model2HA.res)
plot(Training$HorsePower,model2HA.res,ylab = 'Residuals',xlab = 'Horsepower')
abline(0,0)

plot(Training$Acceleration,model2HA.res,ylab = 'Residuals',xlab = 'Acceleration')
abline(0,0)
      
      
##########################################################################################################################
##########################################################################################################################
#3 variable
##############################################
#MPG ~ Displacement+HorsePower+Weight (10)
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
      
#MPG ~ Displacement+HorsePower+Acceleration (14)


model3DHA <- lm(MPG ~ Displacement+HorsePower+Acceleration, data = Training)
summary(model5)
coef(model5)
coef(model5)[1]
coef(model5)[2]
coef(model5)[3]
coef(model5)[4]


# Plot for Acceleration vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Acceleration,Training$MPG)
lines(Training$Acceleration,coef(model5)[1]+coef(model5)[2]*Training$Acceleration)


# Plot for HorsePower vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$HorsePower,Training$MPG)
lines(Training$HorsePower,coef(model5)[1]+coef(model5)[3]*Training$HorsePower)


# Plot for DISPLACEMENT vs MPG with coef of model 5(MPG Vs ACCELERATION,HORSEPOWER,DISPLACEMENT)
plot(Training$Displacement,Training$MPG)
lines(Training$Displacement,coef(model5)[1]+coef(model5)[4]*Training$Displacement)

##############################################
#Model for MPG Vs DWA (11)

model3DWA<- lm(MPG ~ Displacement+Weight+Acceleration, data = Training)
summary(model3DWA)


# Plot of Acceleration vs MPG for model3DWA
plot(Training$Acceleration,Training$MPG)
lines(Training$Acceleration,coef(model3DWA)[1]+coef(model3DWA)[4]*Training$Acceleration)


# # Plot of Weight vs MPG for model3DWA
plot(Training$Weight,Training$MPG)
lines(Training$Weight,coef(model3DWA)[1]+coef(model3DWA)[3]*Training$Weight)


# Plot of Displacement vs MPG for model3DWA
plot(Training$Displacement,Training$MPG)
lines(Training$Displacement,coef(model3DWA)[1]+coef(model3DWA)[2]*Training$Displacement)


##############################################
#Model for MPG Vs HWA (12)
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

#4variable Model
############################################## 
#MPG ~ Displacement+ HorsePower + Weight + Acceleration (13)

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

#Plot of A vs MPG for model4
plot(Training$Acceleration,Training$MPG,xlab = "Acceleration",ylab="MPG")
lines(Training$Acceleration,coef(model4)[1]+coef(model4)[5]* Training$Acceleration)



