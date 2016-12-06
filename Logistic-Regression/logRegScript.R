#Importing CSV data set
heart<-read.csv('Heart.csv', header=TRUE)

#omitting incomplete rows in dataset 
heart<-na.omit(heart[,-1])

summary(heart)

#Cleaning Response Variable
heart$AHD<-heart$AHD=="Yes"

#Checking levels of categorical variables
levels(heart$Slope)
levels(heart$ChestPain)
levels(heart$Thal)

#Creating Dummy Variables for Chestpain
#ChestPain Typical is Control group
heart$chestpainAsymtomatic<-heart$ChestPain=="asymptomatic"
heart$chestpainNonanginal<-heart$ChestPain=="nonanginal"
heart$chestpainNontypical<-heart$ChestPain=="nontypical"
heart<-heart[,-3]

#Creating dummy variables for Thal
#Thal=fixed is control group
heart$thalNormal<-heart$Thal=="normal"
heart$thanReversable<-heart$Thal=="reversable"
heart<-heart[,-12] #removing Thal

#Creating dummy variables for CA
#CA=0 is control group
heart$ca1<-heart$Ca==1
heart$ca2<-heart$Ca==2
heart$ca3<-heart$Ca==3
heart<-heart[,-11]

#Creating dummy variables for slope
#Slope=1 is control group
heart$slope2<-heart$Slope==2
heart$slope3<-heart$Slope==3
heart<-heart[,-10]

View(heart)

#Checking coorelations
cor(heart,method="pearson")

#Creating Training and Test data sets.
train<-heart[1:250,]
test<-heart[251:nrow(heart),]
View(train)
View(test)


#Traning Model with all variables included
model<-glm(AHD~.,family=binomial,data=train)
summary(model)

#Removing slope3
model<-glm(AHD~Age+Sex+RestBP+Chol+Fbs+RestECG+MaxHR+ExAng+Oldpeak+
             chestpainAsymtomatic+chestpainNonanginal+chestpainNontypical+
             thalNormal+thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing Age
model<-glm(AHD~Sex+RestBP+Chol+Fbs+RestECG+MaxHR+ExAng+Oldpeak+
             chestpainAsymtomatic+chestpainNonanginal+chestpainNontypical+
             thalNormal+thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing ThalNormal
model<-glm(AHD~Sex+RestBP+Chol+Fbs+RestECG+MaxHR+ExAng+Oldpeak+
             chestpainAsymtomatic+chestpainNonanginal+chestpainNontypical+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing chestPainNonAngial
model<-glm(AHD~Sex+RestBP+Chol+Fbs+RestECG+MaxHR+ExAng+Oldpeak+
             chestpainAsymtomatic+chestpainNontypical+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing chestpainNontypical
model<-glm(AHD~Sex+RestBP+Chol+Fbs+RestECG+MaxHR+ExAng+Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing ExAng
model<-glm(AHD~Sex+RestBP+Chol+Fbs+RestECG+MaxHR+Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing Chol
model<-glm(AHD~Sex+RestBP+Fbs+RestECG+MaxHR+Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing MaxHR
model<-glm(AHD~Sex+RestBP+Fbs+RestECG+Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing Fbs
model<-glm(AHD~Sex+RestBP+RestECG+Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing RestECG
model<-glm(AHD~Sex+RestBP+Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing Sex
model<-glm(AHD~Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3+slope2,family=binomial,data=train)
summary(model)

#Removing slope2
model<-glm(AHD~Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca2+ca3,family=binomial,data=train)
summary(model)

#Removing ca2
model<-glm(AHD~Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1+ca3,family=binomial,data=train)
summary(model)

#Removing ca3
model<-glm(AHD~Oldpeak+
             chestpainAsymtomatic+
             thanReversable+ca1,family=binomial,data=train)
summary(model)


