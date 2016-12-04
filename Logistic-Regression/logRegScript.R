heart<-read.csv('Heart.csv', header=TRUE)
View(heart)
nrow(heart)
heart<-heart[,-1]
class(heart)

#Cleaning Response Variable
heart$AHD<-heart$AHD=="Yes"

levels(heart$ChestPain)
levels(heart$Thal)

#Creating Dummy Variables for Chestpain
heart$chestpainAsymtomatic<-heart$ChestPain=="asymptomatic"
heart$chestpainNonanginal<-heart$ChestPain=="nonanginal"
heart$chestpainNontypical<-heart$ChestPain=="nontypical"
heart$chestpainTypical<-heart$ChestPain=="typical"
heart<-heart[,-3]

#Creating dummy variables for Thal
heart$thalFixed<-heart$Thal=="fixed"
heart$thalNormal<-heart$Thal=="normal"
heart$thalReversable<-heart$Thal=="reversable"
heart<-heart[,-12] #removing Thal


View(heart)


train<-heart[1:250,]
test<-heart[251:nrow(heart),]
View(train)
View(test)

#Traning data set
model4<-glm(AHD~.,family=binomial,data=train)
summary(model4)


