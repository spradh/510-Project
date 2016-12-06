heart<-read.csv('Heart.csv', header=TRUE)
View(heart)
nrow(heart)
heart<-heart[,-1]

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
heart$thanReversable<-heart$Thal=="reversable"
heart<-heart[,-12] #removing Thal

#Creating dummy variables for CA
heart$ca1<-heart$Ca==0
heart$ca1<-heart$Ca==1
heart$ca2<-heart$Ca==2
heart$ca3<-heart$Ca==3
heart<-heart[,-11]

#Creating dummy variables for slope
heart$slope1<-heart$Slope==1
heart$slope2<-heart$Slope==2
heart$slope3<-heart$Slope==3
heart<-heart[,-10]

View(heart)


train<-heart[1:250,]
test<-heart[251:nrow(heart),]
View(train)
View(test)
