######################
#Importing Data
######################
data<-read.csv("wdbc.data",header = FALSE)
names(data)<-c('id',"diagnosis",
               "radiusM","textureM","perimeterM","areaM","smoothnessM","compactnessM","concavityM","concavePointsM","symmetryM","fractalDimensionM",
               "radiusSE","textureSE","perimeterSE","areaSE","smoothnessSE","compactnessSE","concavitySE","concavePointsSE","symmetrySE","fractalDimensionSE",
               "radiusW","textureW","perimeterW","areaW","smoothnessW","compactnessW","concavityW","concavePointsW","symmetryW","fractalDimensionW")

# Encoding categorical data
data$diagnosis = factor(data$diagnosis,
                    levels = c('M', 'B'),
                    labels = c(1, 0))
id<-data[,1]
data<-data[,-1]        #taking the patient ids aways for modelling purpose
header<-names(data)
dim(data)

######################
#Training Data
######################
set.seed(101)

#Assisning training data set size as 75% of total data
training_size<-round(dim(data)[1]*.75)
training_size

#Creating a random index sample 
trainingIndex<-sample(1:dim(data)[1], size = training_size,replace = FALSE)

#Assigning the training set
training<-data[trainingIndex,]
dim(training)

#separating nonfactors before scaling
trainingDiagnosis<-training[,1]
trainingFactors<-data.matrix(training[,2:31], rownames.force = NA)


#preProcess
#install.packages('caret')
library(caret)
prep<-preProcess(trainingFactors,method=c('center','scale'))
summary(prep)
trainingFactors<-predict(prep,trainingFactors)
training<-cbind.data.frame(trainingFactors,trainingDiagnosis)
View(training)
dim(training)

######################
#Test Data
######################
test<-data[-trainingIndex,]
View(test)
dim(test)
sapply(test,class)
testDiagnosis<-test[,1]
testFactors<-data.matrix(test[,2:31], rownames.force = NA)
class(testFactors)

test<-predict(prep,testFactors)

test<-data.frame(test)
dim(test)
View(test)






####################################
# Linear Discriminant Analysis LDA
####################################

#install.packages('MASS')
library(MASS)
classifier.LDA=  lda(trainingDiagnosis ~ ., data=training)
classifier.LDA

#Predicting on training
classifier.LDA.values<-predict(classifier.LDA)

#predicting the Test set results
testPred.LDA=predict(classifierLDA, newdata = test)$class
testPred.LDA

#confusion matrix
cm.LDA = table(testDiagnosis, testPred.LDA)
cm.LDA
accuracy.LDA = (cm.LDA[1,1] + cm.LDA[2,2]) / sum(cm.LDA)
accuracy.LDA #0.9295775


#######################################################
#LOGISTIC REGRESSION
#install.packages('e1071')
#fit Logistic Regression to the Training set
#library('e1071')
classifier.GLM= glm(formula = trainingDiagnosis ~ .,
                     family = binomial(link='logit'),
                     data = training)
summary(classifier.GLM)
classifier.GLM
#predicting the Test set results
testPred.GLM=predict(classifier.GLM, type = 'response', newdata = test)
class(testPred.GLM)
testPred.GLM<-testPred.GLM>=0.5
testPred.GLM

#confusion matrix
cm.GLM = table(testDiagnosis, testPred.GLM)
cm.GLM
accuracy.GLM = (cm.GLM[1,1] + cm.GLM[2,2]) / sum(cm.GLM)
accuracy.GLM #0.9507042


############################################################
#K-NN
#fit K-NN to the Training set and predict Test set results
#install.packages('class')
#create your classifier
library(class)


classifier.knn.list<-vector()
accuracy.knn<-vector()
for(i in seq(1,100, by=1)){
  cls<-paste("classifier.knn.",i,sep="");
  classifier.knn.list<-c(classifier.knn.list, cls)
  
  acc<-paste("accuracy.knn.",i,sep="");
  accuracy.knn.List<-c(accuracy.knn.List, acc)
  
  classifier.knn<-knn(trainingFactors,test,trainingDiagnosis,k=i)
  accuracy.knn<-c(accuracy.knn, sum(classifier.knn==testDiagnosis)/length(testDiagnosis))
  
  assign(cls, classifier.knn)
  
}
plot(seq(1,100, by=1),xlab="Nearest Neighbors",ylab="Accuracy",accuracy.knn,type='l',main="Accuracy Plot for k-NN")
max(accuracy.knn)    # 0.971831
classifier.knn.list[max(accuracy.knn)==accuracy.knn]

#########################################################################

# Fitting SVM to the Training set
#install.packages('e1071')
#library(e1071)

#SVM kernel= 'linear'
#########################
classifier.SVM.linear = svm(formula = trainingDiagnosis ~ .,
                            data = training,
                            type = 'C-classification',
                            kernel = 'linear')

# Predicting the Test set results
testPred.SVM.linear = predict(classifier.SVM.linear, newdata = test)
# Making the Confusion Matrix
cm.SVM.linear = table(testDiagnosis, testPred.SVM.linear)
cm.SVM.linear #accuracy = 135/142 = 0.9507
accuracy.SVM.linear = (cm.SVM.linear[1,1] + cm.SVM.linear[2,2]) / sum(cm.SVM.linear)
accuracy.SVM.linear


#SVM kernel= 'rbf'
#########################
classifier.SVM.rbf = svm(formula = trainingDiagnosis ~ .,
                            data = training,
                            type = 'C-classification',
                            kernel = 'radial')

# Predicting the Test set results
testPred.SVM.rbf = predict(classifier.SVM.rbf, newdata = test)
# Making the Confusion Matrix
cm.SVM.rbf = table(testDiagnosis, testPred.SVM.rbf)
cm.SVM.rbf #accuracy = 135/142 = 0.9507
accuracy.SVM.rbf = (cm.SVM.rbf[1,1] + cm.SVM.rbf[2,2]) / sum(cm.SVM.rbf)
accuracy.SVM.rbf


#SVM kernel= 'polynomial'
#########################
classifier.SVM.polynomial = svm(formula = trainingDiagnosis ~ .,
                         data = training,
                         type = 'C-classification',
                         kernel = 'polynomial')

# Predicting the Test set results
testPred.SVM.polynomial= predict(classifier.SVM.polynomial, newdata = test)
# Making the Confusion Matrix
cm.SVM.polynomial = table(testDiagnosis, testPred.SVM.polynomial)
cm.SVM.polynomial #accuracy = 135/142 = 0.9507
accuracy.SVM.polynomial = (cm.SVM.polynomial[1,1] + cm.SVM.polynomial[2,2]) / sum(cm.SVM.polynomial)
accuracy.SVM.polynomial#0.8521127



#SVM kernel= 'sigmoid'
#########################
classifier.SVM.sigmoid = svm(formula = trainingDiagnosis ~ .,
                                data = training,
                                type = 'C-classification',
                                kernel = 'sigmoid')

# Predicting the Test set results
testPred.SVM.sigmoid= predict(classifier.SVM.sigmoid, newdata = test)
# Making the Confusion Matrix
cm.SVM.sigmoid = table(testDiagnosis, testPred.SVM.sigmoid)
cm.SVM.sigmoid #accuracy = 135/142 = 0.9507
accuracy.SVM.sigmoid = (cm.SVM.sigmoid[1,1] + cm.SVM.sigmoid[2,2]) / sum(cm.SVM.sigmoid)
accuracy.SVM.sigmoid#0.9577465
