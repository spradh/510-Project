#Loading data
#Note: please save r script in the same folder as minst.csv
mnist_data<-read.csv('mnist.csv')
#View(mnist_data)
dim(mnist_data)

#Creating PCA model
pca_model<-princomp(mnist_data[,-1])
options(max.print=1000000)
summary(pca_model)
#taking 154 component because it covers 95% of the variance
#View(pca_model$score[,1:154])


a<-matrix(0,ncol=10,nrow =dim(mnist_data)[1])
for(i in 1:dim(mnist_data)[1]){
  if(mnist_data[i,"X5"]==0){
    a[i,10]<-1
  }else{
    a[i,mnist_data[i,"X5"]]<-1
  }
}
colnames(a)<-c("one","two","three","four","five","six","seven","eight","nine","zero")
head(a)

input<-cbind(a[1:1000,],pca_model$score[,1:154][1:1000,])
View(input)


target<-mnist_data[,1]

train_index<-sample(1:1000,750, replace = FALSE, prob = NULL)

train<-input[train_index,]
dim(train)
test<-input[-train_index,]
dim(test)

View(train)
View(test)


cols<-colnames(train)
dep_var<-paste(cols[1:10],collapse = "+")
ind_var<-paste(cols[11:164],collapse = "+")
form<-as.formula(paste(dep_var,"~",ind_var))
library(neuralnet)
model_list<-vector()

#65%to75%
for(i in 100:120){
  a<-paste("Hidden Nodes: ",i)
  cat("\nModel",i,"\n")
  nn_model<-neuralnet(form,train,hidden =i,linear.output = FALSE, stepmax = 1e+06)
  cat("Error: ",nn_model$result.matrix[1],"\n")
  cat("======================================\n")
  assign(a,nn_model)
  model_list<-c(model_list, a)
}
errors<-vector()
for(i in 1:21){
  cat("\nModel",(i+109),"\n")
  pred<-compute(get(model_list[i]), test[,-(1:10)])
  e<-sum((pred$net.result-test[,1:10])^2)
  errors<-c(errors, e)
  print(e)
}

plot(100:120, errors, ty="l",xlab = "Nodes in Hidden Layer",ylab ="Error",main="Error on Test Set")

n<-100:120
n[min(errors)==errors] #112
