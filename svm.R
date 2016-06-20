## import data and feature creation
Train.746Data <- read.csv("~/Documents/Spring'16/ECEN_689/newHIV-1_data_ECEN689/Train-746Data.txt", header=FALSE)
Train.746Data$V2=as.factor(Train.746Data$V2)
amino<-list("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
# function for feature creation
feat_creat<-function(data,amino){
feat<-matrix(0,nrow = nrow(data),ncol=160)
  for (i in 1:nrow(data)){
  a<-unlist(strsplit(as.character(data[i,1]),""))
  for (j in 1:length(a)){
    for (k in 1:20){
      if (a[j]==amino[k]){
        feat[i,(j-1)*20+k]=1
      }
    }
  }
  }
return(feat)
}
feat<-feat_creat(Train.746Data,amino)
## building svm models
require(e1071)
model<-svm(x=feat,y=Train.746Data$V2,type="nu-classification",kernel="sigmoid",cross=3,probability=T)
model1<-svm(x=feat,y=Train.746Data$V2,type="nu-classification",kernel="linear",cross=3,probability=T)
summary(model)
summary(model1)
#rbf kernel 
install.packages("caret")
install.packages("kernlab")
library(caret)
library(kernlab)
ctrl<-trainControl(method = "cv",number = 3,summaryFunction = twoClassSummary,classProbs = T)
try<-Train.746Data$V2
levels(try)<-make.names(levels(factor(try)))
rbf_svm<-train(x=feat,y=try,method = "svmRadial",tuneLength = 10,preProcess = c("center","scale")
               ,metric = "ROC",trControl = ctrl)
rbf_svm
grid<-expand.grid(sigma=c(0.001,0.004,0.008),C=c(7,8,9))
rbf_tune<-train(x=feat,y=try,method = "svmRadial",preProcess = c("center","scale"),metric = "ROC",
                tuneGrid = grid, trControl = ctrl)
rbf_tune

##importing test data and getting predictions
Test.impensData <- read.csv("~/Documents/Spring'16/ECEN_689/newHIV-1_data_ECEN689/Test-impensData.txt", header=FALSE)
Test.impensData$V2=as.factor(Test.impensData$V2)
test_feat<-feat_creat(Test.impensData,amino)

pred_test<-predict(model,test_feat,probability = T)
test_prob<-attr(pred_test,"probabilities")

pred_test_lin<-predict(model1,test_feat,probability = T)
test_prob_lin<-attr(pred_test_lin,"probabilities")

test_resp<-Test.impensData$V2
levels(test_resp)<-make.names(levels(factor(test_resp)))
test_pred<-predict(rbf_tune$finalModel,test_feat,type="prob")

## plotting roc curves
require(pROC)
b<-roc(response=Test.impensData$V2,predictor =test_prob[1:947],plot=T,col="red",auc = T,print.auc=T)
c<-roc(response=Test.impensData$V2,predictor =test_prob_lin[1:947],plot=T,auc = T,add=T,col="blue",print.auc=T,print.auc.y=40) 
d<-roc(response=test_resp,predictor =test_pred[,1],plot=T,auc = T,print.auc=T,col="green") 
legend("bottomright",legend = c("Linear","Sigmoid","RBF"),col=c("blue","red","green"),lwd=2)

