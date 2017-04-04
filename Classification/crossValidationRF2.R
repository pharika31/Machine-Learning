library(gdata)
library(e1071)
library(randomForest)
library(mlbench)
library(caret)
library(Boruta)
library(psych)
library(limma)
library(graph)
library(RBGL)
library(PerfMeas)
library(varSelRF)

show('File name is : crossValidationRF2.R - Choose this File from Stored Folder Path')
filename = "crossValidationRF2.R"
filepath=1
filepath = file.choose()
if(filepath!= 1){
  dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
  dir
  setwd(dir)





rawData=read.csv("TrainingData2.csv",header=FALSE)
rawDataLabel=read.csv("TrainingLabel2.csv",header=FALSE)[,1]




#pre-processing : removing columns with 0 Standard Deviation
preProcRawData = preProcess(rawData, method=c("zv","nzv","range"))
rawData = predict(preProcRawData,rawData)


boruta.train <- Boruta(x=rawData, y=as.factor(rawDataLabel), doTrace = 2)

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))


final.boruta <- TentativeRoughFix(boruta.train)


rawData <- rawData[,getSelectedAttributes(final.boruta, withTentative = F)]



#1st try gridDF = expand.grid(.mtry=c(2^seq(1,12)),ntree=c(100,500,1000))
#2nd try  512,600,650,700,800,900,1024
gridDF = expand.grid(.mtry=c(18,9),ntree=c(1100,1050))



#function for RF with cross-validation
crossVal = function(rawData, rawDataLabel,bkp_rawData=rawData){
  
  
  completeSet =sample(seq(1,nrow(rawData)), nrow(rawData),replace=FALSE)
  
  #create k folds
  k_folds= 3
  flds = createFolds(completeSet, k_folds, list = TRUE, returnTrain = FALSE)
  
  model_list=list()
  fold_preds=list()
  selected_vars=list()
  cv_auc = c()
  sd_auc = c()
  count=1
  #rawDataLabel=as.factor(rawDataLabel[,1])
  uniqueLabel = as.numeric(unique(rawDataLabel))
  for(i in 1:nrow(gridDF)){
   
    
    error_var=c()    
    for(j in 1:k_folds){   
   
      
      
      fold_model = randomForest(x=rawData[unlist(flds[-j]),] ,y=as.factor(rawDataLabel[unlist(flds[-j])]), mtry=gridDF$.mtry[i],ntree=gridDF$ntree[i],importance=TRUE)
      
      fold_preds = as.numeric(predict(fold_model,rawData[unlist(flds[j]),]))
      true_values = as.numeric(rawDataLabel[unlist(flds[j])])
      
      
      predicted_matrix=dummy.code(fold_preds)
      if(length(setdiff(uniqueLabel, unique(fold_preds))) > 0){
        missingLabels = setdiff(uniqueLabel, unique(fold_preds))
        insertCols = matrix(0,nrow(predicted_matrix),length(missingLabels))    
        colnames(insertCols) = missingLabels
        predicted_matrix = cbind(predicted_matrix,insertCols)
      }
      predicted_matrix = predicted_matrix[,as.character(sort(uniqueLabel))]
      true_matrix=dummy.code(true_values)
      #handle condition - if any class is not present in predictions or true values, we are including that column with 0s
      
      if(length(setdiff(uniqueLabel, unique(true_values))) > 0){
        missingLabels = setdiff(uniqueLabel, unique(true_values))
        insertCols = matrix(0,nrow(true_matrix),length(missingLabels))    
        colnames(insertCols) = missingLabels
        true_matrix = cbind(true_matrix,insertCols)
      }
      #error_var = c(error_var,AUC.single(fold_preds1, true_values))
      error_var = c(error_var,AUC.single.over.classes(true_matrix, predicted_matrix)$average)
      
    }
    cv_auc[i] = mean(error_var) 
    sd_auc[i] = sd(error_var) 
  }
  return (cv_auc)
}#end of function

#performing repeat CV
cv_auc_repeat=matrix(0,1,nrow(gridDF))

for (m in 1:100){
  show(paste("m = " , m) )
  
  cv_auc_repeat=rbind(cv_auc_repeat,crossVal(rawData,rawDataLabel,bkp_rawData=rawData))
}
cv_auc_repeat=cv_auc_repeat[-1,]
param_metrics=colMeans(cv_auc_repeat)


best_params=gridDF[which.max(param_metrics),]
show(best_params)
max(param_metrics)

# //predictions for test data
best_model = randomForest(x=rawData ,y=as.factor(rawDataLabel), mtry=best_params$.mtry,ntree=best_params$ntree,importance=TRUE)

testDataRaw = read.csv("TestData2.csv",header = FALSE)

#pre-processing : removing columns with 0 Standard Deviation
testDataPP = predict(preProcRawData,testDataRaw,type="votes")
testDataFS=testDataPP[,getSelectedAttributes(final.boruta, withTentative = F)]
testPreds = as.numeric(predict(best_model,testDataFS))
write.table(testPreds, file="PunyamurthulaClassificationCV2.txt",row.names = FALSE,col.names = FALSE)

show("Predictions file created. Please check file PunyamurthulaClassificationCV2.txt")

show(dir)

}else{
  show('Please choose the File mentioned : crossValidationRF2.R')
}
