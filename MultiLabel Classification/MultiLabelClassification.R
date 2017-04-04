library(mlr)
library(rpart)
library(caret)

show('File name is : MultiLabelClassification.R - Choose this File from Stored Folder Path')
filename = "MultiLabelClassification.R"
filepath=1
filepath = file.choose()
if(filepath!= 1){
  dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
  dir
  setwd(dir)
 
  
  rawData = read.csv("MultLabelTrainData.csv",header=FALSE)
  labels = read.csv("MultLabelTrainLabel.csv",header=FALSE)
  testData = read.csv("MultLabelTestData.csv",header=FALSE)
  
  # combining Columns
  combinedData=cbind(labels,rawData)
  
  
  #Changing the Label names for Labels
  colnames(combinedData)[1:14]=paste("label",seq(1,ncol(labels)),sep="")
  
  
  #Convert the 1,0 to TRUE FALSE
  combinedData[,1:14]=as.data.frame(lapply(combinedData[,1:14],as.logical))
  
  #Labels names
  labelnames=paste("label",seq(1,ncol(labels)),sep="")
  
  combinedData.task = makeMultilabelTask(id = "multi", data = combinedData, target = labelnames)
  
  
  # Learning is done
  parList=list()
  perfList=c()
  
  maxdepthGrid=c(2)
  cpGrid=c(0.09)
  minSplitValues=(10)
  gridDF = expand.grid(maxdepthGrid,cpGrid,minSplitValues)
  
  
  #create k folds
  crossvalGenerateGroups <- function
  
  (
    n, ##<< number of points
    numGroups, ##<< number of groups
    shuffle=TRUE ##<< whether to shuffle the indices before partitioning
  )
  {
    if (numGroups < 2) {
      stop("numGroups should be greater than or equal to 2")
    }
    if (numGroups > n) {
      stop("numGroups should be less than or equal to the number of observations")
    }
    o = if (shuffle) sample(1:n) else 1:n
    
    if (numGroups == n) {
      groups <- as.list(o)
      return(groups)
    }
    
    q = n %/% numGroups   # all groups are size q or (q+1)
    r = n %% numGroups    # r of the groups need to be of size (q+1)
    
    groups = vector("list", numGroups)
    start = 1
    for (group in 1:numGroups) {
      span =  q + (if (group <= r) 1 else 0)
      end = start + span - 1
      groups[[group]] = o[start:end]
      start = end + 1
    }
    return(groups)
    
  }
  k_folds= 5
  flds = crossvalGenerateGroups(nrow(combinedData),k_folds)
  
  
  
  fold_preds_perf = list()
  
  
  for(i in 1 :nrow(gridDF))
  {
    
    # Parameter tuning: cp,minsplit,maxdepth
    parList[["cp"]]=gridDF$Var2[i]
    parList[["maxdepth"]]=gridDF$Var1[i]
    parList[["minsplit"]]=gridDF$Var3[i]
    
    
    lrn.br = makeLearner("classif.rpart", predict.type = "prob",par.vals =parList)
    lrn.br = makeMultilabelBinaryRelevanceWrapper(lrn.br)
    lrn.br
    
    
    
    
    # Problem Transformation Method is done
    for(j in 1:k_folds){
      
      
      mod = mlr::train(lrn.br, combinedData.task)
      mod = mlr::train(lrn.br, combinedData.task, subset = unlist(flds[-j]))
      mod
      
      
      
      # Training is Done
      
      pred = predict(mod, task = combinedData.task, subset =unlist(flds[j]) )
      #pred = predict(mod, newdata = combinedData[400:500,])
      names(as.data.frame(pred))
      
      
      
      # Performance
      #show(performance(pred))
      perfList[j] = performance(pred)
      
      
    }                                
    
    
    
    fold_preds_perf[[i]] = mean(perfList)
    
    
    
  }
  
  #show minimum error and for which parameter
  minError = which(unlist(fold_preds_perf)==min(unlist(fold_preds_perf)))
  bestParams = gridDF[minError,]
  #show(bestParams)
  
  
  #predictions for test data set
  
  parList[["cp"]]=bestParams$Var2[1]
  parList[["maxdepth"]]=bestParams$Var1[1]
  parList[["minsplit"]]=bestParams$Var3[1]
  
  
  test.lrn.br = makeLearner("classif.rpart", predict.type = "prob",par.vals =parList)
  test.lrn.br = makeMultilabelBinaryRelevanceWrapper(test.lrn.br)
  test.lrn.br
  
  mod1 = mlr::train(test.lrn.br, combinedData.task)
  mod1
  
  
  testDataPred = predict(mod1, newdata = testData )
  response=testDataPred$data[,15:ncol(testDataPred$data)]
  
  response[response==TRUE]=1
  response[response==FALSE]=0
  
  View(response)
  write.table(response,"SrinivasanMultClassification.txt",row.names = FALSE, col.names = FALSE, sep = "\t")
  show('Output is stored in SrinivasanMultClassification.txt in the following path ')
  show(dir)
  
}else
  {
    show('Please choose the File mentioned : MultiLabelClassification.R')
  }


