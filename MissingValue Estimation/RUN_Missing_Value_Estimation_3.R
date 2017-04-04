


library(softImpute)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

show('File name is : RUN_Missing_Value_Estimation_3.R - Choose this File from Stored Folder Path')
filename = "RUN_Missing_Value_Estimation_3.R"
filepath=1
filepath = file.choose()
if(filepath!= 1){
  dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
  dir
  setwd(dir)

rawData = read.csv("MissingData3.csv",header=FALSE)
rawData[rawData==1.000e+99]<-NA
actualDataMadeMissing=data.matrix(rawData)

rawDataM = data.matrix(rawData)
notNA=which(!is.na(rawDataM))
inputWithMissing = sample(notNA,0.04*length(notNA),replace=FALSE)
rawDataM[inputWithMissing]<-NA 

min(dim(actualDataMadeMissing))-1

gridDF = expand.grid(lam=c(3),rank=c(10))

errorList = c()
for(i in 1:nrow(gridDF)){
  
  softImputedValues = softImpute(rawDataM,lambda=gridDF$lam[i],rank.max = gridDF$rank[i])
  
  umat=((softImputedValues["u"][[1]]))
  dmat =((softImputedValues["d"][[1]]))
  vmat = ((softImputedValues["v"][[1]]))
  
  diagD=matrix(0,ncol(umat),ncol(umat))
  diag(diagD) = c(dmat)
  
  imputedFinal = umat %*% diagD %*% t(vmat)
  
  missingImputedFinal=rawDataM
  missingImputedFinal[ which(is.na(rawDataM))]= imputedFinal[ which(is.na(rawDataM))]
  
  result= missingImputedFinal
  
  actual = actualDataMadeMissing[inputWithMissing]
  predicted = result[inputWithMissing]
  error <- actual - predicted
  errorList[i]=rmse(error)
  
  
}

best_param_in = (which(errorList==min(errorList)))


softImputedValues = softImpute(actualDataMadeMissing,lambda=gridDF$lam[best_param_in],rank.max = gridDF$lam[best_param_in])
umat=((softImputedValues["u"][[1]]))
dmat =((softImputedValues["d"][[1]]))
vmat = ((softImputedValues["v"][[1]]))

diagD=matrix(0,ncol(umat),ncol(umat))
diag(diagD) = c(dmat)

imputedFinal = umat %*% diagD %*% t(vmat)
missingImputedFinal = actualDataMadeMissing
missingImputedFinal[ which(is.na(actualDataMadeMissing))]= imputedFinal[which(is.na(actualDataMadeMissing))]






write.table(missingImputedFinal, file="PunyamurthulaMissingResult3.txt",row.names = FALSE,col.names = FALSE,sep="\t")

show("Predictions file created. Please check file PunyamurthulaMissingResult3.txt in the following Path")

show(dir)

}else{
  show('Please choose the File mentioned : RUN_Missing_Value_Estimation_3.R')
}
