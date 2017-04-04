#Classification Packages
install.packages('gdata',dependencies = TRUE)
install.packages('e1071',dependencies = TRUE)
install.packages('randomForest',dependencies = TRUE)
install.packages('mlbench',dependencies = TRUE)
install.packages('caret',dependencies = TRUE)
install.packages('Boruta',dependencies = TRUE)
install.packages('psych',dependencies=TRUE)

source("https://bioconductor.org/biocLite.R") 
biocLite("limma")
source("https://bioconductor.org/biocLite.R") 
biocLite("graph") 
source("https://bioconductor.org/biocLite.R") 
biocLite("RBGL")


install.packages('PerfMeas',dependencies=TRUE)
install.packages('varSelRF',dependencies=TRUE)



#Missing Value Estimation
install.packages("softImpute",dependencies = TRUE)

#Multilabel Classification Packages
install.packages('mlr',dependencies = TRUE)
install.packages('rpart',dependencies=TRUE)