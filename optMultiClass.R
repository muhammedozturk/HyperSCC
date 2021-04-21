library("rBayesianOptimization")
library("utiml")

###get 100 random instances
set.seed(123)
randomN <- sample(1:1000,100)
dataGroup <- read.csv("processedPosts.csv")
dataGroup <- dataGroup[,-c(1:2)]

###this block gets specific columns of the features
fileM1 <- dataGroup[,1:50]
###(labeling values)change this line according to the number of extracted features
fileM2 <- dataGroup[,10559:10579]
fileM <- data.frame(fileM1,fileM2)
fileM <- fileM[randomN,]


 pre.process <- function (mdata) {
   aux <- remove_skewness_labels(mdata, 2)  # Remove infrequent labels (less than 2)
   aux <- remove_unlabeled_instances(aux)   # Remove instances without labels
   aux <- remove_unique_attributes(aux)     # Remove constant attributes
   return(aux) }

####function to be optimized
Test_Fun <- function(x) {
 fileM <- mldr_from_dataframe(fileM, labelIndices = c(51:71), name = "fileM") 
class(fileM)
fileM <- pre.process(fileM)
 ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
model <- mbr(ds$train, "RF",phi=x)
 predictions <- predict(model, ds$test)
 results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  results <- round(results, 4)
list(Score = results[2],
Pred = 0)
}
###Test_fun ending
## Set larger init_points and n_iter for better optimization result


OPT_Res <- BayesianOptimization(Test_Fun,
bounds = list(x = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)),
init_points = 2, n_iter = 3,
acq = "ucb", kappa = 2.576, eps = 0.0,
verbose = TRUE)
###end bayesian


##########################################
##genetic performance
fileM1 <- dataGroup[,1:50]
fileM2 <- dataGroup[,10559:10579]
fileM <- data.frame(fileM1,fileM2)
fileM <- fileM[randomN,]
 fileM <- mldr_from_dataframe(fileM, labelIndices = c(51:71), name = "fileM") 
class(fileM)
fileM <- pre.process(fileM)
 ds <- create_holdout_partition(fileM, c(train=0.8, test=0.2))
f1 <- function(x){
model <- mbr(ds$train, "RF",phi=x)
 predictions <- predict(model, ds$test)
 results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  results <- round(results, 4)
return(results[2])
}
library("GA")
f <- function(x)  f1(x)
GA <- ga(type = "real-valued", fitness = f,maxiter=10, lower = 0.1, upper = 0.4)
summary(GA)
plot(GA)
GA@solution
##########end genetic################################
##################nelder performance
fileM1 <- dataGroup[,1:50]
fileM2 <- dataGroup[,10559:10579]
fileM <- data.frame(fileM1,fileM2)
fileM <- fileM[randomN,]
 fileM <- mldr_from_dataframe(fileM, labelIndices = c(51:71), name = "fileM") 
class(fileM)
fileM <- pre.process(fileM)
optimize <-function(z){
 ds <- create_holdout_partition(fileM, c(train=0.7, test=0.3))
model <- mbr(ds$train, "RF",phi=z)
 predictions <- predict(model, ds$test)
 results <- multilabel_evaluate(ds$test, predictions, c("example-based", "macro-F1"))
  results <- round(results, 4)
return(results[3])
}
library(nloptr)
# Bounded version of Nelder-Mead
lower <- 0.1
upper <- 0.5
S <- neldermead(0.1, optimize, lower, upper, nl.info = TRUE)
S[1]
################evaluate performance
fileM1 <- dataGroup[,1:50]
fileM2 <- dataGroup[,10559:10579]
fileM <- data.frame(fileM1,fileM2)
fileM <- fileM[1:randomN,]
 fileM <- mldr_from_dataframe(fileM, labelIndices = c(51:71), name = "fileM") 
class(fileM)
fileM <- pre.process(fileM)
measures <- c("hamming-loss", "subset-accuracy", "one-error")
#measures <- c("example-based", "macro-F1")
 results <- cv(fileM, method="mbr", base.algorithm="SVM",cv.folds=3,phi=0.4, cv.results=TRUE,cv.predictions=TRUE, cv.sampling="stratified", cv.measures=measures, cv.seed=123)
 t(results$multilabel)
resultsG <- round(sapply(results$labels, colMeans), 4)
##end nelder-mead################################################
################evaluate GENERALperformance


fileM <- dataGroup
fileM1 <- dataGroup[,1:5]
fileM2 <- dataGroup[,10559:10579]
fileM <- data.frame(fileM1,fileM2)
fileM <- fileM[1:5000,]
 fileM <- mldr_from_dataframe(fileM, labelIndices = c(6:26), name = "fileM") 
class(fileM)
fileM <- pre.process(fileM)
measures <- c("hamming-loss", "subset-accuracy", "one-error")
#measures <- c("example-based", "macro-F1")
 results <- cv(fileM, method="lift", base.algorithm="SVM",ratio=0.4,cv.folds=10, cv.results=TRUE,cv.predictions=TRUE, cv.sampling="stratified", cv.measures=measures)

t(results$multilabel)
resultsG <- round(sapply(results$labels, colMeans), 4)
write.csv(resultsG,"results.csv")



#############################################################


