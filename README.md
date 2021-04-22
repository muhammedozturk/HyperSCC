![hyperscc](https://github.com/muhammedozturk/HyperSCC/blob/main/hyperscc.gif)

# HYPERSCC

## Hyperscc: A hyperparameter optimization framework for multi-class programming language prediction

### Description: Hyperscc is a hyperparameter optimization framework, designed for multi-class code prediction. All the codes were created using R.

### Key Features:

### 1. Feature extraction from stack overflow data sets
### 2. Rule-based manual labeling of stack overflow posts
### 3. One-class programming language prediction using XGBoost and Random Forest
### 4. Multi-class classification of processed posts including 21 programming language labels.

### Files:

### labeling.R and featureExtraction.R were devised to create the file to be exposed to the multi-class prediction. If you are not interested in that process, just use optOnceClass.R and optMultiClass.R to perform one-label and multi-label prediction, respectively.

### Stack overflow public data sets used in the experiment:

### 1. Questions.csv (https://www.kaggle.com/stackoverflow/stacksample)
### 2. Tags.csv (https://www.kaggle.com/stackoverflow/stacksample)

### Note: Questions.csv includes 1264216 posts. It is very difficult to load all the instances to the RAM. Instead you can download question1.csv to make a small experiment.

### Detailed description for public use:

### 1. Download processedPosts.R in your working directory of R GUI to disregard the feature extraction process. Otherwise, you should create the processed file by employing the next two steps. Download the two stack overflow public data sets given above.
### 2. First, the labeling.R file (including 21 functions to label instances) should be executed on the R GUI. Each of these functions returns 0/1 depending on the tagging. However, manual labeling does not only depend on that process but also utilized the rules specified in featureExtraction.R file.
### 3. Run featureExtraction.R file by specifying the number of instances you will analyze by changing "fileCount". At the end of the execution of that file, processedPosts.csv emerges in the working directory of R GUI.
### 4. optOneClass.R is devised to yield mean results of XGBoost and Random forest.
### 5. optMultiClass.R is devised to yield F1 Score and hamming measures of multi-class prediction methods including ensemble of binary relevance (EBR) , random k-labelsets (RAKEL), controlled label correlation exploitation (CTRL), ensemble of classifier chain (ECC), ensemble of single label (ESL), label specific features (LIFT), and meta-br (MBR). optMultiClass.R is exemplified for one type of those methods. Please change the parameters and name of the method to complete the hole experiment. 

### CODE EXPLANATIONS

Following codes can be used to process 1000 instances to predict one-label programming language with PLIin R. 

library("algorithmia")
mydata <- read.csv("question1.csv")

###This code is written for 1000 instances
##question1 data set includes 20000 instances that is the boundary for "i"
i <- 1
while(i<1000){
input <- mydata[i,7]
client <- getAlgorithmiaClient("simZW9mP7NnqvJ4juloA5Tw25be1")
algo <- client$algo("PetiteProgrammer/ProgrammingLanguageIdentification/0.1.3")
algo$setOptions(timeout=300) # optional
result <- algo$pipe(input)$result
print(result)
i <- i+1
}

The function given below is used to detect the number of words in a given string.

countWords <- function(x){
	result <- sapply(strsplit(x, " "), length)
return (result)
}

####y1=>javascript;y2=>sql;y3=>java;y4=>C#;y5=>python;y6=>c++
###y7=>c;y8=>php;y9=>ruby;y10=>swift;y11=>objectivec;y12=>vb.net
###y13=>perl;y14=>bash;y15=>css;y16=>scala;y17=>html;y18=>lua
###y19=>haskell; y20=>markdown;y21=>R

The line given below is in featureExtraction.R. This line checks tag data set along with a rule base.

resultF18 <- if(resultF18>=1 || tagCalculateLua(mydata[i,2])==1) 1 else 0

The pearson correlation is performed with the following lines:
library(caret)
correlationMatrix <- cor(resultF)
wC <- resultF$wordCount
cC <- resultF$charCount
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
resultF = subset(resultF, select = -highlyCorrelated )
resultF <- data.frame(wC,cC,resultF)

##############To remove redundant temporary .csv files in the workin directory###########################################
i=1
while(i<5000){
file.remove(paste0("freq",i, ".csv"))
i <- i+1
}

##Note: To ask further questio, please contact with me using muhammedozturk@sdu.edu.tr
