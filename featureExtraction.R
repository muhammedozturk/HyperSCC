###This code generates feature matrix from stack overflow posts
###Manual labeling was established by utilizing Tags.csv and specific rules.

library(rlist)
library("base")
library("tm")
library("utiml")
mydata <- read.csv("question1.csv")
tagData<- read.csv("Tags.csv") 
###TITLE===>mydata[7]
###TEXT====>mydata[8]




##The function calculating the number of wors in a sentence
countWords <- function(x){
	result <- sapply(strsplit(x, " "), length)
return (result)
}


fileCount <- 5001
k=1

##########################
##labeling.R call
###########################
############################################
while(k<fileCount){
###title
yeni<-unlist(mydata[8], use.names=FALSE)
##text
yeni2 <-unlist(mydata[7], use.names=FALSE)
##bu ayarlama alti adet dokuman cikarir tdm de
yeni <- as.character(yeni[k])
yeni2 <- as.character(yeni2[k])
###word and char count
wordCount <- countWords(yeni)
wordCount <- wordCount+countWords(yeni2)
vektor <-c(wordCount)
names(vektor) <- "wordCount"
charCount <- nchar(yeni)
charCount <- charCount+nchar(yeni2)
vektor2 <- c(charCount)
###############word and charcount end
corpus <- c(yeni,yeni2)
tm_corpus <- Corpus(VectorSource(corpus))
tm_corpus <- tm_map(tm_corpus, tolower)
#mystopwords <- c(stopwords('english'), "never", "<strong>")
#tm_corpus <- tm_map(tm_corpus, removeWords, mystopwords)
tm_corpus <- tm_map(tm_corpus, removePunctuation)
#tm_corpus <- tm_map(tm_corpus, toSpace, "<") 
tm_corpus <- tm_map(tm_corpus, removeNumbers)
tm_corpus <- tm_map(tm_corpus, stripWhitespace)
tdm <- TermDocumentMatrix(tm_corpus)
x<-as.matrix(tdm)

##frequency of the words
freq <- rowSums(x)
write.csv(freq,"freq.csv")

###to get each unique word as a new feature,
##convert row to column
 jk <- read.csv("freq.csv")
jk2 <- data.frame(t(jk[-1]),vektor,vektor2)
index <- length(jk2)
index2 <- index-1
colnames(jk2) <- jk[, 1]
#write.csv(jk2,"freq.csv")
names(jk2)[index] <- "charCount"
names(jk2)[index2] <- "wordCount"
 write.csv(jk2, file = paste0("freq",k, ".csv"))
k<- k+1
}

##########merge .csv feature files
library(plyr)
i=1
dataList <- list()
while(i<fileCount){
mydata2 <- read.csv(paste0("freq",i, ".csv"))
dataList=list.append(dataList,mydata2)
i
i<- i+1
}
resultF <- do.call('rbind.fill',dataList)
resultF[is.na(resultF)] = 0
###data frame e listeyi donusturuyoruz
resultF <- as.data.frame(resultF)
##birinci sutunda x ler var bunlari siliyoruz
resultF <- resultF[,-1]
write.csv(resultF,"resultF.csv")
resultF
########The end of feature merging###
###########rule-based labeling###########
####03.02.2020
####y1=>javascript;y2=>sql;y3=>java;y4=>C#;y5=>python;y6=>c++
###y7=>c;y8=>php;y9=>ruby;y10=>swift;y11=>objectivec;y12=>vb.net
###y13=>perl;y14=>bash;y15=>css;y16=>scala;y17=>html;y18=>lua
###y19=>haskell; y20=>markdown;y21=>R
columns <- colnames(resultF)
col <- length(resultF[1,])
row <- length(resultF[,1])

i=1
j=1
arrayCount=0
domCount=0
javascriptCount=0
jqueryCount=0
groupCount=0
orderCount=0
selectCount=0
sqlCount=0
javaCount=0
jarCount=0
netCount=0
aspCount=0
djangoCount=0
pyCount=0
pythonCount=0
problemCount=0
instanceCount=0
caseCount=0
compilerCount=0
cppCount=0
stdCount=0
addressCount=0
charCount=0
gccCount=0
mysqlCount=0
phpCount=0
activerecordCount=0
gemCount=0
hashCount=0
blockCount=0
betaCount=0
appleCount=0
iosCount=0
cocoaCount=0
datumCount=0
delegateCount=0
formCount=0
cgiCount=0
cpanCount=0
awkCount=0
bashCount=0
binCount=0
alignCount=0
backgroundCount=0
bodyCount=0
actorCount=0
applicationCount=0
cabalCount=0
dataframeCount=0
datasetCount=0
factorCount=0
attemptCount=0
buttonCount=0
classCount=0
codeCount=0
y1 <- c()
y2 <- c()
y3 <- c()
y4 <- c()
y5 <- c()
y6 <- c()
y7 <- c()
y8 <- c()
y9 <- c()
y10 <- c()
y11 <- c()
y12 <- c()
y13 <- c()
y14 <- c()
y15 <- c()
y16 <- c()
y17 <- c()
y18 <- c()
y19 <- c()
y20 <- c()
y21 <- c()
while(i<=row)
{

	while(j<=col)
	{
		if(columns[j]=="array")
		arrayCount <- arrayCount+resultF[i,j]
		if(columns[j]=="dom")
		domCount <- domCount+resultF[i,j]
		if(columns[j]=="javascript")
		javascriptCount <- javascriptCount+resultF[i,j]
		if(columns[j]=="jquery")
		jqueryCount <- jqueryCount+resultF[i,j]
		if(columns[j]=="group")
		groupCount <- groupCount+resultF[i,j]
		if(columns[j]=="order")
		orderCount <- orderCount+resultF[i,j]
		if(columns[j]=="select")
		selectCount <- selectCount+resultF[i,j]
		if(columns[j]=="sql")
		sqlCount <- sqlCount+resultF[i,j]
		if(columns[j]=="java")
		javaCount <- javaCount+resultF[i,j]
		if(columns[j]=="jar")
		jarCount <- jarCount+resultF[i,j]
		if(columns[j]=="net")
		netCount <- netCount+resultF[i,j]
		if(columns[j]=="asp")
		aspCount <- aspCount+resultF[i,j]
		if(columns[j]=="instance")
		instanceCount <- instanceCount+resultF[i,j]
		if(columns[j]=="django")
		djangoCount <- djangoCount+resultF[i,j]
		if(columns[j]=="py")
		pyCount <- pyCount+resultF[i,j]
		if(columns[j]=="python")
		pythonCount <- pythonCount+resultF[i,j]
		if(columns[j]=="problem")
		problemCount <- problemCount+resultF[i,j]
		if(columns[j]=="case")
		caseCount <- caseCount+resultF[i,j]
		if(columns[j]=="cpp")
		cppCount <- cppCount+resultF[i,j]
		if(columns[j]=="std")
		stdCount <- stdCount+resultF[i,j]
		if(columns[j]=="compiler")
		compilerCount <- compilerCount+resultF[i,j]
			if(columns[j]=="address")
		addressCount <- addressCount+resultF[i,j]
				if(columns[j]=="char")
		charCount <- charCount+resultF[i,j]
		if(columns[j]=="gcc")
		gccCount <- gccCount+resultF[i,j]
		if(columns[j]=="mysql")
		mysqlCount <- mysqlCount+resultF[i,j]
		if(columns[j]=="php")
		phpCount <- phpCount+resultF[i,j]
		if(columns[j]=="activerecord")
		activerecordCount <- activerecordCount+resultF[i,j]
		if(columns[j]=="gem")
		gemCount <- gemCount+resultF[i,j]
		if(columns[j]=="hash")
		hashCount <- hashCount+resultF[i,j]
			if(columns[j]=="block")
		blockCount <- blockCount+resultF[i,j]
	if(columns[j]=="beta")
		betaCount <- betaCount+resultF[i,j]
	if(columns[j]=="ios")
		iosCount <- iosCount+resultF[i,j]
	if(columns[j]=="apple")
		appleCount <- appleCount+resultF[i,j]	
	if(columns[j]=="cocoa")
		cocoaCount <- cocoaCount+resultF[i,j]
	if(columns[j]=="datum")
		datumCount <- datumCount+resultF[i,j]
	if(columns[j]=="delegate")
		delegateCount <- delegateCount+resultF[i,j]
	if(columns[j]=="form")
		formCount <- formCount+resultF[i,j]
	if(columns[j]=="cgi")
		cgiCount <- cgiCount+resultF[i,j]
	if(columns[j]=="cpan")
		cpanCount <- cpanCount+resultF[i,j]
	if(columns[j]=="awk")
		awkCount <- awkCount+resultF[i,j]
	if(columns[j]=="bash")
		bashCount <- bashCount+resultF[i,j]
	if(columns[j]=="align")
		alignCount <- alignCount+resultF[i,j]
	if(columns[j]=="background")
		backgroundCount <- backgroundCount+resultF[i,j]
	if(columns[j]=="body")
		bodyCount <- bodyCount+resultF[i,j]	
	if(columns[j]=="actor")
		actorCount <- actorCount+resultF[i,j]	
	if(columns[j]=="cabal")
		cabalCount <- cabalCount+resultF[i,j]
		if(columns[j]=="dataframe")
		dataframeCount <- dataframeCount+resultF[i,j]
	if(columns[j]=="dataset")
		datasetCount <- datasetCount+resultF[i,j]
	if(columns[j]=="factor")
		factorCount <- factorCount+resultF[i,j]
	if(columns[j]=="application")
		applicationCount <- applicationCount+resultF[i,j]
	if(columns[j]=="attempt")
		attemptCount <- attemptCount+resultF[i,j]
	if(columns[j]=="button")
		buttonCount <- buttonCount+resultF[i,j]
	if(columns[j]=="class")
		classCount <- classCount+resultF[i,j]
		j=j+1	
	}
	resultF1 <- arrayCount+domCount+javascriptCount+jqueryCount
	resultF2 <- groupCount+orderCount+selectCount+sqlCount
	resultF3 <- javaCount+jarCount
	resultF4 <- netCount+aspCount+instanceCount
	resultF5 <- djangoCount+pyCount+pythonCount+problemCount+caseCount
	resultF6 <- cppCount+stdCount+compilerCount
	resultF7 <- addressCount+charCount+gccCount
	resultF8 <- mysqlCount+phpCount
	resultF9 <- activerecordCount+gemCount+hashCount+blockCount
	resultF10 <- betaCount + appleCount + iosCount
	resultF11 <- cocoaCount + datumCount + delegateCount
	resultF12 <- formCount+aspCount
	resultF13 <- cpanCount+cgiCount+caseCount
	resultF14 <- awkCount+bashCount+binCount
	resultF15 <- alignCount+backgroundCount+bodyCount
	resultF16 <- actorCount+applicationCount+arrayCount
	resultF18 <- attemptCount+buttonCount+arrayCount
	resultF19 <- cabalCount+caseCount+classCount
	resultF20 <- codeCount+classCount+bodyCount
	resultF21 <- dataframeCount+factorCount+datasetCount
	resultF1 <- if (resultF1>=1 || tagCalculateJs(mydata[i,2])==1) 1 else 0
	resultF2 <- if (resultF2>=2 || tagCalculateSql(mydata[i,2])==1) 1 else 0
	resultF3 <- if (resultF3>=1 || tagCalculateJava(mydata[i,2])==1) 1 else 0
	resultF4 <- if (resultF4>=2 || tagCalculateSharp(mydata[i,2])==1) 1 else 0
	resultF5 <- if (resultF5>=1 || tagCalculatePy(mydata[i,2])==1) 1 else 0
	resultF6 <- if (resultF6>=1 || tagCalculateCpp(mydata[i,2])==1) 1 else 0
	resultF7 <- if (resultF7>=1 || tagCalculateC(mydata[i,2])==1) 1 else 0
	resultF8 <- if (resultF8>=1 || tagCalculatephp(mydata[i,2])==1) 1 else 0
	resultF9 <- if (resultF9>=1 || tagCalculateRuby(mydata[i,2])==1) 1 else 0
	resultF10 <- if(resultF10>=1 || tagCalculateSwift(mydata[i,2])==1) 1 else 0
	resultF11 <- if(resultF11>=1 || tagCalculateObjective(mydata[i,2])==1) 1 else 0
	resultF12 <- if(resultF12>=2 || tagCalculateVb(mydata[i,2])==1) 1 else 0
	resultF13 <- if(resultF13>=2 || tagCalculatePerl(mydata[i,2])==1) 1 else 0
	resultF14 <- if(resultF14>=1 || tagCalculateBash(mydata[i,2])==1) 1 else 0
	resultF15 <- if(resultF15>=1 || tagCalculateCss(mydata[i,2])==1) 1 else 0
	resultF16 <- if(resultF16>=1 || tagCalculateScala(mydata[i,2])==1) 1 else 0
	resultF17 <- if(tagCalculateHtml(mydata[i,2])==1) 1 else 0
	resultF18 <- if(resultF18>=1 || tagCalculateLua(mydata[i,2])==1) 1 else 0
	resultF19 <- if(resultF19>=1 || tagCalculateHaskell(mydata[i,2])==1) 1 else 0
	resultF20 <- if(resultF20>=1 || tagCalculateMarkdown(mydata[i,2])==1) 1 else 0
	resultF21 <- if(resultF21>=1 || tagCalculateR(mydata[i,2])==1) 1 else 0
		y1 <- append(y1,resultF1)
		y2 <- append(y2,resultF2)
		y3 <- append(y3,resultF3)
		y4 <- append(y4,resultF4)
		y5 <- append(y5,resultF5)
		y6 <- append(y6,resultF6)
		y7 <- append(y7,resultF7)
		y8 <- append(y8,resultF8)
		y9 <- append(y9,resultF9)
		y10 <- append(y10,resultF10)
		y11 <- append(y11,resultF11)
		y12 <- append(y12,resultF12)
		y13 <- append(y13,resultF13)
		y14 <- append(y14,resultF14)
		y15 <- append(y15,resultF15)
		y16 <- append(y16,resultF16)
		y17 <- append(y17,resultF17)
		y18 <- append(y18,resultF18)
		y19 <- append(y19,resultF19)
		y20 <- append(y20,resultF20)
		y21 <- append(y21,resultF21)
	i=i+1
	j=1
	arrayCount=0
	domCount=0
	javascriptCount=0
	jqueryCount=0
	groupCount=0
	orderCount=0
	selectCount=0
	sqlCount=0
	javaCount=0
	jarCount=0
	netCount=0
	aspCount=0
	instanceCount=0
	djangoCount=0
	pyCount=0
	pythonCount=0
	problemCount=0
	caseCount=0
	compilerCount=0
	cppCount=0
	stdCount=0
	addressCount=0
	charCount=0
	gccCount=0
	mysqlCount=0
	phpCount=0
	activerecordCount=0
	gemCount=0
	hashCount=0
	blockCount=0
	betaCount=0
	appleCount=0
	iosCount=0
	cocoaCount=0
	datumCount=0
	delegateCount=0
	formCount=0
	cgiCount=0
	cpanCount=0
	awkCount=0
	bashCOunt=0
	binCount=0
	alignCount=0
	backgroundCount=0
	bodyCount=0
	actorCount=0
	applicationCount=0
	cabalCount=0
	dataframeCount=0
	datasetCount=0
	factorCount=0
	attemptCount=0
	buttonCount=0
	classCount=0
	codeCount=0
}
###End of rule-based labeling
###############feature selection
###delete features having high correlations
library(caret)
correlationMatrix <- cor(resultF)
wC <- resultF$wordCount
cC <- resultF$charCount
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
resultF = subset(resultF, select = -highlyCorrelated )
resultF <- data.frame(wC,cC,resultF)
############################
#####Adding Id
##################################
#############################
fileCount<- fileCount-1
resultF <- data.frame(mydata[1:fileCount,2],resultF)
names(resultF)[1]<-paste("Id")
####################################
#######The end of deleting rows having high pearson correlations
#########################################


############1-50 get columns####################
resultF <- resultF[,1:50]
########multi-class adding############
resultF <- data.frame(resultF,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21)
write.csv(resultF,file="processedPosts.csv")



##############To remove redundant temporary .csv files###########################################
i=1
while(i<5000){
file.remove(paste0("freq",i, ".csv"))
i <- i+1
}









