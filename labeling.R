########labeling functions######################
###While calling function, if the skip option is set during taking data from question.csv
###call function as tagData[j+skip,1]
###the functions below are devised for 3000 instances
###to search all instances, replace 3000 with rowCount 
tagCalculateJs <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if(tagData[j,2]=="javascript")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateSql <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if(tagData[j,2]=="sql")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateJava <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if(tagData[j,2]=="java")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateSharp <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="asp.net" || tagData[j,2]=="C#"  || tagData[j,2]==".net")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculatePy <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="python")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateCpp <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="c++")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateC <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="c")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculatephp <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="php")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateRuby <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="ruby")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateSwift <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="swift" || tagData[j,2]=="swiftmailer")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateObjective <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="objective-c")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateVb <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="vb.net")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculatePerl <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="perl")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateBash <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="bash")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateCss <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="css")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateScala <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="scala")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateHtml <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="html")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateLua <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="lua")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateHaskell <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="haskell")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateMarkdown <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="markdown")
		return(1)
	}
j <- j+1
}
return(0)
}
tagCalculateR <- function(x){
j=1
rowCount <- length(tagData[,1])
while(j<3000){
	if(x==tagData[j,1])
	{
		if( tagData[j,2]=="r")
		return(1)
	}
j <- j+1
}
return(0)
}
########################################