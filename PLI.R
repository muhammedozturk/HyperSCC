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
