####  Bir üst dizinde bulunan Source_Codes isimli dizin içerisindeki tüm dosyaları tarayarak ####
####  içerisindeki açıklama satırlarını yakalayıp comments.txt dosyasının içerisine ekleme 


source_codes_main_directory <- "../Source_Codes"


dir_list <- list.dirs(source_codes_main_directory,full.names = TRUE, recursive = FALSE) 
print (dir_list)



file_extensions <- c("php", "java", "c", "py", "json", "js", "css")

source_code_list <- list()  


# Kaynak Kodların Bulunduğu Dizin İçerisindeki Dosyaların Listelerini Oluşturur.
for (directory in dir_list) {
  print(directory)
  
  subdir_list <- list.files(
    path = directory,
    pattern = paste0("\\.(?:", paste(file_extensions, collapse = "|"), ")$"),
    full.names = TRUE,
    recursive = TRUE
  )
  
  source_code_list <- c(source_code_list, list(subdir_list))
}



# Comment Dizini içerisine proje klasörlerini oluşturma işlemi.
create_comment_directory <- function(source_codes_main_directory) {
  dir_list <- list.files(source_codes_main_directory, full.names = FALSE, recursive = FALSE)
    dir_list <- dir_list[file.info(file.path(source_codes_main_directory, dir_list))$isdir]
    for (dir_name in dir_list) {
    new_dir_path <- file.path("Comments", dir_name)
    dir.create(new_dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  return(dir_list)
}


comment_directory_list <- create_comment_directory(source_codes_main_directory)


# Açıklama satırı yakalama ve temizleme işlemleri
library(stringr)
library(tm)

length(source_code_list)

clean_utf8 <- function(x) {
  for (i in seq_along(x)) {
    x[i] <- iconv(x[i], from = "UTF-8", to = "UTF-8", sub="")
  }
  return(x)
}


for (files_list in 1:length(source_code_list)) {
  print("-----------------")
  print(comment_directory_list[files_list])
  print("-----------------")
    for (x in 1:length(source_code_list[[files_list]])) {
    
      print(source_code_list[[files_list]][[x]])
      
      # Dosyanın okunması ve satırların vektöre dönüştürülmesi
      dosya  <- readLines(source_code_list[[files_list]][[x]], encoding = "UTF-8")
      
      # Açıklama satırlarını içeren bir boş karakter vektörü oluşturulması
      aciklama_satirlari <- character()
      
      for (i in 1:length(dosya)) {
        satir <- dosya[i]
        
        # Eğer satır "//" ile başlıyorsa
        if(grepl("^\\s*//", satir)){
          aciklama_satirlari <- c(aciklama_satirlari, satir)
        }
        
        # Eğer satır "#" ile başlıyorsa
        if(grepl("^\\s*#", satir)){
          #cat(satir,  sep = "\n")
          aciklama_satirlari <- c(aciklama_satirlari, satir)
          
        }
        
        # Eğer satır "/*" ile başlıyorsa
        if (grepl("^\\s*/\\*", satir)) {
          aciklama_satirlari <- c(aciklama_satirlari, satir)
          
          # Sonraki satırları ara ve yazdır
          for (j in (i+1):length(dosya)) {
            satir2 <- dosya[j]
            #cat(satir2, "\n")
            aciklama_satirlari <- c(aciklama_satirlari, satir2)
            
            
            # Eğer satır "*/" ile bitiyorsa döngüden çık
            if (grepl("\\*/\\s*$", satir2)) {
              break
            }
          }
        }
      }
      
     
      # Önişleme işlemleri
      text <- clean_utf8(aciklama_satirlari)
      
      text <- gsub("[^[:alnum:][:space:]]", " ", text) # Alfanümerik olmayan karakterleri yani özel karakterleri (/ , . ) ve boşlukları eşleştirir.
      text <- gsub("[[:punct:]]+", " ", text) # Noktalama işaretlerini boşluklarla değiştir
      text <- tolower(text)
      text <- gsub("(?i)\\b(https?://|www\\.)\\S+\\b|\\b\\w+\\.(java|php|html)\\b", " ", text, perl=TRUE) # http/https, www, .java, .php ve .html uzantıları içeren kelimeleri boşluk ile değiştir
      #text <- gsub("(?i)\\bhttps?://\\S+\\b|\\bwww\\S*\\b", " ", text, perl=TRUE) # http/https ve www içeren kelimeleri boşluk ile değiştir
      text <- gsub("\\b\\w{1}\\b", " ", text)  # Tek harfli ifadeleri boşluk ile değiştir
      text <- gsub("\\b\\d+\\b", " ", text) # Sayıları boşluk ile değiştir
      text <- removeWords(text, stopwords("en")) # İngilizce stop words'leri boşluk ile değiştir
      text <- gsub("\\b(?:i|me|my|myself|we|us|our|ours|ourselves|you|your|yours|yourself|yourselves|he|him|his|himself|she|her|hers|herself|it|its|itself|they|them|their|theirs|themselves|what|which|who|whom|whose|this|that|these|those|am|is|are|was|were|be|been|being|have|has|had|having|do|does|did|doing|will|would|shall|should|can|could|may|might|must|ought|to|for|with|at|by|about|against|between|into|through|during|before|after|above|below|to|from|up|down|in|out|on|off|over|under|again|further|then|once|here|there|when|where|why|how|all|any|both|each|few|more|most|other|some|such|no|nor|not|only|own|same|so|than|too|very|s|t|just|don|should|now)\\b", " ", text, perl=TRUE) # İngilizce stop words'leri boşluk ile değiştir
      text <- gsub("\\s+", " ", text) # birden çok olan boşlukları tek boşluk haline getir
      text <- gsub("^\\s+|\\s+$", "", text) # satır başı ve sonundaki boşlukları sil
      aciklama_satirlari <- text[nchar(trimws(text)) > 0] # boş satırları sil
      
      logFile = paste0("Comments/",comment_directory_list[files_list],"/ALL-CommentLines.txt")
      cat(aciklama_satirlari, file=logFile, append=TRUE, sep = "\n")
  }
}

options(warn = -1)


#################################################################################################################################
#################################################################################################################################
#################################################################################################################################




#### Projelerdeki Kelime Frekanslarının Hesaplanarak csv formatında kaydedilmesi ####

library(stringr)

#Comments dizini içerisindeki projelerin yorum satırı dosyaları seçilerek devam edilir. 
text <- readLines(file.choose())
words <- strsplit(text, " ")
words <- unlist(words)
tf <- table(words)
idf <- log(length(text) / (1 + table(words) > 0))
tf_idf <- tf * idf


df <- data.frame(term = names(tf), tf = tf, idf = idf, tf_idf = tf * idf)
df <- df[order(-df$tf_idf.Freq),]  #tf_idf büyükten küçüğe sıralanması
 
df<-df[,-c(1,2,5)]
names(df) <- c("freq", "idf","tf_idf")

total_words <- sum(df$freq)
df$"freq/totalword" <- df$freq / total_words

write.csv(df , file="term_freq_ElasticSearch.csv", row.names = TRUE )



#################################################################################################################################
#################################################################################################################################
#################################################################################################################################




#### Word2Vec Model Oluşturma İşlemleri ####
# Bu aşamada 3 tane ayrı proje (ElasticSearch - Codeigniter - BigCloneBench) için 
# MinCount=5 olan model oluşturulmuştur.


library(word2vec)
library(udpipe)

set.seed(123456789)
yazi <- file.choose()
yazi <- readLines(yazi)
summary(yazi)
typeof(yazi)
yazi <- unlist(yazi)
yazi <- as.character(yazi)
typeof(yazi)


model1 <- word2vec(x = yazi, type = "cbow", dim = 15, iter = 100, min_count=5)
class(model1)

model1$vocabulary
model1_kelimeler <- summary(model1, type = "vocabulary")
summary(model1_kelimeler)

#Matris
model1_kelimeler
emb1 <- as.matrix(model1) #model1 matrisi
emb1Names <- rownames(emb1)
emb1Names
nrow(emb1)  #satır
ncol(emb1)  #sutun
head(emb1)

write.table(emb1, file="MinCount-5_ElasticSearch.txt", sep = "\t")


#################################################################################################################################
#################################################################################################################################
#################################################################################################################################





#### GLMNET #####

library("glmnet")
yazi <- read.table("D:/makaleler/codeSnippetPrediction/1002/cloneProject/Comment_Line_Detect-V13/Comment_Line_Detect-V13/MinCount-5_ElasticSearch.txt",fill=TRUE)
emb1 <- makeX(yazi)
boyut <- dim(emb1)
#############percentage split rates configuration
tr <- round((boyut[1]*70)/100)
trX <- tr+1
test <- boyut[1]-tr
tr <- c(1:tr)
test <- c(trX:test)


i <- 1
j <- 1
vector <- c()
labels <- c()
sum <- 0
while(i<=boyut[1])
{
  j <- 1
  while(j<=15){
    sum <- sum + emb1[i,j]
    j <- j+1
  }
  print(sum)
  vector <- append(vector,sum)
  sum <- 0
  i <- i+1	
}
labels <- rbinom( length(emb1[,1]), 1, 0.5)

i <- 1
while(i<=boyut[1])
{
  if(vector[i]<0)
    labels[i] <- 0
  else
    labels[i] <- 1 
  i <- i+1
}

labels[1:6000] <- 0
table(labels)
library(glmnet)

t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = emb1[tr,], y = labels[tr], 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "auc",
                              # 5-fold cross-validation
                              nfolds = 4,
                              # high value is less accurate, but has faster training
                              thresh = 1e-9,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))
plot(glmnet_classifier)


preds = predict(glmnet_classifier, emb1[test,], type = 'response')[,1]
glmnet:::auc(labels[test], preds)





#### Grid Search Maxit - Alpha için yapıldı ####
library("rlist")
library("NMOF")
library("Metrics")
library("glmnet")

testFun <- function(x){
  liste <-c(x[1L],x[2L])
  return(liste)
}
par1 <- c(0.1,0.2,0.3,0.35,0.4,0.5,0.6,0.7,0.8,0.9)
par2 <- c(1e1,1e2,1e3,1e4,1e5)

sol <- gridSearch(fun = testFun, levels = list(par1, par2))
sol$minfun
sol$minlevels
i <- 1
j <- 2

uzunluk <- length(sol$values)
sonucList <- c("x")
aucList <- c()
alphaList <- c()
itList <- c()
while(i<uzunluk){
  glmnet_classifier = cv.glmnet(x = emb1[tr,], y = labels[tr], 
                                family = 'binomial', 
                                # L1 penalty
                                alpha = sol$values[i],
                                # interested in the area under ROC curve
                                type.measure = "auc",
                                # 5-fold cross-validation
                                nfolds = NFOLDS,
                                # high value is less accurate, but has faster training
                                thresh = 1e-9,
                                # again lower number of iterations for faster training
                                maxit = sol$values[j])
  preds = predict(glmnet_classifier, emb1[test,], type = 'response')[,1]
  aucResult <- glmnet:::auc(labels[test], preds)
  aucList <- append(aucList,aucResult)
  alphaList <- append(alphaList,sol$values[i])
  itList <-  append(itList,sol$values[j]) 
  i <- i+2
  j <- j+2
  
}
sonucList <- data.frame(aucList, alphaList,itList)
write.csv(sonucList,"GridSearch.csv")

#################################################################################################################################
#################################################################################################################################




#### Grid Search Treshold Eklenerek Yapıldı.#### 

library("rlist")
library("NMOF")
library("Metrics")
library("glmnet")

testFun <- function(x){
  liste <-c(x[1L],x[2L],x[3L])
  return(liste)
}
par1 <- c(0.1,0.2,0.3,0.35,0.4,0.5,0.6,0.7,0.8,0.9)
par2 <- c(1e1,1e2,1e3,1e4,1e5)
par3 <- c(1e-10,1e-9, 1e-8, 1e-7, 1e-6)

sol <- gridSearch(fun = testFun, levels = list(par1, par2,par3))
sol$minfun
sol$minlevels
i <- 1
j <- 2
k <- 3

uzunluk <- length(sol$values)
sonucList <- c("x")
aucList <- c()
alphaList <- c()
itList <- c()
treshList <- c()
while(i<uzunluk){
  glmnet_classifier = cv.glmnet(x = emb1[tr,], y = labels[tr], 
                                family = 'binomial', 
                                # L1 penalty
                                alpha = sol$values[i],
                                # interested in the area under ROC curve
                                type.measure = "auc",
                                # 5-fold cross-validation
                                nfolds = 4,
                                # high value is less accurate, but has faster training
                                thresh = sol$values[k],
                                # again lower number of iterations for faster training
                                maxit = sol$values[j])
  preds = predict(glmnet_classifier, emb1[test,], type = 'response')[,1]
  aucResult <- glmnet:::auc(labels[test], preds)
  aucList <- append(aucList,aucResult)
  alphaList <- append(alphaList,sol$values[i])
  itList <-  append(itList,sol$values[j])
  treshList <-  append(treshList,sol$values[k])
  i <- i+3
  j <- j+3
  k <- k+3
}
sonucList <- data.frame(aucList, alphaList,itList,treshList)


# aucList sütunundaki değerleri büyükten küçüğe sırala 
sorted_indices <- order(sonucList$aucList, decreasing = TRUE)

# sonucList DataFrame'ini sıralanmış indekslere göre güncelle
sorted_sonucList <- sonucList[sorted_indices, ]

write.csv(sorted_sonucList,"GridSearchTresh.csv")


plot(glmnet_classifier)




# AUC'nin açılımı "Area Under the Curve" olarak ifade edilir. ROC eğrisi, hassasiyet (true positive rate)
# ile özgüllük (true negative rate) arasındaki ilişkiyi gösteren bir grafiktir. Bu eğri, sınıflandırma 
# modelinin farklı kesme noktalarında ne kadar iyi performans gösterdiğini gösterir. Kesme noktası, 
# sınıflandırma eşik değerini ifade eder. Eşik değerini değiştirerek modelin hassasiyet ve özgüllük 
# performansını ayarlanabilir.
# AUC, ROC eğrisi altındaki alanı ifade eder ve sınıflandırma modelinin performansını özetler. 
# AUC'nin değeri 0 ile 1 arasındadır.

# Eğer AUC değeri 1'e yakınsa, model mükemmel bir sınıflandırma performansına sahiptir.
# Eğer AUC değeri 0.5'e yakınsa, model rastgele sınıflandırma ile aynı performansa sahiptir.
# Eğer AUC değeri 0'a yakınsa, model tamamen ters sınıflandırma yapmaktadır.

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################








#################################################################################################################################

####   2. Model İçerisine 1. Modelde Bulunan Ama 2.Modelde Bulunmayan Önemli ilk 100 kelimenin yerleştirilmesi     #### 


#Model Matrislerinin Okunması:

emb1<-as.matrix(read.table("MinCount-5-Codeigniter.txt"))

emb2<-as.matrix(read.table("MinCount-5_ElasticSearch.txt"))

emb2Names <- rownames(emb2)


#Birinci Modelin Frekans Dosyası okunur.
df <- read.table("term_freq_Codeigniter.csv", sep = ",", header = TRUE)



top_100 <- head(df$X,100)  #Data Frame İçerisindeki tf_idf en yüksek olan kelimeler



setdiff(top_100, emb2Names) #frekans tablosunda bulunan, ikinci matriste olmayan satır isimleri
length(setdiff(top_100, emb2Names)) #frekans tablosunda bulunan, ikinci matriste olmayan satır isimleri


intersect(top_100, emb2Names) #İki matristede olan satır isimleri
length(intersect(top_100, emb2Names)) #İki matristede olan satır sayısı



#Ana projedeki kelimelerden en önemli 100 tanesinin matrise eklenmesini sağlayan fonksiyon
#df: İlk 100 kelimenin çıkarılacağı veri çerçevesi.
#emb1: Matrisin çekileceği yer.
#emb2: Yeni matrisin ekleneceği yer.
#emb2Names: emb2'deki kelime listesi.
matris_add_new_row <- function(df, emb1, emb2, emb2Names) {
  top_100 <- head(df$X,100) #Data Frame İçerisindeki tf_idf en yüksek olan kelimeler
  eklenecek_kelimeler <- setdiff(top_100, emb2Names)
  my_matrix_row <- emb1[eklenecek_kelimeler, ]
  emb2 <- rbind(emb2, my_matrix_row)
  return(emb2)
}

emb2 <- matris_add_new_row(df, emb1, emb2, emb2Names)

#kaynak matrisdeki değer
kaynak <- which(row.names(emb1)=="codeigniter")
emb1[kaynak, , drop=FALSE]


#eklendiği matrisdeki değer
hedef <- which(row.names(emb2)=="codeigniter")
emb2[hedef, , drop=FALSE]

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################





##### Matris içerisinde bulunan bir satırın silinmesi ####

#Matris içerisindeki </s> ifadesinin bulunduğu satırın silinmesi
emb1["</s>",] 
silinecek_ifade <-"</s>"
satir_indeks <- which(rownames(emb1) == silinecek_ifade)
emb1 <- emb1[-satir_indeks, ]

#################################################################################################################################
#################################################################################################################################
#################################################################################################################################
