setwd("C:\\Users\\lojze\\OneDrive\\Documents\\faks\\2.letnik\\1.semester\\UI\\1.seminarska")
getwd()


main_ucna<-read.table("./ucnaSem1.txt",sep=",",header=T)
main_testna<-read.table("./testnaSem1.txt",sep=",",header=T)
ucna<-main_ucna
testna<-main_testna
#delaš klasifikacijski problem

##faktorske podatke faktoriziramo(regija,namembnost,oblačnost, smer_vetra)
ucna$regija<-as.factor(ucna$regija)
testna$regija<-as.factor(testna$regija)
ucna$namembnost<-as.factor(ucna$namembnost)
testna$namembnost<-as.factor(testna$namembnost)
ucna$oblacnost<-as.factor(ucna$oblacnost)
testna$oblacnost<-as.factor(testna$oblacnost)
##smer vetra je regresijski podatek ampak nam je bolj uporaben kot diskretni

source("./as.smeri_vetra.R")
ucna$smer_vetra<-as.smeri_vetra(ucna$smer_vetra)
testna$smer_vetra<-as.smeri_vetra(testna$smer_vetra)


#spremenimo atirubt datum iz character v date type


#dodamo atribut sezone
source("./date.to.seasons.R")
ucna$season<-date.to.seasons(ucna$datum)
testna$season<-date.to.seasons(testna$datum)
#dodamo atribut vikend
library(lubridate)
source("./is.weekend.R")
ucna$vikend<-is.weekend(ucna$datum)
testna$vikend<-is.weekend(testna$datum)


#dodamo dosedanjo povprečno porabo
source("./dosedanja.povprecna.poraba.R")
ucna$dosedanja_povprecna<-dosedanja.povprecna.poraba(ucna)
testna$dosedanja_povprecna<-dosedanja.povprecna.poraba(testna)

  
#dodamo dosedanjo skupno porabo
source("./dosedanja.skupna.poraba.R")
ucna$dosedanja_skupna<-dosedanja.skupna.poraba(ucna)
testna$dosedanja_skupna<-dosedanja.skupna.poraba(testna)


##izločimo atribut stavba saj je odvečen atribut
ucna$stavba<-NULL
testna$stavba<-NULL

#vizualizacija podatkov 
#-nekaj grafov(porazdeliteb vrednosti, soodvisnosti med atributi,
#ponavljajoče se vzorce, ipd)
ucna$vikend<-as.logical(ucna$vikend)
usage_by_year<-aggregate(poraba ~ vikend, ucna,mean)
plot(usage_by_year)

#ocenjevanje in konstrukcija atributov
#-oceni kvaliteto podanih atributov
#-konstruiraj nove atribute(letni časi, povprečna poraba iz trenutno znanih obdobji)
#-vsaj 3 nove atribute
library(CORElearn)
sort(attrEval(namembnost ~ ., ucna, "Relief"), decreasing = TRUE)
sort(attrEval(namembnost ~ ., ucna, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(namembnost ~ ., ucna, "ReliefFexpRank"), decreasing = TRUE)
## iz avaluacije atributov opazimo da imajo atributi ki opisujejo vremenske razmere in datum meritev le teh zelo majheno povezavo z 
## namembnostjo stavbe zato jih lahko izločimo
ucna$temp_zraka<-NULL
ucna$pritisk<-NULL
ucna$temp_rosisca<-NULL
ucna$padavine<-NULL
ucna$hitrost_vetra<-NULL
ucna$smer_vetra<-NULL
ucna$oblacnost<-NULL
ucna$datum<-NULL
ucna$season<-NULL
ucna$vikend<-NULL
testna$temp_zraka<-NULL
testna$pritisk<-NULL
testna$temp_rosisca<-NULL
testna$padavine<-NULL
testna$hitrost_vetra<-NULL
testna$smer_vetra<-NULL
testna$oblacnost<-NULL
testna$datum<-NULL
testna$season<-NULL
testna$vikend<-NULL
ucna$dosedanja_skupna<-NULL
testna$dosedanja_skupna<-NULL
library(rpart)

myTrainFunc <- function(formula, traindata)
{
  CoreModel(formula, traindata, model="tree", selectionEstimator="ReliefFequalK")
}

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}


myEvalFunc <- function(predicted, observed, trained)
{
  # vracamo napako modela, saj wrapper minimizira vrednost ocene
  1.0 - mean(observed == predicted)	
}

source("./wrapper.R")
set.seed(0)
#wrapper(namembnost ~ ., ucna, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)
## z rezulati z wrapper
library(future)
plan("multisession")

#nastavimo obsMAT  
library(nnet)
obsMat <- class.ind(testna$namembnost)

# pravilni razredi testnih primerov (potrebujemo jih za ocenjevanje klasifikacijske tocnosti)
observed <- testna$namembnost

# funkcija za izracun klasifikacijske tocnosti
CA <- function(observed, predicted)
{
  mean(observed == predicted)
}

# funkcija za izracun povprecne Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

#--------------------------------------------------------------------------------------------------------------
#casual tree
  
library(rpart)
library(rpart.plot)
dt<-rpart(namembnost~.,ucna, cp=0)
predicted<-predict(dt,testna,type="class")
CA(observed,predicted)
predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)
tab <- printcp(dt)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th
dt <- prune(dt, cp=th)
rpart.plot(dt)
CA(observed,predicted)
predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)


#podatki %<-% neka_funkcija(...)
#naredimo vraper za drevo za najboljšo točnost
myTrainFunc <- function(formula, traindata)
{
  rpart(formula, traindata)	
}
myPredictFunc <- function(model, testdata)
  
{
  predict(model, testdata, type="class")
}
myEvalFunc <- function(predicted, observed, trained)
  
{
  # vracamo napako modela, saj wrapper minimizira vrednost ocene
  1.0 - mean(observed == predicted)	
}
#md1 <- wrapper(namembnost~.,ucna,myTrainFunc,myPredictFunc,myEvalFunc,cvfolds=10)
#-------rezultat:best model: estimated error =  0.007543904 , selected feature subset =  namembnost ~ povrsina + leto_izgradnje

dt.min_napaka<-rpart(namembnost~povrsina + leto_izgradnje,ucna,cp=0)
predicted<-predict(dt.min_napaka,testna,type="class")
CA(observed,predicted)
predMat <- predict(dt.min_napaka, testna, type = "prob")
brier.score(obsMat, predMat)
tab <- printcp(dt.min_napaka)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th
dt.min_napaka <- prune(dt.min_napaka, cp=th)
rpart.plot(dt.min_napaka)
CA(observed,predicted)
predMat <- predict(dt.min_napaka, testna, type = "prob")
brier.score(obsMat, predMat)

#wraper za najboljši briar score
myPredictFuncProb <- function(model, testdata)
{
  predict(model, testdata, type="prob")
}

myEvalFuncBrier <- function(predicted, observed, trained)
{
  obsMat <- model.matrix(~observed-1)
  sum((obsMat - predicted) ^ 2) / nrow(predicted)	
}

set.seed(0)
#md1 <- wrapper(namembnost~.,ucna,myTrainFunc, myPredictFuncProb, myEvalFuncBrier, cvfolds=10)
#-----rezultat:best model: estimated error =  0.01390878 , selected feature subset =  namembnost ~ povrsina + leto_izgradnje

dt<-rpart(namembnost~povrsina + leto_izgradnje,ucna,cp=0)
predicted<-predict(dt,testna,type="class")
CA(observed,predicted)
predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)
tab <- printcp(dt)
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th
dt <- prune(dt, cp=th)
rpart.plot(dt)
CA(observed,predicted)
predMat <- predict(dt, testna, type = "prob")
brier.score(obsMat, predMat)


#------------------------------------------
#naivni bayes

library(CORElearn)
nb <- CoreModel(namembnost ~ ., data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")

CA(observed, predicted)
predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)

myBayesTrainFunc <- function(formula, traindata)
{
  CoreModel(formula, data = traindata, model="bayes")	
}

#wrapper(namembnost~.,ucna,myBayesTrainFunc, myPredictFuncProb, myEvalFuncBrier, cvfolds=10)
#------rezultat:best model: estimated error =  0.4879203 , selected feature subset =  namembnost ~ leto_izgradnje + povrsina + regija + dosedanja_povprecna 
nb <- CoreModel(namembnost ~ leto_izgradnje + povrsina + regija + dosedanja_povprecna , data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")

CA(observed, predicted)
predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)

#wrapper(namembnost~.,ucna,myBayesTrainFunc,myPredictFunc,myEvalFunc,cvfolds=10)
#------best model: estimated error =  0.3538237 , selected feature subset =  namembnost ~ leto_izgradnje + povrsina + regija
nb. <- CoreModel(namembnost ~ leto_izgradnje + povrsina + regija, data = ucna, model="bayes")
predicted <- predict(nb, testna, type="class")

CA(observed, predicted)
predMat <- predict(nb, testna, type = "prob")
brier.score(obsMat, predMat)



#-----------------------------------------------------------------------------------------------
#K-najbljižih sosedov
library(CORElearn)
knn <- CoreModel(namembnost ~ ., data = ucna, model="knn", kInNN = 5)
predicted <- predict(knn, testna, type="class")
CA(observed, predicted)

predMat <- predict(knn, testna, type = "prob")
brier.score(obsMat, predMat)

# model lahko dodatno izboljsamo z izbiro najbolj ustreznega parametra "kInNN"

# za izbiro parametra "kInNN" lahko uporabimo precno preverjanje na ucni mnozici

n <- nrow(ucna)
k <- 10

set.seed(0)
fold.id <- rep(1:k, length.out=n)
s <- sample(1:n, n, FALSE)
fold.id <- fold.id[s]

# poskusili bomo vse vrednosti parametra "kInNN" na intervalu od 1 do 20
minkInNN <- 1
maxkInNN <- 20
est <- vector()
for (val in minkInNN:maxkInNN)
{
  print(paste("Testiram za nastavitev kInNN", val))
  flush.console()
  
  q <- vector()
  for (i in 1:k)
  {	
    sel <- fold.id == i
    knn <- CoreModel(namembnost ~ ., data = ucna[!sel,], model="knn", kInNN = val)
    pred <- predict(knn, ucna[sel,], type= "class")
    obs <- ucna$position[sel]
    q[sel] <- pred == obs
  }
  
  est <- append(est, mean(q))
}

names(est) <- minkInNN:maxkInNN
est

which.max(est)


knn <- CoreModel(namembnost ~ ., data = ucna, model="knn", kInNN = 18)
predicted <- predict(knn, testna, type="class")
CA(observed, predicted)

predMat <- predict(knn, testna, type = "prob")
brier.score(obsMat, predMat)

#----------------------------------------------------------------------------------------------------
#naključno gozd
library(CORElearn)
rf <- CoreModel(namembnost ~ ., data = ucna, model="rf")
predicted <- predict(rf, testna, type="class")

CA(observed, predicted)
predMat <- predict(rf, testna, type = "prob")
brier.score(obsMat, predMat)

#----------------------------------------------------------------------------------------------
#SVM
library(kernlab)

sm <- ksvm(namembnost ~ ., data = ucna, kernel = "rbfdot")
predicted <- predict(sm, testna, type = "response")
CA(observed, predicted)

sm <- ksvm(namembnost ~ ., data = ucna, kernel = "rbfdot", prob.model = T)
predMat <- predict(sm, testna, type = "prob")
brier.score(obsMat, predMat)
CA(observed, predicted)
#-------------------------------------------------------------
#umetne nevronske mreže
library(nnet)

set.seed(0)
nn <- nnet(namembnost ~ ., data = ucna, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, testna, type = "class")
CA(observed, predicted)



# implementacija funkcije za ucenje nevronske mreze daje boljse rezultate v primeru,
# ko so ucni primeri normalizirani. 

# poiscemo zalogo vrednosti zveznih atributov
# (v nasem primeru so vsi atributi zvezni, razen spr. "regija" in "namembnost", ki je 1. in 2. stolpec)

summary(ucna)
names(ucna)
class <- which(names(ucna) == "regija"|names(ucna) == "namembnost")
class

max_train <- apply(ucna[,-class], 2, max)
min_train <- apply(ucna[,-class], 2, min)

# normaliziramo podatke
ucna_scaled <- scale(ucna[,-class], center = min_train, scale = max_train - min_train)
ucna_scaled <- data.frame(ucna_scaled)
ucna_scaled$regija <- ucna$regija
ucna_scaled$namembnost <- ucna$namembnost
# vse vrednosti atributov v ucni mnozici so na intervalu [0,1]
summary(ucna_scaled)


# testno mnozico skaliramo na zalogo vrednosti iz ucne mnozice!
testna_scaled <- scale(testna[,-class], center = min_train, scale = max_train - min_train)
testna_scaled <- data.frame(testna_scaled)
testna_scaled$regija <- testna$regija
testna_scaled$namembnost <- testna$namembnost
# v testni mnozici ne bodo vse vrednosti na intervalu [0,1]!
summary(testna_scaled)

set.seed(0)
nn <- nnet(namembnost ~ ., data = ucna_scaled, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, testna_scaled, type = "class")
CA(observed, predicted)

predMat <- predict(nn, testna_scaled, type = "raw")
brier.score(obsMat, predMat)


#--------------------------------------------------------------------------------------
# za kobinirane modele bom uporabil dve skupine algoritmov 
#1. skupina z najižjim berier scorom med zgornjimi
#neveronske mreže
#naivni baes - min napaka
#SVM

  #
  # Glasovanje
  #
  
modelNM <- nnet(namembnost ~ ., data = ucna_scaled, size = 5, decay = 0.0001, maxit = 10000)
modelNB <- CoreModel(namembnost ~ leto_izgradnje + povrsina + regija, data = ucna, model="bayes")
modelSVM <- ksvm(namembnost ~ ., data = ucna, kernel = "rbfdot",prob.model = T)

predNM <- predict(modelNM, testna_scaled, type = "class")
caNM <- CA(observed, predNM)
caNM
length(observed)
predNB <- predict(modelNB, testna, type="class")
caNB <- CA(observed, predNB)
caNB

predSVM <- predict(modelSVM, testna, type="response")
caSVM <- CA(observed, predSVM)
caSVM
# zdruzimo napovedi posameznih modelov v en podatkovni okvir
pred <- data.frame(predNM, predNB, predSVM)
head(pred)

# testni primer klasificiramo v razred z najvec glasovi
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  res
}
levels(ucna$namembnost)
predClass <- voting(pred)
head(predClass)
length(predClass)
predClass[predClass=="1"]<-"izobrazevalna" 
predClass[predClass=="2"]<-"javno_storitvena" 
predClass[predClass=="3"]<-"kulturno_razvedrilna" 
predClass[predClass=="4"]<-"poslovna" 
predClass[predClass=="5"]<-"stanovanjska"
predClass
ucna$namembnost<-as.factor(ucna$namembnost)
levels(ucna$namembnost)
predicted <- factor(predClass, levels=levels(ucna$namembnost))
length(observed)
length(predicted)
CA(observed, predicted)




#
# Utezeno glasovanje
#

predNM.prob <- predict(modelNM, testna, type="raw")
predNB.prob <- predict(modelNB, testna, type = "prob")
predSVM.prob <- predict(modelSVM, testna, type="prob")

# sestejemo napovedane verjetnosti s strani razlicnih modelov
predProb <- predNM.prob + predNB.prob + predSVM.prob

head(predProb)

head(max.col(predProb))

# izberemo razred z najvecjo verjetnostjo
predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(ucna$namembnost))
head(predicted)

CA(observed, predicted)

library(adabag)

bm <- boosting(namembnost~ ., ucna, mfinal=100)
predictions <- predict(bm, testna)
names(predictions)

predicted <- predictions$class
CA(testna$namembnost, predicted)


summary(ucna)
#------------------------------------------------------------------------------------------------------

ucna_vzhodna<-ucna[ucna$regija=="vzhodna",]
#ostranimo atribut regija saj je neuporaben
ucna_vzhodna$regija<-NULL
testna_vzhodna<-testna

obsMat <- class.ind(testna_vzhodna$namembnost)
observed<-testna_vzhodna$namembnost

summary(ucna_vzhodna)

#nevronske
summary(ucna_vzhodna)
names(ucna_vzhodna)
class <- which(names(ucna_vzhodna) == "namembnost")
class
summary(ucna_vzhodna)
summary(testna_vzhodna)
max_train <- apply(ucna_vzhodna[,-class], 2, max)
min_train <- apply(ucna_vzhodna[,-class], 2, min)
max_train
min_train
ucna_vzhodna
# normaliziramo podatke
ucna_scaled <- scale(ucna_vzhodna[,-class], center = min_train, scale = max_train - min_train)
ucna_scaled <- data.frame(ucna_scaled)
ucna_scaled$namembnost <- ucna_vzhodna$namembnost
# vse vrednosti atributov v ucni mnozici so na intervalu [0,1]
summary(ucna_scaled)

class <- which(names(ucna) == "regija"|names(ucna) == "namembnost")
class
# testno mnozico skaliramo na zalogo vrednosti iz ucne mnozice!
testna_scaled <- scale(testna_vzhodna[,-class], center = min_train, scale = max_train - min_train)
testna_scaled <- data.frame(testna_scaled)
testna_scaled$namembnost <- testna_vzhodna$namembnost
# v testni mnozici ne bodo vse vrednosti na intervalu [0,1]!
summary(testna_scaled)

set.seed(0)
nn <- nnet(namembnost ~ ., data = ucna_scaled, size = 5, decay = 0.0001, maxit = 10000)
predicted <- predict(nn, testna_vzhodna, type = "class")
CA(observed, predicted)

predMat <- predict(nn, testna_vzhodna, type = "raw")
brier.score(obsMat, predMat)
table(obsMat)
table(predMat)

#
# Glasovanje
#

modelNM <- nnet(namembnost ~ ., data = ucna_scaled, size = 5, decay = 0.0001, maxit = 10000)
modelNB <- CoreModel(namembnost ~ leto_izgradnje + povrsina, data = ucna_vzhodna, model="bayes")
modelSVM <- ksvm(namembnost ~ ., data = ucna_vzhodna, kernel = "rbfdot",prob.model = T)

predNM <- predict(modelNM, testna_vzhodna, type = "class")
caNM <- CA(observed, predNM)
caNM
length(observed)
predNB <- predict(modelNB, testna_vzhodna, type="class")
caNB <- CA(observed, predNB)
caNB

predSVM <- predict(modelSVM, testna_vzhodna, type="response")
caSVM <- CA(observed, predSVM)
caSVM
# zdruzimo napovedi posameznih modelov v en podatkovni okvir
pred <- data.frame(predNM, predNB, predSVM)
head(pred)

# testni primer klasificiramo v razred z najvec glasovi
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  res
}
levels(ucna_vzhodna$namembnost)
predClass <- voting(pred)
head(predClass)
length(predClass)
predClass[predClass=="1"]<-"izobrazevalna" 
predClass[predClass=="2"]<-"javno_storitvena" 
predClass[predClass=="3"]<-"kulturno_razvedrilna" 
predClass[predClass=="4"]<-"poslovna" 
predClass[predClass=="5"]<-"stanovanjska"
predClass
ucna_vzhodna$namembnost<-as.factor(ucna_vzhodna$namembnost)
levels(ucna$namembnost)
predicted <- factor(predClass, levels=levels(ucna_vzhodna$namembnost))
length(observed)
length(predicted)
CA(observed, predicted)






#svm
sm <- ksvm(namembnost ~ ., data = ucna_vzhodna, kernel = "rbfdot")
predicted <- predict(sm, testna_vzhodna, type = "response")
CA(observed, predicted)

sm <- ksvm(namembnost ~ ., data = ucna_vzhodna, kernel = "rbfdot", prob.model = T)
predMat <- predict(sm, testna_vzhodna, type = "prob")
brier.score(obsMat, predMat)
CA(observed, predicted)



#modeliranje
#-uporabi vsaj  3 učne algorimte za napovedovanje
#-primerjaj modele če izbereš samo nekaj atributov ali pa uporabiš vse
#-naredi različne načine kombiniranja modelov(glasovanje,uteženo glasovnaje, bagging ipd) in jih primerjaj z osnovnimi metodami
#-primerjaj uspešnost modelov ki se učijo samo iz podatkov ene regije z modeli ki seučijo
#iz zahodne in vzhodne

#evaluacija modelov
#upoštevaj kronologijo podatkov - združi podatke iz učne in testne množice
# - razdeli na 12 podmnožic glede na mesec zajema
# - uprabi januarske  za učne in februarske za testne
# - nato januarske in februarske za učne in marčeve za testne
# - nadaljuj do decembra
library(nnet)
library(kknn)
library(randomForest)
ucna$dosedanja_skupna<-NULL
testna$dosedanja_skupna<-NULL
ucna$datum<-main_ucna$datum
testna$datum<-main_testna$datum
data<-rbind(ucna,testna)
data
#podatke razdelimo po mesecih

jan <- data[grepl("2016-01", data$datum),]
feb <- data[grepl("2016-02", data$datum),]
mar <- data[grepl("2016-03", data$datum),]
apr <- data[grepl("2016-04", data$datum),]
may <- data[grepl("2016-05", data$datum),]
jun <- data[grepl("2016-06", data$datum),]
jul <- data[grepl("2016-07", data$datum),]
aug <- data[grepl("2016-08", data$datum),]
sep <- data[grepl("2016-09", data$datum),]
oct <- data[grepl("2016-10", data$datum),]
nov <- data[grepl("2016-11", data$datum),]
dec <- data[grepl("2016-12", data$datum),]

#odstranimo atribut datum
jan$datum <- NULL
feb$datum <- NULL
mar$datum <- NULL
apr$datum <- NULL
may$datum <- NULL
jun$datum <- NULL
jul$datum <- NULL
aug$datum <- NULL
sep$datum <- NULL
oct$datum <- NULL
nov$datum <- NULL
dec$datum <- NULL


NM_fun <- function(train, test)
{
  obsMat <- class.ind(test$namembnost)
  observed<-test$namembnost
  summary(ucna)
  names(ucna)
  class <- which(names(train) == "regija"|names(train) == "namembnost")
  class
  
  max_train <- apply(train[,-class], 2, max)
  min_train <- apply(train[,-class], 2, min)
  
  # normaliziramo podatke
  ucna_scaled <- scale(train[,-class], center = min_train, scale = max_train - min_train)
  ucna_scaled <- data.frame(ucna_scaled)
  ucna_scaled$regija <- train$regija
  ucna_scaled$namembnost <- train$namembnost
  # vse vrednosti atributov v ucni mnozici so na intervalu [0,1]
  summary(ucna_scaled)
  
  
  # testno mnozico skaliramo na zalogo vrednosti iz ucne mnozice!
  testna_scaled <- scale(test[,-class], center = min_train, scale = max_train - min_train)
  testna_scaled <- data.frame(testna_scaled)
  testna_scaled$regija <- test$regija
  testna_scaled$namembnost <- test$namembnost
  
  nn <- nnet(namembnost ~ ., data = ucna_scaled, size = 5, decay = 0.0001, maxit = 10000)
  predicted <- predict(nn, testna_scaled, type = "class")
  cat("CA:")
  print(CA(observed, predicted))
  cat("brier:")
  predMat <- predict(nn, testna_scaled, type = "raw")
  print(brier.score(obsMat, predMat))
  return(c(CA(observed, predicted),brier.score(obsMat, predMat)))
}
train<-NULL
NM_list=list()
NM_list[[1]]<-NM_fun(jan,feb)
train <- rbind(jan,feb)
NM_list[[2]]<-NM_fun(train,mar)
train <- rbind(train,mar)
NM_list[[3]]<-NM_fun(train,apr)
train <- rbind(train,apr)
NM_list[[4]]<-NM_fun(train,may)
train <- rbind(train,may)
NM_list[[5]]<-NM_fun(train,jun)
train <- rbind(train,jun)
NM_list[[6]]<-NM_fun(train,jul)
train <- rbind(train,jul)
NM_list[[7]]<-NM_fun(train,aug)
train <- rbind(train,aug)
NM_list[[8]]<-NM_fun(train,sep)
train <- rbind(train,sep)
NM_list[[9]]<-NM_fun(train,oct)
train <- rbind(train,oct)
NM_list[[10]]<-NM_fun(train,nov)
train <- rbind(train,nov)
NM_list[[11]]<-NM_fun(train,dec)
NM_list

SVM_fun <- function(train, test)
{
  obsMat <- class.ind(test$namembnost)
  observed<-test$namembnost
  sm <- ksvm(namembnost ~ ., data = train, kernel = "rbfdot", prob.model = T)
  predicted <- predict(sm, test, type = "response")
  predMat <- predict(sm, test, type = "prob")
  cat("CA:")
  print(CA(observed, predicted))
  cat("brier:")
  print(brier.score(obsMat, predMat))
  return(c(CA(observed, predicted),brier.score(obsMat, predMat)))
}
train<-NULL
library(kernlab)
SVM_list=list()
SVM_list
SVM_list[[1]]<-SVM_fun(jan,feb)
train <- rbind(jan,feb)
SVM_list[[2]]<-SVM_fun(train,mar)
train <- rbind(train,mar)
SVM_list[[3]]<-SVM_fun(train,apr)
train <- rbind(train,apr)
SVM_list[[4]]<-SVM_fun(train,may)
train <- rbind(train,may)
SVM_list[[5]]<-SVM_fun(train,jun)
train <- rbind(train,jun)
SVM_list[[6]]<-SVM_fun(train,jul)
train <- rbind(train,jul)
SVM_list[[7]]<-SVM_fun(train,aug)
train <- rbind(train,aug)
SVM_list[[8]]<-SVM_fun(train,sep)
train <- rbind(train,sep)
SVM_list[[9]]<-SVM_fun(train,oct)
train <- rbind(train,oct)
SVM_list[[10]]<-SVM_fun(train,nov)
train <- rbind(train,nov)
SVM_list[[11]]<-SVM_fun(train,dec)
SVM_list

VOTE_fun<-function(train,test){
  obsMat <- class.ind(test$namembnost)
  observed<-test$namembnost
  summary(ucna)
  names(ucna)
  class <- which(names(train) == "regija"|names(train) == "namembnost")
  class
  
  max_train <- apply(train[,-class], 2, max)
  min_train <- apply(train[,-class], 2, min)
  
  # normaliziramo podatke
  ucna_scaled <- scale(train[,-class], center = min_train, scale = max_train - min_train)
  ucna_scaled <- data.frame(ucna_scaled)
  ucna_scaled$regija <- train$regija
  ucna_scaled$namembnost <- train$namembnost
  # vse vrednosti atributov v ucni mnozici so na intervalu [0,1]
  summary(ucna_scaled)
  
  
  # testno mnozico skaliramo na zalogo vrednosti iz ucne mnozice!
  testna_scaled <- scale(test[,-class], center = min_train, scale = max_train - min_train)
  testna_scaled <- data.frame(testna_scaled)
  testna_scaled$regija <- test$regija
  testna_scaled$namembnost <- test$namembnost
  
  modelNM <- nnet(namembnost ~ ., data = ucna_scaled, size = 5, decay = 0.0001, maxit = 10000)
  modelNB <- CoreModel(namembnost ~ leto_izgradnje + povrsina + regija, data = train, model="bayes")
  modelSVM <- ksvm(namembnost ~ ., data = train, kernel = "rbfdot",prob.model = T)
  
  predNM <- predict(modelNM, testna_scaled, type = "class")
  caNM <- CA(observed, predNM)
  caNM
  length(observed)
  predNB <- predict(modelNB, test, type="class")
  caNB <- CA(observed, predNB)
  caNB
  
  predSVM <- predict(modelSVM, test, type="response")
  caSVM <- CA(observed, predSVM)
  caSVM
  # zdruzimo napovedi posameznih modelov v en podatkovni okvir
  pred <- data.frame(predNM, predNB, predSVM)
  head(pred)
  
  # testni primer klasificiramo v razred z najvec glasovi
  voting <- function(predictions)
  {
    res <- vector()
    
    for (i in 1 : nrow(predictions))  	
    {
      vec <- unlist(predictions[i,])
      res[i] <- names(which.max(table(vec)))
    }
    
    res
  }
  levels(train$namembnost)
  predClass <- voting(pred)
  head(predClass)
  length(predClass)
  predClass[predClass=="1"]<-"izobrazevalna" 
  predClass[predClass=="2"]<-"javno_storitvena" 
  predClass[predClass=="3"]<-"kulturno_razvedrilna" 
  predClass[predClass=="4"]<-"poslovna" 
  predClass[predClass=="5"]<-"stanovanjska"
  predClass
  train$namembnost<-as.factor(train$namembnost)
  levels(train$namembnost)
  predicted <- factor(predClass, levels=levels(train$namembnost))
  length(observed)
  length(predicted)
  cat("CA:")
  print(CA(observed, predicted))
  return(CA(observed, predicted))
}
library(CORElearn)
train<-NULL
VOTE_list=list()
VOTE_list[[1]]<-VOTE_fun(jan,feb)
train <- rbind(jan,feb)
VOTE_list[[2]]<-VOTE_fun(train,mar)
train <- rbind(train,mar)
VOTE_list[[3]]<-VOTE_fun(train,apr)
train <- rbind(train,apr)
VOTE_list[[4]]<-VOTE_fun(train,may)
train <- rbind(train,may)
VOTE_list[[5]]<-VOTE_fun(train,jun)
train <- rbind(train,jun)
VOTE_list[[6]]<-VOTE_fun(train,jul)
train <- rbind(train,jul)
VOTE_list[[7]]<-VOTE_fun(train,aug)
train <- rbind(train,aug)
VOTE_list[[8]]<-VOTE_fun(train,sep)
train <- rbind(train,sep)
VOTE_list[[9]]<-VOTE_fun(train,oct)
train <- rbind(train,oct)
VOTE_list[[10]]<-VOTE_fun(train,nov)
train <- rbind(train,nov)
VOTE_list[[11]]<-VOTE_fun(train,dec)

sum<-0
for (i in 1 : length(NM_list)){
  sum<-sum+NM_list[[i]][1]
}
sum/length(NM_list)

sum<-0
for (i in 1 : length(SVM_list)){
  sum<-sum+SVM_list[[i]][1]
}
sum/length(SVM_list)

sum<-0
for (i in 1 : length(VOTE_list)){
  sum<-sum+NM_list[[i]][1]
}
sum/length(VOTE_list)


