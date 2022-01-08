######################################################################################################
#
library(rpart)
library(rpart.plot)
library(nnet)
# Na podlagi podatkovne mnozice "movies.txt" zgradite model za
# napovedovanje, ali je dolocen film komedija ali ne.
#
# 1. Nalozite podatke o filmih iz vhodne datoteke "movies.txt"
getwd
getwd()
movies<-read.table("./faks/2.letnik/1.semester/UI/naloge iz vaj/movies.txt",sep=",",header=T)
summary(movies)

#
# 2. Faktorizirajte atribute "Action", "Animation", "Comedy", "Drama", "Documentary", 
#    "Romance" in "Short"
movies$Action<-as.factor(movies$Action)
movies$Animation<-as.factor(movies$Animation)
movies$Comedy<-as.factor(movies$Comedy)
movies$Drama<-as.factor(movies$Drama)
movies$Documentary<-as.factor(movies$Documentary)
movies$Romance<-as.factor(movies$Romance)
movies$Short<-as.factor(movies$Short)
summary(movies)
table(movies$mpaa)
#iz summary vidimo da lahko factoriziramo tudi mpaa
movies$mpaa<-as.factor(movies$mpaa)
#
# 3. POMEMBNO: 
#    - Iz podatkovne mnozice odstranite atribut "title" (neuporaben za generalizacijo)
#    - Iz podatkovne mnozice odstranite atribut "budget" (manjkajoce vrednosti)

movies$title<-NULL
movies$budget<-NULL
summary(movies)
# 4. Razdelite podatkovno mnozico na ucni in testni del. Ucno mnozico naj predstavljajo 
#    filmi posneti pred letom 2004. Testno mnozico naj predstavljajo filmi posneti leta 
#    2004 in pozneje

learn<-movies[movies$year<2004,]
test<-movies[movies$year>=2004,]
summary(learn)
#year atrubut ni več uporaben zato ga odstranimo
learn$year<-NULL
test$year<-NULL
# 5. Zgradite odlocitveno drevo na podlagi ucne mnozice 
decision_tree <- rpart(Comedy~.,learn)#comedy~. pomeni da uporavimo vse ostale atribute da napovemo comedy
rpart.plot(decision_tree)
# 
# 6. Ocenite kvaliteto naucenega modela na testni mnozici 
#    (klasifikacijska tocnost, senzitivnost, specificnost, Brierjeva mera)
observed<-test$Comedy
predicted<-predict(decision_tree,test,type="class")
CA <- function(obs, pred)
{
  t <- table(obs, pred)
  
  sum(diag(t)) / sum(t)
}
CA(observed,predicted)
Sensitivity <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  
  tab[pos.class, pos.class] / sum(tab[pos.class,])
}

Specificity <- function(obs, pred, pos.class)
{
  tab <- table(obs, pred)
  neg.class <- which(row.names(tab) != pos.class)
  
  tab[neg.class, neg.class] / sum(tab[neg.class,])
}
Sensitivity(observed, predicted, "1")
Specificity(observed, predicted, "1")

predMat <- predict(decision_tree, test, type = "prob")
obsMat <- class.ind(test$Comedy)
brier.score <- function(obsMat, predMat)
{
  sum((obsMat - predMat) ^ 2) / nrow(predMat)
}
brier.score(obsMat, predMat)


#
# 7. Narisite krivuljo ROC za vas model


#
######################################################################################################

######################################################################################################
#
# Klasifikator je bil testiran na dvorazrednem problemu in dosegel na testni mnozici naslednjo 
# matriko napak:
#
# +-----------------------+-----+-----+
# | Pravi \ Napov. razred |     |     |
# | razred \              |   0 |   1 |
# +-----------------------+-----+-----+
# | 0                     | 300 |   0 | 
# +-----------------------+-----+-----+
# | 1                     |  80 | 120 |
# +-----------------------+-----+-----+         
# 
# Izracunaj:
# 
# a) klasifikacijsko tocnost klasifikatorja
#
# b) pricakovano tocnost vecinskega klasifikatorja 
#    (predpostavi, da vecinski razred v testni mnozici je vecinski tudi v ucni mnozici)
#
# c) senzitivnost klasifikatorja
# 
# d) specificnost klasifikatorja
#
######################################################################################################

######################################################################################################
#
# Klasifikator je na 4-razrednem problemu klasificiral pet testnih primerov. 
# V spodnji tabeli je podana napovedana verjetnostna porazdelitev po stirih razredih za vsakega od 
# petih testnih primerov:
#	             
# Pravi razred  | Napov. verjetnosti:  C1    C2    C3    C4
# --------------+------------------------------------------
#            C4 |                    0.65  0.25  0.00  0.10
#            C2 |                    0.20  0.55  0.25  0.00
#            C1 |                    0.75  0.00  0.25  0.00
#            C2 |                    0.25  0.50  0.00  0.25
#            C3 |                    0.10  0.10  0.60  0.20
# 
# Izracunaj:
# 
# a) klasifikacijsko tocnost klasifikatorja 
# 
# b) povprecno Brierjevo mero 
#   
# c) povprecno informacijsko vsebino odgovora, ce je apriorna porazdelitev po razredih:
#    P(C1) = 0.1, P(C2) = 0.5, P(C3) = 0.2 in P(C4) = 0.2.
#
######################################################################################################