#######################################################################################################################
#
# NALOGE 
#
#######################################################################################################################
#
# Nalozite podatkovno mnozico "movies.txt" z ukazom:
#
md <- read.table("C:\\Users\\lojze\\OneDrive\\Documents\\faks\\2.letnik\\1.semester\\UI\\naloge iz vaj\\movies.txt", sep=",", header=TRUE)
md
#
# Odgovorite na naslednja vprasanja:
#
# - Ali je v nasi podatkovni bazi vec filmov, ki so krajsi od 100 minut, ali je vec tistih, ki so dolgi 100 oz. vec minut?
#   (odgovor podajte numericno in graficno) 
tab <- table(md$length<100)
tab
names(tab)<-c("krajši", "daljši od 100")
tab
pie(tab)
# - Narisite histogram ocen za komedije.
hist(md$rating[md$Comedy == "1"])
# - Ali so v povprecju dokumentarci krajsi kot komedije?
#   (odgovor podajte numericno in graficno)
dokumentarci <- md$length[md$Documentary == "1"]
komedije <- md$length[md$Comedy == "1"]

mean(dokumentarci)
mean(komedije)

boxplot(dokumentarci, komedije, names=c("Dokumentarci", "Komedije"), ylab="Dolzina", main="Dolzina dokumentarcev in komedij")

abline(h=mean(dokumentarci), col="red")
abline(h=mean(komedije), col="blue")
#
# - Prikazite stevilo animirank v nasi bazi od leta 1995 naprej.
sel<- md$year > 1995
t <- table(md$Animation[sel], md$year[sel])
x <- colnames(t)
y <- t[2,]
x
y
plot(x,y, type="l", xlab="Leto", ylab="Stevilo filmov", main="Stevilo animirank po letih")
agg <- aggregate((Animation == "1") ~ year, md[md$year >= 1995,], sum)
agg
plot(agg[,1], agg[,2], type="l", xlab="Leto", ylab="Stevilo filmov", main="Stevilo animirank po letih")
# - Prikazite povprecno dolzino akcijskih komedij po letih od 1950 do 2005.

sel<-md$Action[md$year<2005&md$year>1950]
agg <- aggregate(length~year,md[md$year<2005&md$year>1950 & md$Action==1 & md$Comedy==1,],mean)
agg
#######################################################################################################################
#
# Nalozite podatkovno mnozico "players.txt" z ukazom:
md <- read.table("C:\\Users\\lojze\\OneDrive\\Documents\\faks\\2.letnik\\1.semester\\UI\\naloge iz vaj\\players.txt", sep=",", header=TRUE)
summary(md)
#
#	players <- read.table("players.txt", sep=",", header = T)
#
# - Izrisite razmerje stevila igralcev glede na igralne polozaje.
table(md$position)
pie(table(md$position))
#
# - Primerjajte stevilo dobljenih skokov ("reb") med igralci na razlicnih igralnih polozajih.
#
boxplot(reb~position,md)
# - Narisite histogram uspesnosti izvajanja prostih metov.
#   (uspesnost je definirana kot razmerje med stevilom zadetih prostih metov ("ftm") in stevilom vseh prostih metov ("fta").
hist(md$ftm/md$fta)
#
# - Primerjajte stevilo uspesnih metov za tri tocke glede na igralne polozaje. 
#   (v primerjavo vkljucite samo igralce aktivne v obdobju 1990-2007).
sel<-md$firstseason>=1990 & md$lastseason<=2007
boxplot(md$ftm[sel]~md$position[sel])
#
# - Kako se iz leta v leto spreminja povprecna dolzina igralne kariere upokojenih igralcev?
#   (ignorirajte igralce, ki imajo vrednost atributa "lastseason" nastavljeno na 2008).
  
#
#######################################################################################################################