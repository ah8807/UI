#1.  Zgradite vektor, ki vsebuje elemente: 1,2,3,...,19,20
vec1 <- seq(1,20)
vec1
# 2.  Zgradite vektor, ki vsebuje elemente: 1,2,3,...,19,20,19,...,3,2,1
vec2 <- c(seq(1,20),seq(20,1,-1)) # z c() združimo 2 vektorja ali pa creairamo vektor
vec2
# 3.  Zgradite vektor, ki vsebuje elemente: 1,3,5,1,3,5,...,1,3,5 
#     pri cemer velja, da se vrednosti 1, 3 in 5 ponovijo 10 krat.
vec3 <- rep(c(1,3,5),times=10)
vec3
# 4.  Izracunajte vrednosti funkcije sin(x) v tockah 0, 0.1, 0.2, 0.3, ..., 1.0
vec4 <- seq(0.1,1,0.1)
sin(vec4)
# 5.  Imamo zbrane podatke o visini in tezi desetih oseb:
#
# vektor z visinami oseb v 'cm'
height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)

# vektor s tezami oseb v 'kg'
weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)# 5.  Imamo zbrane podatke o visini in tezi desetih oseb:
#
      # vektor z visinami oseb v 'cm'
      height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)

      # vektor s tezami oseb v 'kg'
      weight <- c(95, 89, 70, 80, 92, 86, 100, 63, 72, 70)

#     Izracunajte indeks telesne mase za vseh deset oseb s pomocjo formule:
#     bmi = teza_v_kg / (visina_v_m)^2
#
#     Namig: najprej prevedite visine v metre, nato uporabite zgornjo formulo.

#     Izracunajte indeks telesne mase za vseh deset oseb s pomocjo formule:
#     bmi = teza_v_kg / (visina_v_m)^2
#
#     Namig: najprej prevedite visine v metre, nato uporabite zgornjo formulo.
height <- height*0.01
height
bmi <- weight/(height)^2
bmi

# 6.  Podan je vektor:
# 
x <- c(1, -2, 3, -4, 5, -6, 7, -8)

#     Vektor x spremenite tako, da negativne elemente postavite na 0, 
#     pozitivne pa pomnozite z 10.

y <- x>0
x[y] <- x[y]*10
x[!y] <- 0
x

# 7.  Brez uporabe Rja, dolocite rezultat naslednjega izracuna:
#
x <- c(1,2,3)
x[1]/x[2]^2-1+2*x[3]-x[1+1]
# 8.  Podan je vektor:
#
x <- 1:200

#     Prestejte koliko elementov vektoja x je deljivih z 11.
#
#     Namig: operator za celostevilcno deljenje je %/%
#            operator za izracun ostanka pri deljenju je %% 
y <- (x%%11)==0
y
z<-sum(y)
z

# 9.  Podani so naslednji podatki:
#
height <- c(179, 185, 183, 172, 174, 185, 193, 169, 173, 168)
weight <- c(55, 89, 70, 80, 62, 86, 50, 63, 72, 57)
gender <- factor(c("f","m","m","m","f","m","f","f","m","f"))
student <- c(T, T, F, F, T, T, F, F, F, T)
age = c(20, 21, 30, 25, 27, 19, 24, 27, 28, 24)
name = c("Joan","Tom","John","Mike","Anna","Bill","Tina","Beth","Steve","Kim")

df <- data.frame(name, gender, age, height, weight, student)

#     
#     - izracunajte povprecno starost oseb v nasi bazi podatkov. 
#       (Namig: uporabite funkcijo "mean")
x<-mean(df$age)
x
#
#     - izracunajte povprecno starost studentov v nasi bazi podatkov.
x<-mean(df$age[df$student])
x#
#     - koliko je moskih in koliko zensk v nasi bazi podatkov? 
#       (Namig: uporabite funkcijo "table")
table(df$gender)
#     - izpisite osebe, ki studirajo. 
df[df$student,]
#
#     - izpisite osebe z visino med 1.8 m in 1.9 m. 
df[(df$height*0.01)>=1.8&(df$height*0.01)<=1.9,]
#
#     - izpisite vse studente, ki so nadpovprecno visoki 
#       (glede na vse osebe v bazi podatkov).
df[df$student&df$height>mean(df$height),]
#
#     - uredite osebe po starosti. 
#       (Namig: uporabite funkcijo "order")
df[order(df$age),]

