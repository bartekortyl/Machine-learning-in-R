#----------------statystyki i wykresy

summary(diabetes$glucose)
summary(diabetes$pressure)
summary(diabetes$mass)
summary(diabetes$age)
summary(diabetes$triceps)
summary(diabetes$insulin)


dane<-data.frame(diabetes$glucose, diabetes$pressure, diabetes$mass, diabetes$age, diabetes$triceps, diabetes$insulin, diabetes$diabetes)

library(dplyr)
dane <-mutate(dane, dane$diabetes.diabetes)
summary(dane)


par(mfrow=c(2,3))
boxplot(cukrzyca$diabetes.glucose, main = "glucose")
boxplot(cukrzyca$diabetes.pressure, main = "pressure")
boxplot(cukrzyca$diabetes.age, main = "age")
boxplot(cukrzyca$diabetes.mass, main = "mass")
boxplot(cukrzyca$diabetes.triceps, main = "triceps")
boxplot(cukrzyca$diabetes.insulin, main = "insulin")

nrow(dane)

cukrzyca <- dane[!(dane$diabetes.glucose==0 | dane$diabetes.pressure== 0 |
                         dane$diabetes.mass == 0 | dane$diabetes.age == 0 | dane$diabetes.insulin == 0 | dane$diabetes.triceps == 0),]

library("corrplot")

macierz_korelacji <- cukrzyca

for (j in 1:392)
{
  if(as.character(macierz_korelacji[j,6]) == "pos")
  {
    macierz_korelacji[j,6]= 1
  }
  else 
  {
    macierz_korelacji[j,6]= 0
  }
}
macierz_korelacji[,6] <- as.numeric(macierz_korelacji[,6])
macierz_korelacji <- cor(macierz_korelacji[1:6])
corrplot(macierz_korelacji, method="color", tl.col = "orange3", addgrid.col ="black",
         addCoef.col = "black")

#----------------Podzia³ na zbiór ucz¹cy i testowy

set.seed(9)

index <- sample(392,300,replace = F)       
uczacy.cukrzyca <- cukrzyca[index,]
testowy.cukrzyca <- cukrzyca[-index,]

# standaryzacja danych
uczacy.cukrzyca1 <- as.data.frame(scale(uczacy.cukrzyca[,-7]))
uczacy.cukrzyca1$diabetes.diabetes <- uczacy.cukrzyca$diabetes.diabetes
testowy.cukrzyca1 <- as.data.frame(scale(testowy.cukrzyca[,-7]))
testowy.cukrzyca1$diabetes.diabetes <- testowy.cukrzyca$diabetes.diabetes

# statystyki zbioru testowego i uczacego

summary(uczacy.cukrzyca)
summary(testowy.cukrzyca)  



#-------------------------------------------------------------------------------------------------------------
#
#                           Metoda k najblizszych sasiadow(KNN)
#
#-------------------------------------------------------------------------------------------------------------

#wybór k dla knn

acc <- c()
for (i in 1:30){
  
  cukrzyca.knn <- knn(train = uczacy.cukrzyca1[,-7],          #wszystkie poza zmienna y 
                     test = testowy.cukrzyca1[,-7],
                     cl = uczacy.cukrzyca1[, 7],           # okresla co prognozujemy, u mnie 7 kol
                     k = i )
  
  t = table(cukrzyca.knn, testowy.cukrzyca1[, 7])
  acc[i] = (t[1,1] + t[2,2])/sum(t)
}

library(ggplot2)

ggplot(data.frame(acc, k =1:30))+
  geom_line(aes(x = k, y = acc))+
  scale_x_continuous(breaks = 1:30)
acc[16]

#--------------------------------
library(class)
#zbiór uczacy
knn.uczacy <- knn(train = uczacy.cukrzyca1[,-7],          #wszystkie poza zmienna y 
                  test = uczacy.cukrzyca1[,-7],
                  cl = uczacy.cukrzyca1[, 7],           # okresla co prognozujemy, u mnie 7 kol
                  k = 3                            # liczba sasiadow ktora chce uwzglednic
                    )
knn.uczacy

#macierz bledu

tmb<- table(knn.uczacy, uczacy.cukrzyca1$diabetes.diabetes)
tmb

#dokladnosc na zbiorze uczacym
(tmb[1,1] + tmb[2,2])/sum(tmb)


#zbiór testowy

knn.testowy <- knn(train = testowy.cukrzyca1[,-7],          #wszystkie poza zmienna y 
                  test = testowy.cukrzyca1[,-7],
                  cl = testowy.cukrzyca1[, 7],           # okresla co prognozujemy, u mnie 7 kol
                  k = 3                            # liczba sasiadow ktora chce uwzglednic
)
knn.testowy

tmb2<- table(knn.testowy, testowy.cukrzyca1$diabetes.diabetes)
tmb2
#dokladnosc na zbiorze testowym
(tmb2[1,1] + tmb2[2,2])/sum(tmb2)

#-------------------------------------------------------------------------------------------------------------
#
#                                  Bayes
#
#---------------------------------------------------------------------------------------------------------------

library(e1071)
cukrzyca <- cukrzyca[, -c(5)]

colnames(cukrzyca)[1] <- "Glukoza"
colnames(cukrzyca)[2] <- "Cisnienie"
colnames(cukrzyca)[3] <- "Insulina"
colnames(cukrzyca)[4] <- "BMI"
colnames(cukrzyca)[5] <- "Wiek"
colnames(cukrzyca)[6] <- "Klasyfikacja"

# Zamiana charakteru zmiennej  z ilosciowego na kategoryczny

bayes.cukrzyca <- as.data.frame(cukrzyca)

for (i in 1:nrow(bayes.cukrzyca))
{
  if(bayes.cukrzyca[i,1] < 140)
  {
    bayes.cukrzyca[i,1] = 1 # dobra glukoza
  }
  else
  {
    bayes.cukrzyca[i,1] = 2 #  nieprawidlowa glukoza
  }
}



for (i in 1:nrow(bayes.cukrzyca))
{
  if(bayes.cukrzyca[i,2] >= 50 & bayes.cukrzyca[i,2] <=90)
  {
    bayes.cukrzyca[i,2] = 1 # Cisnienie rozkurczowe prawidlowe
  }
  else if((bayes.cukrzyca[i,2] > 90))
  {
    bayes.cukrzyca[i,2] = 2 # Nadcisnienie rozkurczowe
  }
  else
  {
    bayes.cukrzyca[i,2] = 3 # Niedocisnienie rozkurczowe
  }
}



for (i in 1:nrow(bayes.cukrzyca))
{
  if(bayes.cukrzyca[i,3] >= 100 & bayes.cukrzyca[i,3] <= 225)
  {
    bayes.cukrzyca[i,3] = 1 # Prawidlowy poziom insuliny
  }
  else if((bayes.cukrzyca[i,3] < 100))
  {
    bayes.cukrzyca[i,3] = 2 # Za niski poziom insuliny
  }
  else
  {
    bayes.cukrzyca[i,3] = 3 # Za wysoki poziom insuliny
  }
}



for (i in 1:nrow(bayes.cukrzyca))
{
  if(bayes.cukrzyca[i,4] >= 18.5 & bayes.cukrzyca[i,4] <= 24.99)
  {
    bayes.cukrzyca[i,4] = 1 # Waga prawidlowa
  }
  else if((bayes.cukrzyca[i,4] < 24.99))
  {
    bayes.cukrzyca[i,4] = 2 # Zbyt mala waga
  }
  else
  {
    bayes.cukrzyca[i,4] = 3 # Zbyt duza waga
  }
}



for (i in 1:nrow(bayes.cukrzyca))
{
  if(bayes.cukrzyca[i,5] >= 35 & bayes.cukrzyca[i,5] <= 55)
  {
    bayes.cukrzyca[i,5] = 1 # Sredni wiek
  }
  else if((bayes.cukrzyca[i,5] < 35))
  {
    bayes.cukrzyca[i,5] = 2 # Mlody wiek
  }
  else
  {
    bayes.cukrzyca[i,5] = 3 # Stary wiek
  }
}

# Podzial danych na zbior uczacy i testowy bayes

index.bayes <- sample(392, 300, replace = F)
uczacy.bayes <- bayes.cukrzyca[index.bayes,]
testowy.bayes <- bayes.cukrzyca[-index.bayes,]

#statystyki
summary(uczacy.bayes)
summary(testowy.bayes)
#---------------------------uczacy

#klasyfikator naiwny bayesa

cukrzyca.nb <- naiveBayes(uczacy.bayes$Klasyfikacja~., data = uczacy.bayes)
cukrzyca.nb

# prognoza na zbiorze u
nb.prog.ucz <- predict(cukrzyca.nb, uczacy.bayes)
nb.prog.ucz

head(predict(cukrzyca.nb, uczacy.bayes, type = "raw"))


#macierzbledow
tmb2 <- table(nb.prog.ucz, uczacy.bayes$Klasyfikacja)
tmb2
#dokladnosc
sum(diag(tmb2))/sum(tmb2)

#---------------------------testowy

#klasyfikator naiwny bayesa

cukrzyca.nb2 <- naiveBayes(testowy.bayes$Klasyfikacja~., data = testowy.bayes)
cukrzyca.nb2

# prognoza na zbiorze u
nb.prog.test <- predict(cukrzyca.nb2, testowy.bayes)
nb.prog.test

head(predict(cukrzyca.nb2, testowy.bayes, type = "raw"))


#macierzbledow
tmb2 <- table(nb.prog.test, testowy.bayes$Klasyfikacja)
tmb2
#dokladnosc
sum(diag(tmb2))/sum(tmb2)


#------------------------Krzywa ROC i AUC


library(pROC)

# AUC dla zbioru u

ROC.uczacy <- roc(uczacy.bayes$Klasyfikacja, as.numeric(nb.prog.ucz))
auc(ROC.uczacy)

# AUC dla zbioru t

ROC.testowy <- roc(testowy.bayes$Klasyfikacja ,as.numeric(nb.prog.test))
auc(ROC.testowy)

# Krzywa ROC (Bayes)

ggroc(list(Uczacy = ROC.uczacy, Testowy = ROC.testowy), size=1.3)+
  labs(colour="Zbior:")+
  geom_abline(intercept = 1, color="darkgreen", linetype="dashed")


auc(ROC.testowy)
auc(ROC.uczacy)
