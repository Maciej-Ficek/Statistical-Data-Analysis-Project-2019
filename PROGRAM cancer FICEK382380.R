cancer<-load("C:/Users/Maciej/Desktop/SAD/PROGRAM/cancer.Rdata",verbose=TRUE)
data.train<-as.data.frame(data.train)
## wybor pewnej iczby predyktorow
korelacje<-numeric(17737)
for( i in 1:17737)
  korelacje[i]<-cor(data.train[i],data.train[17738])
korelacje
wybor<-numeric(17737)
for( i in 1:17737)
  if(abs(korelacje[i])>=0.29) wybor[i]=1
wybor
sum(wybor==1)
WYBOR<-c(wybor==1)
WYBOR<-as.vector(WYBOR)
vybor<-c(0)
for(i in 1:17737)
if (WYBOR[i]==TRUE) vybor<-c(vybor,i)
WYBOR<-vybor[2:132]
#### model liniowy
model<-lm(Y~.,data=dane)
summary(model)
predykcja1<-predict(model,data.test)
# predykcja 1 - predykcja modelu liniowego dla danych testowych
# MSE modelu lniowego dla danych treningowych
MSEregresjaliniowa<-sum((data.train[,17738]-predict(model,data.train))**2)
### Model drugi - drzewo
library(MASS)
library(rpart)
drzewo<-rpart(Y~.,data=dane)
plot(drzewo)
text(drzewo)
predykcja2<-predict(drzewo,data.test)
# predykcja2 - predykcja dla danych testowych dla modelu drzewo
# MSE drzewa dla danych treningowych
MSEdrzewo<-sum((predict(drzewo,data.train)-data.train[,17738])**2)
#### model 3 - regresja grzbietowa
library(glmnet)
dane<-as.matrix(dane)
model3<-glmnet(dane[,-132],dane[,132],alpha=0)
summary(model3)
model3$lambda
coef(model3)
coefs<-coef(model3)
predykcja3<-numeric(254)
# parametr sciagajacy dobralem "na oko", sprawdzajac wiele wartosci
# az uzyskalem zblizone predykcje do modelu liniowego i drzewa
for( i in 1:254)
predykcja3[i]<-sum(data.test[i,WYBOR]*coefs@x[2+20*132:132+20*132])
predykcja3<-predykcja3+coefs@x[1+20*132]
### predykcja3 - predykcja regresji grzbietowej dla danych testowych

colnames(dane)
  
  