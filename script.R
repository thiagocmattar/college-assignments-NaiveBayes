rm(list=ls())

pdfnvar<- function(x,m,K,n) ((1/(sqrt((2*pi)^n*(det(K)))))*
                               exp(-0.5*(t(x-m) %*% (solve(K)) %*% (x-m))))

data(iris)
plot(iris)

#1. Divisão da base de dados em classes
ic1<-which(iris$Species=='versicolor')
ic2<-which(iris$Species=='setosa' | iris$Species=='virginica')

c1<-iris[ic1,]
c2<-iris[ic2,]
summary(c1)
summary(c2)

#2. Divisão das classes em treino e teste
set.seed(10)
ACC<-c()
for(k in 1:30)
{
  c1<-as.matrix(c1[sample(1:nrow(c1)),1:4])
  c1.train<-c1[1:(0.7*nrow(c1)),]
  c1.test<-c1[(0.7*nrow(c1)+1):nrow(c1),]
  
  c2<-as.matrix(c2[sample(1:nrow(c2)),1:4])
  c2.train<-c2[1:(0.7*nrow(c2)),]
  c2.test<-c2[(0.7*nrow(c2)+1):nrow(c2),]
  
  #3. Média e matriz de covariância para os dados de teste
  m1<-c(mean(c1.train[,1]),mean(c1.train[,2]),mean(c1.train[,3]),mean(c1.train[,4]))
  K1<-cov(c1.train[,1:4])
  m2<-c(mean(c2.train[,1]),mean(c2.train[,2]),mean(c2.train[,3]),mean(c2.train[,4]))
  K2<-cov(c2.train[,1:4])
  
  #4. Funções de probabilidade para cada amostra e classe
  px1c1<-c()
  px1c2<-c()
  for(i in 1:nrow(c1.train))
  {
    px1c1[i]<-pdfnvar(c1.train[i,],m1,K1,4)*nrow(c1.train)/(nrow(c1.train)+nrow(c2.train))
    px1c2[i]<-pdfnvar(c1.train[i,],m2,K2,4)*nrow(c2.train)/(nrow(c1.train)+nrow(c2.train))
  }
  
  px2c1<-c()
  px2c2<-c()
  for(i in 1:nrow(c2.train))
  {
    px2c1[i]<-pdfnvar(c2.train[i,],m1,K1,4)*nrow(c1.train)/(nrow(c1.train)+nrow(c2.train))
    px2c2[i]<-pdfnvar(c2.train[i,],m2,K2,4)*nrow(c2.train)/(nrow(c1.train)+nrow(c2.train))
  }
  
  #5. Estimativa de classes para dados de treinamento
  y<-vector(mode="numeric",length=(nrow(c1.train)+nrow(c2.train)))
  y[1:nrow(c1.train)]<-1
  
  y1hat<-1*(px1c1>px1c2)
  y2hat<-1*(px2c1>px2c2)
  yhat<-c(y1hat,y2hat)
  
  table(y,yhat)
  
  #5. Estimativa de classes para dados de teste
  px1c1<-c()
  px1c2<-c()
  for(i in 1:nrow(c1.test))
  {
    px1c1[i]<-pdfnvar(c1.test[i,],m1,K1,4)*nrow(c1.test)/(nrow(c1.test)+nrow(c2.test))
    px1c2[i]<-pdfnvar(c1.test[i,],m2,K2,4)*nrow(c2.test)/(nrow(c1.test)+nrow(c2.test))
  }
  
  px2c1<-c()
  px2c2<-c()
  for(i in 1:nrow(c2.test))
  {
    px2c1[i]<-pdfnvar(c2.test[i,],m1,K1,4)*nrow(c1.test)/(nrow(c1.test)+nrow(c2.test))
    px2c2[i]<-pdfnvar(c2.test[i,],m2,K2,4)*nrow(c2.test)/(nrow(c1.test)+nrow(c2.test))
  }
  y<-vector(mode="numeric",length=(nrow(c1.test)+nrow(c2.test)))
  y[1:nrow(c1.test)]<-1
  
  y1hat<-1*(px1c1>px1c2)
  y2hat<-1*(px2c1>px2c2)
  yhat<-c(y1hat,y2hat)
  
  cm<-table(y,yhat)
  ACC[k]<-sum(diag(cm))/sum(cm)
}
print(mean(ACC))
print(sd(ACC))
print(c(sd(ACC)/mean(ACC)*100,'%'))

plot(iris$Petal.Length[iris$Species=='setosa'],
     iris$Sepal.Width[iris$Species=='setosa'],
     col='red',xlim=c(0,10),ylim=c(0,10),xlab='',ylab='')
par(new=TRUE)
plot(iris$Petal.Length[iris$Species=='versicolor'],
     iris$Sepal.Width[iris$Species=='versicolor'],
     col='blue',xlim=c(0,10),ylim=c(0,10),xlab='',ylab='')
par(new=TRUE)
plot(iris$Petal.Length[iris$Species=='virginica'],
     iris$Sepal.Width[iris$Species=='virginica'],
     col='green',xlim=c(0,10),ylim=c(0,10),
     xlab='Petal Length',ylab='Sepal Width',
     main='Distribuição espacial Petal Length x Sepal Width')
legend(x=7,y=10,legend=c('Setosa','Versicolor','Virginica'),
       fill=c('red','blue','green'))

plot(iris$Sepal.Length[iris$Species=='setosa'],
     iris$Petal.Width[iris$Species=='setosa'],
     col='red',xlim=c(4,10),ylim=c(0,5),xlab='',ylab='')
par(new=TRUE)
plot(iris$Sepal.Length[iris$Species=='versicolor'],
     iris$Petal.Width[iris$Species=='versicolor'],
     col='blue',ylim=c(0,5),xlim=c(4,10),xlab='',ylab='')
par(new=TRUE)
plot(iris$Sepal.Length[iris$Species=='virginica'],
     iris$Petal.Width[iris$Species=='virginica'],
     col='green',ylim=c(0,5),xlim=c(4,10),
     xlab='Sepal Length',ylab='Petal Width',
     main='Distribuição espacial Sepal Length x Petal Width')
legend(x=8,y=5,legend=c('Setosa','Versicolor','Virginica'),
       fill=c('red','blue','green'))
