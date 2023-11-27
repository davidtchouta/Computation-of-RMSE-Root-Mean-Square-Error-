library(tidyverse)
library(caret)
library(MASS)
set.seed(1)

n<-100
sigma <- 9*matrix(c(1.0, 0.5,0.5, 1.0), 2, 2)

dat <- mvrnorm(n=100, c(69, 69), sigma) %>%
  data.frame() %>% setNames((c("x", "y")))

#Q1 :
#Nous allons construire 100 modèles linéaires en utilisant les données ci-dessus
#et calculer la moyenne et l'écart-type des modèles combinés.

#Ensuite, dans une boucle replicate(), 
#(1) partitionnez l'ensemble de données en 
#ensembles de test et d'entraînement avec p = 0,5 et en utilisant dat$y pour générer vos indices

#(2) entraînez un modèle linéaire prédisant y à partir de x, 
#(3) générez des prédictions sur l'ensemble de test, et (4) calculez la RMSE de ce modèle.

#(4) Indiquez ensuite la moyenne et l'écart-type (ET) des RMSE de l'ensemble des 100 modèles.

rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat %>% slice(test_index)
  test_set <- dat %>% slice(-test_index)
  fit <- lm(y~x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
})

mean(rmse)
sd(rmse)


#Q2 changer la taille n <- c(100, 500, 1000, 5000, 10000) , utiliser une fonction map() ou sapply()

n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  sigma <- 9*matrix(c(1.0, 0.5,0.5, 1.0), 2, 2)
  
  dat <- mvrnorm(n, c(69, 69), sigma) %>%
    data.frame() %>% setNames((c("x", "y")))

  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
    train_set <- dat %>% slice(test_index)
    test_set <- dat %>% slice(-test_index)
    fit <- lm(y~x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat-test_set$y)^2))
    
  })
  c(avg=mean(rmse), sd=sd(rmse))
  
})

res

#Q3 : Que constatez-vous au regard de la question 2 ? 


#Q4: Créeons un coorélation forte entre y et x :

sigma <- 9*matrix(c(1.0, 0.95,0.95, 1.0), 2, 2)

dat <- mvrnorm(n=100, c(69, 69), sigma) %>%
  data.frame() %>% setNames((c("x", "y")))

rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat %>% slice(test_index)
  test_set <- dat %>% slice(-test_index)
  fit <- lm(y~x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
})

#Q5 : Calculer le rmse avec un modèle à deux variables explicatives : x_1 et x_2

sigma <- matrix(c(1.0, 0.75,0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)

dat <- mvrnorm(n=100, c(0, 0, 0), sigma) %>%
  data.frame() %>% setNames((c("y", "x_1", "x_2")))

test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
train_set <- dat %>% slice(test_index)
test_set <- dat %>% slice(-test_index)


#Rmse avec juste x_1
fit <- lm(y~x_1, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))


#Rmse avec juste x_2
fit <- lm(y~x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))


#Rmse avec à la fois x_1 et x_2
fit <- lm(y~x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))

# Le meilleur modèle est celui qui utilise à la fois les deux variables x_1 et x_2 car le rmse est le
# plus faible 

#Q6 : Que se passe-t-il si la corrélation entre les variables explicatives (x_1 et x_2) est forte ?
# quel impact au niveau du rmse ?

sigma <- matrix(c(1.0, 0.75,0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)


test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
train_set <- dat %>% slice(test_index)
test_set <- dat %>% slice(-test_index)

#Rmse avec juste x_1
fit <- lm(y~x_1, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))


#Rmse avec juste x_2
fit <- lm(y~x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))


#Rmse avec à la fois x_1 et x_2
fit <- lm(y~x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))

# le rmse est bas (donc le modèle est meilleur) lorsque nous utilisons les deux variables x_1 et x_2
# mais 


