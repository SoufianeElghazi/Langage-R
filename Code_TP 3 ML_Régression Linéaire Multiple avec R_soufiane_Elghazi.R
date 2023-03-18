###########################################
############___ELGHAZI_SOUFIANE___#########
############_________ICSD_________#########
############_________N°13_________#########
###########################################


setwd("C:\\Users\\elgha\\OneDrive\\Bureau\\my data\\ESI\\cours\\cycle_2année\\S4\\Modèles de régression\\TP3")
##I.Exploration du dataset
### installer les packages :
                        #install.packages("glmnet")
                        #install.packages("car")
                        #install.packages("carData")
                        #install.packages("questionr")
### Importer le dataset
happiness<-read.table("C:/Users/elgha/OneDrive/Bureau/my data/ESI/cours/cycle_2année/S4/Modèles de régression/TP2/Happiness.csv",sep=";",dec=".",header = T,row.names = 1)
### Afficher les premières lignes
head(happiness)
### Afficher les dernières lignes
tail(happiness)
### Afficher la structure du dataset et le résumé des variables
str(happiness)
### Afficher la structure du dataset et le résumé des variables
summary(happiness)
### Afficher la description des variables
library(questionr)
describe(happiness)
### Afficher la matrice de corrélation entre les variables
####                       Algébriquement:
round(cor(happiness[,-1]),2)
####                       Graphiquement :
pairs(happiness[,-1])
## II.Régression linéaire multiple
### Faire une RLM pour prédire la variable Score en fonction des autres variable
rlm<- lm(Score~.-Overall.rank,data=happiness)
rlm
summary(rlm)
## Faire une RLM pour prédire la variable Score en fonction des autres variables après avoir écarté les variables les moins significatives
rlm1<- lm(Score~.-Overall.rank-Generosity,data=happiness)
rlm1
summary(rlm1)

##  Comparer les deux modèles:
##          ◦ Critère d’information d’Akaike (plus c’est petit, mieux est le modèle)
AIC(rlm,rlm1)
##          ◦ Critère d’information Bayesien (plus c’est petit, mieux est le modèle)
BIC(rlm,rlm1)


## À partir du deuxième modèle, refaire une RLM pour prédire la variable Score en fonction des autres variables après avoir écarté les variables les moins significatives
rlm2<- lm(Score~.-Overall.rank-Generosity-Perceptions.of.corruption,data=happiness)
rlm2
summary(rlm2)

##  Comparer les trois modèles:
##          ◦ Critère d’information d’Akaike (plus c’est petit, mieux est le modèle)
AIC(rlm,rlm1,rlm2)
##          ◦ Critère d’information Bayesien (plus c’est petit, mieux est le modèle)
BIC(rlm,rlm1,rlm2)

## utilisant extractAIC()
extractAIC(rlm)
extractAIC(rlm1)
extractAIC(rlm2)

#####------> on conclu que le modele rlm1 est mieux parmi ces modeles mais cela ne signifie que c'est le meilleur!!!


### Afficher les coefficients du modèle
coef(rlm1)
### Afficher les résidus du modèle
residuals(rlm1)

confint.default(rlm1)
# et on constate que l'intervalle de confiance pour les pontes ne contient pas zero
# alors c'est bon/il ya une bonne corrélation entre les 
# variables


### Qualité du modèle
##      Le R² ajusté:
#             On a la valeur de R deux (R²) est 0.7787 est proche de 1 ce que signifie que le model est bon.
##      Adjusted R-squared:
#             Adjusted R-squared:  0.7713 

### Tester la validité du modèle
#       Le modèle de rlm1 est considéré comme valide si les résidus sont:

###            1. Indépendants:
##                     ◦ Test de Durbin-Watson:
#                             Tester l’hypothèse nulle H0 = les résidus sont indépendants.
#                             Si la p-value < 0,05 l’hypothèse est rejetée --
library("carData")
library("car")
durbinWatsonTest(rlm1)

#---------->H0 rejeté!!!! p-value=0.048

###           2. Distribués selon une loi normale de moyenne 0
#                     ◦ Test de Shapiro-Wilk
#                              Tester H0 = les résidus suivent une loi normale. 
#                              Si la p-value < 0,05 l’hypothèse est rejetée
residus<- residuals(rlm1)
shapiro.test(residus)

#---------->H0 rejeté!!!! p-value=0.01891

###           3. Distribués de façon homogène (i.e. varianceconstante)
#                     ◦ Test de Breush-Pagan
#                             Tester H0 = les résidus sont distribués de façon homogène.
#                              Si la p-value < 0,05 l’hypothèse est rejetée
ncvTest(rlm1)

#---------->H0 accepté!!!! p-value=0.19931


##Critere1:courbe residuels vs valeurs predits par model

plot(rlm1)
##Critere2:QQ-plot
plot(rlm1)


### Utilisation du modèle
observation<-data.frame(Overall.rank=0,GDP.per.capita=0.949,Social.support=1.265,Healthy.life.expectancy=0.831,Freedom.to.make.life.choices=0.470,Generosity=0,Perceptions.of.corruption=0.047)
predict(rlm1,newdata=observation)


##      ◦ Intervalle de confiance
#                 La vraie valeur de Score pour cette observation a une
#                 probabilité de 0,95 d’être dans l’intervalle [5.5136 , 5.794441]
predict(rlm1,newdata=observation,interval="confidence",level=0.95)

##      ◦ Intervalle de prédiction
#                 L’intervalle [4.59766  , 6.710381] a une probabilité de 0,95 de
#                 contenir la prédiction de Score pour une nouvelle
#                 observation qui a Overall.rank=0,GDP.per.capita=0.949,Social.support=1.265,Healthy.life.expectancy=0.831
#                 ,Freedom.to.make.life.choices=0.470,Generosity=0,Perceptions.of.corruption=0.047
predict(rlm1,newdata=observation,interval="prediction",level=0.95)

## Explorer l’utilisation de la fonction glmnet() pour réaliser une régression régularisée

#   ◦ Adapter les données au format exigé par glmnet():
x_vars<-data.matrix(happiness[,-c(1,2)])
y<-data.matrix(happiness[,2])
#   ◦ Préparer la liste des valeurs de lambda à tester
lambdas<-10^seq(2,-2,-0.1)
#   ◦  Réaliser une régression ridge:
library(glmnet)
library(Matrix)
reg_ridge = glmnet(x_vars,y,nlambda=50,alpha=0,family='gaussian',lambda=lambdas)
reg_ridge
#   ◦ Trouver la meilleure valeur de lambda
reg_ridge_cv = cv.glmnet(x_vars,y,alpha=0,nlambda=50,family='gaussian',lambda=lambdas)
reg_ridge_cv$lambda.min

#   ◦ appliquer sur la valeur de lambda trouvé:

lambda_opt<- reg_ridge_cv$lambda.min
reg_ridge = glmnet(x_vars,y,alpha=0,nlambda=50,family='gaussian',lambda=lambda_opt)
reg_ridge














