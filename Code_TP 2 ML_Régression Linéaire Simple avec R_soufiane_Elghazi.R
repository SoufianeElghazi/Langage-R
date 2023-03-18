###########################################
############___ELGHAZI_SOUFIANE___#########
############_________ICSD_________#########
############_________N°13_________#########
###########################################

##I.Exploration du dataset
### installer les packages :
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
## II.Régression linéaire simple
### Faire une RLS pour prédire la variable Score en fonction de la variable GDP.per.capita
rls<- lm(Score~GDP.per.capita,data=happiness)
rls
##__Alors: Score = 3,406 + 2,204 ´ GDP.per.capita__##
### Afficher la droite de régression
      #méthode1:
plot(Score~GDP.per.capita,data = happiness,main="Regression de GDP.per.capita sur Score")
abline(rls,col='red',lwd=3)
    #méthode2:  
library("carData")
library("car")
scatterplot(Score~GDP.per.capita,data = happiness,
            xlab="GDP.per.capita",ylab="Score",main="Regression de GDP.per.capita sur Score"
            ,regLine=TRUE,ellipse=FALSE,smooth = FALSE,grid =TRUE)

### Afficher les coefficients du modèle
coef(rls)
### Afficher les résidus du modèle
residuals(rls)
### Afficher le résumé du modèle
summary(rls)
# on remarque que notre pont est différente à 0(=2.2043)et la valeur significatif 
#  <2e-16 *** est inférieure à 0.05 
confint.default(rls)
# et on constate que l'intervalle de confiance pour la ponte ne contient pas zero
# ([1.935899 , 2.472649]) alors c'est bon/il ya une bonne corrélation entre les deux
# variables
### Qualité du modèle
##      Le R² ajusté:
#             On a la valeur de R deux (R²) est 0.6263 est proche de 1 ce que signifie que le model est bon.
##       Le RSS (Residual Sum of Squares):
#             plus le RSS est petit, plus le modèle est bon, et dans notre cas le RSS =0.6772 n'est pas assez petit!
### Tester la validité du modèle
#       Le modèle de RLS est considéré comme valide si les résidus sont:

###            1. Indépendants:
##                     ◦ Test de Durbin-Watson:
#                             Tester l’hypothèse nulle H0 = les résidus sont indépendants.
#                             Si la p-value < 0,05 l’hypothèse est rejetée --
library("carData")
library("car")
durbinWatsonTest(rls)
###           2. Distribués selon une loi normale de moyenne 0
#                     ◦ Test de Shapiro-Wilk
#                              Tester H0 = les résidus suivent une loi normale. 
#                              Si la p-value < 0,05 l’hypothèse est rejetée
residus<- residuals(rls)
shapiro.test(residus)
###           3. Distribués de façon homogène (i.e. varianceconstante)
#                     ◦ Test de Breush-Pagan
#                             Tester H0 = les résidus sont distribués de façon homogène.
#                              Si la p-value < 0,05 l’hypothèse est rejetée
ncvTest(rls)

###Critere1:courbe residuels vs valeurs predits par model
plot(rls)
###Critere2:QQ-plot
plot(rls)
### Élaborer d’autres modèles de RLS pour expliquer la variable Score
##                  model_2
rls2<-lm(Score~Social.support,data = happiness)
rls2
      #méthode1:
plot(Score~Social.support,data = happiness)
abline(rls2,col='blue',lwd=3)
      #méthode2:
scatterplot(Score~Social.support,data = happiness,xlab="Social.support",ylab="Score",main="Regression de Social.support sur Score",regLine=TRUE,ellipse=FALSE,smooth = FALSE,grid =TRUE)
##                  model_3
rls3<-lm(Score~Healthy.life.expectancy,data = happiness)
rls3
      #méthode1:
plot(Score~Healthy.life.expectancy,data = happiness)
abline(rls3,col='green',lwd=3)
      #méthode2:
scatterplot(Score~Healthy.life.expectancy,data = happiness,xlab="Healthy.life.expectancy",ylab="Score",main="Regression de Healthy.life.expectancy sur Score",regLine=TRUE,ellipse=FALSE,smooth = FALSE,grid =TRUE)
##                  model_4
rls4<-lm(Score~Freedom.to.make.life.choices,data = happiness)
rls4
    #méthode1:
plot(Score~Freedom.to.make.life.choices,data = happiness)
abline(rls4,col='orange',lwd=3)
    #méthode2:
scatterplot(Score~Freedom.to.make.life.choices,data = happiness,xlab="Freedom.to.make.life.choices",ylab="Score",main="Regression de Freedom.to.make.life.choices sur Score",regLine=TRUE,ellipse=FALSE,smooth = FALSE,grid =TRUE)
##                  model_5
rls5<-lm(Score~Generosity,data = happiness)
rls5
      #méthode1:
plot(Score~Generosity,data = happiness)
abline(rls5,col='pink',lwd=3)
      #méthode2:
scatterplot(Score~Generosity,data = happiness,xlab="Generosity",ylab="Score",main="Regression de Generosity sur Score",regLine=TRUE,ellipse=FALSE,smooth = FALSE,grid =TRUE)
##                  model_6
rls6<-lm(Score~Perceptions.of.corruption,data = happiness)
rls6
    #méthode1:
plot(Score~Perceptions.of.corruption,data = happiness)
abline(rls6,col='yellow',lwd=3)
    #méthode2:
scatterplot(Score~Perceptions.of.corruption,data = happiness,xlab="Perceptions.of.corruption",ylab="Score",main="Regression de Perceptions.of.corruption sur Score",regLine=TRUE,ellipse=FALSE,smooth = FALSE,grid =TRUE)
### Comparaison entre les modèles
##          ◦ Critère d’information d’Akaike (plus c’est petit, mieux est le modèle)
AIC(rls,rls2,rls3,rls4,rls5,rls6)
##          ◦ Critère d’information Bayesien (plus c’est petit, mieux est le modèle)
BIC(rls,rls2,rls3,rls4,rls5,rls6)
### Utilisation du modèle
new.gdp<-data.frame(GDP.per.capita=1.3)
predict(rls,newdata=new.gdp)

new.gdps<-data.frame(GDP.per.capita=1.3,1.269,0.801)
predict(rls,newdata=new.gdps)
##      ◦ Intervalle de confiance
#                 La vraie valeur de Score pour cette observation a une
#                 probabilité de 0,95 d’être dans l’intervalle [lwr , upr]
predict(rls,newdata=new.gdp,interval="confidence")
##      ◦ Intervalle de prédiction
#                 L’intervalle [lwr , upr] a une probabilité de 0,95 de
#                 contenir la prédiction de Score pour une nouvelle
#                 observation qui a GDP.per.capita = 1,300
predict(rls,newdata=new.gdp,interval="prediction")















