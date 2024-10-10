library("haven")
library("dplyr")
library("tidyr")
library("pander")
library(foreign)
library(psych)
library(MASS)
library(VGAM)
library(ordinal)
library(brant)
library(lmtest)
library(generalhoslem)
library(pscl)
library(erer)
library(glm.predict)
library(aod)
library(ggplot2)
library(dplyr)
library(memisc)
#Выгрузим данные
df <- read.csv(file.choose())
df <- subset(df, select = c("INTERIN", "ESTUDIOSSP", "EDADSP", "SEXOSP", "ECIVILLEGALSP", "NHIJOSD", "SITURESSP"))
colnames(df) <- c("income", "edu", "age", "gender", "ms", "child", "hisp") #переименуем столбцы
df <- na.omit(df)
#Преобразуем данные
df$income[df$income < 3] <- 1
df$income[df$income == 3] <- 2
df$income[df$income == 4] <- 2
df$income[df$income == 5] <- 2
df$income[df$income > 5] <- 3

df$gender[df$gender == 1] <- 0
df$gender[df$gender == 6] <- 1
df$ms[df$ms > 2] <- 3
df$hisp[df$hisp == 6] <- 0

df$income <- as.factor(df$income) #Преобразуем отклик


ologit.hh <- polr(income ~ edu + age + gender + ms + child + hisp, Hess=TRUE,
                    data=df, method="logistic") #Оцениваем модель 
pander(summary(ologit.hh))
exp(coef(ologit.hh))

logitgof(df$income, fitted(ologit.hh), ord = TRUE) #тест Хосмера-Лемешова
ologit.clm <- clm(income ~ edu + age + gender + ms + child + hisp, Hess=TRUE,
                  data=df, link = "logit")

wald.test(b = coef(ologit.clm ), Sigma = vcov(ologit.hh), Terms = 1) # образование

pred_class <- predict(ologit.hh, df)
head(pred_class, 3)
caret::confusionMatrix(pred_class, df$income))
