library(readr)
library(tidyverse)
library(glm)
library(nnet)
library(ggpubr)
library(ggeffects)
library(sjPlot)


df110 <- read_csv("~/MEGA/Manu/df110.csv", 
                  col_types = cols(...1 = col_skip(), EDAD = col_number(), 
                                   Municipio = col_number()))
View(df110)

df110 <- df110[order(df110$Clase),]

model1 <- glm(Ideología ~ Clase + Educación + Religion + Sexo + EDAD + Municipio, 
            data = df110, family=binomial)
summary(model1)

model2 <- glm(Ideología ~ Clase, 
              data = df110, family=binomial)
summary(model2)


# NOTA: es un ggplot
plot(ggpredict(model1, terms = "Clase")) +  
  labs(title="Probabilidad de votar a la izquierda en 2011 según grupo ocupacional",
       subtitle="frente a la derecha tradicional (CI: 95%)",
       y= " ", x= " ") + geom_hline(yintercept= 0.357)

# Ahora 2019


df190 <- read_csv("~/MEGA/Manu/df190.csv", 
                  col_types = cols(...1 = col_skip(), P42 = col_number(), 
                                   Municipio = col_number()))
View(df190)

model3 <- multinom(Ideología ~ Clase + Educación + Religion + Sexo + P42 + Municipio, 
              data = df190)

summary(model3)

# El multinom no nos sirve bien pa plotear
# Por ello hacemos dos logit binomial separadas

df191 <- df190[!df190$Ideología=='exda',]
View(df191)

# recodificamos numéricamente

df191$Ideología <- recode(df191$Ideología, "izda"=1, "dcha"=0)

df191 <- df191[order(df191$Clase),]


model4 <- glm(Ideología ~ Clase + Educación + Religion + Sexo + P42 + Municipio, 
                   data = df191, family=binomial)
summary(model4)

plot(ggpredict(model4, terms = "Clase")) + 
  labs(title="Probabilidad de votar a la izquierda en 2019 según grupo ocupacional",
       subtitle="frente a la derecha tradicional (CI: 95%)", y= " ", x= " ") +
  geom_hline(yintercept= 0.592)

# Ahora la extrema derecha

df192 <- df190[!df190$Ideología=='dcha',]
View(df192)

df192$Ideología <- recode(df192$Ideología, "izda"=1, "exda"=0)

df192 <- df192[order(df192$Clase),]


model5 <- glm(Ideología ~ Clase + Educación + Religion + Sexo + P42 + Municipio, 
              data = df192, family=binomial)

plot(ggpredict(model5, terms = "Clase")) + 
  labs(title="Probabilidad de votar a la izquierda en 2019 según grupo ocupacional",
       subtitle="frente a la extrema derecha (CI: 95%)", y= " ", x= " ") +
  geom_hline(yintercept= 0.79)
