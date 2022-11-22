library(tidyverse)
library(readxl)
library(lfe)
library(broom)

DiD <- read_excel("C:/Users/USUARIO/Desktop/DiD.xlsx", 
                  col_types = c("numeric", "numeric", "text", 
                                "numeric", "numeric", "text"))

View(DiD)

# variables dummy para treatment

DiD <- DiD %>% filter(Año < 2019.5) %>%
              mutate(Dummy_crisis = (Año > 2008),
               Dummy_reconversion = (Año > 1992),
               treat_group = (Provincia %in% "Murcia"),
               treat_reconversion = Dummy_reconversion*treat_group,
               treat_crisis = Dummy_crisis*treat_group)

View(DiD)

# gráfico chulo

DiD %>% ggplot(aes(x=Año, y= Voto, group = Provincia, color = Provincia)) +
  geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept=c(1992, 2008), linetype= "dashed") + 
  ylab("Porcentaje de Voto al PSOE")

# DiD
# si lo hacemos así en lfe no salen los coeficientes de los fixed effects 
# | 0 | Provincia es clusterizar los errores por la evolución intra-grupo que puedan tener. Como solo tenemos dos grupos no es necesario
felm(Voto ~ treat_reconversion | Dummy_reconversion + treat_group | 0 | Provincia, DiD) %>% tidy()

felm(Voto ~ treat_reconversion | Dummy_reconversion + treat_group, DiD) %>% tidy()

# Vemos ahora si se cumple con la crisis del 2008
felm(Voto ~ treat_crisis | Dummy_crisis + treat_group, DiD) %>% tidy()


# Como no podemos controlar en ambas variables, hacemos un anova para Murcia solo
a <- DiD %>% filter(Provincia == "Murcia")

summary(aov(Voto ~ Dummy_reconversion + RpCap + Paro, a))


summary(aov(Voto ~ Dummy_crisis + RpCap + Paro, a))

b <- DiD %>% filter(Provincia == "Sevilla")

summary(aov(Voto ~ Dummy_crisis, b))

summary(aov(Voto ~ Dummy_reconversion, b))

# Boxplot
DiD %>% ggplot(aes(x=Dummy_reconversion, y= Voto, color=Provincia)) + geom_boxplot() +
  theme_bw() + xlab("Reconversión industrial: antes y después")

DiD %>% ggplot(aes(x=Dummy_crisis, y= Voto, color=Provincia)) + geom_boxplot() +
  theme_bw() + xlab("Crisis económica: antes y después")


# Efectos DiD

df <- data.frame(efecto = c(-13.7, -3.99), em = c(1.64*7.81, 1.64*7.60), Periodo = c("reconversión", "crisis"))

df %>% ggplot(aes(x=Periodo, y=efecto, color=Periodo)) + geom_point(size=2) + 
  geom_errorbar(aes(ymin=efecto-em, ymax=efecto+em), width=.2) + theme_bw() +
  ylab("Efecto diferencial en el voto") + labs(subtitle = "CI 90%")