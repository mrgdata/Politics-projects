library(readr)
library(tidyverse)
library(nnet)
library(ggpubr)
library(ggeffects)
library(sjPlot)
library(ggrepel)
library(gridExtra)
library(broom)
library(gt)
library(haven)

df11 <- read_sav("MEGA/MEGA/Manu/Science/voto de clase/2011.sav")

df15 <- read_sav("MEGA/MEGA/Manu/Science/voto de clase/2015.sav")

df16 <- read_sav("MEGA/MEGA/Manu/Science/voto de clase/2016.sav")

df19a <- read_sav("MEGA/MEGA/Manu/Science/voto de clase/2019a.sav")

df19b <- read_sav("MEGA/MEGA/Manu/Science/voto de clase/2019b.sav")

my_recodeclass <- function(P52, OCUMAR11) {
  ifelse(P52 == 4 & OCUMAR11 > 3, "Empresarios",
         ifelse(P52 == 3, "Empresarios",
                ifelse(OCUMAR11 == 1.0 & (P52 < 3 | P52 == 4), "Directivos y gerentes",
                       ifelse(OCUMAR11 == 2.0 & (P52 < 3 | P52 == 4), "Profesionales culturales",
                              ifelse(OCUMAR11 == 3.0 & (P52 < 3 | P52 == 4), "Profesionales técnicos",
                                     ifelse(OCUMAR11 == 4.0 & P52 < 3, "Oficinistas",
                                            ifelse(OCUMAR11 == 5.0 & P52 < 3, "Trabajadores de servicios",
                                                   ifelse((OCUMAR11 == 7.0 | OCUMAR11 == 8.0) & P52 < 3, "Obreros cualificados",
                                                          ifelse((OCUMAR11 == 6.0 | OCUMAR11 == 9.0) & P52 < 3, "Obreros no cualificados",
                                                                 NA)))))))))
}

# Apply the function to each row of the data frame
vec_recodeclass <- Vectorize(my_recodeclass)

# Apply the vectorized function to the dataframe columns
df11$Clase <- vec_recodeclass(df11$C17, df11$C16)

table(df11$Clase)

df15$Clase <- vec_recodeclass(df15$C17, df15$C16)

table(df15$Clase)

df16$Clase <- vec_recodeclass(df16$C17, df16$C16)

table(df16$Clase)

df19a$Clase <- vec_recodeclass(df19a$C17, df19a$C16)

table(df19a$Clase)

df19b$Clase <- vec_recodeclass(df19b$C17, df19b$C16)

table(df19b$Clase)

df11$Comicio = "2011"

df15$Comicio = "2015"

df16$Comicio = "2016"

df19a$Comicio = "2019a"

df19b$Comicio = "2019b"

# recodificar educación
# ver sexo, edad y tamuni
# ver PNAC


df11$EDU = factor(df11$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))

df15$EDU = factor(df15$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))

df16$EDU = factor(df16$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))

df19a$EDU = factor(df19a$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))

df19b$C11A_transformed <- ifelse(df19b$C11A %in% c(0, 1, 2), 1, 
                                 ifelse(df19b$C11A %in% c(3, 5, 7), 2, 
                                        ifelse(df19b$C11A %in% c(4, 6), 3, 
                                               ifelse(df19b$C11A %in% c(8, 9, 10, 11, 12, 13, 14, 15), 4, 
                                                      ifelse(df19b$C11A %in% c(16, 98, 99), 5, NA)))))

# Creating the factor with the proper labels
df19b$EDU <- factor(df19b$C11A_transformed, levels = c(1, 2, 3, 4, 5), 
                     labels = c("Primaria o sin estudios", "FP", "Secundaria", "Estudios superiores", "Secundaria")) # NA imputado a mod

########################################

df11$SEX = factor(df11$SEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))

df15$SEX = factor(df15$PSEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))

df16$SEX = factor(df16$P47, levels = c(1, 2), labels = c("Hombre", "Mujer"))

df19a$SEX = factor(df19a$P41, levels = c(1, 2), labels = c("Hombre", "Mujer"))

df19b$SEX = factor(df19b$C9, levels = c(1, 2), labels = c("Hombre", "Mujer"))




df11$EDAD <- as.integer(df11$EDAD)  # Convert to integer
df11$EDAD <- ifelse(df11$EDAD == 99, NA, df11$EDAD)



df15$EDAD <- as.integer(df15$P28)  # Convert to integer
df15$EDAD <- ifelse(df15$P28 == 99, NA, df15$P28)


df16$EDAD <- as.integer(df16$P48)  # Convert to integer
df16$EDAD <- ifelse(df16$P48 == 99, NA, df16$P48)



df19a$EDAD <- as.integer(df19a$P42)  # Convert to integer
df19a$EDAD <- ifelse(df19a$P42 == 99, NA, df19a$P42)


df19b$EDAD <- as.integer(df19b$C10)  # Convert to integer
df19b$EDAD <- ifelse(df19b$C10 == 99, NA, df19b$C10)


df11$PNAC <- ifelse(df11$p36 %in% c(2, 3, 11), 1, 
                    ifelse(df11$p36 %in% c(1, 4), 2, NA))



df11$LEFT = factor(df11$PNAC, levels = c(1, 2), 
                   labels = c("Izquierda", "Otros"))

df15$PNAC <- ifelse(df15$PRECUERDO %in% c(2, 3, 5, 6, 7, 10), 1, 
                    ifelse(df15$PRECUERDO %in% c(1, 4), 2, NA))


df15$LEFT = factor(df15$PNAC, levels = c(1, 2), 
                   labels = c("Izquierda", "Otros"))


df16$PNAC <- ifelse(df16$RECUERDO16 %in% c(2, 3, 5, 6, 9), 1, 
                     ifelse(df16$RECUERDO16 %in% c(1, 4), 2, NA))


df16$LEFT = factor(df16$PNAC, levels = c(1, 2), 
                    labels = c("Izquierda", "Otros"))

df11$RIG <- NA
df11$EXRIG <- NA

df15$RIG <- NA
df15$EXRIG <- NA

df16$RIG <- NA
df16$EXRIG <- NA


df19a$PNAC <- ifelse(df19a$P23R %in% c(2, 6, 7, 21), 1, 
                     ifelse(df19a$P23R %in% c(1, 4, 14), 2, 
                            ifelse(df19a$P23R %in% c(18), 3, NA)))


df19b$PNAC <- ifelse(df19b$B22R %in% c(2, 5, 6, 7, 21, 50, 67), 1, 
                                 ifelse(df19b$B22R %in% c(1, 4, 14), 2, 
                                        ifelse(df19b$B22R %in% c(18), 3, NA)))


df19a$LEFT = factor(df19a$PNAC, levels = c(1, 2, 3), 
                 labels = c("Izquierda", "Otros", "Otros"))

df19a$RIG = factor(df19a$PNAC, levels = c(1, 2, 3), 
                labels = c("Otros", "Derecha", "Otros"))

df19a$EXRIG = factor(df19a$PNAC, levels = c(1, 2, 3), 
                  labels = c("Otros", "Otros", "Extrema derecha"))


df19b$LEFT = factor(df19b$PNAC, levels = c(1, 2, 3), 
                    labels = c("Izquierda", "Otros", "Otros"))

df19b$RIG = factor(df19b$PNAC, levels = c(1, 2, 3), 
                   labels = c("Otros", "Derecha", "Otros"))

df19b$EXRIG = factor(df19b$PNAC, levels = c(1, 2, 3), 
                     labels = c("Otros", "Otros", "Extrema derecha"))


df11 <- df11 %>%
  mutate(TAMUNI = case_when(
    TAMUNI == 1 ~ 1000,
    TAMUNI == 2 ~ 6000,
    TAMUNI == 3 ~ 35000,
    TAMUNI == 4 ~ 75000,
    TAMUNI == 5 ~ 250000,
    TAMUNI == 6 ~ 700000,
    TAMUNI == 7 ~ 1000000,
    TRUE ~ TAMUNI
  ))

df15 <- df15 %>%
  mutate(TAMUNI = case_when(
    TAMUNI == 1 ~ 1000,
    TAMUNI == 2 ~ 6000,
    TAMUNI == 3 ~ 35000,
    TAMUNI == 4 ~ 75000,
    TAMUNI == 5 ~ 250000,
    TAMUNI == 6 ~ 700000,
    TAMUNI == 7 ~ 1000000,
    TRUE ~ TAMUNI
  ))

df16 <- df16 %>%
  mutate(TAMUNI = case_when(
    TAMUNI == 1 ~ 1000,
    TAMUNI == 2 ~ 6000,
    TAMUNI == 3 ~ 35000,
    TAMUNI == 4 ~ 75000,
    TAMUNI == 5 ~ 250000,
    TAMUNI == 6 ~ 700000,
    TAMUNI == 7 ~ 1000000,
    TRUE ~ TAMUNI
  ))

df19a <- df19a %>%
  mutate(TAMUNI = case_when(
    TAMUNI == 1 ~ 1000,
    TAMUNI == 2 ~ 6000,
    TAMUNI == 3 ~ 35000,
    TAMUNI == 4 ~ 75000,
    TAMUNI == 5 ~ 250000,
    TAMUNI == 6 ~ 700000,
    TAMUNI == 7 ~ 1000000,
    TRUE ~ TAMUNI
  ))

df19b <- df19b %>%
  mutate(TAMUNI = case_when(
    TAMUNI == 1 ~ 1000,
    TAMUNI == 2 ~ 6000,
    TAMUNI == 3 ~ 35000,
    TAMUNI == 4 ~ 75000,
    TAMUNI == 5 ~ 250000,
    TAMUNI == 6 ~ 700000,
    TAMUNI == 7 ~ 1000000,
    TRUE ~ TAMUNI
  ))

df11 <- df11[c("Clase", "EDU", "EDAD", "SEX", "TAMUNI", "Comicio", "LEFT", "RIG", "EXRIG")]
df15 <- df15[c("Clase", "EDU", "EDAD", "SEX", "TAMUNI", "Comicio", "LEFT", "RIG", "EXRIG")]
df16 <- df16[c("Clase", "EDU", "EDAD", "SEX", "TAMUNI", "Comicio", "LEFT", "RIG", "EXRIG")]
df19a <- df19a[c("Clase", "EDU", "EDAD", "SEX", "TAMUNI", "Comicio", "LEFT", "RIG", "EXRIG")]
df19b <- df19b[c("Clase", "EDU", "EDAD", "SEX", "TAMUNI", "Comicio", "LEFT", "RIG", "EXRIG")]


df <- rbind(df11, df15, df16, df19a, df19b)

df <- df[complete.cases(df[c("LEFT")]), ]


# imputar por mediana y moda

variables_to_impute <- c("Clase", "EDU", "salesforEDAD", "SEX", "TAMUNI")

getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (col_name in names(df)) {
  if (is.character(df[[col_name]])) {
    df[[col_name]] <- as.factor(df[[col_name]])
  }
}

for (col in variables_to_impute) {
  if (is.numeric(df[[col]])) {
    median_value <- median(df[[col]], na.rm = TRUE)
    df[[col]][is.na(df[[col]])] <- median_value
  }
 else if (is.factor(df[[col]])) {
  # Impute categorical variables with mode
  mode_value <- getmode(df[[col]])
  df[[col]][is.na(df[[col]])] <- mode_value
}}


###########################################################


filtered_df <- subset(df, Comicio %in% c('2011', '2015', '2016'))

filtered_df$LEFT <- relevel(filtered_df$LEFT, ref = "Otros")

model1 <- glm(LEFT ~ Clase * Comicio + EDU + SEX + EDAD + TAMUNI, 
              data = filtered_df, family=binomial)
summary(model1)

predictions <- ggpredict(model1, terms = c("Clase", "Comicio"))

mean_probability <- mean(predictions$predicted)

# NOTA: es un ggplot
plot(ggpredict(model1, terms = c("Clase", "Comicio")))  + 
  geom_hline(yintercept = mean_probability, linetype = "dashed", color = "black") +
  labs(title="Probabilidad de votar a la izquierda según grupo ocupacional (2011-2016)",
       subtitle="frente a la derecha (CI: 95%)",
       y= " ", x= " ") +
  theme(axis.text.x = element_text(size = 12, face ='bold'))

#########################################
df_clean <- na.omit(df)

df_clean$LEFT <- relevel(df_clean$LEFT, ref = "Otros")

model2 <- glm(LEFT ~ Clase * Comicio + EDU + SEX + EDAD + TAMUNI, 
              data = df_clean, family=binomial)
summary(model2)

predictions <- ggpredict(model2, terms = c("Clase", "Comicio"))

mean_probability <- mean(predictions$predicted)

# NOTA: es un ggplot
g1 <- plot(ggpredict(model2, terms = c("Clase", "Comicio")))  + 
  geom_hline(yintercept = mean_probability, linetype = "dashed", color = "black") +
  labs(title="Izquierda frente a derecha",
       subtitle="",
       y= " ", x= " ") +
  theme(axis.text.x = element_blank(),  # This will remove the x axis text/labels
        axis.title.x = element_blank())
########################################################

df_clean$EXRIG <- relevel(df_clean$EXRIG, ref = "Otros")

model3 <- glm(EXRIG ~ Clase * Comicio + EDU + SEX + EDAD + TAMUNI, 
              data = df_clean, family=binomial)
summary(model3)

predictions <- ggpredict(model3, terms = c("Clase", "Comicio"))

mean_probability <- mean(predictions$predicted)

g2 <- plot(ggpredict(model3, terms = c("Clase", "Comicio")))  + 
  geom_hline(yintercept = mean_probability, linetype = "dashed", color = "black") +
  labs(title="Derecha radical frente a izquierda y derecha tradicional",
       subtitle="",
       y= " ", x= " ") +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, face ='bold'))

ggarrange(g1, g2, ncol=1)


############################################################
tidy_glm <- tidy(model1, conf.int = TRUE, exponentiate = TRUE)

# Use gt to create a nicely formatted table
gt_table <- tidy_glm %>%
  mutate(p.value = signif(p.value, digits = 2)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  gt() %>%
  cols_label(
    term = "Variable",
    estimate = "Estimación (Odds Ratio)",
    conf.low = "IC Inferior (2.5%)",
    conf.high = "IC Superior (97.5%)",
    p.value = "P-valor"
  ) %>%
  fmt_number(
    columns = vars(estimate, conf.low, conf.high),
    decimals = 2
  )

# Print the table
print(gt_table)

tidy_glm <- tidy(model2, conf.int = TRUE, exponentiate = TRUE)

# Use gt to create a nicely formatted table
gt_table <- tidy_glm %>%
  mutate(p.value = signif(p.value, digits = 2)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  gt() %>%
  cols_label(
    term = "Variable",
    estimate = "Estimación (Odds Ratio)",
    conf.low = "IC Inferior (2.5%)",
    conf.high = "IC Superior (97.5%)",
    p.value = "P-valor"
  ) %>%
  fmt_number(
    columns = vars(estimate, conf.low, conf.high),
    decimals = 2
  )

# Print the table
print(gt_table)

tidy_glm <- tidy(model3, conf.int = TRUE, exponentiate = TRUE)

# Use gt to create a nicely formatted table
gt_table <- tidy_glm %>%
  mutate(p.value = signif(p.value, digits = 2)) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  gt() %>%
  cols_label(
    term = "Variable",
    estimate = "Estimación (Odds Ratio)",
    conf.low = "IC Inferior (2.5%)",
    conf.high = "IC Superior (97.5%)",
    p.value = "P-valor"
  ) %>%
  fmt_number(
    columns = vars(estimate, conf.low, conf.high),
    decimals = 2
  )

# Print the table
print(gt_table)
###########################################################
dfess <- read_sav("MEGA/MEGA/Manu/Science/voto de clase/ESS9.sav")

dfess <- subset(dfess, cntry == "ES")

table(dfess$class16_r)


dfess$class8_p[dfess$class16_r == 2 | dfess$class16_r == 9 | dfess$class16_r == 10] <- 1
dfess$class8_p[dfess$class16_r == 1 | dfess$class16_r == 3 | dfess$class16_r == 4] <- 2
dfess$class8_p[dfess$class16_r == 5 | dfess$class16_r == 6] <- 3
dfess$class8_p[dfess$class16_r == 7] <- 4
dfess$class8_p[dfess$class16_r == 8] <- 5
dfess$class8_p[dfess$class16_r == 11 | dfess$class16_r == 12] <- 6
dfess$class8_p[dfess$class16_r == 13 | dfess$class16_r == 14] <- 7
dfess$class8_p[dfess$class16_r == 15 | dfess$class16_r == 16] <- 8


dfess$class8_p = factor(dfess$class8_p, levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                        labels = c("Directivos y gerentes",
                                   "Empresarios",
                                   "Profesionales técnicos",
                                   "Obreros cualificados",
                                   "Obreros no cualificados",
                                   "Oficinistas",
                                   "Profesionales culturales",
                                   "Trabajadores de servicios"))


table(dfess$class8_p)

dfess$Ideology <- ifelse(dfess$prtvtees %in% c(2, 3, 4, 7, 8, 9), "Izquierda", 
                     ifelse(dfess$prtvtees %in% c(1, 5, 17), "Derecha", 
                            ifelse(dfess$prtvtees %in% c(16), "Extrema derecha", NA)))


dfess$gndr = factor(dfess$gndr, levels = c(1, 2), labels = c("Female", "Male"))

dfess$EDLEVR = factor(dfess$EDLEVR, levels = c(1, 2, 3, 4, 5), labels = c("Primaria o menos", "Secundaria", "Secundaria", "FP", "Estudios superiores"))

dfess$domicil = factor(dfess$domicil, levels = c(1, 2, 3, 4, 5, 7, 8, 9), 
                    labels = c("A big city", "Suburbs", "Small city", "Village", "Village", NA, NA, NA))


for (col in c("agea", "FAC1_1", "FAC2_1", "FAC2_1", "FAC3_1", "FAC4_1", "atchctr")) {
  dfess[[col]] <- as.numeric(dfess[[col]])
}

# FAC1_1 Inmigración, FAC2_1 Confianza instituciones, FAC3_1 Homosexualidad, FAC4_1 Redistribución
dfess = dfess[c("Ideology", "class8_p", "EDLEVR", "domicil", "gndr", "agea", "FAC1_1", "FAC1_2", "FAC2_1", "FAC3_1", "FAC4_1", "atchctr")]

for (col_name in names(dfess)) {
  if (is.character(dfess[[col_name]])) {
    dfess[[col_name]] <- as.factor(dfess[[col_name]])
  }
}

variables_to_impute = c("class8_p", "EDLEVR", "domicil", "gndr", "agea", "FAC1_1", "FAC1_2", "FAC2_1", "FAC3_1", "FAC4_1", "atchctr")

for (col in variables_to_impute) {
  if (is.numeric(dfess[[col]])) {
    median_value <- median(dfess[[col]], na.rm = TRUE)
    dfess[[col]][is.na(dfess[[col]])] <- median_value
  }
  else if (is.factor(dfess[[col]])) {
    # Impute categorical variables with mode
    mode_value <- getmode(dfess[[col]])
    dfess[[col]][is.na(dfess[[col]])] <- mode_value
  }}


dfess <- na.omit(dfess)

mean_value <- mean(dfess$atchctr, na.rm = TRUE)
std_dev <- sd(dfess$atchctr, na.rm = TRUE)

# Standardize the variable
dfess$atchctr <- (dfess$atchctr - mean_value) / std_dev

dfess$FAC4_2 <- NA
dfess$FAC4_2 <- dfess$FAC4_1 * -1
dfess$FAC3_2 <- dfess$FAC3_1 * -1
  
dfess1 <- dfess %>%
  rename(
    Inmigración = FAC1_2,
    `Confianza en instituciones` = FAC2_1,
    Homosexualidad = FAC3_2,
    Redistribución = FAC4_2, 
    Nacionalismo = atchctr
  )

df_long <- dfess1 %>%
  pivot_longer(cols = c("Inmigración", `Confianza en instituciones`, "Homosexualidad", "Redistribución", "Nacionalismo"), 
               names_to = "variable", values_to = "value")

ggplot(df_long, aes(x = value, fill = Ideology)) + 
  geom_density(alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("Izquierda" = "red", "Derecha" = "skyblue", "Extrema derecha" = "forestgreen"),
                    name = "Bloque") +
  labs(x = "", y = "Densidad de la distribución", title = "") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"))

means_df1 <- dfess %>%
  group_by(class8_p) %>%
  summarise(
    Inmigración = mean(Inmigración, na.rm = TRUE),
    Redistribución = mean(Redistribución, na.rm = TRUE)
  )

means_df2 <- dfess %>%
  group_by(Ideology) %>%
  summarise(
    Inmigración = mean(Inmigración, na.rm = TRUE),
    Redistribución = mean(Redistribución, na.rm = TRUE)
  )



long_means_df <- df_ess %>%
  pivot_longer(
    cols = c(mean_var1, mean_var2),
    names_to = "variable",
    values_to = "mean"
  )

# Sample data based on your graph
df <- data.frame(
  Dimension1 = c(0.294, -0.124, 0.118, -0.0908, -0.167, 0.0985, 0.128, -0.106, 0.230, -0.186, -0.720),
  Dimension2 = c(0.190, 0.135, 0.0990, -0.268, -0.217, -0.126, -0.146, -0.0266, -0.217, 0.179, 0.261),
  Labels = c("Directivos",
             "Empresarios",
             "Prof. técnicos",
             "Obreros cual.",
             "Obreros no cual.",
             "Oficinistas",
             "Prof. culturales",
             "Trab. servicios",
             "Izquierda",
             "Derecha",
             "Ext. Dcha."),
  Shape = c("cross", "cross", "cross", "circle", "circle", "circle", "circle", "square", "diamond", "diamond", "diamond"),
  Color = c("black", "black", "black", "black", "black", "black", "black", "black", "red", "skyblue", "forestgreen")
)

# Define shapes to use in ggplot
shape_mapping <- c("circle" = 16, "cross" = 3, "diamond" = 18, "square" = 22) # ggplot2 shape codes

ggplot(df, aes(x = Dimension2, y = Dimension1)) +
  geom_point(aes(shape = Shape, color = Color), size = 7) +
  scale_shape_manual(values = shape_mapping) +
  scale_color_identity() +
  geom_text(size = 7, aes(label = Labels), vjust = -0.5, hjust = -0.5, angle = 5) +
  labs(title = "Valores políticos de los partidos y las clases sociales en España",
       subtitle = "Valores estandarizados (ESS9 - 2019)",
       x = "Redistribución", y = "Inmigración") +
  xlim(-0.3, 0.3) +
  theme_minimal() +
  theme(legend.position = "none", # Hide the legend, as shapes and colors are self-explanatory
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))