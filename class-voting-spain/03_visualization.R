# =============================================================================
# Voto de Clase en España - Visualizaciones y tablas
# Gráficos de probabilidades predichas, tablas de odds ratios,
# análisis ESS9 y diagrama de valores políticos
# =============================================================================

library(tidyverse)
library(ggpubr)
library(ggeffects)
library(ggrepel)
library(gridExtra)
library(broom)
library(gt)
library(haven)

# --- Cargar modelos ---
source("02_models.R")

# --- Gráfico 1: Probabilidad izquierda 2011-2016 ---

predictions <- ggpredict(model1, terms = c("Clase", "Comicio"))
mean_probability <- mean(predictions$predicted)

plot(ggpredict(model1, terms = c("Clase", "Comicio")))  + 
  geom_hline(yintercept = mean_probability, linetype = "dashed", color = "black") +
  labs(title="Probabilidad de votar a la izquierda según grupo ocupacional (2011-2016)",
       subtitle="frente a la derecha (CI: 95%)",
       y= " ", x= " ") +
  theme(axis.text.x = element_text(size = 12, face ='bold'))

# --- Gráfico 2: Izquierda + Extrema derecha (todos los comicios) ---

predictions <- ggpredict(model2, terms = c("Clase", "Comicio"))
mean_probability <- mean(predictions$predicted)

g1 <- plot(ggpredict(model2, terms = c("Clase", "Comicio")))  + 
  geom_hline(yintercept = mean_probability, linetype = "dashed", color = "black") +
  labs(title="Izquierda frente a derecha", subtitle="", y= " ", x= " ") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())

predictions <- ggpredict(model3, terms = c("Clase", "Comicio"))
mean_probability <- mean(predictions$predicted)

g2 <- plot(ggpredict(model3, terms = c("Clase", "Comicio")))  + 
  geom_hline(yintercept = mean_probability, linetype = "dashed", color = "black") +
  labs(title="Derecha radical frente a izquierda y derecha tradicional", subtitle="", y= " ", x= " ") +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, face ='bold'))

ggarrange(g1, g2, ncol=1)

# --- Tablas de Odds Ratios ---

for (mod in list(model1, model2, model3)) {
  tidy_glm <- tidy(mod, conf.int = TRUE, exponentiate = TRUE)
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
    fmt_number(columns = vars(estimate, conf.low, conf.high), decimals = 2)
  print(gt_table)
}

# =============================================================================
# Análisis ESS9 - Valores políticos de clase y partidos en España
# =============================================================================

tryCatch({
  dfess <- read_sav("data/ESS9.sav")
}, error = function(e) {
  stop("No se encontró data/ESS9.sav. Descarga ESS9 y colócalo en class-voting-spain/data/.\n",
       "Error: ", e$message)
})

dfess <- subset(dfess, cntry == "ES")

dfess$class8_p[dfess$class16_r == 2 | dfess$class16_r == 9 | dfess$class16_r == 10] <- 1
dfess$class8_p[dfess$class16_r == 1 | dfess$class16_r == 3 | dfess$class16_r == 4] <- 2
dfess$class8_p[dfess$class16_r == 5 | dfess$class16_r == 6] <- 3
dfess$class8_p[dfess$class16_r == 7] <- 4
dfess$class8_p[dfess$class16_r == 8] <- 5
dfess$class8_p[dfess$class16_r == 11 | dfess$class16_r == 12] <- 6
dfess$class8_p[dfess$class16_r == 13 | dfess$class16_r == 14] <- 7
dfess$class8_p[dfess$class16_r == 15 | dfess$class16_r == 16] <- 8

dfess$class8_p = factor(dfess$class8_p, levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                        labels = c("Directivos y gerentes", "Empresarios",
                                   "Profesionales técnicos", "Obreros cualificados",
                                   "Obreros no cualificados", "Oficinistas",
                                   "Profesionales culturales", "Trabajadores de servicios"))

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
  } else if (is.factor(dfess[[col]])) {
    mode_value <- getmode(dfess[[col]])
    dfess[[col]][is.na(dfess[[col]])] <- mode_value
  }
}

dfess <- na.omit(dfess)

mean_value <- mean(dfess$atchctr, na.rm = TRUE)
std_dev <- sd(dfess$atchctr, na.rm = TRUE)
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

# --- Diagrama de valores políticos de clases y partidos ---

df_diagram <- data.frame(
  Dimension1 = c(0.294, -0.124, 0.118, -0.0908, -0.167, 0.0985, 0.128, -0.106, 0.230, -0.186, -0.720),
  Dimension2 = c(0.190, 0.135, 0.0990, -0.268, -0.217, -0.126, -0.146, -0.0266, -0.217, 0.179, 0.261),
  Labels = c("Directivos", "Empresarios", "Prof. técnicos", "Obreros cual.",
             "Obreros no cual.", "Oficinistas", "Prof. culturales", "Trab. servicios",
             "Izquierda", "Derecha", "Ext. Dcha."),
  Shape = c("cross", "cross", "cross", "circle", "circle", "circle", "circle", "square", "diamond", "diamond", "diamond"),
  Color = c("black", "black", "black", "black", "black", "black", "black", "black", "red", "skyblue", "forestgreen")
)

shape_mapping <- c("circle" = 16, "cross" = 3, "diamond" = 18, "square" = 22)

ggplot(df_diagram, aes(x = Dimension2, y = Dimension1)) +
  geom_point(aes(shape = Shape, color = Color), size = 7) +
  scale_shape_manual(values = shape_mapping) +
  scale_color_identity() +
  geom_text(size = 7, aes(label = Labels), vjust = -0.5, hjust = -0.5, angle = 5) +
  labs(title = "Valores políticos de los partidos y las clases sociales en España",
       subtitle = "Valores estandarizados (ESS9 - 2019)",
       x = "Redistribución", y = "Inmigración") +
  xlim(-0.3, 0.3) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))
