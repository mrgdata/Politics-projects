library(readr)
library(tidyverse)
library(transformr)
library(ggpubr)
library(gganimate)
library(gifski)
library(png)

mov <- read_csv("C:/Users/USUARIO/Desktop/movilidad experiment-table.csv", 
                                       col_types = cols(`[run number]` = col_skip(), 
                                                        `%-mejora` = col_number(), densidad = col_number(), 
                                                        umbral = col_number(), `[step]` = col_number(), 
                                                        `movilidad-vulnerables` = col_number()), 
                                       skip = 6)
View(mov)

mov0 <- mov %>% filter(`[step]` == 1) %>%
  group_by(`condición`, `%-mejora`, umbral) %>% 
  summarise(movub = mean(`movilidad-vulnerables`))

a <- mov0 %>% filter(umbral == 20) %>%
  ggplot(aes(x = `%-mejora`, color = `condición`)) + 
  geom_line(aes(y= movub)) + theme_bw() + ylim(0, 55) +
  labs(fill = "Tipo de política",title = " ",
       x = " ", 
       y = " ",
       subtitle = "Umbral de homofilia: 20") +
  theme(legend.position = "none")
       
b <- mov0 %>% filter(umbral == 40) %>%
  ggplot(aes(x = `%-mejora`, color = `condición`)) + 
  geom_line(aes(y= movub)) + theme_bw() + ylim(0, 55) +
  labs(fill = "Tipo de política",title = " ",
       x = " ", 
       y = " ",
       subtitle = "Umbral de homofilia: 40") +
  theme(legend.position = "none")
  
c <- mov0 %>% filter(umbral == 60) %>%
  ggplot(aes(x = `%-mejora`, color = `condición`)) + 
  geom_line(aes(y= movub)) + theme_bw() + ylim(0, 55) +
  labs(fill = "Tipo de política",title = " ",
       x = " ", 
       y = " ", 
       subtitle = "Umbral de homofilia: 60") +
  theme(legend.position = "none")

d <- mov0 %>% filter(umbral == 80) %>%
  ggplot(aes(x = `%-mejora`, color = `condición`)) + 
  geom_line(aes(y= movub)) + theme_bw() + ylim(0, 55) +
  labs(fill = "Tipo de política",
    title = " ",
       x = " ", 
       y = " ",
    subtitle = "Umbral de homofilia: 80") +
  theme(legend.position = "none")

e <- ggarrange(a, b, c, d, common.legend = TRUE, font.label = list(size = 10), align = "hv",
          heights = c(1, 1))

annotate_figure(e, 
                bottom = text_grob("% de ciudadanos que mejoran", size = 12),
                left = text_grob("% de ciudadanos que se marchan", rot = 90, size = 12))
# gif

p <- mov0 %>% ggplot(aes(x = `%-mejora`, color = `condición`)) + 
  geom_line(aes(y= movub)) + theme_bw() + ylim(0, 55) +
    labs(title = "Movilidad residencial según tipo de política y homofilia", 
          fill = "Tipo de política",
          x = "% de ciudadanos que reciben la mejora", 
          y = "% de ciudadanos que se marchan del barrio",
          subtitle = "Umbral de homofilia: {frame_time}") +
          transition_time(umbral)

animate(p, renderer = gifski_renderer())

