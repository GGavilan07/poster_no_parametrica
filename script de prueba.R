library(tidyverse)
library(haven)
options(scipen = 999)
file.choose()
df <- read_dta("C:\\Users\\gabo\\Documents\\poster_no_parametrica\\casen_2024.dta")
casen_2024_maule <- df %>% 
  filter(region == 7, y1 > 0) %>% 
  mutate(sexo = case_when(sexo == 1 ~ "Hombre",
                   sexo == 2 ~ "Mujer"
         )) %>%
  group_by(sexo) %>% 
  summarise(mediana_ingreso=median(y1,na.rm = TRUE))

prueba_signos_mujeres <- df %>%
  filter(region == 7, sexo == 2, y1 > 0) %>% 
  mutate(
    signo = case_when(
      y1 > 500000 ~ "Positivo",
      y1 < 500000 ~ "Negativo",
      y1 == 500000 ~ "Empate"
    )
  )
table(prueba_signos_mujeres$signo)
datos <- df %>% filter(region == 7)
library(Hmisc)

df %>%
  filter(region == 7,
         sexo == 1,
         y1 > 0) %>%
  summarise(
    mediana = wtd.quantile(
      y1,
      weights = expr,
      probs = 0.5
    )
  )
library(Hmisc)

df %>%
  filter(region == 7,
         sexo == 2,
         y1 > 0) %>%
  summarise(
    mediana = wtd.quantile(
      y1,
      weights = expr,
      probs = 0.5
    )
  )
wilcox.test(
  y1 ~ sexo,
  data = datos,
  alternative = "greater"
)
str(df$sexo)
table(df$sexo)
summary(df$y1)
head(df[,c("sexo","y1","region","expr")])
library(dplyr)
library(Hmisc)

df %>%
  filter(region == 7,
         y1 > 0) %>%
  group_by(sexo) %>%
  summarise(
    mediana_ponderada = wtd.quantile(
      y1,
      weights = expr,
      probs = 0.5,
      na.rm = TRUE
    )
  )
df %>%
  filter(region == 7,
         y1 > 0) %>%
  mutate(
    sexo = factor(
      sexo,
      levels = c(1,2),
      labels = c("Hombre","Mujer")
    )
  ) %>%
  group_by(sexo) %>%
  summarise(
    personas_expandidas = sum(expr),
    mediana_ponderada = wtd.quantile(
      y1,
      weights = expr,
      probs = 0.5,
      na.rm = TRUE
    )
  )
library(ggplot2)

datos <- df %>%
  filter(region == 7,
         y1 > 0) %>%
  mutate(
    sexo = factor(
      sexo,
      levels = c(1,2),
      labels = c("Hombre","Mujer")
    )
  )

ggplot(datos,
       aes(x = sexo,
           y = y1,
           fill = sexo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  labs(
    title = "Distribución del ingreso laboral principal",
    subtitle = "Región del Maule - CASEN 2024",
    x = "",
    y = "Ingreso líquido mensual ($)"
  ) +
  theme_minimal()
ggplot(datos,
       aes(x = sexo,
           y = y1,
           fill = sexo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  scale_y_log10() +
  labs(
    title = "Distribución del ingreso laboral principal",
    subtitle = "Escala logarítmica",
    y = "Ingreso líquido mensual ($)"
  ) +
  theme_minimal()

# survey ------------------------------------------------------------------


install.packages("survey")
library(survey)
library(survey)

datos <- df %>%
  filter(region == 7,
         y1 > 0) %>%
  mutate(
    sexo = as.numeric(sexo),
    expr = as.numeric(expr),
    y1 = as.numeric(y1)
  )

diseno <- svydesign(
  ids = ~1,
  weights = ~expr,
  data = datos
)

svyranktest(
y1 ~ factor(sexo),
design = diseno,
test = "wilcoxon"
)
