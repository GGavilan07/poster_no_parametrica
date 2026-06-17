library(tidyverse) #necesario para trabajar con los datos
library(haven) #librería que permite leer datos provinientes de STATA  (.dta)
library(Hmisc) #librería que permite calcular estadísticos con ponderaciones
library(survey) #librería que permite trabajar con los diseños de las encuestas
library(nortest) #librería que contiene test estadísticos.
options(scipen = 999) #opción que muestra la extensión de los decimales
file.choose() #escogemos el archivo para conocer su ruta
df <- read_dta("C:\\Users\\gabo\\Documents\\poster_no_parametrica\\casen_2024.dta")
#218367 obs

# Tratamiento inicial de los datos ----------------------------------------

# region == 1 -------------------------------------------------------------
r1 <- df %>% filter(region == 1) %>% nrow()
r1 #9732
r1_sexo <- df %>% filter(region == 1) %>% group_by(sexo) %>% summarise(n=n())
#4596 Hombres, 5136 Mujeres
r1_suma_sexo <- bind_rows(
  summarise(r1_sexo, sexo = "Total", n = sum(n)))
r1_suma_sexo #9732

# region == 2 -------------------------------------------------------------
r2 <- df %>% filter(region == 2) %>% nrow()
r2 #10605
r2_sexo <- df %>% filter(region == 2) %>% group_by(sexo) %>% summarise(n=n())
#5143 Hombres, 5462 Mujeres
r2_suma_sexo <- bind_rows(
  summarise(r2_sexo, sexo = "Total", n = sum(n)))
r2_suma_sexo #10605

# region == 3 -------------------------------------------------------------

r3 <- df %>% filter(region == 3) %>% nrow()
r3 #9298
r3_sexo <- df %>% filter(region == 3) %>% group_by(sexo) %>% summarise(n=n())
#4467 Hombres, 4831 Mujeres
r3_suma_sexo <- bind_rows(
  summarise(r3_sexo, sexo = "Total", n = sum(n)))
r3_suma_sexo #9298

# region == 4 -------------------------------------------------------------

r4 <- df %>% filter(region == 4) %>% nrow()
r4 #10285
r4_sexo <- df %>% filter(region == 4) %>% group_by(sexo) %>% summarise(n=n())
#4884 Hombres, 5401 Mujeres
r4_suma_sexo <- bind_rows(
  summarise(r4_sexo, sexo = "Total", n = sum(n)))
r4_suma_sexo #10285

# region == 5 -------------------------------------------------------------

r5 <- df %>% filter(region == 5) %>% nrow()
r5 #22951
r5_sexo <- df %>% filter(region == 5) %>% group_by(sexo) %>% summarise(n=n())
#10851 Hombres, 12100 Mujeres
r5_suma_sexo <- bind_rows(
  summarise(r5_sexo, sexo = "Total", n = sum(n)))
r5_suma_sexo #22951
# region == 6 -------------------------------------------------------------

r6 <- df %>% filter(region == 6) %>% nrow()
r6 #14590
r6_sexo <- df %>% filter(region == 6) %>% group_by(sexo) %>% summarise(n=n())
#6974 Hombres, 7616 Mujeres
r6_suma_sexo <- bind_rows(
  summarise(r6_sexo, sexo = "Total", n = sum(n)))
r6_suma_sexo #14590
# region == 7 -------------------------------------------------------------

r7 <- df %>% filter(region == 7) %>% nrow()
r7 #14657
r7_sexo <- df %>% filter(region == 7) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#6930 Hombres, 7727 Mujeres
r7_suma_sexo <- bind_rows(
  summarise(r7_sexo, sexo = "Total", n = sum(n)))
r7_suma_sexo #14657

# region == 8 -------------------------------------------------------------

r8 <- df %>% filter(region == 8) %>% nrow()
r8 #22447
r8_sexo <- df %>% filter(region == 8) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#10585 Hombres, 11862 Mujeres
r8_suma_sexo <- bind_rows(
  summarise(r8_sexo, sexo = "Total", n = sum(n)))
r8_suma_sexo #22447

# region == 9 -------------------------------------------------------------

r9 <- df %>% filter(region == 9) %>% nrow()
r9 #14457
r9_sexo <- df %>% filter(region == 9) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#6776 Hombres, 7681 Mujeres
r9_suma_sexo <- bind_rows(
  summarise(r9_sexo, sexo = "Total", n = sum(n)))
r9_suma_sexo #14457

# region == 10 ------------------------------------------------------------

r10 <- df %>% filter(region == 10) %>% nrow()
r10 #11343
r10_sexo <- df %>% filter(region == 10) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#5403 Hombres, 5940 Mujeres
r10_suma_sexo <- bind_rows(
  summarise(r10_sexo, sexo = "Total", n = sum(n)))
r10_suma_sexo #11343

# region == 11 -------------------------------------------------------------

r11 <- df %>% filter(region == 11) %>% nrow()
r11 #4258
r11_sexo <- df %>% filter(region == 11) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#2063 Hombres, 2195 Mujeres
r11_suma_sexo <- bind_rows(
  summarise(r11_sexo, sexo = "Total", n = sum(n)))
r11_suma_sexo #4258

# region == 12 ------------------------------------------------------------

r12 <- df %>% filter(region == 12) %>% nrow()
r12 #5992
r12_sexo <- df %>% filter(region == 12) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#2915 Hombres, 3077 Mujeres
r12_suma_sexo <- bind_rows(
  summarise(r12_sexo, sexo = "Total", n = sum(n)))
r12_suma_sexo #5992

# region == 13 ------------------------------------------------------------

r13 <- df %>% filter(region == 13) %>% nrow()
r13 #40658
r13_sexo <- df %>% filter(region == 13) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#19297 Hombres, 21361 Mujeres
r13_suma_sexo <- bind_rows(
  summarise(r13_sexo, sexo = "Total", n = sum(n)))
r13_suma_sexo #40658

# region == 14 ------------------------------------------------------------

r14 <- df %>% filter(region == 14) %>% nrow()
r14 #9167
r14_sexo <- df %>% filter(region == 14) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#4342 Hombres, 4825 Mujeres
r14_suma_sexo <- bind_rows(
  summarise(r14_sexo, sexo = "Total", n = sum(n)))
r14_suma_sexo #9167

# region == 15 ------------------------------------------------------------

r15 <- df %>% filter(region == 15) %>% nrow()
r15 #8535
r15_sexo <- df %>% filter(region == 15) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#3990 Hombres, 4545 Mujeres
r15_suma_sexo <- bind_rows(
  summarise(r15_sexo, sexo = "Total", n = sum(n)))
r15_suma_sexo #8535

# region == 16 ------------------------------------------------------------

r16 <- df %>% filter(region == 16) %>% nrow()
r16 #9392
r16_sexo <- df %>% filter(region == 16) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#4400 Hombres, 4992 Mujeres
r16_suma_sexo <- bind_rows(
  summarise(r16_sexo, sexo = "Total", n = sum(n)))
r16_suma_sexo #9392

# comprobación del filtro -------------------------------------------------
sum(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16)
#comprobamos que la suma de los filtros por región es igual a las observaciones originales

# filtro de personas con ingresos por remuneración principal mayor --------

r7 <- df %>% filter(region == 7) %>% nrow()
r7 #14657
r7_sexo <- df %>% filter(region == 7) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#6930 Hombres, 7727 Mujeres
r7_suma_sexo <- bind_rows(
  summarise(r7_sexo, sexo = "Total", n = sum(n)))
r7_suma_sexo #14657
r7_y1 <- df %>% filter(region == 7, y1 > 0) %>%
  mutate(sexo = case_when(sexo == 1~ "Hombre",
                          sexo == 2~ "Mujer"))
r7_y1%>% group_by(sexo) %>% 
  summarise(n=n())
#2406 Hombres, 1928 Mujeres
r7_y1_suma_sexo <- bind_rows(
  summarise(r7_y1, sexo = "Total", n = sum(n))
)
r7_y1_suma_sexo #4334

# filtro área de interés --------------------------------------------------
#En este caso enseñanza (rama4 %in%c(8501, 8502, 8599))
r7_y1_ensenanza <- r7_y1 %>% filter(rama4 %in%c(8501, 8502, 8599))
#r7_y1_ensenanza son nuestros datos de interés.

# análisis descriptivo ----------------------------------------------------
r7_y1_ensenanza %>% group_by(sexo) %>%  summarise(media = weighted.mean(y1, w = expr),
                                                  mediana = wtd.quantile(y1, probs = 0.5, w = expr),
                                                  min = min(as.numeric(y1)),
                                                  max = max(as.numeric(y1)),
                                                  rango = max(y1)-min(y1),
                                                  Q1 = wtd.quantile(y1, probs = 0.25, w = expr),
                                                  Q3 = wtd.quantile(y1, probs = 0.75, w = expr),
                                                  RIQ = Q3-Q1,
                                                  varianza = wtd.var(y1, w = expr),
                                                  sd = sqrt(varianza),
                                                  cv = sd/media * 100
                                                  )
ggplot(data = r7_y1_ensenanza, aes(x = sexo, y = y1, fill= sexo)) +
  geom_boxplot()


  