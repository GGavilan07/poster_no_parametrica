paquetes <- c("tidyverse", "haven", "Hmisc", "survey", "nortest")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
options(survey.lonely.psu = "adjust")
options(scipen = 999) #opción que muestra la extensión de los decimales
df <- read_dta(file.choose())#escogemos el archivo para conocer su ruta
#218367 obs

# Tratamiento inicial de los datos ----------------------------------------
#Analizaremos cuántas observaciones van quedando atrás después de aplicar cada filtro
# region == 1 -------------------------------------------------------------
r1 <- df %>% filter(region == 1) %>% nrow()
r1 #9732
r1_sexo <- df %>% filter(region == 1) %>% group_by(sexo) %>% summarise(n=n())
#4596 Hombres, 5136 Mujeres
r1_suma_sexo <- bind_rows(
  summarise(r1_sexo, sexo = "Total", n = sum(n)))
r1==r1_suma_sexo #comprobamos que el filtro por sexo no elimina observaciones
nrow(df)-r1 #208635 filas que no serán utilizadas
# region == 2 -------------------------------------------------------------
r2 <- df %>% filter(region == 2) %>% nrow()
r2 #10605
r2_sexo <- df %>% filter(region == 2) %>% group_by(sexo) %>% summarise(n=n())
#5143 Hombres, 5462 Mujeres
r2_suma_sexo <- bind_rows(
  summarise(r2_sexo, sexo = "Total", n = sum(n)))
r2==r2_suma_sexo #comprobamos que el filtro por sexo no elimina observaciones
nrow(df)-r2 #207762 filas que no serán utilizadas
# region == 3 -------------------------------------------------------------

r3 <- df %>% filter(region == 3) %>% nrow()
r3 #9298
r3_sexo <- df %>% filter(region == 3) %>% group_by(sexo) %>% summarise(n=n())
#4467 Hombres, 4831 Mujeres
r3_suma_sexo <- bind_rows(
  summarise(r3_sexo, sexo = "Total", n = sum(n)))
r3==r3_suma_sexo #comprobamos que el filtro por sexo no elimina observaciones
nrow(df)-r3 #209069 filas que no serán utilizadas
# region == 4 -------------------------------------------------------------

r4 <- df %>% filter(region == 4) %>% nrow()
r4 #10285
r4_sexo <- df %>% filter(region == 4) %>% group_by(sexo) %>% summarise(n=n())
#4884 Hombres, 5401 Mujeres
r4_suma_sexo <- bind_rows(
  summarise(r4_sexo, sexo = "Total", n = sum(n)))
r4==r4_suma_sexo #comprobamos que el filtro por sexo no elimina observaciones
nrow(df)-r4 #208082 filas que no serán utilizadas

# region == 5 -------------------------------------------------------------

r5 <- df %>% filter(region == 5) %>% nrow()
r5 #22951
r5_sexo <- df %>% filter(region == 5) %>% group_by(sexo) %>% summarise(n=n())
#10851 Hombres, 12100 Mujeres
r5_suma_sexo <- bind_rows(
  summarise(r5_sexo, sexo = "Total", n = sum(n)))
r5==r5_suma_sexo #comprobamos que el filtro por sexo no elimina observaciones
nrow(df)-r5 #195416 filas que no serán utilizadas
# region == 6 -------------------------------------------------------------

r6 <- df %>% filter(region == 6) %>% nrow()
r6 #14590
r6_sexo <- df %>% filter(region == 6) %>% group_by(sexo) %>% summarise(n=n())
#6974 Hombres, 7616 Mujeres
r6_suma_sexo <- bind_rows(
  summarise(r6_sexo, sexo = "Total", n = sum(n)))
r6==r6_suma_sexo #comprobamos que el filtro por sexo no elimina observaciones
nrow(df)-r6 #203777 filas que no serán utilizadas
# region == 7 -------------------------------------------------------------

r7 <- df %>% filter(region == 7) %>% nrow()
r7 #14657
r7_sexo <- df %>% filter(region == 7) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#6930 Hombres, 7727 Mujeres
r7_suma_sexo <- bind_rows(
  summarise(r7_sexo, sexo = "Total", n = sum(n)))
r7==r7_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r7 #203710 filas que no serán utilizadas

# region == 8 -------------------------------------------------------------

r8 <- df %>% filter(region == 8) %>% nrow()
r8 #22447
r8_sexo <- df %>% filter(region == 8) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#10585 Hombres, 11862 Mujeres
r8_suma_sexo <- bind_rows(
  summarise(r8_sexo, sexo = "Total", n = sum(n)))
r8==r8_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r8 #195920 filas que no serán utilizadas

# region == 9 -------------------------------------------------------------

r9 <- df %>% filter(region == 9) %>% nrow()
r9 #14457
r9_sexo <- df %>% filter(region == 9) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#6776 Hombres, 7681 Mujeres
r9_suma_sexo <- bind_rows(
  summarise(r9_sexo, sexo = "Total", n = sum(n)))
r9==r9_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r9 #203910 filas que no serán utilizadas
# region == 10 ------------------------------------------------------------

r10 <- df %>% filter(region == 10) %>% nrow()
r10 #11343
r10_sexo <- df %>% filter(region == 10) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#5403 Hombres, 5940 Mujeres
r10_suma_sexo <- bind_rows(
  summarise(r10_sexo, sexo = "Total", n = sum(n)))
r10==r10_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r10 #207024 filas que no serán utilizadas
# region == 11 -------------------------------------------------------------

r11 <- df %>% filter(region == 11) %>% nrow()
r11 #4258
r11_sexo <- df %>% filter(region == 11) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#2063 Hombres, 2195 Mujeres
r11_suma_sexo <- bind_rows(
  summarise(r11_sexo, sexo = "Total", n = sum(n)))
r11==r11_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r11 #214109 filas que no serán utilizadas

# region == 12 ------------------------------------------------------------

r12 <- df %>% filter(region == 12) %>% nrow()
r12 #5992
r12_sexo <- df %>% filter(region == 12) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#2915 Hombres, 3077 Mujeres
r12_suma_sexo <- bind_rows(
  summarise(r12_sexo, sexo = "Total", n = sum(n)))
r12==r12_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r12 #212375 filas que no serán utilizadas

# region == 13 ------------------------------------------------------------

r13 <- df %>% filter(region == 13) %>% nrow()
r13 #40658
r13_sexo <- df %>% filter(region == 13) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#19297 Hombres, 21361 Mujeres
r13_suma_sexo <- bind_rows(
  summarise(r13_sexo, sexo = "Total", n = sum(n)))
r13==r13_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r13 #177709 filas que no serán utilizadas
# region == 14 ------------------------------------------------------------

r14 <- df %>% filter(region == 14) %>% nrow()
r14 #9167
r14_sexo <- df %>% filter(region == 14) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#4342 Hombres, 4825 Mujeres
r14_suma_sexo <- bind_rows(
  summarise(r14_sexo, sexo = "Total", n = sum(n)))
r14==r14_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r14 #209200 filas que no serán utilizadas

# region == 15 ------------------------------------------------------------

r15 <- df %>% filter(region == 15) %>% nrow()
r15 #8535
r15_sexo <- df %>% filter(region == 15) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#3990 Hombres, 4545 Mujeres
r15_suma_sexo <- bind_rows(
  summarise(r15_sexo, sexo = "Total", n = sum(n)))
r15==r15_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r15 #209832 filas que no serán utilizadas

# region == 16 ------------------------------------------------------------

r16 <- df %>% filter(region == 16) %>% nrow()
r16 #9392
r16_sexo <- df %>% filter(region == 16) %>% 
  group_by(sexo) %>% 
  summarise(n=n())
#4400 Hombres, 4992 Mujeres
r16_suma_sexo <- bind_rows(
  summarise(r16_sexo, sexo = "Total", n = sum(n)))
r16==r16_suma_sexo #comprobamos que el agrupamiento por sexo no elimina observaciones
nrow(df)-r16 #208975 filas que no serán utilizadas
# Comprobación del filtro por región-------------------------------------------------
sum(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16) == nrow(df)
#comprobamos que la suma de los filtros por región es igual a las observaciones originales

# Filtro de personas con ingresos por remuneración principal mayor a cero --------

r7 <- df %>% filter(region == 7) %>% nrow()
r7 #14657
r7_sexo <- df %>% filter(region == 7) %>% 
  group_by(sexo) %>% 
  nrow()
r7_sexo == r7 #confirmamos que el agrupamiento por sexo no eliminó observaciones
#filtramos los casos en los que los ingresos por ocupación principal es mayor a cero.
r7_y1 <-  df %>% filter(region == 7, y1 > 0) %>% 
  mutate(sexo = case_when(sexo == 1~ "Hombre",
                          sexo == 2~ "Mujer"))
r7_y1 %>% group_by(sexo) %>% 
  summarise(n=n()) #2406 hombres, 1928 mujeres
r7_y1_conteo <-  nrow(r7_y1)
r7-r7_y1_conteo #10323 filas que no serán utilizadas, es decir 10323 personas que cuyos ingresos por ocupación principal no son mayores a 0
#Nos quedamos únicamente con 4334 filas
# Filtro área de interés --------------------------------------------------
#En este caso enseñanza (rama4 %in%c(8501, 8502, 8599))
r7_y1_ensenanza <- r7_y1 %>% filter(rama4 %in%c(8501, 8502, 8599))
#r7_y1_ensenanza son nuestros datos de interés.
nrow(r7_y1_ensenanza) == sum(r7_y1$rama4 %in% c(8501, 8502, 8599))
#comprobamos que el filtro no eliminó ninguna fila.
#conteo de NAs
r7_y1_ensenanza %>% 
  select(y1, sexo, expr, varunit, varstrat, id_persona, id_vivienda, folio, rama4) %>% 
  summarise(across(everything(), ~sum(is.na(.))))
#no se reporta ningún NA en nuestras variables de interés.

# Análisis descriptivo ----------------------------------------------------
#diseño CASEN
options(survey.lonely.psu = "adjust") #evita error cuando un estrato tiene una sola PSU tras filtrar
diseno <- svydesign(
  ids = ~varunit, #conglomerados de varianza
  strata = ~varstrat, #estratos de varianza
  weights = ~expr, #factor de expansión regional
  data = r7_y1_ensenanza #datos de interés
) 
#sub-diseño por cada de sexo
diseno_hombre <- subset(diseno, sexo == "Hombre")
diseno_mujer  <- subset(diseno, sexo == "Mujer")

calcular_stats <- function(diseno_sub) {
  media   <- svymean(~y1, diseno_sub)
  var_y   <- svyvar(~y1, diseno_sub)
  cuantiles <- svyquantile(~y1, diseno_sub, quantiles = c(0.25, 0.5, 0.75),
                           ci = FALSE)
  
  Q1 <- cuantiles$y1[1]
  mediana <- cuantiles$y1[2]
  Q3 <- cuantiles$y1[3]
  
  tibble(
    media    = coef(media)[1],
    mediana  = mediana,
    Q1       = Q1,
    Q3       = Q3,
    RIQ      = Q3 - Q1,
    varianza = coef(var_y)[1],
    sd       = sqrt(coef(var_y)[1]),
    cv       = sd / media * 100,
    min      = min(diseno_sub$variables$y1, na.rm = TRUE),
    max      = max(diseno_sub$variables$y1, na.rm = TRUE),
    rango    = max - min
  )
}


resultados <- bind_rows(
  calcular_stats(diseno_hombre) %>% mutate(sexo = "Hombre"),
  calcular_stats(diseno_mujer)  %>% mutate(sexo = "Mujer")
) %>% relocate(sexo)
resultados
# Interpretación:
#la media está siendo arrastrada hacia arriba por unos pocos casos de ingresos altos, especialmente en el grupo de hombres
#la mediana no se deja sesgar por los datos atípicos y se reporta una diferencia de $250.000CLP a favor de los hombres
#con respecto a los demás cuartiles, se evidencia inferioridad de parte de las mujeres de $80.000CLP en Q1 y de $510.000CLP en Q3
#los hombres tienen cerca del doble de rango intercuartilico por encima de las mujeres, esto quiere decir que existe más variedad de hombres con ingresos bajos y hombres con ingresos altos
#similar a lo que ocurre con el RIQ, los hombres tienen una mayor varianza y desviación estándar que las mujeres debido a la variedad en sus ingresos
#en ambos casos se dice que los ingresos son medianamente homogeneos

# Gráficos con svy --------------------------------------------------------


#Boxplot
svyboxplot(y1~sexo,
           diseno,
           xlab= "Sexo",
           ylab= "Ingresos por ocupación principal",
           main = "Boxplot de los ingresos por ocupación principal (Enseñanza) de hombres y mujeres de la región Del Maule",
           col = c("lightblue", "pink"),
           outlier = TRUE)
#Se evidencia un sesgo a la derecha y una observación atípica en ambos diagramas.
#El Q2 de Hombres es: 900000
#El Q2 de Mujeres es: 650000
#La mediana de las mujeres es matemáticamente menor que la de los hombres
#Los hombres cuentan con una mayor dispersión en sus observaciones (bigotes más largos)

#Histogramas
par(mfrow = c(1, 2))

#Histograma para Hombres
svyhist(~y1, 
        design = diseno_hombre, 
        xlab = "Ingresos", 
        ylab = "Densidad",
        main = "Hombres (Enseñanza)", 
        col = "lightblue",
        probability = TRUE)

#Histograma para Mujeres
svyhist(~y1, 
        design = diseno_mujer, 
        xlab = "Ingresos", 
        ylab = "Densidad",
        main = "Mujeres (Enseñanza)", 
        col = "pink",
        probability = TRUE)

#Ambos histogramas presentan distribuciones con asimetría positiva
#Las frecuencias se extienden de manera decreciente a medida que aumentan los ingresos en ambos sexos
#El intervalo con mayor densidad en el caso de los hombres se encuentra entre 500000 y 1000000
#El intervalo con mayor densidad se concentra aproximadamente entre 500000 y 750000

# Test --------------------------------------------------------------------
#H_0: La distribución acumulada de los ingresos de la remuneración principal (enseñanza) de los hombres es igual a la de las mujeres de la región Del Maule.
#H_a: La distribución acumulada de los ingresos de la remuneración principal (enseñanza) de los hombres es distinta a la de las mujeres de la región Del Maule.
#Con un nivel alpha=0.05
svyranktest(y1~sexo, design = diseno, test = "wilcoxon")
#Existe evidencia estadística para rechazar H_0, como p-valor < alpha, decimos que: Con un 95% de confianza
#la distribución acumulada de los ingresos de la remuneración principal en el sector de la enseñanza presenta
#diferencia entre hombres y mujeres de la región del Maule.

