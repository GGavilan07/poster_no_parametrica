library(tidyverse) #necesario para trabajar con los datos
library(haven) #librería que permite leer datos provinientes de STATA  (.dta)
options(scipen = 999) #opción que muestra la extensión de los decimales
file.choose() #escogemos el archivo para saber la ruta
df <- read_dta("C:\\Users\\gabo\\Documents\\poster_no_parametrica\\casen_2024.dta") #leemos la base de datos
media_segun_sexo <- df %>% #tomamos la base original
  filter(region == 7, y1 > 0) %>% #filtramos por la región del maule e y1 > 0
  mutate(sexo = case_when(sexo == 1 ~ "Hombre", #cambiamos la variable sexo por mera comodidad
                   sexo == 2 ~ "Mujer"
         )) %>%
  group_by(sexo) %>% #agrupamos por sexo
  summarise(media_ingreso=mean(y1), n =n()) #tabla resumen de la media segun el sexo del salario liquido en su trabajo principal

# Media de salario líquido en su trabajo principal (y1) según sexo PONDERADA con factor de expansión regional (expr) ----------------------

#ahora calculamos la media que según la misma CASEN afirma que es correcta para realizar pruebas estadísticas significativas
#en otras palabras, calcularemos la media con su respectivo factor de expansión regional
media_ponderada <- df %>%
  filter(region == 7, y1 > 0) %>% #filtramos por región y por las personas cuyos salarios del trabajo principal son mayores que 0
  mutate(sexo = case_when( #modificamos la variable sexo para que nuestra tabla resumen sea clara.
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer")) %>%
  group_by(sexo) %>% #agrupamos por sexo
  summarise(media_ponderada = weighted.mean( #utilizamos weighted.mean() del paquete stats
      y1, #la variable a la cual queremos calcularle la media ponderada
      w = expr), #la ponderación, en este caso el factor de expansión regional
    n = n()) #cantidad de observaciones

media_ponderada

# para preguntar el miércoles ------------------------------------------------------------------

library(Hmisc)

datos <- df %>%
  filter(region == 7,
         y1 > 0) %>%   mutate(sexo = case_when( #modificamos la variable sexo para que nuestra tabla resumen sea clara.
           sexo == 1 ~ "Hombre",
           sexo == 2 ~ "Mujer"))

percentiles <- datos %>%
  group_by(sexo) %>%
  summarise(
    p05 = wtd.quantile(y1, weights = expr, probs = 0.05),
    p15 = wtd.quantile(y1, weights = expr, probs = 0.15),
    P25 = wtd.quantile(y1, weights = expr, probs = 0.25),
    P50 = wtd.quantile(y1, weights = expr, probs = 0.50),
    P75 = wtd.quantile(y1, weights = expr, probs = 0.75),
    P90 = wtd.quantile(y1, weights = expr, probs = 0.90),
    p95 = wtd.quantile(y1, weights = expr, probs = 0.95),
    p99 = wtd.quantile(y1, weights = expr, probs = 0.99)
  )
# Separar hombres y mujeres
hombres <- percentiles %>% filter(sexo == "Hombre")
mujeres <- percentiles %>% filter(sexo == "Mujer")

# Tabla final para el póster
tabla_brecha <- tibble(
  Percentil = c("P05","P15","P25","P50","P75","P90","P95","P99"),
  Hombre = as.numeric(hombres[1, 2:9]),
  Mujer = as.numeric(mujeres[1, 2:9])
) %>%
  mutate(
    Brecha_Porcentual = round(100 * (Hombre - Mujer) / Mujer, 1)
  )

tabla_brecha
tabla_poster <- tabla_brecha %>%
  mutate(
    Hombre = format(round(Hombre, 0), big.mark = ".", scientific = FALSE),
    Mujer = format(round(Mujer, 0), big.mark = ".", scientific = FALSE),
    Brecha_Porcentual = paste0(Brecha_Porcentual, "%")
  )

tabla_poster
