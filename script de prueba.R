library(tidyverse) #necesario para trabajar con los datos
library(haven) #librería que permite leer datos provinientes de STATA  (.dta)
library(Hmisc) #librería que permite calcular estadísticos con ponderaciones
library(survey) #librería que permite trabajar con los diseños de las encuestas
library(nortest) #librería que contiene test estadísticos.
options(scipen = 999) #opción que muestra la extensión de los decimales
file.choose() #escogemos el archivo para conocer su ruta
df <- read_dta("C:\\Users\\gabo\\Documents\\poster_no_parametrica\\casen_2024.dta") #leemos la base de datos
datos <- df %>% #tomamos la base original
  filter(region == 7, y1 > 0) %>% #filtramos por la región del maule e y1 > 0
  mutate(sexo = case_when(sexo == 1 ~ "Hombre", #cambiamos la variable sexo por mera comodidad
                          sexo == 2 ~ "Mujer"))
media_segun_sexo <- datos %>% 
  group_by(sexo) %>% #agrupamos por sexo
  summarise(media_ingreso=mean(y1), n =n()) #tabla resumen de la media segun el sexo del salario liquido en su trabajo principal

# Media de salario líquido en su trabajo principal (y1) según sexo PONDERADA con factor de expansión regional (expr) ----------------------

#ahora calculamos la media que según la misma CASEN afirma que es correcta para realizar pruebas estadísticas significativas
#en otras palabras, calcularemos la media con su respectivo factor de expansión regional
media_ponderada <- datos %>% 
  group_by(sexo) %>% #agrupamos por sexo
  summarise(media_ponderada = weighted.mean( #utilizamos weighted.mean() del paquete stats
      y1, #la variable a la cual queremos calcularle la media ponderada
      w = expr), #la ponderación, en este caso el factor de expansión regional
    n = n()) #cantidad de observaciones

media_ponderada

# para preguntar el miércoles ------------------------------------------------------------------

mediana_ponderada <- datos %>% 
  group_by(sexo) %>% 
  summarise(mediana= wtd.quantile(y1, w = expr, probs = 0.50))
mediana_ponderada



# limpieza de los datos ---------------------------------------------------

datos_a_limpiar <- datos %>% select(region, sexo, y1, expr, varunit, varstrat, id_vivienda, id_persona, folio, rama4)
conteo_na <- datos_a_limpiar %>% summarise(across(everything(), ~sum(is.na(.)))) #revisamos columna por columna todas las observaciones, buscando sumar todos los NA.
conteo_na #0 NA
# Buscamos duplicados usando los ids únicas de la CASEN
duplicados_reales <- datos_a_limpiar %>%
  group_by(id_vivienda, folio, id_persona) %>% #Agrupamos por id único de vivivienda y persona
  filter(n() > 1) #Filtramos por cada id_vivienda e id_persona que esté presente más de una vez
  sum(duplicated(duplicados_reales[c("id_persona", "id_vivienda")])) #sumamos cada id_persona, id_vivienda y folio que esté repetido
  #0 columnas duplicads
df_sin_duplicados <- datos_a_limpiar %>% 
  distinct(id_vivienda, id_persona, .keep_all = TRUE)
#con distinct seleccionamos todos los id_vivienda e id_persona que no se repiten. Y .keep asegura que persistan los demás ids

# boxplot para ver sesgo--------------------------------------------------
library(ggplot2)
ggplot(data = df_sin_duplicados, aes(x= sexo, y=y1, fill = sexo)) +
  geom_boxplot(alpha=0.7)+
  scale_fill_manual(values = c("Hombre" = "lightblue", "Mujer" = "lightpink"))+
  labs(
    title= "Boxplots de Salario por Ocupación Principal según Sexo",
    x = "Sexo",
    y= "Salario por Ocupación Principal",
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# resumen descriptivo -----------------------------------------------------


#Confirmamos sesgo a la derecha en ambos casos, procedemos a log-normalizar.
# log-normal para atípicos ------------------------------------------------
##Importante preguntarle al profe si es necesario realizar un test de log-normalidad a los datos
##Log-normalizamos porque son datos sesgados a la derecha.
##FUNDAMENTAR CON EVIDENCIA EMPÍRICA (buscar estudios que hayan hecho esto)

##Atípicos
datos_log <- df_sin_duplicados %>%
  mutate(
    log_y1 = log(y1),
    media_log = mean(log_y1),
    sd_log = sd(log_y1),
    # Detectamos a más de 3 desviaciones estándar, pero en escala logarítmica
    es_atipico_log = abs(log_y1 - media_log) > (3 * sd_log)
  )

table(datos_log$es_atipico_log)
atipicos <- datos_log %>% filter(es_atipico_log == TRUE)
atipicos
##Trabajaremos con los atípicos incluidos, entonces
# Planteamiento de Hipotésis y su test respectivo -------------------------
ensenanza <- datos_limpios %>% filter(rama4 %in% c(8501, 8502, 8599))
options(survey.lonely.psu = "adjust")

diseno_casen <- svydesign(
  ids = ~varunit, #Conglomerados de varianza
  strata = ~varstrat, #Estratos de varianza
  weights = ~expr, #Factor de expansión regional
  data = ensenanza)

ensenanza %>%
  group_by(sexo) %>%
  summarise(
    mediana_segun_sexo = wtd.quantile(y1, w = expr, probs = 0.5),
    media_segun_sexo = weighted.mean(y1, w = expr))

#H_0: Las distribuciones de los ingresos de la ocupación principal (enseñanza) de hombres y mujeres en la región del Maule son idénticas.
#H_1: Las distribuciones de los ingresos de la ocupación principal (enseñanza) de hombres y mujeres en la region del Maule difieren significativamente entre hombres y mujeres.
#Con un nivel de significancia alpha=0.05:
svyranktest(y1~sexo, diseno_casen, test = "wilcoxon")
#Rechazamos H_0, p-valor < alpha. Por lo tanto:
#Con un nivel de significancia alpha=0.05, rechazamos H_0. Es decir: Las distribuciones de los ingresos de la ocupación principal (enseñanza) de hombre y mujeres en la region del Maule difieren significativamente entre hombres y mujeres.

