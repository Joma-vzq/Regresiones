# ============================================================
# ROLLING REGRESSION CON roll_lm
# Autor: José Manuel Vázquez Nicolás
# Descripción: Estima una regresión móvil sobre una base con
# fechas, una variable dependiente y varias independientes.
# ============================================================


# ----- PASO 0: Instalar paqueterías (solo la primera vez) -----

# install.packages() descarga e instala los paquetes desde CRAN.
# Si ya los tienes instalados, deja esta línea comentada con #
# para no volver a instalarlos cada vez que corras el script.
# install.packages(c("roll", "dplyr", "lubridate", "ggplot2", "tidyr"))


# ----- PASO 1: Cargar las paqueterías en la sesión -----

library(roll)       # contiene la función roll_lm() para regresión móvil
library(dplyr)      # %>% (pipe), mutate(), arrange(), filter(), etc.
library(lubridate)  # funciones para trabajar con fechas
library(ggplot2)    # sistema de gráficos basado en capas
library(tidyr)      # pivot_longer() para reorganizar datos antes de graficar


# ----- PASO 2: Cargar la base de datos -----

# read.csv() lee un archivo CSV y lo guarda como data.frame.
# Cambia "tu_base.csv" por la ruta real de tu archivo.
df <- read.csv("tu_base.csv")

# Inspección rápida para confirmar que se cargó bien:
str(df)      # muestra el tipo (numeric, character, etc.) de cada columna
head(df)     # primeras 6 filas para ver cómo lucen los datos
summary(df)  # estadísticas básicas y conteo de NAs por columna


# ----- PASO 3: Convertir la columna de fecha a tipo Date -----

# Casi siempre las fechas llegan como texto ("character"). R no las
# entiende como fechas hasta que se las conviertes explícitamente.
# format = "%Y-%m-%d" significa "año-mes-día" (ej. 2024-03-15).
# Si tu fecha viene como "15/03/2024", usarías format = "%d/%m/%Y".
df$fecha <- as.Date(df$fecha, format = "%Y-%m-%d")

# arrange() ordena el data frame por la columna fecha de menor a mayor.
# Es CRÍTICO porque la rolling regression depende del orden cronológico.
df <- df %>% arrange(fecha)

# Verifica que la conversión funcionó: si hay NAs aquí, alguna fecha
# no coincidió con el formato y se volvió NA.
sum(is.na(df$fecha))


# ----- PASO 4: Limpiar valores faltantes -----

# drop_na() elimina cualquier fila que tenga al menos un NA.
# La regresión no puede correr con NAs, así que hay que decidir:
# eliminarlos (como aquí) o imputarlos. Para empezar, eliminar es lo más simple.
df <- df %>% drop_na()


# ----- PASO 5: Preparar la variable dependiente y la matriz de independientes -----

# y es un vector con la variable que quieres explicar.
y <- df$y

# X debe ser una MATRIZ (no data.frame) porque roll_lm() lo exige.
# as.matrix() convierte las columnas seleccionadas en matriz numérica.
# Cambia "x1", "x2", "x3" por los nombres reales de tus variables.
# IMPORTANTE: no agregues una columna de unos para el intercepto;
# roll_lm() lo incluye automáticamente.
X <- as.matrix(df[, c("x1", "x2", "x3")])


# ----- PASO 6: Definir el tamaño de ventana -----

# La ventana es cuántas observaciones usa cada regresión.
# Regla práctica: al menos 10 veces el número de variables independientes.
# Con 3 variables, 60 es razonable. Ajústalo según tu frecuencia de datos:
#   - Diarios financieros: 60, 120 o 252 (un año bursátil)
#   - Mensuales: 24 o 36
ventana <- 60


# ----- PASO 7: Estimar la rolling regression -----

# roll_lm() corre una regresión por cada ventana móvil.
# Devuelve una lista con:
#   $coefficients: matriz de coeficientes (filas = fechas, columnas = variables)
#   $r.squared:    R² por ventana
#   $std.error:    errores estándar por ventana
modelo <- roll_lm(x = X, y = y, width = ventana)


# ----- PASO 8: Armar un data frame con los resultados -----

# La notación [, k] extrae la columna k de una matriz:
#   - antes de la coma: filas (vacío = todas)
#   - después de la coma: columnas
# La columna 1 es el intercepto, la 2 es el coef de x1, la 3 de x2, etc.
resultados <- data.frame(
  fecha       = df$fecha,
  intercepto  = modelo$coefficients[, 1],  # todas las filas, columna 1
  beta_x1     = modelo$coefficients[, 2],  # todas las filas, columna 2
  beta_x2     = modelo$coefficients[, 3],
  beta_x3     = modelo$coefficients[, 4],
  r2          = modelo$r.squared,
  se_x1       = modelo$std.error[, 2],     # error estándar de beta_x1
  se_x2       = modelo$std.error[, 3],
  se_x3       = modelo$std.error[, 4]
) %>% drop_na()  # elimina las primeras ventana-1 filas que son NA


# ----- PASO 9: Calcular bandas de confianza al 95% -----

# Un intervalo de confianza al 95% se aproxima como:
#   coeficiente ± 1.96 × error estándar
# Si la banda cruza el cero, el coeficiente NO es significativo
# estadísticamente en esa ventana.
resultados <- resultados %>%
  mutate(
    x1_lower = beta_x1 - 1.96 * se_x1,
    x1_upper = beta_x1 + 1.96 * se_x1,
    x2_lower = beta_x2 - 1.96 * se_x2,
    x2_upper = beta_x2 + 1.96 * se_x2,
    x3_lower = beta_x3 - 1.96 * se_x3,
    x3_upper = beta_x3 + 1.96 * se_x3
  )


# ----- PASO 10: Gráfico de un coeficiente con banda de confianza -----

# ggplot construye gráficos por capas (se "suman" con +).
ggplot(resultados, aes(x = fecha, y = beta_x1)) +
  # geom_ribbon dibuja una banda entre ymin e ymax (intervalo de confianza)
  geom_ribbon(aes(ymin = x1_lower, ymax = x1_upper),
              fill = "steelblue", alpha = 0.2) +
  # geom_line dibuja la línea con la serie del coeficiente
  geom_line(color = "steelblue", linewidth = 0.8) +
  # geom_hline traza una línea horizontal de referencia en y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # labs() pone título, subtítulo y nombres de ejes
  labs(title    = "Evolución del coeficiente de x1",
       subtitle = paste("Ventana móvil de", ventana, "observaciones"),
       x        = "Fecha",
       y        = expression(beta[x1])) +
  theme_minimal()  # estilo limpio sin fondo gris


# ----- PASO 11: Gráfico comparativo de todos los coeficientes -----

# pivot_longer pasa los datos de formato "ancho" (columnas separadas
# por variable) a "largo" (una columna que dice qué variable es y otra
# con el valor). Es el formato que ggplot prefiere para colorear por grupo.
resultados %>%
  select(fecha, beta_x1, beta_x2, beta_x3) %>%
  pivot_longer(cols = -fecha,                  # todas menos fecha
               names_to  = "variable",         # columna con el nombre
               values_to = "coef") %>%         # columna con el valor
  ggplot(aes(x = fecha, y = coef, color = variable)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Evolución de los coeficientes",
         x = "Fecha", y = "Coeficiente") +
    theme_minimal()


# ----- PASO 12: Gráfico del R² móvil -----

# Sirve para ver cómo cambia la capacidad explicativa del modelo
# en el tiempo. Caídas fuertes suelen indicar cambios estructurales.
ggplot(resultados, aes(x = fecha, y = r2)) +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  labs(title    = "R² móvil",
       subtitle = "Capacidad explicativa del modelo en el tiempo",
       x        = "Fecha",
       y        = expression(R^2)) +
  theme_minimal()


# ----- PASO 13: Exportar resultados (opcional) -----

# write.csv() guarda el data frame en un archivo CSV.
# row.names = FALSE evita que se añada una columna con el número de fila.
write.csv(resultados, "resultados_rolling.csv", row.names = FALSE)

# Fin del script
