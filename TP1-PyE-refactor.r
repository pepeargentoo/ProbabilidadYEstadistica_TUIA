# Verificar si los paquetes dplyr y ggplot2 están instalados
if (!require("dplyr")) {
  # Si no están instalados, verificar si el paquete remotes está instalado y, si no, instalarlo
  if (!require("remotes")) {
    install.packages("remotes")
  }
  
  # Intentar instalar dplyr desde CRAN
  if (!remotes::install_cran("dplyr")) {
    message("No se pudo instalar el paquete 'dplyr'.")
  }
}

if (!require("ggplot2")) {
  # Si no está instalado, instalar ggplot2 desde CRAN
  if (!install.packages("ggplot2")) {
    message("No se pudo instalar el paquete 'ggplot2'.")
  }
}

# Cargar los paquetes
library(dplyr)
library(ggplot2)

# Definir una lista con los nombres de los archivos y sus URL de descarga
archivos <- list(
  list(nombre = "ejercicio1.csv", url = "https://raw.githubusercontent.com/pepeargentoo/ProbabilidadYEstadistica_TUIA/main/ejercicio1.csv"),
  list(nombre = "ejercicio2.csv", url = "https://raw.githubusercontent.com/pepeargentoo/ProbabilidadYEstadistica_TUIA/main/ejercicio2.csv"),
  list(nombre = "ejercicio3.csv", url = "https://raw.githubusercontent.com/pepeargentoo/ProbabilidadYEstadistica_TUIA/main/ejercicio3.csv")
)

# Definir la ruta base
ruta_base <- paste0(getwd(), "/tpR/")

# Función para descargar y leer los archivos
descargar_archivos <- function() {
  if (!dir.exists(ruta_base)) {
    dir.create(ruta_base)
  }
  
  for (archivo in archivos) {
    archivo_completo <- paste(ruta_base, archivo$nombre, sep = "")
    
    if (!file.exists(archivo_completo)) {
      download.file(archivo$url, destfile = archivo_completo, mode = "wb")
    }
    
    asignar_nombre_columnas(read.csv(archivo_completo, sep = ";", fileEncoding = "ISO-8859-1", stringsAsFactors = TRUE),
                            archivo$nombre)
  }
}

# Función para asignar nombres de columnas sin espacios ni puntos
asignar_nombre_columnas <- function(df, nombre_archivo) {
  colnames(df) <- gsub("\\.| ", "", colnames(df))
  assign(nombre_archivo, df, envir = .GlobalEnv)
}

# Descargar y leer los archivos
descargar_archivos()

# ACTIVIDAD 1

csv1 <- read.csv(paste0(ruta_base,"ejercicio1.csv"), 
                 sep = ",",
                 fileEncoding = "UTF-8")

"
analisis descripctivo
"
estudio_previo <- csv1$Formación.académica.previa
realiza_ejercicio <- csv1$X.Realiza.ejercicio.físico.de.manera.frecuente.
te_gusta_el_verde <- csv1$X.Le.gusta.el.color.verde.
vistamina_d <- csv1$Vitamina.D.en.sangre..ng.mL.

vistamina_d <- gsub(",", ".", vistamina_d)
vistamina_d <- as.numeric(vistamina_d)


pie(table(estudio_previo), main = "Estudios Previos")
pie(table(realiza_ejercicio), main = "Realiza Ejercicio" )
barplot(table(te_gusta_el_verde), main = "Te gusta El verde")
datos <- data.frame(errores = rnorm(table(vistamina_d), mean = mean(vistamina_d), sd = sd(vistamina_d)))
boxplot(datos$errores, main = 'Vitamina D en sangre mL')


# ACTIVIDAD 2

csv2 <- read.csv(paste0(ruta_base,"ejercicio2.csv"), 
                 sep = ";",
                 fileEncoding = "ISO-8859-1",
                 stringsAsFactors = FALSE
)

defectos_por_placa <- as.numeric(csv2$N..de.defectos)
media <- mean(defectos_por_placa)
desviacion_estandar <- sd(defectos_por_placa)
cat("La media es:", media, "\n")
cat("La Mediana es:", median(defectos_por_placa))
cat("Quartil 25:", quantile(defectos_por_placa, 0.25)) 
cat("Quartil 75:",quantile(defectos_por_placa, 0.75))

#dispersion
cat("La desviación estándar es:", desviacion_estandar, "\n")
cat("Recorrido Inter Cuartilico:",IQR(defectos_por_placa) )
cat("Varianza:", var(defectos_por_placa))
cat("Rango:",range(defectos_por_placa))

# gráficos
frecuencias <- table(defectos_por_placa)
barplot(frecuencias, main = "Frecuencia de defectos")

# Crear un gráfico de frecuencia acumulada
frecuencias_acumuladas <- cumsum(table(defectos_por_placa))
plot(frecuencias_acumuladas, type = "s", main = "Frecuencia acumulada de defectos")

# Creamos el gráfico de caja
datos <- data.frame(errores = rnorm(table(defectos_por_placa), mean = media, sd = desviacion_estandar))
boxplot(datos$errores, main = "Errores en la Fabricación", ylab = "Error")


# ACTIVIDAD 3

csv3 <- read.csv(paste0(ruta_base,"ejercicio3.csv"), 
                 sep = ";",
                 stringsAsFactors = FALSE,
                 fileEncoding = "ISO-8859-1")

# PARA B
csv3$B <- gsub(",", ".", csv3$B)
csv_B <- as.numeric(csv3$B)

media <- mean(csv_B)
desviacion_estandar <- sd(csv_B)
cat("La media es:", media, "\n")
cat("La desviación estándar es:", desviacion_estandar, "\n")


#caja pivote
datos <- data.frame(errores = rnorm(table(csv_B), mean = media, sd = desviacion_estandar))
boxplot(datos$errores, main = "B", ylab = "B")

# Crear histograma
datos <- rnorm(table(csv_B), mean = media, sd = desviacion_estandar)
hist(datos)
# Crear gráfico de densidad
plot(density(datos))


