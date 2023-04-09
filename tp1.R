# esta funcion se encarga de descargar los archivos
path = 'tpR/'
download_csv <- function(){
  if(!file.exists(paste0(path,'ejercicio1.csv'))){
    url <- "https://raw.githubusercontent.com/pepeargentoo/ProbabilidadYEstadistica_TUIA/main/ejercicio1.csv"
    download.file(url, destfile = paste0(path,"ejercicio1.csv"), mode = "wb")
  }
  
  if(!file.exists(paste0(path,'ejercicio2.csv'))){
    url <- "https://raw.githubusercontent.com/pepeargentoo/ProbabilidadYEstadistica_TUIA/main/ejercicio2.csv"
    download.file(url, destfile = paste0(path,"ejercicio2.csv"), mode = "wb")
  }
  
  if(!file.exists(paste0(path,'ejercicio3.csv'))){
    url <- "https://raw.githubusercontent.com/pepeargentoo/ProbabilidadYEstadistica_TUIA/main/ejercicio3.csv"
    download.file(url, destfile = paste0(path,"ejercicio3.csv"), mode = "wb")
  }
}
download_csv()

# pregunta 1

#población: todos alumnos que ingresan al inicio de la cursada y el ciclo PPDAC

# variables
# Área.de.estudio.previo : cualitativas,
# X.Le.gusta.el.color.verde: cualitativa,
# X.Realiza.ejercicio.físico.de.manera.frecuente. : cualitativa

path = '/tpR/'
csv1 <- read.csv(paste0(getwd(),path,"ejercicio1.csv"), 
                 sep = ";",
          fileEncoding = "ISO-8859-1")

"
analisis descripctivo
"
estudio_previo <- csv1[5]
realiza_ejercicio <- csv1[9]
te_gusta_el_verde <- csv1[8]
# Área.de.estudio.previo = estudio_previo
# X.Realiza.ejercicio.físico.de.manera.frecuente. = realiza_ejercicio
# X.Le.gusta.el.color.verde = te_gusta_el_verde
pie(table(estudio_previo), main = "Estudios Previos")
barplot(table(realiza_ejercicio), main = "Realiza Ejercicio" )
barplot(table(te_gusta_el_verde), main = "Te gusta El verde")

# ACTIVIDAD 2 

csv2 <- read.csv(paste0(getwd(),path,"ejercicio2.csv"), 
                 sep = ";",
                 fileEncoding = "ISO-8859-1",
                 stringsAsFactors = FALSE
                 )
#poblacion: las placas que son creadas en la fabrica
#variable: N.de.defectos: cuantitativa discreta
#parámetro de interés: la media y la desviación estándar del número de defectos por placa en el lote
#objetivo: determinar si este lote es apto o no para la ventas
# colnames(csv2) nombre de columnas de un csv
defectos_por_placa <- as.numeric(csv2$N..de.defectos)
media <- mean(defectos_por_placa)
desviacion_estandar <- sd(defectos_por_placa)
cat("La media es:", media, "\n")
cat("La desviación estándar es:", desviacion_estandar, "\n")
"
Si la media es menor a 1,2 y la desviación estándar es baja, entonces se podría 
concluir que el lote cumple con las exigencias del cliente y se podría enviar 
el lote. De lo contrario, no se cumplirían las exigencias del cliente y se 
debería preparar otro lote.
"

"
conclusion preliminares, aca tengo duda ya que para ser preliminares, 
si estamos hablando de una muestra y tenemos que hablar solo de esta muestra es definitivo.
Pero si es preliminar si estas hablando de un conujnto de datos, y sino cumple la 
La ley de los grandes números .
esta es nuestra duda
"

"
otro datos que agregaria seria la fecha, y hora en vase a esso podria determinar en que 
rango horario hay mas errores y podrias determinar y es un factor humano ya sea 
casancio del profesional. y la fecha para ver en que momento se trabaja mejor 
"
# graficos
frecuencias <- table(defectos_por_placa)

# Crear un gráfico de barras
barplot(frecuencias, main = "Frecuencia de defectos")
# frecuencias acumuladas

frecuencias_acumuladas <- cumsum(table(defectos_por_placa))

# Crear un gráfico de frecuencia acumulada
plot(frecuencias_acumuladas, type = "s", main = "Frecuencia acumulada de defectos")

# caja o pivote


datos <- data.frame(errores = rnorm(table(defectos_por_placa), mean = media, sd = desviacion_estandar))

# Creamos el gráfico de caja
boxplot(datos$errores, main = "Errores en la Fabricacion", ylab = "Error")





# ACTIVIDAD 3


csv3 <- read.csv(paste0(getwd(),path,"ejercicio3.csv"), 
                 sep = ";",
                 stringsAsFactors = FALSE,
                 fileEncoding = "ISO-8859-1")
"
Plantee el problema, defina población, variable y parámetro/s de interés
poblacion
variable cuantitativa continua
"

# PARA A
csv3$A <- gsub(",", ".", csv3$A)
csv_A <- as.numeric(csv3$A)

media <- mean(csv_A)
desviacion_estandar <- sd(csv_A)
cat("La media es:", media, "\n")
cat("La desviación estándar es:", desviacion_estandar, "\n")


#caja pivote
datos <- data.frame(errores = rnorm(table(csv_A), mean = media, sd = desviacion_estandar))
boxplot(datos$errores, main = "A", ylab = "A")

# Crear histograma
datos <- rnorm(table(csv_A), mean = media, sd = desviacion_estandar)
hist(datos)
# Crear gráfico de densidad
plot(density(datos))


" 
para b, c, d son lo mismo que para a luego la conclusion es la comparacion 
de las 4, muestras con un poco de chamuyo :)
"
#PARA B
#PARA C
#PARA D


