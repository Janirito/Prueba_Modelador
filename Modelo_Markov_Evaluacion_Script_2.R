# Script numero 2 Prueba de Seleccion Modelador
# Janier Hersain Rosero Urbina

" El objetivo de este codigo es la compacion de los 2 tratamientos teniendo encuenta sus condiciones en datos relaes, con el fin de validar si la sugerencia de utilizar 
el tratamiento 1 del primer analisis, mejora la calidad de vida de los pacientes."

# -------------------------------
## 1. CARGA DE DATOS Y PARÁMETROS
### en esta seccion importamoslas librerias y leeremos el archivo adjunto al problema el cual consiste en los datos de propgacion de la infeccion

library(ggplot2)
library(readxl)

### Datos de propagación ingresados como dataaframe
datos <- read_excel("C:/Users/JAN/Desktop/Prueba_Salud/Propagación.xlsx")

### Desarrollo de Graficos del la propgacion

ggplot(datos, aes(x=dia, y=Infectado)) +
  geom_line(color="red", linewidth=1.5) +
  labs(title="Crecimiento de infectados", x="Día", y="Número de infectados") +
  theme_minimal()
# -------------------------------

# -------------------------------
## 2. Ingreso de Parámetros clínicos 
### En esta seccion se ingresa manualmente cada una de las consideraciones de los tratamientos, relacionados con la mortalidad, crisis, seculas.
### Para cada tratmiento se consideran las condiciones descritas en el problema inicial 

duracion_asintomatico <- 10/365
duracion_crisis <- 14/365
duracion_secuelas <- 10

### Tratamiento 1 (reduce mortalidad, secuelas moderadas)
mortalidad_t1 <- 0.03
calidad_crisis_t1 <- 0.88  # 12% reducción
calidad_secuelas_t1 <- 0.88 # Igual que en crisis

### Tratamiento 2 (no afecta mortalidad, reduce secuelas)
mortalidad_t2 <- 0.15
calidad_crisis_t2 <- 0.50  # 50% reducción (efectos adversos)
calidad_secuelas_t2 <- 0.96 # Solo 4% reducción
# -------------------------------

# -------------------------------
## 3. Funcion DE CAlculo de Calidad de Vida
### Se desarriolla una funcion simple que evalua la calidad de vida de cada paciente acorde a las condiciones de cada tratamiento
### mediante esta funcion es posible estimar la calidad de vida de cada tratamiento y la interaccion de cada variable con los estados del proceso infeccioso del paciente

calcular_qalys <- function(mortalidad, calidad_crisis, calidad_secuelas) {
  qaly_asint <- duracion_asintomatico * 1  # Fase asintomática (100% calidad)
  qaly_crisis <- duracion_crisis * calidad_crisis
  qaly_secuelas <- duracion_secuelas * calidad_secuelas * (1 - mortalidad) # Solo sobrevivientes
  
  return(qaly_asint + qaly_crisis + qaly_secuelas)
  
}

# -------------------------------

# -------------------------------
## 4. Calculo Comparativo
### En esta seccion se busca el valuar el numero de casos infectados y realizar la comparacion con la funcion anterior
### adicionalmente se esrtima la calidad de vida esperada y las muertes, estos datos se obtienen con las condiciones da cada tratamiento

total_infectados <- max(datos$Infectado)

# QALYs por paciente
qaly_t1 <- calcular_qalys(mortalidad_t1, calidad_crisis_t1, calidad_secuelas_t1)
qaly_t2 <- calcular_qalys(mortalidad_t2, calidad_crisis_t2, calidad_secuelas_t2)

# QALYs totales
qaly_total_t1 <- total_infectados * qaly_t1
qaly_total_t2 <- total_infectados * qaly_t2

# Muertes esperadas
muertes_t1 <- total_infectados * mortalidad_t1
muertes_t2 <- total_infectados * mortalidad_t2

# -------------------------------

# -------------------------------
## 5. RESULTADOS
### Luego de realizar los calclos  se procede a crear un data frame con las variables resultado y desarrollar las graficas que nos evidencias la interaccion de los inferados con la calidad de vida de cada tratamiento

resultados <- data.frame(
  Tratamiento = c("Tratamiento 1", "Tratamiento 2"),
  QALYs_por_paciente = c(qaly_t1, qaly_t2),
  QALYs_totales = c(qaly_total_t1, qaly_total_t2),
  Muertes_esperadas = c(muertes_t1, muertes_t2),
  Diferencia_QALYs = c(NA, qaly_t2 - qaly_t1)
)

print(resultados)

### Desarrollo de Graficos del los calculos

ggplot(resultados, aes(x=Tratamiento, y=QALYs_por_paciente, fill=Tratamiento)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(QALYs_por_paciente,2)), vjust=-0.5) +
  labs(title="Comparación de QALYs por paciente entre tratamientos",
       y="Calidad de Vida", x="") +
  theme_minimal()
# -------------------------------

# -------------------------------
## 6. ANÁLISIS DE SENSIBILIDAD

### Finalmente se incluye un analisis de sensibilidad el cual busca comparar en unidades la diferencia entre cada tratamiento
### ¿Cuánto deberían mejorar las secuelas del Tratamiento 2 para ser mejor?
umbral_secuelas_t2 <- calidad_secuelas_t1 * (1 - mortalidad_t1) / (1 - mortalidad_t2)

cat("\nEl Tratamiento 2 necesitaría que sus secuelas tengan >", 
    round(umbral_secuelas_t2,4)*100, "% de calidad\n",
    "para superar al Tratamiento 1 (actualmente es ", calidad_secuelas_t2*100, "%)")
# -------------------------------
