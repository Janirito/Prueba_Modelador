# Script numero 1 Prueba de Seleccion Modelador
# Janier Hersain Rosero Urbina


# El objetivo de este codigo es realizar las simulaciones atraves del modelo de Markov para definir que tratamiento ofrece una mejor alternativa en la calidad de vida de los pacientes infectados 

# -------------------------------
## 1. Importacion de Librerias
### En esta seccion utilzaremos 4 librerias las cuales corresponden
### a el desarrollo de graficos como ggplot y diagram y otras relacionados con la modelacion como tidyr y heemod.
# -------------------------------
library(heemod)
library(ggplot2)
library(diagram)
library(tidyr)

# -------------------------------
## 2. Consruccion de Matrices.
### En el archivo adjunto de este codigo se encuentra el desarrollo del sistema de ecuaciones.
### donde se reflejan las considraciones necesarias para la elaboracion de las matrice a continuacion

#### A = ASINTOMATICO
#### B = CRISIS
#### C = MUERTE
#### D = POST-CONTAGIO 

### Para los dos tratamientos se utilizo define_transition el cual; convierte los valores en una matriz de transicion
### las cuales usaremos para emplear la libreria de heemod.
### Esta matriz es fundamental en modelos de Markov para simular cómo los pacientes avanzan entre diferentes estados de salud descritos como(A,B,C y D)
# -------------------------------
Tratamiento_1 <- define_transition(

  0,1,0,0,
  0,0,0.03,0.97,
  0,0,1,0,
  0,0,0,1
)

Tratamiento_2 <- define_transition(

  0,1,0,0,
  0,0,0.15,0.85,
  0,0,1,0,
  0,0,0,1
)
# -------------------------------
## 3. Representacion Grafia de Grafos
### en este seccion se visualizan los grafos relacionados con el problema y las matrices establecidas utiliando la funcion plot y estableciendo un color azul para el tratamiento 1 y rojo para el tratamiento 2

plot(Tratamiento_1, box.col = 4,cex.txt = 1.8)
plot(Tratamiento_2, box.col = 2, cex.txt = 1.8)
# -------------------------------

# -------------------------------
## 4. Definicion de estados
### Estados
### A continuacion definimos 6 estados utilizando la funcion define_state para cada tratamiento, utilizaremos el consto 100 de manera constante 
### y los demas seran feninidos seguns las especificaciones el texto
### La variable utility sera aquella que este relacionada con la calidad de vida

state_A <- define_state(    ### Este estado es el ideal ya que su calidad de vida es 100%
  cost = 100,
  utility = 1
)

state_C <- define_state(    ### Este estado se refiere a el proceso de muerte
  cost = 0,
  utility = 0
)

### Estados específicos Tratamiento 1

state_B_tx1 <- define_state(  ### Crisis con Tratamiento 1
  cost = 0,                   ### Costo del tratamiento 0
  utility = 0.88              ### Calidad de vida: 88% (100% - 12%) a corde a la definicion
)

state_D_tx1 <- define_state(  ### Secuelas con Tratamiento 1
  cost = 100,                 ### Costo del tratamiento 100
  utility = 0.88              ### Efecto permanente
)

### Estados específicos Tratamiento 2 ----
state_B_tx2 <- define_state(  ### Crisis con Tratamiento 2
  cost = 100,                 ### Costo del tratamiento 0
  utility = 0.50              ### Calidad de vida reducida al 50%
)

state_D_tx2 <- define_state(  ### Secuelas con Tratamiento 2
  cost = 100,                 ### Costo del tratamiento 0
  utility = 0.96              ### Solo 4% de reducción
)
# -------------------------------

# -------------------------------
## 5. Definición de estrategias
### en esta seccion utilizamos define Strategy  ya que esta define la transcicion en cada tratamiento le adiciona las condiciones de cada estado en la seccion 4 y su interaccion entre ambos.
### Es importantre resaltar que para evaluar los tratamientos utilizaremos los estados que le corresponden a cada uno.

trat_1 <- define_strategy(
  transition = Tratamiento_1,
  A = state_A,
  B = state_B_tx1,
  C = state_C,
  D = state_D_tx1
)

trat_2 <- define_strategy(
  transition = Tratamiento_2,
  A = state_A,
  B = state_B_tx2,
  C = state_C,
  D = state_D_tx2
)
# -------------------------------

# -------------------------------
## 6. Ejecucion del modelo
### Inicialmente el modelo markov fue direccionado a 10 años, esto debido a las especificaciones de la descripcion de problema, 
### en este caso utilizaremos la comparacion de los 2 tratamientos con sus estados correspondientes y se obtendra la variable de costo y utility
### la funcion utilizada es run_model la cual ejecuta el modelo sugerido para este estudio

res_mod <- run_model(
  tratramiento_1 = trat_1,
  tratramiento_2 = trat_2,
  cycles = 10,
  cost = cost,
  effect = utility
  
)

summary(res_mod)
# -------------------------------

# -------------------------------
## 7. Desarrollo de Graficos del Modelo
### para realizar una mejor manipuolacion de los datos resultado utilizaremos eval_strategy_list el cual nos devuelve una lista con los valores de las simulaciones del modelo

valores_tratamiento_1 <- res_mod$eval_strategy_list$tratramiento_1$counts
valores_tratamiento_2 <- res_mod$eval_strategy_list$tratramiento_2$counts

### en este orden de ideas convertimos esa lista a un dataframe para mejorar la manipulacion de los datos

df_tratamiento1 <- as.data.frame(valores_tratamiento_1)
df_tratamiento2 <- as.data.frame(valores_tratamiento_2)


### Debido a que en el procesamiento anterior se conviertieron los valores resultado en data frame, utilizaremos la biblioteca tidyr, para generar una table pivote de los estados
### Adicionalmente utilizamos ggplot para desarrollar un grafico de linea con cada estado evaluado

## Grafica de simulacion y estados el tratamiento 1 

df_plot <- df_tratamiento1 %>% 
  dplyr::mutate(cycle = 1:nrow(.)) %>%
  tidyr::pivot_longer(cols = -cycle, 
                      names_to = "Estado", 
                      values_to = "Población")

ggplot(df_plot, aes(x = cycle, y = Población, color = Estado, group = Estado)) +
  geom_smooth(method = "loess",   
              formula = y ~ x,    
              se = FALSE,         
              linewidth = 1.2,    
              alpha = 0.6) +      
  geom_point(size = 2, alpha = 0.5) +  
  labs(title = "Evolución de Estados - Tratamiento 1",
       x = "Ciclo (Años)",
       y = "Número de Individuos",
       color = "Estado") +
  theme_minimal() +
  scale_color_manual(values = c("A" = "blue", 
                                "B" = "black", 
                                "C" = "green", 
                                "D" = "red"),
                     labels = c("Asintomático", "Crisis", "Muerte", "Post-crisis")) +
  scale_x_continuous(breaks = scales::pretty_breaks())


## Grafica de simulacion y estados el tratamiento 2
df_plot <- df_tratamiento2 %>% 
  dplyr::mutate(cycle = 1:nrow(.)) %>%
  tidyr::pivot_longer(cols = -cycle, 
                      names_to = "Estado", 
                      values_to = "Población")

ggplot(df_plot, aes(x = cycle, y = Población, color = Estado, group = Estado)) +
  geom_smooth(method = "loess",   
              formula = y ~ x,    
              se = FALSE,         
              linewidth = 1.2,    
              alpha = 0.6) +      
  geom_point(size = 2, alpha = 0.5) +  
  labs(title = "Evolución de Estados - Tratamiento 1",
       x = "Ciclo (Años)",
       y = "Número de Individuos",
       color = "Estado") +
  theme_minimal() +
  scale_color_manual(values = c("A" = "blue", 
                                "B" = "black", 
                                "C" = "green", 
                                "D" = "red"),
                     labels = c("Asintomático", "Crisis", "Muerte", "Post-crisis")) +
  scale_x_continuous(breaks = scales::pretty_breaks())
# -------------------------------