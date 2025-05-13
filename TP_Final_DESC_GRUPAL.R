library(corrplot)
library(class)
library(lubridate)
library(readr)
library(funModeling)
library(ggplot2)
library(dplyr)
numericas <- function(dataframe) {
  columnas_numericas <- sapply(dataframe, is.numeric)
  return(dataframe[, columnas_numericas, drop=FALSE])
}
columnas_no_numericas <- function(df) {
  no_numericas <- sapply(df, function(x) !is.numeric(x))
  return(names(df)[no_numericas])
}
moda_dataframe <- function(vec) {
  moda <- names(sort(table(vec), decreasing = TRUE)[1])
  
  return(moda)
}

remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- 1
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}
#1
funcion=function(df,xa=0.01){
  ub=which(is.na(df),arr.ind=TRUE)
  
  coll=unique(ub[,2])
  mat=matrix(colnames(df),dim(df)[2],11,byrow = FALSE)
  for (i in coll){
    mat[i,2]=sum(ub[,2]==i)
  }
  
  for (i in 1:dim(mat)[1]){
    if (mat[i,1]==mat[i,2])
      mat[i,2]=0
  }
  for (i in 1:dim(mat)[1]){
    if (is.numeric(df[[i]])){
      (mat[i,3]=round(mean(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,3]="Texto"}
    if (is.numeric(df[[i]])){
      (mat[i,5]=round(quantile(df[[i]],0,25,na.rm=T),digits = 2))
    }else{mat[i,5]="Texto"}
    if (is.numeric(df[[i]])){
      (mat[i,6]=round(max(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,6]="Texto"}
    if (is.numeric(df[[i]])){
      (mat[i,7]=round(min(df[[i]],na.rm=T),digits = 2))
    }else{mat[i,7]="Texto"}  
    if(is.numeric(df[[i]])){
      (mat[i,8]=quantile(df[[i]],0.75,na.rm=T))
    } else{mat[i,8]="Texto"}
    if(is.numeric(df[[i]])){
      puni=100*round(length(unique(df[[i]]))/length(df[[1]]),digits = 2)
      (mat[i,10]=puni)
    }else{mat[i,10]="Texto"}
  }
  
  for (i in 1:dim(mat)[1]){
    mat[i,4]=round(length(unique(df[[i]])),digits = 1)
    
  }
  
  matdf=data.frame(mat)
  cn=c('columna', 'cant_NA','media','uniq','1 quant','max','min', "3 quant", "extremos", "%Uniq")
  colnames(matdf)=cn
  return(matdf)  
  
}  

reemplazar_fuera_de_rango <- function(x, limite_inferior, limite_superior) {
  x[x < limite_inferior | x > limite_superior] <- NA
  return(x)
}


setwd("C:/Users/avenu/Documents/Itba_2024-1/Descriptiva")
fifa_players_tp <- read_csv("fifa_players_tp.csv")
stat_norm <- read_csv("fifa_players_tp.csv")
fifa_players_ORIG <- read_csv("fifa_players.csv")
B= funcion(fifa_players_ORIG)


A=funcion(fifa_players_tp)

#altura de jugadores
hist(fifa_players_tp$height_cm, xlim = c(150,210),breaks=619)

info_df <- df_status(fifa_players_tp)
ggplot(fifa_players_tp, aes(x = age, y =)) +
  geom_boxplot() +
  theme_minimal()


limite_inferior <- 1
limite_superior <- 99


# Armamos los conjuntos de variables, correlacionadas, para futura imputacion
tiro_columns <- c("long_shots", "shot_power", "freekick_accuracy", "penalties", "finishing", "volleys", "positioning")
for (columna in tiro_columns) {
  fifa_players_tp[[columna]] <- reemplazar_fuera_de_rango(fifa_players_tp[[columna]], limite_inferior, limite_superior)
}
stat_norm[, tiro_columns] <- sapply(fifa_players_tp[, tiro_columns], function(x) reemplazar_fuera_de_rango(x, limite_inferior, limite_superior))
fifa_players_tp$tiro <- round(rowMeans(stat_norm[, tiro_columns], na.rm = TRUE))
lista_indices_na <- list()
for (columna in tiro_columns) {
  lista_indices_na[[columna]] <- which(is.na(fifa_players_tp[[columna]]))
  fifa_players_tp[[columna]][lista_indices_na[[columna]]] <- fifa_players_tp$tiro[lista_indices_na[[columna]]]
}



pase_columnas <-  c("short_passing", "long_passing", "crossing","curve", "vision")
for (columna in pase_columnas) {
  fifa_players_tp[[columna]] <- reemplazar_fuera_de_rango(fifa_players_tp[[columna]], limite_inferior, limite_superior)
}
stat_norm[, pase_columnas] <- sapply(fifa_players_tp[, pase_columnas], function(x) reemplazar_fuera_de_rango(x, limite_inferior, limite_superior))
fifa_players_tp$pase <- round(rowMeans(stat_norm[, pase_columnas], na.rm = TRUE))
lista_indices_na <- list()
for (columna in pase_columnas) {
  lista_indices_na[[columna]] <- which(is.na(fifa_players_tp[[columna]]))
  fifa_players_tp[[columna]][lista_indices_na[[columna]]] <- fifa_players_tp$pase[lista_indices_na[[columna]]]
}


defensa_columnas <- c("heading_accuracy", "standing_tackle", "sliding_tackle", "marking", "interceptions")
for (columna in defensa_columnas) {
  fifa_players_tp[[columna]] <- reemplazar_fuera_de_rango(fifa_players_tp[[columna]], limite_inferior, limite_superior)
}
stat_norm[, defensa_columnas] <- sapply(fifa_players_tp[, defensa_columnas], function(x) reemplazar_fuera_de_rango(x, limite_inferior, limite_superior))
fifa_players_tp$defensa <- round(rowMeans(stat_norm[, defensa_columnas], na.rm = TRUE))
lista_indices_na <- list()
for (columna in defensa_columnas) {
  lista_indices_na[[columna]] <- which(is.na(fifa_players_tp[[columna]]))
  fifa_players_tp[[columna]][lista_indices_na[[columna]]] <- fifa_players_tp$defensa[lista_indices_na[[columna]]]
}


regate_columnas <- c("agility", "balance", "ball_control", "composure", "dribbling", "reactions")
for (columna in regate_columnas) {
  fifa_players_tp[[columna]] <- reemplazar_fuera_de_rango(fifa_players_tp[[columna]], limite_inferior, limite_superior)
}
stat_norm[, regate_columnas] <- sapply(fifa_players_tp[, regate_columnas], function(x) reemplazar_fuera_de_rango(x, limite_inferior, limite_superior))
fifa_players_tp$regate <- round(rowMeans(stat_norm[, regate_columnas], na.rm = TRUE))
lista_indices_na <- list()
for (columna in regate_columnas) {
  lista_indices_na[[columna]] <- which(is.na(fifa_players_tp[[columna]]))
  fifa_players_tp[[columna]][lista_indices_na[[columna]]] <- fifa_players_tp$regate[lista_indices_na[[columna]]]
}


ritmo_columnas <-c("acceleration","sprint_speed")
for (columna in ritmo_columnas) {
  fifa_players_tp[[columna]] <- reemplazar_fuera_de_rango(fifa_players_tp[[columna]], limite_inferior, limite_superior)
}
stat_norm[, ritmo_columnas] <- sapply(fifa_players_tp[, ritmo_columnas], function(x) reemplazar_fuera_de_rango(x, limite_inferior, limite_superior))
fifa_players_tp$ritmo <- round(rowMeans(stat_norm[, ritmo_columnas], na.rm = TRUE))
lista_indices_na <- list()
for (columna in ritmo_columnas) {
  lista_indices_na[[columna]] <- which(is.na(fifa_players_tp[[columna]]))
  fifa_players_tp[[columna]][lista_indices_na[[columna]]] <- fifa_players_tp$ritmo[lista_indices_na[[columna]]]
}


fisico_columnas <- c("strength","aggression","jumping","stamina")
for (columna in fisico_columnas) {
  fifa_players_tp[[columna]] <- reemplazar_fuera_de_rango(fifa_players_tp[[columna]], limite_inferior, limite_superior)
}
stat_norm[, fisico_columnas] <- sapply(fifa_players_tp[, fisico_columnas], function(x) reemplazar_fuera_de_rango(x, limite_inferior, limite_superior))
fifa_players_tp$fisico <- round(rowMeans(stat_norm[, fisico_columnas], na.rm = TRUE))
lista_indices_na <- list()
for (columna in fisico_columnas) {
  lista_indices_na[[columna]] <- which(is.na(fifa_players_tp[[columna]]))
  fifa_players_tp[[columna]][lista_indices_na[[columna]]] <- fifa_players_tp$fisico[lista_indices_na[[columna]]]
}

Summary_tp <- fifa_players_tp[,c(1,2,3,4,5,6,9,10,52,53,54,55,56,57)]
summary(Summary_tp)
colnames(Summary_tp)
A=funcion(fifa_players_tp)
#otros outliers

#EDAD 
fifa_players_tp$age<- remove_outliers(fifa_players_tp$age)
fifa_players_tp$birth_date <- mdy(fifa_players_tp$birth_date)
fecha <- mdy("9/9/2018")
fifa_players_tp$age_calculada <- floor(interval(fifa_players_tp$birth_date, fecha) / years(1))
birth_na <-which(is.na(fifa_players_tp$birth_date))
age_na <- which(is.na(fifa_players_tp$age))
fifa_players_tp$age[age_na] <- fifa_players_tp$age_calculada[age_na]





#altura, supuesto la altura sigue una distribucion normal

altura=remove_outliers(fifa_players_tp$height_cm)
altura_normal <- rnorm(17954, mean(altura, na.rm = T), sd(altura,na.rm=T))
ecdf(altura) %>% plot()
ecdf(altura_normal) %>% plot(add=T)
ks.test(altura, altura_normal)
hist(altura)
#la altura deberia ser normal
#p value extremadamente chico -> Rechazo H0 de altura normal
altura_NA=altura
reemplazar_menor_155 <- function(datos) {
  # Reemplazar los valores menores a 160 con NA
  datos[datos < 155] <- NA
  return(datos)
}
altura_NA=reemplazar_menor_155(altura_NA)
datos_sin_na_alt <-altura_NA[!is.na(altura_NA) ]

create_dummy <- function(data, position_col, values_to_find) {
  # Verificar que la columna especificada existe en el data frame
  if (!position_col %in% colnames(data)) {
    stop("La columna especificada no existe en el data frame.")
  }
  
  # Crear la columna dummy inicializada en 0
  data$dummy <- 0
  
  # Asignar 1 a las filas donde se encuentran los valores especificados
  data$dummy[data[[position_col]] %in% values_to_find] <- 1
  
  return(data)
}
# Valores a buscar
values_to_find <- c('GK', 'CB', 'ST')

# Aplicar la función a los datos de ejemplo

data_regresion <- data.frame(
  nombre=fifa_players_tp$name,
  fisico = fifa_players_tp$fisico,
  peso = fifa_players_tp$weight_kgs,
  fuerza= fifa_players_tp$strength,
  cabezazo=fifa_players_tp$heading_accuracy,
  compostura= fifa_players_tp$composure,
  aceleracion=fifa_players_tp$acceleration,
  balance=fifa_players_tp$balance,
  regate=fifa_players_tp$regate,
  posicion=fifa_players_tp$positions,
  
  
  altura_na = altura_NA  # Asumiendo que 'altura_na' es la variable objetivo en fifa_players_tp
)
data_regresion <- create_dummy(data_regresion, 'posicion', values_to_find)
na_indices_alt <- which(!is.na(data_regresion$altura_na) & complete.cases(data_regresion[, c("peso", "fisico", "cabezazo", "compostura", "regate", "fuerza", "balance", "aceleracion")]))

data_regresion_clean <- data_regresion[!is.na(data_regresion$altura_na), ]



modelo <- lm(altura_na ~  fisico + cabezazo + fuerza + compostura + aceleracion + regate + balance + dummy, data = data_regresion[na_indices_alt,])

# Ver el resumen del modelo
summary(modelo)
# Definir la función
na_indices <- which(is.na(data_regresion$altura_na))

data_na <- data_regresion[na_indices,]

predicciones_na <- predict(modelo, newdata = data_na)
data_na$altura_na <- predicciones_na
fifa_players_tp$height_cm[na_indices] <- round(predicciones_na,1)
hist(fifa_players_tp$height_cm)

cesc= data_regresion %>% filter(nombre=="Cesc Fàbregas")


#peso 

peso=remove_outliers(fifa_players_tp$weight_kgs)
peso_normal <- rnorm(17954, mean(peso, na.rm = T), sd(peso,na.rm=T))
ecdf(peso) %>% plot()
ecdf(peso_normal) %>% plot(add=T)
ks.test(peso, peso_normal)
hist(peso)
#el peso deberia ser normal
#p value extremadamente chico -> Rechazo H0 de peso normal

data_regresion2 <- data.frame(
  nombre=fifa_players_tp$name,
  fisico = fifa_players_tp$fisico,
  altura = fifa_players_tp$height_cm,
  fuerza= fifa_players_tp$strength,
  compostura= remove_outliers(fifa_players_tp$composure),
  aceleracion=remove_outliers(fifa_players_tp$acceleration),
  balance=remove_outliers(fifa_players_tp$balance),
  regate=fifa_players_tp$regate,
  agilidad= fifa_players_tp$agility,
  skill=fifa_players_tp$`skill_moves(1-5)`,
  sprint=fifa_players_tp$sprint_speed,
  reaccion=fifa_players_tp$reactions,
  posicion= fifa_players_tp$positions,
  peso = peso  # Asumiendo que 'altura_na' es la variable objetivo en fifa_players_tp
)
data_regresion2 <- create_dummy(data_regresion2, 'posicion', values_to_find)
data_regresion_clean2 <- data_regresion2[!is.na(data_regresion$peso), ]
modelo2 <- lm(peso ~ dummy+ reaccion+  agilidad + altura + fisico + skill+ fuerza + compostura + aceleracion  + balance  , data = data_regresion_clean2 )
summary(modelo2)
data_na2 <- data_regresion2[is.na(data_regresion2$peso),]

predicciones_na2 <- predict(modelo2, newdata = data_na2)
data_na2$peso <- predicciones_na2

fifa_players_tp$weight_kgs[is.na(data_regresion2$peso)] <- round(predicciones_na2,1)








#KNN

obtener_primer_valor <- function(cadena) {
  valores <- unlist(strsplit(cadena, ","))  # Dividir la cadena en valores separados por comas
  primer_valor <- valores[1]  # Obtener el primer valor
  return(primer_valor)
}
set.seed(133)
columns <- c(46, 53, 54, 55, 56, 57,2)

DATA_KNN <- fifa_players_tp[, columns]
DATA_KNN$positions_1 <- factor(sapply(DATA_KNN$positions, obtener_primer_valor))
datos_con_na <- DATA_KNN[is.na(DATA_KNN$positions), ]
datos_sin_na <- DATA_KNN[!is.na(DATA_KNN$positions), ]

train_index <- 1:round(0.7 * nrow(datos_sin_na))
train_data <- datos_sin_na[train_index, ]
test_data <- datos_sin_na[-train_index, ]

train_features <- scale(train_data[, 2:7])
train_labels <- train_data$positions_1
test_features <- scale(test_data[, 2:7])
test_labels <- test_data$positions_1
datos_na <- datos_con_na[,2:7]


predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = 10)
test_data$prediccion=predictions

datos <- data.frame(
  columna_con_comas=test_data$positions,
  valor_a_verificar = test_data$prediccion # Valores a verificar si están incluidos en 'columna_con_comas'
)

# Verificar si el valor de 'valor_a_verificar' está incluido en 'columna_con_comas' para cada fila
datos$predictions <- as.character(datos$valor_a_verificar)
datos$columna_con_comas <- as.character(datos$columna_con_comas)

# Aplicar la función a cada fila del data frame
datos$esta_incluido <- sapply(seq_len(nrow(datos)), function(i) {
  valor_a_verificar <- datos[i, "predictions"]
  valores_separados <- unlist(strsplit(datos[i, "columna_con_comas"], ","))
  as.integer(valor_a_verificar %in% valores_separados)
})

frecuencias_predicted <- table(datos$esta_incluido)
proporcion <- mean(datos$esta_incluido)
pie(frecuencias_predicted, main = "Proporción de Valores TRUE y FALSE del KNN")



predicciones_nuevos_datos <- knn(train = train_features, test = scale(datos_na), cl = train_labels, k = 10)

# Obtener los índices de los datos con valores NA en la columna 'positions'
NA_indices <- which(is.na(fifa_players_tp$positions))
as.character(predicciones_nuevos_datos)
# Reemplazar los valores NA con las predicciones correspondientes
fifa_players_tp$positions[NA_indices]<- as.character(predicciones_nuevos_datos)



#Tipo de cuerpo


ggplot(fifa_players_tp, aes(x=height_cm, y=weight_kgs, col= factor(body_type))) + geom_point()
set.seed(999)
columns2 <- c(49, 2,3, 32, 57, 34, 39)
DATA_KNN2 <- fifa_players_tp[, columns2]
datos_con_na2 <- DATA_KNN2[is.na(DATA_KNN2$body_type), ]
datos_sin_na2 <- DATA_KNN2[!is.na(DATA_KNN2$body_type), ]

train_index2 <- 1:round(0.7 * nrow(datos_sin_na2))
train_data2 <- datos_sin_na2[train_index2, ]
test_data2 <- datos_sin_na2[-train_index2, ]

train_features <- scale(train_data2[, 2:6])
train_labels <- train_data2$body_type
test_features <- scale(test_data2[, 2:6])
test_labels <- test_data2$body_type
datos_na <- datos_con_na2[,2:6]

predictions2 <- knn(train = train_features, test = test_features, cl = train_labels, k = 10)

test_data2$prediccion=predictions2
test_data2
test_data2$accuracy <- ifelse(test_data2$body_type == test_data2$prediccion, 1, 0)
mean(test_data2$accuracy)

predicciones_nuevos_datos2 <- knn(train = train_features, test = scale(datos_na), cl = train_labels, k = 10)

# Obtener los índices de los datos con valores NA en la columna 'positions'
NA_indices <- which(is.na(fifa_players_tp$body_type))
as.character(predicciones_nuevos_datos2)
# Reemplazar los valores NA con las predicciones correspondientes
fifa_players_tp$body_type[NA_indices]<- as.character(predicciones_nuevos_datos2)

valores_1_5 <- function(column){
  column[column > 5] <- NA
  return(column)
}
fifa_players_tp$`international_reputation(1-5)` <- valores_1_5(fifa_players_tp$`international_reputation(1-5)`)

columns <- c(8, 52,53, 54, 55, 56, 57,1 )

DATA_KNN <- fifa_players_tp[, columns]
datos_con_na <- DATA_KNN[is.na(DATA_KNN$`international_reputation(1-5)`), ]
datos_sin_na <- DATA_KNN[!is.na(DATA_KNN$`international_reputation(1-5)`), ]

train_index <- 1:round(0.7 * nrow(datos_sin_na))
train_data <- datos_sin_na[train_index, ]
test_data <- datos_sin_na[-train_index, ]

train_features <- scale(train_data[, 2:8])
train_labels <- train_data$`international_reputation(1-5)`
test_features <- scale(test_data[, 2:8])
test_labels <- test_data$`international_reputation(1-5)`
datos_na <- datos_con_na[,2:8]


predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = 3)
test_data$prediccion=predictions

datos <- data.frame(
  fama=test_data$`international_reputation(1-5)`,
  valor_a_verificar = test_data$prediccion # Valores a verificar si están incluidos en 'columna_con_comas'
)
datos$esta_incluido <- ifelse(datos$fama==datos$valor_a_verificar,1,0)

frecuencias_predicted <- table(datos$esta_incluido)
mean(datos$esta_incluido)
pie(frecuencias_predicted, main = "Proporción de Valores TRUE y FALSE del KNN")


predicciones_nuevos_datos <- knn(train = train_features, test = scale(datos_na), cl = train_labels, k = 10)

# Obtener los índices de los datos con valores NA en la columna 'positions'
NA_indices <- which(is.na(fifa_players_tp$`international_reputation(1-5)`))
as.character(predicciones_nuevos_datos)
# Reemplazar los valores NA con las predicciones correspondientes
fifa_players_tp$`international_reputation(1-5)`[NA_indices]<- predicciones_nuevos_datos



fifa_players_tp$`skill_moves(1-5)` <- valores_1_5(fifa_players_tp$`skill_moves(1-5)`)
ggplot(fifa_players_tp, aes(x=regate, y=agility, col= factor(`skill_moves(1-5)`))) + geom_point()
columns <- c(10, 19,26,55 )

DATA_KNN <- fifa_players_tp[, columns]
datos_con_na <- DATA_KNN[is.na(DATA_KNN$`skill_moves(1-5)`), ]
datos_sin_na <- DATA_KNN[!is.na(DATA_KNN$`skill_moves(1-5)`), ]

train_index <- 1:round(0.7 * nrow(datos_sin_na))
train_data <- datos_sin_na[train_index, ]
test_data <- datos_sin_na[-train_index, ]

train_features <- scale(train_data[, 2:4])
train_labels <- train_data$`skill_moves(1-5)`
test_features <- scale(test_data[, 2:4])
test_labels <- test_data$`skill_moves(1-5)`
datos_na <- datos_con_na[,2:4]


predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = 3)
test_data$prediccion=predictions

datos <- data.frame(
  fama=test_data$`skill_moves(1-5)`,
  valor_a_verificar = test_data$prediccion # Valores a verificar si están incluidos en 'columna_con_comas'
)
datos$esta_incluido <- ifelse(datos$fama==datos$valor_a_verificar,1,0)
skill <- datos %>% group_by(fama) %>% summarise(mean=mean(esta_incluido))
frecuencias_predicted <- table(datos$esta_incluido)
mean(datos$esta_incluido)
pie(frecuencias_predicted, main = "Proporción de Valores TRUE y FALSE del KNN")


predicciones_nuevos_datos <- knn(train = train_features, test = scale(datos_na), cl = train_labels, k = 3)

# Obtener los índices de los datos con valores NA en la columna 'positions'
NA_indices <- which(is.na(fifa_players_tp$`skill_moves(1-5)`))
as.character(predicciones_nuevos_datos)
# Reemplazar los valores NA con las predicciones correspondientes
fifa_players_tp$`skill_moves(1-5)`[NA_indices]<- predicciones_nuevos_datos

fifa_players_tp$`weak_foot(1-5)`<- valores_1_5(fifa_players_tp$`weak_foot(1-5)`)
columns <- c(9,37,33,26,23,15 )

DATA_KNN <- fifa_players_tp[, columns]
datos_con_na <- DATA_KNN[is.na(DATA_KNN$`weak_foot(1-5)`), ]
datos_sin_na <- DATA_KNN[!is.na(DATA_KNN$`weak_foot(1-5)`), ]

train_index <- 1:round(0.7 * nrow(datos_sin_na))
train_data <- datos_sin_na[train_index, ]
test_data <- datos_sin_na[-train_index, ]

train_features <- scale(train_data[, 2:6])
train_labels <- train_data$`weak_foot(1-5)`
test_features <- scale(test_data[, 2:6])
test_labels <- test_data$`weak_foot(1-5)`
datos_na <- datos_con_na[,2:6]


predictions <- knn(train = train_features, test = test_features, cl = train_labels, k = 3)
test_data$prediccion=predictions

datos <- data.frame(
  fama=test_data$`weak_foot(1-5)`,
  valor_a_verificar = test_data$prediccion # Valores a verificar si están incluidos en 'columna_con_comas'
)
datos$esta_incluido <- ifelse(datos$fama==datos$valor_a_verificar,1,0)
skill <- datos %>% group_by(fama) %>% summarise(mean=mean(esta_incluido))
frecuencias_predicted <- table(datos$esta_incluido)
mean(datos$esta_incluido)
pie(frecuencias_predicted, main = "Proporción de Valores TRUE y FALSE del KNN")


predicciones_nuevos_datos <- knn(train = train_features, test = scale(datos_na), cl = train_labels, k = 3)

# Obtener los índices de los datos con valores NA en la columna 'positions'
NA_indices <- which(is.na(fifa_players_tp$`weak_foot(1-5)`))
as.character(predicciones_nuevos_datos)
# Reemplazar los valores NA con las predicciones correspondientes
fifa_players_tp$`weak_foot(1-5)`[NA_indices]<- predicciones_nuevos_datos

# pierna preferida
NA_indices <- which(is.na(fifa_players_tp$preferred_foot))
a=moda_dataframe(fifa_players_tp$preferred_foot)
table(fifa_players_tp$preferred_foot)
fifa_players_tp$preferred_foot[NA_indices]<-a

A=funcion(fifa_players_tp)


#Rating
fifa_players_tp$overall_rating=ifelse(fifa_players_tp$overall_rating>100, NA,fifa_players_tp$overall_rating)
data_regresion2 <- data.frame(
  media=fifa_players_tp$overall_rating,
  positions=fifa_players_tp$positions,
  nombre=fifa_players_tp$name,
  fisico = fifa_players_tp$fisico,
  altura = fifa_players_tp$height_cm,
  ritmo= fifa_players_tp$ritmo,
  tiro=fifa_players_tp$tiro,
  regate=fifa_players_tp$regate,
  pase= fifa_players_tp$pase,
  defensa=fifa_players_tp$defensa,
  fama=fifa_players_tp$`international_reputation(1-5)`
)

data_regresion_clean2 <- data_regresion2[!is.na(data_regresion2$media), ]
data_na2 <- data_regresion2[is.na(data_regresion2$media),]

for(i in unique(data_na2$positions)){
  train_data <- data_regresion_clean2 %>% filter(positions==i)
  fila <-  which(fifa_players_tp$positions == i & is.na(fifa_players_tp$overall_rating))
  data_na2 <- data_regresion2[is.na(data_regresion2$media),] %>% filter(positions==i)
  
  modelo2 <- lm(media ~ fisico  + altura + tiro + regate + pase + defensa + fama    , data = train_data )
  print(summary(modelo2))

  predicciones_na2 <- round(predict(modelo2, newdata = data_na2))
  fifa_players_tp$overall_rating[fila] <- predicciones_na2
}

#potential

fifa_players_tp$potential <- stat_norm$potential
fifa_players_tp$potential=ifelse(fifa_players_tp$potential>100, NA,fifa_players_tp$potential)
data_regresion2 <- data.frame(
  media=fifa_players_tp$overall_rating,
  positions=fifa_players_tp$positions,
  nombre=fifa_players_tp$name,
  fisico = fifa_players_tp$fisico,
  altura = fifa_players_tp$height_cm,
  ritmo= fifa_players_tp$ritmo,
  edad=fifa_players_tp$age,
  potencial=fifa_players_tp$potential,
  fama=fifa_players_tp$`international_reputation(1-5)`
)

data_regresion_clean2 <- data_regresion2[!is.na(data_regresion2$potencial), ]
data_na2 <- data_regresion2[is.na(data_regresion2$potencial),]

for(i in unique(data_na2$positions)){
  train_data <- data_regresion_clean2 %>% filter(positions==i)
  fila <-  which(fifa_players_tp$positions == i & is.na(fifa_players_tp$potential))
  data_na2 <- data_regresion2[is.na(data_regresion2$potencial),] %>% filter(positions==i)
  
  modelo2 <- lm(potencial ~ fisico  + altura + media + edad + fama    , data = train_data )
  print(summary(modelo2))
  
  predicciones_na2 <- round(predict(modelo2, newdata = data_na2))
  fifa_players_tp$potential[fila] <- predicciones_na2
}

data_na2 <- data_regresion2[is.na(fifa_players_tp$potential),]
data_regresion_clean2 <- data_regresion2[!is.na(data_regresion2$potencial), ]
modelo2 <- lm(potencial ~ fisico  + altura + media + edad + fama    , data = data_regresion_clean2 )
print(summary(modelo2))
indices <- which(is.na(fifa_players_tp$potential))
predicciones_na2 <- round(predict(modelo2, newdata = data_na2))
data_na2$potencial<- predicciones_na2
fifa_players_tp$potential[indices] <- predicciones_na2

unique(fifa_players_tp$national_team)

unique(fifa_players_ORIG$body_type)

