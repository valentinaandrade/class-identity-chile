
rm(list = ls()) # Limpiar la consola
#----------------------------------------------------------------------------------------
#Carga de Datos
#----------------------------------------------------------------------------------------
#install.packages("xlsx")
#install.packages('rJava')
#install.packages('xlsxjars')
#install.packages('xlsx')
library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(quanteda)
library(purrr)
library(tidyverse)
library(rJava)
library(xlsxjars)
library(xlsx)
library("readxl")
library(knitr)
library(e1071)

#Inputs o Variables Necesarias
# Parametrizacion sencilla, llamar excel a distintas celdas
# Parametrizacion compleja, esperar Andres.

args <- commandArgs(trailingOnly = TRUE)

dir_bases <- args[1]
input_train <- args[2]
#input_test <- args[3]
stopwords_list <- args[4]
class_list <- args[5]

train_clase <- args[6]
train_id <- args[7]
train_texto <- args[8]

#test_clase <- args[9]
#test_id <- args[10]
#test_texto <- args[11]

# Outputs
#forecast_test <- args[12]
#forecast_test_p <- args[13]
#forecast_train <- args[14]


texts <-read_excel(paste(dir_bases, input_train, sep="")) # Input Glosas Entrenamiento

texts$x <- texts$x %>% gsub("\\|", "",.) #Eliminar caracteres extraños |


# Seleccion de variables
texts <- texts %>% select(train_clase,train_id,train_texto)

# Se renombran las variables con nombres mÃ¡s prÃ¡cticos
texts <- texts %>% rename(clase = train_clase, texto = train_texto, text_id = train_id)


#-------------------------------------------------------------------------------------------
#Limpieza de textos y tokenización
#-------------------------------------------------------------------------------------------
limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada texto
texts <- texts %>% mutate(texto_tokenizado = map(.x = texto,
                                                 .f = limpiar_tokenizar))


#------------------------------------------------------------------------------------------
#Análisis Exploratorio
#------------------------------------------------------------------------------------------
texts_tidy <- texts %>% select(-texto) %>% unnest() #expandir
texts_tidy <- texts_tidy %>% rename(token = texto_tokenizado)


#5-StopWords
#---------------------------------------------------------
lista_stopwords <- stopwords("spanish")
lista_stopwords_producto <- read_excel(paste(dir_bases, stopwords_list, sep=""))

# Se añaden términos al listado de stopwords
lista_stopwords <- c(lista_stopwords, lista_stopwords_producto$Var)

# Se filtran las stopwords
texts_tidy <- texts_tidy %>% filter(!(token %in% lista_stopwords))

texts_tidy$token <- str_replace_all(texts_tidy$token, "[áàäâåã]", "a")
texts_tidy$token <- str_replace_all(texts_tidy$token, "[éèëê]", "e")
texts_tidy$token <- str_replace_all(texts_tidy$token, "[íïîì]", "i")
texts_tidy$token <- str_replace_all(texts_tidy$token, "[óôöòõô]", "o")
texts_tidy$token <- str_replace_all(texts_tidy$token, "[úûùü]", "u")
texts_tidy$token <- str_replace_all(texts_tidy$token, "[ç]", "c")

#-------------------------------------------------------------------------------------------
#Frecuencia de Término (TF) y Frecuencia Inversa de Documento (IDF)
#-------------------------------------------------------------------------------------------
#TF
# Número de veces que aparece cada término por texto
texts_tf <- texts_tidy %>% group_by(text_id, token) %>% summarise(n = n())

# Se añade una columna con el total de términos por texto
texts_tf <- texts_tf %>% mutate(total_n = sum(n))

# Se calcula el tf
texts_tf <- texts_tf %>% mutate(tf = n / total_n )

#IDF
total_documentos = texts_tidy$text_id %>% unique() %>% length()

# Número de documentos en los que aparece cada término
texts_idf <- texts_tidy %>% distinct(token, text_id) %>% group_by(token) %>%
  summarise(n_documentos = n())

# Cálculo del idf
texts_idf <- texts_idf %>% mutate(idf = n_documentos/ total_documentos) %>%
  arrange(desc(idf))

#TF-IDF
texts_tf_idf <- left_join(x = texts_tf, y = texts_idf, by = "token") %>% ungroup()
texts_tf_idf <- texts_tf_idf %>% mutate(tf_idf = tf * idf)
texts_tf_idf %>% select(-text_id) %>% head() %>% kable()

#-------------------------------------------------------------------------------------------
#Clasificación de Textos
#-------------------------------------------------------------------------------------------

#Conjuntos de Datos de entrenamiento y prueba
#------------------------------------------------

clase_list <- read_excel(paste(dir_bases, class_list, sep=""))

texts_sector <- texts %>% filter(clase %in% c(clase_list$Clases))

train <- sample(x = 1:nrow(texts_sector), size = 1 * nrow(texts_sector))
texts_train <- texts_sector[train, ]

#Vectorización TF-IDF
#------------------------------------------------
# Limpieza y tokenización de los documentos de entrenamiento
texts_train$texto <- texts_train$texto %>% map(.f = limpiar_tokenizar) %>%
  map(.f = paste, collapse = " ") %>% unlist()

# Creación de la matriz documento-término
matriz_tfidf_train <- dfm(x = texts_train$texto, remove = lista_stopwords)

# Se reduce la dimensión de la matriz eliminando aquellos términos que
# aparecen en menos de 5 documentos. Con esto se consigue eliminar ruido.
matriz_tfidf_train <- dfm_trim(x = matriz_tfidf_train, min_docfreq = 5)

# Conversión de los valores de la matriz a tf-idf
matriz_tfidf_train <- tfidf(matriz_tfidf_train, scheme_tf = "prop",
                            scheme_df = "inverse")

# Identificación de las dimensiones de la matriz de entrenamiento
# Los objetos dm() son de clase S4, se accede a sus elementos mediante @
dimensiones_matriz_train <- matriz_tfidf_train@Dimnames$features
# Conversión de vector a diccionario pasando por lista
dimensiones_matriz_train <- as.list(dimensiones_matriz_train)
names(dimensiones_matriz_train) <- unlist(dimensiones_matriz_train)
dimensiones_matriz_train <- dictionary(dimensiones_matriz_train)

#Support Vector Machine
#---------------------------------------------------------------------------------


#Ajuste del modelo
modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(texts_train$clase),
                  kernel = "linear", cost = 0.75, scale = TRUE, probability = TRUE,
                  type = "C-classification")

# Guardar Local SVM y Objetos necesarios

save(modelo_svm, file='output/modelo_svm.RData')
save(dimensiones_matriz_train, file='output/dimensiones_matriz_train.RData')
save(matriz_tfidf_train, file='output/matriz_tfidf_train.RData')


