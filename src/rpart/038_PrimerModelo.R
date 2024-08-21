# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table ,  rpart  y  rpart.plot
# Correr en Google Cloud con RStudio

# cargo las librerias que necesito
require("data.table")
library("data.table")
require("rpart")
# require("rpart.plot")

### version del script
version = "038"

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("~/buckets/b1") # Establezco el Working Directory

# cargo el dataset pequeno vivencial del disco local
dataset <- fread("~/datasets/vivencial_dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables

hiperparametros <- list(
  cp = -0.9,
  minsplit = 940,
  minbucket = 470,
  maxdepth = 6
)

print("Generando modelo con hiperparametros:")
hpstring <- paste("cp=",hiperparametros$cp,
    "minsplit=",hiperparametros$minsplit,
    "minbucket=",hiperparametros$minbucket,
    "maxdepth=",hiperparametros$maxdepth)
print(hpstring)

modelo <- rpart(
    formula = "clase_ternaria ~ .",
    data = dtrain, # los datos donde voy a entrenar
    xval = 0,
    cp = hiperparametros$cp , # esto significa no limitar la complejidad de los splits
    minsplit = hiperparametros$minsplit, # minima cantidad de registros para que se haga el split
    minbucket = hiperparametros$minbucket, # tamaño minimo de una hoja
    maxdepth = hiperparametros$maxdepth  # profundidad maxima del arbol
)



# grafico el arbol
# prp(modelo,
#     extra = 101, digits = -5,
#     branch = 1, type = 4, varlen = 0, faclen = 0
# )


# aplico el modelo a los datos nuevos
prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

nombre_archivo <- paste0("~/buckets/b1/exp/KA2001/K101_",version,"_viv.csv",collapse = "")
paste("Escribiendo predicción en ",nombre_archivo)
# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = nombre_archivo,
        sep = ","
)

comando_kaggle_submit <- paste0("kaggle competitions submit -c labo-i-vivencial-2024-ba -f ",nombre_archivo," -m '",hpstring,"'")
print(comando_kaggle_submit)
cat(comando_kaggle_submit, file = "kaggle_submit")
