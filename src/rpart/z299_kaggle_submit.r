# Script prueba de submit Kaggle 

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("yaml")

PARAM <- list()

PARAM$modalidad <- "vivencial"  # "conceptual"

PARAM$rpart <- list (
  "cp" = -1,
  "minsplit" = 170,
  "minbucket" = 70,
  "maxdepth" = 7
)

#------------------------------------------------------------------------------

getandincrement <- function( nom_archivo )
{
  contador <- read_yaml(nom_archivo)
  valor <- contador$contador
  contador$contador <- contador$contador + 1L
  write_yaml( contador, file=nom_archivo )
  return( valor )
}
#------------------------------------------------------------------------------

generarmodelo <- function( pmodalidad, param )
{
  # cargo el dataset pequeno
  dataset <- fread( paste0("~/datasets/", pmodalidad, "_dataset_pequeno.csv" ) )

  dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
  dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

  # genero el modelo,  aqui se construye el arbol
  # quiero predecir clase_ternaria a partir de el resto de las variables
  modelo <- rpart(
      formula = "clase_ternaria ~ .",
      data = dtrain, # los datos donde voy a entrenar
      xval = 0,
      control = param,
  )

  # aplico el modelo a los datos nuevos
  prediccion <- predict(
      object = modelo,
      newdata = dapply,
      type = "prob"
  )

  # agrego a dapply una columna nueva que es la probabilidad de BAJA+2
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

  # solo le envio estimulo a los registros
  #  con probabilidad de BAJA+2 mayor  a  1/40
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

  # archivo de salida
  contador <- getandincrement("contador.yml")
  archivo_submit <- paste0( "K100_", pmodalidad, "_",
     sprintf("%.3d", contador),
     ".csv"
  )

  # solo los campos para Kaggle
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = archivo_submit,
         sep = ","
  )

  # preparo todo para el submit
  comentario <- paste0( "'",
      "cp=", param$cp,
      " minsplit=", param$minsplit,
      " minbucket=", param$minbucket,
      " maxdepth=", param$maxdepth,
      "'"
  )

  comando <- paste0( "~/install/proc_kaggle_submit.sh ",
      "TRUE ",
      pmodalidad, " ",
      archivo_submit, " ",
      comentario
  )

  ganancia <- system( comando, intern=TRUE )
  cat( paste0( ganancia, "\t", archivo_submit, "\n"),
      file="tb_ganancias.txt",
      append=TRUE
  )

  return(  as.numeric(ganancia) )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# aqui empieza el programa

# creo la carpeta donde voy a trabajar
dir.create("~/buckets/b1/exp/KA2000", showWarnings = FALSE)
setwd("~/buckets/b1/exp/KA2000")

# creo el contador
if( !file.exists( "contador.yml" ) )
{
  contador <- list( "contador" = 1L )
  write_yaml( contador, file="contador.yml" )
}


# Genero modelo y submit a Kaggle
generarmodelo( PARAM$modalidad, PARAM$rpart )
