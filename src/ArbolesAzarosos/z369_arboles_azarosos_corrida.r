# Ensemble de arboles de decision
# utilizando el naif metodo de Arboles Azarosos
# entreno cada arbol utilizando un subset distinto de atributos del dataset

# mandatoriamente debe correr en Google Cloud
# sube automaticamente a Kaggle

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("rlist")
require("yaml")

# parametros experimento
PARAM <- list()
PARAM$experimento <- 3690

# parametros rpart

#  cargue aqui los hiperparametros elegidos
PARAM$rpart <- data.table( 
  "cp" = c(-1, -1, -1, -1, -1),
  "minsplit" = c(100, 250, 1000, 250, 50),
  "minbucket" = c(5, 50, 50, 5, 5),
  "maxdepth" = c(8, 6, 6, 6, 12)
)

# parametros  arbol
# entreno cada arbol con solo 50% de las variables variables
#  por ahora, es fijo
PARAM$feature_fraction <- 0.5


# voy a generar 512 arboles,
#  a mas arboles mas tiempo de proceso y MEJOR MODELO,
#  pero ganancias marginales
PARAM$num_trees_max <- 512

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt",
                    verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(substitute(reg), ext)

  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )

    cat(linea, file = archivo)
  }

  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )

  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)

  # imprimo por pantalla
  if (verbose) cat(linea)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# cargo los datos
dataset <- fread( miAmbiente$dataset_pequeno )


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(paste0("./exp/KA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


# que tamanos de ensemble grabo a disco
grabar <- c(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)


# defino los dataset de entrenamiento y aplicacion
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# arreglo clase_ternaria por algun distraido ""
dapply[, clase_ternaria := NA ]

# elimino lo que ya no utilizo
rm(dataset)
gc()

# Establezco cuales son los campos que puedo usar para la prediccion
# el copy() es por la Lazy Evaluation
campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))



# Genero las salidas
for( icorrida in seq(nrow(PARAM$rpart)) ){

  registro <- PARAM$rpart[ icorrida ]
  cat( "Corrida ", icorrida, " ; " )

  # aqui se va acumulando la probabilidad del ensemble
  dapply[, prob_acumulada := 0]

  # los parametros que voy a utilizar para rpart
  param_rpart <- PARAM$rpart[ icorrida ]

  set.seed(miAmbiente$semilla_primigenia) # Establezco la semilla aleatoria

  for (arbolito in seq(PARAM$num_trees_max) ) {
    qty_campos_a_utilizar <- as.integer(length(campos_buenos)
       * PARAM$feature_fraction)

    campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

    # paso de un vector a un string con los elementos
    # separados por un signo de "+"
    # este hace falta para la formula
    campos_random <- paste(campos_random, collapse = " + ")

    # armo la formula para rpart
    formulita <- paste0("clase_ternaria ~ ", campos_random)

    # genero el arbol de decision
    modelo <- rpart(formulita,
      data = dtrain,
      xval = 0,
      control = param_rpart
    )

    # aplico el modelo a los datos que no tienen clase
    prediccion <- predict(modelo, dapply, type = "prob")

    dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]

    if (arbolito %in% grabar) {

      # Genero la entrega para Kaggle
      umbral_corte <- (1 / 40) * arbolito
      entrega <- as.data.table(list(
        "numero_de_cliente" = dapply[, numero_de_cliente],
        "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
      )) # genero la salida

      nom_arch_kaggle <- paste0(
        "KA", PARAM$experimento, "_",
        icorrida, "_",
        sprintf("%.3d", arbolito), # para que tenga ceros adelante
        ".csv"
      )

      # grabo el archivo 
      fwrite(entrega,
        file = nom_arch_kaggle,
        sep = ","
      )


      # subo a Kaggle
      # preparo todo para el submit
      comentario <- paste0( "'",
        "trees=", arbolito,
        " cp=", registro$cp,
        " minsplit=", registro$minsplit,
        " minbucket=", registro$minbucket,
        " maxdepth=", registro$maxdepth,
        "'"
      )

      comando <- paste0( "~/install/proc_kaggle_submit.sh ",
        "TRUE ",
        miAmbiente$modalidad, " ",
        nom_arch_kaggle, " ",
        comentario
      )

      ganancia <- system( comando, intern=TRUE )
      linea <- c( 
        list( "ganancia"= ganancia, "arbolitos"=arbolito),
        registro
      )

      loguear( linea, arch="tb_ganancias.txt" )
    }

    cat(arbolito, " ")
  }
}



# copio
system( "~/install/repobrutalcopy.sh" )

# apago la virtual machine  para que no facture Google Cloud
# Give them nothing, but take from them everything.
system( "sudo shutdown" )

