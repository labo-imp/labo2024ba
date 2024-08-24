# Script para conmemorar la curiosidad científica de Mariano Taha
# 10-repated 5-fold cross validation

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")
require("primes")
require("ggplot2")


PARAM <- list()
# reemplazar por su primer semilla
PARAM$semilla_primigenia <- 102191
PARAM$qsemillas <- 10  # la parte de 10-repeated
PARAM$qfolds <- 5  # cantidad de folds del  k-fold Cross Validation


# elegir SU dataset comentando/ descomentando
PARAM$dataset_nom <- "~/datasets/vivencial_dataset_pequeno.csv"
# PARAM$dataset_nom <- "~/datasets/conceptual_dataset_pequeno.csv"


PARAM$rpart1 <- list (
  "cp" = -1,
  "minsplit" = 170,
  "minbucket" = 70,
  "maxdepth" = 7
)


PARAM$rpart2 <- list (
  "cp" = -1,
  "minsplit" = 1900,
  "minbucket" = 800,
  "maxdepth" = 3
)


#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset que consiste
#  en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#  crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# fold_test  tiene el numero de fold que voy a usar para testear,
#  entreno en el resto de los folds
# param tiene los hiperparametros del arbol

ArbolSimple <- function(fold_test, param_rpart) {
  # genero el modelo
  # entreno en todo MENOS el fold_test que uso para testing
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold != fold_test, ],
    xval = 0,
    control = param_rpart
  )

  # aplico el modelo a los datos de testing
  # aplico el modelo sobre los datos de testing
  # quiero que me devuelva probabilidades
  prediccion <- predict(modelo,
    dataset[fold == fold_test, ],
    type = "prob"
  )

  # esta es la probabilidad de baja
  prob_baja2 <- prediccion[, "BAJA+2"]

  # calculo la ganancia
  ganancia_testing <- dataset[fold == fold_test][
    prob_baja2 > 1 / 40,
    sum(ifelse(clase_ternaria == "BAJA+2",
      117000, -3000
    ))
  ]

  # esta es la ganancia sobre el fold de testing, NO esta normalizada
  return(ganancia_testing)
}
#------------------------------------------------------------------------------

DosArbolesCrossValidation <- function(semilla, param_rpart1, param_rpart2, qfolds, pagrupa) {
  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos
  divi <- rep(1, qfolds)

  # particiono en dataset en folds
  particionar(dataset, 
    divi,
    seed = semilla,
    agrupa = pagrupa
  )


  ganancias1 <- mcmapply(ArbolSimple,
    seq(qfolds), # 1 2 3 4 5
    MoreArgs = list(param_rpart1),
    SIMPLIFY = FALSE,
    mc.cores = 1  # dentro NO quiero paralelismo
  )

  ganancias2 <- mcmapply(ArbolSimple,
    seq(qfolds), # 1 2 3 4 5
    MoreArgs = list(param_rpart2),
    SIMPLIFY = FALSE,
    mc.cores = 1  # dentro NO quiero paralelismo
  )

  # elimino el campo fold del dataset, para no ensuciar
  dataset[, fold := NULL]


  tb_salida <- as.data.table( list(
    "semilla" = rep( semilla, qfolds ),
    "ganancia1" = unlist( ganancias1 ) * qfolds,
    "ganancia2" = unlist( ganancias2 ) * qfolds
   ) )

  return(tb_salida)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/") # Establezco el Working Directory


# genero numeros primos
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(PARAM$semilla_primigenia) # inicializo 
# me quedo con PARAM$qsemillas   semillas
PARAM$semillas <- sample(primos, PARAM$qsemillas )


# cargo los datos
dataset <- fread(PARAM$dataset_nom)

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]


dir.create("~/buckets/b1/exp/EX2460", showWarnings = FALSE)
setwd("~/buckets/b1/exp/EX2460")



# la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
#  tantas veces como valores tenga el vector  PARAM$semillas
salidas <- mcmapply(
  DosArbolesCrossValidation,
  PARAM$semillas, # paso el vector de semillas
  MoreArgs = list(PARAM$rpart1, PARAM$rpart2, PARAM$qfolds, "clase_ternaria"), # aqui paso el segundo parametro
  SIMPLIFY = FALSE,
  mc.cores = detectCores()
)


# paso la lista a vector
tb_salida <- rbindlist(salidas)




# grafico densidades

grafico <- ggplot( tb_salida, aes(x=ganancia1)) + geom_density(alpha=0.25)  +
             geom_density(data=tb_salida, aes(x=ganancia2), fill="purple", color="purple",  alpha=0.10)

pdf("densidad_dos.pdf")
print(grafico)
dev.off()


print( tb_salida[ , list( "arbol1" = mean( ganancia1),  "arbol2" = mean(ganancia2) ) ] )

print( tb_salida[ , list( "prob( m1 > m2)" = sum(ganancia1 > ganancia2 )/ .N ) ]  )


wt <- wilcox.test(  tb_salida$ganancia1,  tb_salida$ganancia2 )
cat( "Wilcoxon Test p-value ", wt$p.value, "\n" )


