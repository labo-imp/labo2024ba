# Grafico de la ganancia que visualiza el overfitting
# Solapa graficos de ganancias
#  creado para encender el fuego cognitivo de Ivan Celaye



rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("yaml")
require("ggplot2")

require("ranger")
require("randomForest") # solo se usa para imputar nulos



PARAM <- list()

PARAM$experimento <- 3990

PARAM$active <- list(
  "rpart" = FALSE, # deshabilitado, muy pobres ganancias
  "arbolesazarosos" = TRUE,
  "ranger" = TRUE
)


# cargue a gustto los siguientes hiperparametros
#  preferentemente los mejores que encuentre con una Bayesian Optimization

PARAM$rpart <- list(
  "cp" = -1,
  "minsplit" = 300,
  "minbucket" = 20,
  "maxdepth" = 11
)

PARAM$arbolesazarosos <- list(
  "feature_fraction" = 0.5,
  "num_trees" = 50,
  "cp" = -1,
  "minsplit" = 100,
  "minbucket" = 10,
  "maxdepth" = 10
)


PARAM$ranger <- list(
  "num.trees" = 300, # cantidad de arboles
  "mtry" = 7, # cantidad de atributos que participan en cada split
  "min.node.size" = 380, # tamaño minimo de las hojas
  "max.depth" = 20 # 0 significa profundidad infinita
)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#   que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30),
#  agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
       if (!is.na(seed)) set.seed(seed)

       bloque <- unlist(mapply(
              function(x, y) {
                     rep(y, x)
              },
              division, seq(from = start, length.out = length(division))
       ))

       data[, (campo) := sample(rep(
              bloque,
              ceiling(.N / length(bloque))
       ))[1:.N],
       by = agrupa
       ]
}
#------------------------------------------------------------------------------

modelo_rpart <- function( pdata, param_rpart )
{

  # Entreno el modelo
  # los datos donde voy a entrenar
  # aqui es donde se deben probar distintos hiperparametros
  modelo <- rpart(
    formula = "clase_ternaria ~ . -fold",
    data = pdata[fold == 1, ],
    xval = 0,
    cp = param_rpart$cp,
    minsplit = param_rpart$minsplit,
    minbucket = param_rpart$minbucket,
    maxdepth = param_rpart$maxdepth
  )


  # aplico el modelo a TODOS los datos, inclusive los de training
  prediccion <- predict(modelo, pdata, type = "prob")

  # Pego la probabilidad de  BAJA+2
  tablita <- pdata[, list(fold, clase_ternaria)]
  tablita[, prob_baja2 := prediccion[, "BAJA+2"]]


  # Dibujo la curva de ganancia acumulada
  setorder(tablita, fold, -prob_baja2)

  # agrego una columna que es la de las ganancias
  # la multiplico por 2 para que ya este normalizada
  #  es 2 porque cada fold es el 50%
  tablita[, gan := 2 *ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]
  tablita[, ganancia_acumulada := cumsum(gan), by = fold]
  tablita[, pos := sequence(.N), by = fold]
  tablita[, modelo := ifelse( fold==1, "rpart.train","rpart.test") ]

  return( tablita )
}
#------------------------------------------------------------------------------

modelo_arbolesazarosos <- function( pdata, param_aa )
{
  campos_buenos <- copy(setdiff(colnames(pdata), c("clase_ternaria", "fold")))
  qty_campos_a_utilizar <- as.integer(length(campos_buenos)
    * param_aa$feature_fraction)

  set.seed( miAmbiente$semilla_primigenia ) # Establezco la semilla aleatoria

  # aqui acumulo las probabilidades
  prob_acum <- rep( 0, nrow( pdata) )

  # genero los arbolitos
  for( i in  seq( param_aa$num_trees ) )
  {
    campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

    # paso de un vector a un string con los elementos
    # separados por un signo de "+"
    # este hace falta para la formula
    campos_random <- paste(campos_random, collapse = " + ")

    # armo la formula para rpart
    formulita <- paste0("clase_ternaria ~ ", campos_random)

    # Entreno el modelo
    # los datos donde voy a entrenar
    # aqui es donde se deben probar distintos hiperparametros
    modelo <- rpart(
      formulita,
      data = pdata[fold == 1, ],
      xval = 0,
      cp = param_aa$cp,
      minsplit = param_aa$minsplit,
      minbucket = param_aa$minbucket,
      maxdepth = param_aa$maxdepth
    )


    # aplico el modelo a TODOS los datos, inclusive los de training
    prediccion <- predict(modelo, pdata, type = "prob")

    # acumulo
    prob_acum <- prob_acum + prediccion[, "BAJA+2"]
  }

  # Pego la probabilidad de  BAJA+2
  tablita <- pdata[, list(fold, clase_ternaria)]
  tablita[, prob_baja2 := prob_acum]


  # Dibujo la curva de ganancia acumulada
  setorder(tablita, fold, -prob_baja2)

  # agrego una columna que es la de las ganancias
  # la multiplico por 2 para que ya este normalizada
  #  es 2 porque cada fold es el 50%
  tablita[, gan := 2 *ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]
  tablita[, ganancia_acumulada := cumsum(gan), by = fold]
  tablita[, pos := sequence(.N), by = fold]
  tablita[, modelo := ifelse( fold==1, "AA.train","AA.test") ]

  return( tablita )
}
#------------------------------------------------------------------------------

modelo_ranger <- function( pdata, param_ranger )
{

  # asigno un valor muy negativo
  #  estas dos lineas estan relacionadas con el Data Drifting
  dataset_ranger <- copy( pdata )

  # defino donde entreno y donde aplico el modelo
  setorder(dataset_ranger, fold, clase_ternaria) 
  dtrain <- dataset_ranger[fold==1]
  dapply <- dataset_ranger[fold==2]



  set.seed( miAmbiente$semilla_primigenia ) # Establezco la semilla aleatoria


  # ranger necesita la clase de tipo factor
  factorizado <- as.factor(dtrain$clase_ternaria)
  dtrain[, clase_ternaria := factorizado]

  # imputo los nulos, ya que ranger no acepta nulos
  # Leo Breiman, ¿por que le temias a los nulos?
  dtrain <- na.roughfix(dtrain)

  # genero el modelo de Random Forest llamando a ranger()
  modelo <- ranger(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    probability = TRUE, # para que devuelva las probabilidades
    num.trees = param_ranger$num.trees,
    mtry = param_ranger$mtry,
    min.node.size = param_ranger$min.node.size,
    max.depth = param_ranger$max.depth
  )


  # Carpinteria necesaria sobre  dapply
  # como quiere la Estadistica Clasica, imputar nulos por separado
  # ( aunque en este caso ya tengo los datos del futuro de anteman
  #  pero bueno, sigamos el librito de estos fundamentalistas a rajatabla ...
  dapply[ , clase_ternaria := NULL ]
  dapply <- na.roughfix(dapply)

  # aplico el modelo recien creado a los datos del futuro
  campos_buenos <- setdiff( colnames(dtrain), "clase_ternaria" )
  prediccion <- predict(modelo, 
    rbindlist( list(dtrain[,campos_buenos, with=FALSE], dapply) ) )

  # Pego la probabilidad de  BAJA+2
  tablita <- dataset_ranger[, list(fold, clase_ternaria)]
  tablita[, prob_baja2 := prediccion$predictions[, "BAJA+2"]]


  # Dibujo la curva de ganancia acumulada
  setorder(tablita, fold, -prob_baja2)

  # agrego una columna que es la de las ganancias
  # la multiplico por 2 para que ya este normalizada
  #  es 2 porque cada fold es el 50%
  tablita[, gan := 2 *ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]
  tablita[, ganancia_acumulada := cumsum(gan), by = fold]
  tablita[, pos := sequence(.N), by = fold]
  tablita[, modelo := ifelse( fold==1, "ranger.train","ranger.test") ]

  return( tablita )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui empieza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/GA", PARAM$experimento, "/")
dir.create(paste0("./exp/GA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)

#cargo miAmbiente
miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

# cargo los datos
dataset <- fread( miAmbiente$dataset_pequeno )


# a partir de ahora solo trabajo con 202107, el mes que tiene clase
dataset <- dataset[foto_mes == 202107] # defino donde voy a entrenar


# La division training/testing es 50%, 50%
#  que sea 50/50 se indica con el c(1,1)
particionar(dataset,
  division = c(1, 1),
  agrupa = "clase_ternaria",
  seed = miAmbiente$semilla_primigenia
)


# genero modelo para  rpart
if( PARAM$active$rpart ) {
  tablita <- modelo_rpart( dataset, PARAM$rpart )
  if( !exists("tb_final") ){
    tb_final <- copy(tablita)
  } else {
    tb_final <- rbindlist( list( tb_final, tablita ) )
  }
}


# genero modelo para  Arboles Azarosos
if( PARAM$active$arbolesazarosos ) {
  tablita <- modelo_arbolesazarosos( dataset, PARAM$arbolesazarosos )
  if( !exists("tb_final") ){
    tb_final <- copy(tablita)
  } else {
    tb_final <- rbindlist( list( tb_final, tablita ) )
  }
}


# genero modelo para  ranger
if( PARAM$active$ranger ) {
  tablita <- modelo_ranger( dataset, PARAM$ranger )
  if( !exists("tb_final") ) {
    tb_final <- copy(tablita)
  } else {
    tb_final <- rbindlist( list( tb_final, tablita ) )
  }
}


# muestro ganancias
tb_final[, max(ganancia_acumulada), list( modelo, fold ) ]

# Esta hermosa curva muestra como en el mentiroso training
#   la ganancia es siempre mejor que en el real testing

# defino hasta donde muestra el grafico
amostrar <- ifelse( miAmbiente$modalidad == "conceptual",
  5000,
  20000
)


# grafico de las curvas
gra <- ggplot(
           data = tb_final[pos <= amostrar],
           aes( x = pos, y = ganancia_acumulada,
                color = modelo)
             ) + geom_line()

# pdf( "ganancias_curvas.pdf" )

print( gra )

# dev.off()

