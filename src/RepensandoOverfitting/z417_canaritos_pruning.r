# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("~/buckets/b1/") # establezco la carpeta donde voy a trabajar

# cargo el dataset
dataset <- fread( "~/datasets/vivencial_dataset_pequeno.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/CN4110/", showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd("./exp/CN4110/")

# uso esta semilla para los canaritos
set.seed(102191)

# agrego los siguientes canaritos
for( i in 1:155 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

dtrain[, clase_binaria2 := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]
dtrain[, clase_ternaria := NULL ]

pesos <- dtrain[ , ifelse( clase_binaria2=="POS", 5.0, 1.0 ) ]

# Dejo crecer el arbol sin ninguna limitacion
# sin limite de altura ( 30 es el maximo que permite rpart )
# sin limite de minsplit ( 2 es el minimo natural )
# sin limite de minbukcet( 1 es el minimo natural )
# los canaritos me protegeran
modelo_original <- rpart(
    formula = "clase_binaria2 ~ .",
    data = dtrain,
    model = TRUE,
    xval = 0,
    cp = -1,
    minsplit = 2, # dejo que crezca y corte todo lo que quiera
    minbucket = 1,
    maxdepth = 30,
    weights = pesos
)


# hago el pruning de los canaritos
# haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[
    modelo_original$frame$var %like% "canarito",
    "complexity"
] <- -666

modelo_pruned <- prune(modelo_original, -666)

prediccion <- predict(modelo_pruned, dapply, type = "prob")[, "POS"]

tb_pred <- dapply[ , list(numero_de_cliente) ]
tb_pred[, prob := prediccion ]

setorder( tb_pred, -prob )
tb_pred[ , Predicted := 0L ]
tb_pred[ 1:11000, Predicted := 1L ]

fwrite( tb_pred[, list(numero_de_cliente, Predicted)],
        file= "stopping_at_canaritos.csv",
        sep = ",")


pdf(file = "stopping_at_canaritos.pdf", width = 28, height = 4)
prp(modelo_pruned,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

dev.off()
