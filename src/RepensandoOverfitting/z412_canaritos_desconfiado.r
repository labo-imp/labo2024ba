#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("~/buckets/b1/" )  # establezco la carpeta donde voy a trabajar
# cargo el dataset
dataset <- fread( "~/datasets/vivencial_dataset_pequeno.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/CN4110/", showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd("./exp/CN4110/")

# uso esta semilla para los canaritos
set.seed(102191)

# agrego los siguientes canaritos
for( i in 1:154 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]



#Primero  veo como quedan mis arboles
 modelo  <- rpart(formula= "clase_ternaria ~ . ",
               data = dataset[ foto_mes==202107 ,],
               model = TRUE,
               xval = 0,
               cp = -0.5,
               minsplit = 2000,
               minbucket = 800,
               maxdepth = 6)


pdf(file = "arbol_canaritos_desconfiado.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

