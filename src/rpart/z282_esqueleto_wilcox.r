# esqueleto de wilcox
# Cuales son los Ganadores Indiscutidos segun el test de Wilcoxon ?
# https://es.wikipedia.org/wiki/Conjunto_parcialmente_ordenado

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory
# cargo los datos


# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("~/buckets/b1/exp/HT2810/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/HT2810/" )


tb_grid_search_detalle <- fread( "gridsearch_detalle.txt" )


# genero y grabo el resumen
tb_grid_search <- tb_grid_search_detalle[,
  list( "ganancia_mean" = mean(ganancia_test),
    "qty" = .N,
    "ganancias" = list(ganancia_test )),
  list( cp, maxdepth, minsplit, minbucket )
]

# ordeno descendente por ganancia
setorder( tb_grid_search, -ganancia_mean )

# genero un id a la tabla
tb_grid_search[, id := .I ]


# tabla donde voy a almacenar el orden parcial
tb_orden_parcial <- data.table(
  id1 = integer(),
  id2 = integer(),
  pvalue = integer()
)


# solo el primer registro , por eso va el 1
id1 <- tb_grid_search[ 1, id ]
vganador <- tb_grid_search[ 1, ganancias[[1]] ]


# comparo el primer registro contra el resto
for( j in 2:nrow(tb_grid_search) )
{
  id2 <- tb_grid_search[ j, id ]

  wt <- wilcox.test( 
    vganador,
    tb_grid_search[ j, ganancias[[1]] ],
    paired = TRUE
  )

  tb_orden_parcial <- rbindlist(
    list( tb_orden_parcial,
      list( id1, id2, wt$p.value ) )
  )
}


# grabo la tabla con el orden parcial
fwrite( tb_orden_parcial,
  file = "tb_orden_parcial.txt",
  sep = "\t"
)

