# Este script almacena:
#   la modalidad de cursada que usted ha elegido ( Conceptual, Vivencial )
#   su semilla primigenia = su primer semilla

require("yaml")

miAmbiente <- list()

miAmbiente$modalidad <-  "conceptual"
# miAmbiente$modalidad <-  "vivencial"

miAmbiente$dataset_pequeno <- "~/datasets/conceptual_dataset_pequeno.csv"
# miAmbiente$dataset_pequeno <- "~/datasets/vivencial_dataset_pequeno.csv"


miAmbiente$dataset_competencia <- "~/datasets/conceptual_competencia_2024.csv.gz"
# miAmbiente$dataset_competencia <- "~/datasets/vivencial_competencia_2024.csv.gz"


# aqui va su primer semilla
miAmbiente$semilla_primigenia <- 102191L


write_yaml( miAmbiente,
  file="~/buckets/b1/miAmbiente.yml" )


# mantenimiento del ambiente
dir.create("~/buckets/b1/exp/", showWarnings = FALSE)
dir.create("~/buckets/b1/repos/", showWarnings = FALSE)
dir.create("~/buckets/b1/repos/labo2024ba", showWarnings = FALSE)

cat("Espere unos minutos mientras se hace un backup\n")
system( "~/install/repobrutalcopy.sh" )

cat("\nel ambiente ha sido GRABADO\n")