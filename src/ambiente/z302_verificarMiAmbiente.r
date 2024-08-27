# Este script verifica existencia y consistencia de su ambiente
#   archivo  ~/buckets/b1/miAmbiente.yml

require("yaml")
require("primes")

#------------------------------------------------------------------------------
# Error catching

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  stop("He habido un error catastrofico, solicite ayuda lo antes posible.")
})
#------------------------------------------------------------------------------

if( !file.exists( "~/buckets/b1/miAmbiente.yml" ) )
  stop( "Error catastrofico, no existe el archivo ~/buckets/b1/miAmbiente.yml" )


miAmbiente <- read_yaml( "~/buckets/b1/miAmbiente.yml" )

if( ! is.list( miAmbiente ) )
  stop( "Error catastrofico, formato de archivo no es una lista" )

if( !( "semilla_primigenia" %in% names( miAmbiente ) ) )
  stop( "Error no existe el campo semilla_primigenia" )


if( ! is.integer( miAmbiente$semilla_primigenia ) )
  stop( "Error, semilla_primigenia debe ser un numero entero, verifico que termine con la letra L?" )

if(  miAmbiente$semilla_primigenia < 100000L )
  stop( "Error, semilla_primigenia debe ser mayor a 100000" )

if(  miAmbiente$semilla_primigenia > 1000000L )
  stop( "Error, semilla_primigenia debe ser menor a 1000000" )

if(  ! is_prime( miAmbiente$semilla_primigenia ) )
  stop( "Error, semilla_primigenia debe ser un numero primo" )


#------------------

if( !( "modalidad" %in% names( miAmbiente ) ) )
  stop( "Error no existe el campo  modalidad" )

if( ! is.character( miAmbiente$modalidad ) )
  stop( "Error, modalidad debe ser un character?" )


if( !( miAmbiente$modalidad %in% c( "conceptual", "vivencial" )) )
  stop( "Error el campo modalidad solo puede tomar los valores  'conceptual' o 'vivencial'" )


if( Sys.getenv("USER") == "jenic" &  miAmbiente$modalidad != "vivencial" )
  stop( "Jorge Nicolau, usted debe mandatoriamente elegir la modalidad vivencial" )

if( Sys.getenv("USER") == "hernangalletti" &  miAmbiente$modalidad != "vivencial" )
  stop( "Hernan Galletti, usted debe mandatoriamente elegir la modalidad vivencial" )

#------------------

if( !( "dataset_pequeno" %in% names( miAmbiente ) ) )
  stop( "Error no existe el campo  dataset_pequeno" )

if( ! is.character( miAmbiente$dataset_pequeno ) )
  stop( "Error, dataset_pequeno debe ser un character?" )


if( !( miAmbiente$dataset_pequeno %in% c( "~/datasets/conceptual_dataset_pequeno.csv", "~/datasets/vivencial_dataset_pequeno.csv" )) )
  stop( "Error el campo dataset_pequeno solo puede tomar los valores  '~/datasets/conceptual_dataset_pequeno.csv', '~/datasets/vivencial_dataset_pequeno.csv' " )

if( miAmbiente$modalidad == "conceptual" & miAmbiente$dataset_pequeno != "~/datasets/conceptual_dataset_pequeno.csv" )
  stop( "Error, Inconsistencia usted eligio la modalidad conceptual pero su dataset_pequeno NO es  '~/datasets/conceptual_dataset_pequeno.csv'" )

if( miAmbiente$modalidad == "vivencial" & miAmbiente$dataset_pequeno != "~/datasets/vivencial_dataset_pequeno.csv" )
  stop( "Error, Inconsistencia usted eligio la modalidad vivencial pero su dataset_pequeno NO es  '~/datasets/vivencial_dataset_pequeno.csv'" )


if( ! file.exists( miAmbiente$dataset_pequeno ) )
  stop( paste0( "No existe en su maquina virtual el archivo : ", miAmbiente$dataset_pequeno ) )

#------------------

if( !( "dataset_competencia" %in% names( miAmbiente ) ) )
  stop( "Error no existe el campo  dataset_competencia" )

if( ! is.character( miAmbiente$dataset_competencia ) )
  stop( "Error, dataset_competencia debe ser un character?" )


if( !( miAmbiente$dataset_competencia %in% c( "~/datasets/conceptual_competencia_2024.csv.gz", "~/datasets/vivencial_competencia_2024.csv.gz" )) )
  stop( "Error el campo dataset_competencia solo puede tomar los valores  '~/datasets/conceptual_competencia_2024.csv.gz', '~/datasets/vivencial_competencia_2024.csv.gz'  " )

if( miAmbiente$modalidad == "conceptual" & miAmbiente$dataset_competencia != "~/datasets/conceptual_competencia_2024.csv.gz" )
  stop( "Error, Inconsistencia usted eligio la modalidad conceptual pero su dataset_competencia NO es  '~/datasets/conceptual_competencia_2024.csv.gz'" )

if( miAmbiente$modalidad == "vivencial" & miAmbiente$dataset_competencia != "~/datasets/vivencial_competencia_2024.csv.gz" )
  stop( "Error, Inconsistencia usted eligio la modalidad vivencial pero su dataset_competencia NO es  '~/datasets/vivencial_competencia_2024.csv.gz'" )


if( ! file.exists( miAmbiente$dataset_competencia ) )
  stop( paste0( "No existe en su maquina virtual el archivo : ", miAmbiente$dataset_competencia ) )

#------------------

cat("\n\n  Ambiente verificado, todo se encuentra bien. No tema.\n\n" )