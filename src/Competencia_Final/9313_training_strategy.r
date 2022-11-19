#Necesita para correr en Google Cloud
#  64 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")



#Parametros del script
PARAM  <- list()
PARAM$experimento <- "TS9313"

PARAM$exp_input  <- "FE9253"

PARAM$future       <- c( 202109 )

PARAM$final_train  <- c( 201907, 201908, 201909, 201910, 201911, 201912,
                         202011,202012,
                         202101,202102, 202103,202104,202105,202106, 202107)

PARAM$train$training     <- c( 201905, 201906, 201907, 201908, 201909,  201910, 201911, 201912,
                               202011,202012,
                               202101,202103,202104,202105,202107)
PARAM$train$validation   <- c( 202102 )
PARAM$train$testing      <- c( 202106)
PARAM$train$undersampling  <- 0.4   # 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA
PARAM$train$semilla  <- 100019
# FIN Parametros del script


#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/comp_final/", PARAM$exp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/comp_final/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/comp_final/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


setorder( dataset, foto_mes, numero_de_cliente )

#grabo los datos del futuro
fwrite( dataset[ foto_mes %in% PARAM$future, ],
        file= "dataset_future.csv.gz",
        logical01= TRUE,
        sep= "," )

#grabo los datos donde voy a entrenar los Final Models
fwrite( dataset[ foto_mes %in% PARAM$final_train, ],
        file= "dataset_train_final.csv.gz",
        logical01= TRUE,
        sep= "," )



#grabo los datos donde voy a hacer la optimizacion de hiperparametros
set.seed( PARAM$train$semilla )
dataset[ foto_mes %in% PARAM$train$training , azar := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]

dataset[  , fold_train := 0L ]
dataset[ foto_mes %in% PARAM$train$training & 
         ( azar <= PARAM$train$undersampling  | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) )
         , fold_train := 1L ]

dataset[  , fold_validate := 0L ]
dataset[ foto_mes %in% PARAM$train$validation, fold_validate := 1L ]

dataset[  , fold_test := 0L ]
dataset[ foto_mes %in% PARAM$train$testing, fold_test := 1L ]


fwrite( dataset[ fold_train + fold_validate + fold_test >= 1 , ],
        file= "dataset_training.csv.gz",
        logical01= TRUE,
        sep= "," )

