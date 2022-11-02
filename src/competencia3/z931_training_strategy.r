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
PARAM$experimento <- "TS9310"

PARAM$exp_input  <- "FE9250"

PARAM$future       <- c( 202107 )

PARAM$final_train  <- c( 202103, 202104, 202105 )

PARAM$train$training     <- c( 202101, 202102, 202103 )
PARAM$train$validation   <- c( 202104 )
PARAM$train$testing      <- c( 202105 )
PARAM$train$undersampling  <- 0.1   # 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA ~1:10k
PARAM$train$semilla  <- 100019      # 4176782 Continua , 38478 B+1|B+2 originalmente
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
setwd("C:\\Users\\alfie\\OneDrive\\Documentos\\Maestria_DM\\Materias\\DMEyF_22\\")
#setwd( "~/buckets/b1/" )

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset_fzr_L2_T.csv.gz" )
dataset  <- fread( dataset_input )
dataset_input 

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


setorder( dataset, foto_mes, numero_de_cliente )

#grabo los datos del futuro
fwrite( dataset[ foto_mes %in% PARAM$future, ],
        file= "dataset_future_fzr_L2_T.csv.gz",
        logical01= TRUE,
        sep= "," )

#grabo los datos donde voy a entrenar los Final Models
fwrite( dataset[ foto_mes %in% PARAM$final_train, ],
        file= "dataset_train_final_fzr_L2_T.csv.gz",
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
        file= "dataset_training_fzr_L2_T.csv.gz",
        logical01= TRUE,
        sep= "," )

