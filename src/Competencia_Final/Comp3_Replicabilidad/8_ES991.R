#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#Parametros del script
PARAM <- list()
PARAM$dataset  <- "datasets/competenciaFINAL_2022.csv.gz"
PARAM$experimento <- "ES9910_16"
PARAM$exp_input1 <- "ER9910"
PARAM$exp_input2 <- "ER9916"



#------------------------------------------------------------------------------
#Aqui empieza el programa
#setwd("C:\\Users\\alfie\\OneDrive\\Documentos\\Maestria_DM\\Materias\\DMEyF_22\\")
setwd( "~/buckets/b1/" )


#Leo la predicción de los modelos
semillerio_1 <- fread(paste0('exp/comp_final/',PARAM$exp_input1,'/',PARAM$exp_input1,'.csv'))
colnames(semillerio_1) <- c("numero_de_cliente", "mean_1")
semillerio_2 <- fread(paste0('exp/comp_final/',PARAM$exp_input2,'/',PARAM$exp_input2,'.csv'))
colnames(semillerio_2) <- c("numero_de_cliente", "mean_2")

#Rank a cada modelo 
semillerio_1[, rank_1 := frank(mean_1 , ties.method = "random")]
semillerio_2[, rank_2 := frank(mean_2, ties.method = "random")]

#Union de modelos por numero de cliente
ensamble_semillerios <- semillerio_2 [semillerio_1, on = c("numero_de_cliente")]
ensamble_semillerios <- ensamble_semillerios [, list(numero_de_cliente, rank_1, rank_2)]

#calculo la media del rank por numero de cliente
ensamble_semillerios <- ensamble_semillerios[, .(mean = rowMeans(.SD)), by = numero_de_cliente]

#ordeno de manera creciente el rank
setorder(ensamble_semillerios, mean)
ensamble_semillerios


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/comp_final/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/comp_final/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

# Guardo el semillerio 

nom_submit  <- paste0( PARAM$experimento,".csv" )

fwrite(  ensamble_semillerios,
         file= nom_submit,
         sep= "," )


#genero los archivos para Kaggle
cortes  <- seq( from=  9000,
                to=    11000,
                by=     250 )

for( corte in cortes )
{
  ensamble_semillerios[  , Predicted := 0L ]
  ensamble_semillerios[ 1:corte, Predicted := 1L ]
  
  nom_submit  <- paste0( PARAM$experimento, 
                         "_",
                         sprintf( "%05d", corte ),
                         ".csv" )
  
  # Guardo el submit 
  fwrite(  ensamble_semillerios[ , list( numero_de_cliente, Predicted ) ],
           file= nom_submit,
           sep= "," )
  
}




