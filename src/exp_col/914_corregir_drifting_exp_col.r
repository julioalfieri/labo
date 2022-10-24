#require vm con
#   8 vCPU
#  64 GB  memoria RAM
# 256 GB  espacio en disco


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")



#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "DR9141"

PARAM$exp_input  <- "CA9060"

#valores posibles  "ninguno" "rank_simple" , "rank_cero_fijo" , "deflacion"
PARAM$metodo  <- "rank_cero_fijo"
# FIN Parametros del script


#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio
#Agregar aqui sus PROPIAS VARIABLES manuales



#------------------------------------------------------------------------------
#deflaciona por IPC
#momento 1.0  31-dic-2020 a las 23:59

drift_deflacion  <- function( campos_monetarios )
{
  vfoto_mes <- c( 201901, 201902, 201903, 201904, 201905, 201906,
                  201907, 201908, 201909, 201910, 201911, 201912,
                  202001, 202002, 202003, 202004, 202005, 202006,
                  202007, 202008, 202009, 202010, 202011, 202012,
                  202101, 202102, 202103, 202104, 202105, 202106,
                  202107  )

  vIPC  <- c( 1.9903030878, 1.9174403544, 1.8296186587,
              1.7728862972, 1.7212488323, 1.6776304408,
              1.6431248196, 1.5814483345, 1.4947526791,
              1.4484037589, 1.3913580777, 1.3404220402,
              1.3154288912, 1.2921698342, 1.2472681797,
              1.2300475145, 1.2118694724, 1.1881073259,
              1.1693969743, 1.1375456949, 1.1065619600,
              1.0681100000, 1.0370000000, 1.0000000000,
              0.9680542110, 0.9344152616, 0.8882274350,
              0.8532444140, 0.8251880213, 0.8003763543,
              0.7763107219  )

  tb_IPC  <- data.table( "foto_mes"= vfoto_mes,
                         "IPC" = vIPC )

  dataset[ tb_IPC,
           on= c("foto_mes"),
           (campos_monetarios) :=  .SD * i.IPC ,
           .SDcols = campos_monetarios ]

}

#------------------------------------------------------------------------------

drift_rank_simple  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ , paste0(campo,"_rank") :=  (frank( get(campo), ties.method="random") - 1) / ( .N -1 ), by= foto_mes]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#El cero se transforma en cero
#los positivos se rankean por su lado
#los negativos se rankean por su lado

drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0(campo,"_rank") := 0 ]
    dataset[ get(campo) > 0, paste0(campo,"_rank") :=   frank(  get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ get(campo) < 0, paste0(campo,"_rank") :=  -frank( -get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/exp_col/", PARAM$exp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/exp_col/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/exp_col/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO



#primero agrego las variables manuales
#AgregarVariables( dataset )

#ordeno de esta forma por el ranking (mes y cliente)
setorder( dataset, foto_mes, numero_de_cliente )

#por como armé los nombres de campos, estos son los campos que expresan variables monetarias
campos_monetarios  <- colnames(dataset)
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"]

#aqui aplico un metodo para atacar el data drifting
#hay que probar experimentalmente cual funciona mejor
switch( 
PARAM$metodo,
  "ninguno"        = cat( "No hay correccion del data drifting" ),
  "rank_simple"    = drift_rank_simple( campos_monetarios ),
  "rank_cero_fijo" = drift_rank_cero_fijo( campos_monetarios ),
  "deflacion"      = drift_deflacion( campos_monetarios ) 
)


ncol(dataset)
nrow(dataset)
155+48-2
fwrite( dataset,
        file="dataset.csv.gz",
        sep= "," )
