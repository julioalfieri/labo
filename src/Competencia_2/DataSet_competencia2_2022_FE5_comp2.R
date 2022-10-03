#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

setwd("C:\\Users\\alfie\\OneDrive\\Documentos\\Maestria_DM\\Materias\\DMEyF_22\\")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv")


#Cocientes
imp <- c("ctrx_quarter", "mcaja_ahorro", "mpayroll", "mcuentas_saldo")
dataset[ , cociente_1 := (ctrx_quarter/mcaja_ahorro) ]
dataset[ , cociente_2 := (ctrx_quarter/mpayroll) ]
dataset[ , cociente_3 := (ctrx_quarter/mcuentas_saldo) ]

dataset[ , cociente_4 := (mcaja_ahorro/mpayroll) ]
dataset[ , cociente_5 := (ctrx_quarter/mcuentas_saldo) ]

dataset[ , cociente_6 := (mpayroll/mcuentas_saldo) ]

#Features con Data Drifting segun curvas 613_graficar_densidades_03_05 (34 features seleccionadas)
dd_03_05 <- c("mcomisiones",  "mcuenta_corriente", "mcaja_ahorro", "mcuentas_saldo",
               "mtarjeta_visa_consumo", "mprestamos_personales", "cprestamos_prendarios", "cprestamos_hipotecarios", "cplazo_fijo",
               "mpayroll", "ccuenta_debitos_automaticos", "mcuenta_debitos_automaticos", "ctarjeta_master_debitos_automaticos",
               "mttarjeta_master_debitos_automaticos", "ccomisiones_mantenimiento",  "mforex_sell",
               "ctransferencias_emitidas","ccheques_depositados","ccheques_emitidos","chomebanking_transacciones",
               "Master_mfinanciacion_limite","Master_Fvencimiento", "Master_msaldototal",
               "Master_fultimo_cierre", "Master_mpagado",  "Master_mpagominimo","Visa_mfinanciacion_limite", 
               "Visa_msaldototal","Visa_mconsumospesos","Visa_madelantodolares", "Visa_fultimo_cierre",
               "Visa_mpagosdolares", "Visa_mconsumototal", "Visa_mpagominimo")

#dd_03_05 <- c("mcomisiones", "mrentabilidad_annual", "mcuenta_corriente", "mcaja_ahorro", "mcaja_ahorro_dolares", "mcuentas_saldo",
#            "ctarjeta_debito_transacciones", "mtarjeta_visa_consumo", "ctarjeta_master_transacciones", "mtarjeta_master_consumo", 
#            "mprestamos_personales", "cprestamos_prendarios", "cprestamos_hipotecarios", "cplazo_fijo","mpayroll", "ccuenta_debitos_automaticos",
#             "mcuenta_debitos_automaticos", "ctarjeta_visa_debitos_automaticos","mttarjeta_visa_debitos_automaticos", "ctarjeta_master_debitos_automaticos",
#             "mttarjeta_master_debitos_automaticos", "ccomisiones_mantenimiento", "mcomisiones_otras", "mforex_sell","ctransferencias_recibidas",
#             "ctransferencias_emitidas","ccheques_depositados","ccheques_emitidos","chomebanking_transacciones", "ccajas_otras",
#             "matm_other","Master_mfinanciacion_limite","Master_Fvencimiento", "Master_Finiciomora","Master_msaldototal","Master_msaldopesos",
#             "Master_fultimo_cierre", "Master_mpagado", "Master_mpagospesos", "Master_mpagominimo","Visa_mfinanciacion_limite", 
#             "Visa_msaldototal","Visa_msaldopesos","Visa_mconsumospesos","Visa_madelantodolares", "Visa_fultimo_cierre",
#             "Visa_mpagado","Visa_mpagosdolares", "Visa_mconsumototal", "Visa_mpagominimo")


length(dd_03_05)

# VER "Visa_Finiciomora" y "Master_Finiciomora" QUITAR del dataset

#Evaluamos cuales de estas features tienen observaciones <0 (11)
dd_neg <- c()
for( campo in dd_03_05 )
{ 
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dd_neg <- c(dd_neg, campo)
  }
}

#Features solo con observaciones >0 (23)
dd_pos <- dd_03_05[!(dd_03_05 %in% dd_neg)]


#Rank para variables solo con obs > 0 (dd_pos) en 03 y 05

for( campo in dd_pos  )
{
  dataset[foto_mes==202103 , paste0( campo, "_rank" ) := (frank(dataset[ foto_mes==202103 , get(campo) ], ties.method="min", na.last="keep")-1)/(nrow(dataset[ foto_mes==202103 , ])-1) ]
  dataset[foto_mes==202105 , paste0( campo, "_rank" ) := (frank(dataset[ foto_mes==202105 , get(campo) ], ties.method="min", na.last="keep")-1)/(nrow(dataset[ foto_mes==202105 , ])-1) ]
  dataset[, paste0(campo) := NULL] # elimino la variable original 
  
}



#Rank para variables con alguna obs <0 (dd_neg) en 03 y 05 (perdón el hard code,  no me salía el for loop)

cols_neg <- c()
cols_pos <- c()
for( campo in dd_neg )
{ 
  if(  dataset[ get(campo) < 0, .N ]  > 0 ) {
    dataset[   , paste0( campo, "_neg" ) := ifelse( get(campo)< 0, get(campo), ifelse(is.na(get(campo)),NA,0))]
    cols_neg <- c(cols_neg, paste0( campo, "_neg"))
    dataset[   , paste0( campo, "_pos" ) := ifelse( get(campo)> 0, get(campo), ifelse(is.na(get(campo)),NA,0))]
    cols_pos <- c(cols_pos, paste0( campo, "_pos"))
  }
}


#"mcomisiones"

dataset[ foto_mes==202103  , mcomisiones_neg_rank  := -(frank(-dataset[ foto_mes==202103 , mcomisiones_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , mcomisiones_pos_rank  :=  (frank(dataset[ foto_mes==202103 , mcomisiones_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , mcomisiones_neg_rank  := -(frank(-dataset[ foto_mes==202105 , mcomisiones_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , mcomisiones_pos_rank  :=  (frank(dataset[ foto_mes==202105 , mcomisiones_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , mcomisiones_rank  := ifelse(mcomisiones_neg_rank< 0, mcomisiones_neg_rank, mcomisiones_pos_rank) ]
dataset <- dataset[, c("mcomisiones", "mcomisiones_neg_rank", "mcomisiones_pos_rank","mcomisiones_neg", "mcomisiones_pos"):=NULL]



#"mcuenta_corriente"

dataset[ foto_mes==202103  , mcuenta_corriente_neg_rank  := -(frank(-dataset[ foto_mes==202103 , mcuenta_corriente_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , mcuenta_corriente_pos_rank  :=  (frank(dataset[ foto_mes==202103 , mcuenta_corriente_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , mcuenta_corriente_neg_rank  := -(frank(-dataset[ foto_mes==202105 , mcuenta_corriente_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , mcuenta_corriente_pos_rank  :=  (frank(dataset[ foto_mes==202105 , mcuenta_corriente_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , mcuenta_corriente_rank  := ifelse(mcuenta_corriente_neg_rank< 0, mcuenta_corriente_neg_rank, mcuenta_corriente_pos_rank) ]
dataset[, c("mcuenta_corriente", "mcuenta_corriente_neg_rank", "mcuenta_corriente_pos_rank","mcuenta_corriente_neg", "mcuenta_corriente_pos"):=NULL]


#"mcaja_ahorro"
dataset[ foto_mes==202103  , mcaja_ahorro_neg_rank  := -(frank(-dataset[ foto_mes==202103 , mcaja_ahorro_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , mcaja_ahorro_pos_rank  :=  (frank(dataset[ foto_mes==202103 , mcaja_ahorro_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , mcaja_ahorro_neg_rank  := -(frank(-dataset[ foto_mes==202105 , mcaja_ahorro_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , mcaja_ahorro_pos_rank  :=  (frank(dataset[ foto_mes==202105 , mcaja_ahorro_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , mcaja_ahorro_rank  := ifelse(mcaja_ahorro_neg_rank< 0, mcaja_ahorro_neg_rank, mcaja_ahorro_pos_rank) ]
dataset[, c("mcaja_ahorro", "mcaja_ahorro_neg_rank", "mcaja_ahorro_pos_rank", "mcaja_ahorro_neg", "mcaja_ahorro_pos" ):=NULL]


#"mcuentas_saldo"
dataset[ foto_mes==202103  , mcuentas_saldo_neg_rank  := -(frank(-dataset[ foto_mes==202103 , mcuentas_saldo_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , mcuentas_saldo_pos_rank  :=  (frank(dataset[ foto_mes==202103 , mcuentas_saldo_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , mcuentas_saldo_neg_rank  := -(frank(-dataset[ foto_mes==202105 , mcuentas_saldo_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , mcuentas_saldo_pos_rank  :=  (frank(dataset[ foto_mes==202105 , mcuentas_saldo_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , mcuentas_saldo_rank  := ifelse(mcuentas_saldo_neg_rank< 0, mcuentas_saldo_neg_rank, mcuentas_saldo_pos_rank) ]
dataset[, c("mcuentas_saldo", "mcuentas_saldo_neg_rank", "mcuentas_saldo_pos_rank", "mcuentas_saldo_neg", "mcuentas_saldo_pos" ):=NULL]


#"Master_Fvencimiento"

dataset[ foto_mes==202103  , Master_Fvencimiento_neg_rank  := -(frank(-dataset[ foto_mes==202103 , Master_Fvencimiento_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , Master_Fvencimiento_pos_rank  :=  (frank(dataset[ foto_mes==202103 , Master_Fvencimiento_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , Master_Fvencimiento_neg_rank  := -(frank(-dataset[ foto_mes==202105 , Master_Fvencimiento_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , Master_Fvencimiento_pos_rank  :=  (frank(dataset[ foto_mes==202105 , Master_Fvencimiento_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , Master_Fvencimiento_rank  := ifelse(Master_Fvencimiento_neg_rank< 0, Master_Fvencimiento_neg_rank, Master_Fvencimiento_pos_rank) ]
dataset[, c("Master_Fvencimiento", "Master_Fvencimiento_neg_rank", "Master_Fvencimiento_pos_rank", "Master_Fvencimiento_neg", "Master_Fvencimiento_pos" ):=NULL]



#"Master_msaldototal"

dataset[ foto_mes==202103  , Master_msaldototal_neg_rank  := -(frank(-dataset[ foto_mes==202103 , Master_msaldototal_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , Master_msaldototal_pos_rank  :=  (frank(dataset[ foto_mes==202103 , Master_msaldototal_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , Master_msaldototal_neg_rank  := -(frank(-dataset[ foto_mes==202105 , Master_msaldototal_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , Master_msaldototal_pos_rank  :=  (frank(dataset[ foto_mes==202105 , Master_msaldototal_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , Master_msaldototal_rank  := ifelse(Master_msaldototal_neg_rank< 0, Master_msaldototal_neg_rank, Master_msaldototal_pos_rank) ]
dataset[, c("Master_msaldototal", "Master_msaldototal_neg_rank", "Master_msaldototal_pos_rank", "Master_msaldototal_neg", "Master_msaldototal_pos" ):=NULL]


#"Master_fultimo_cierre"
dataset[ foto_mes==202103  , Master_fultimo_cierre_neg_rank  := -(frank(-dataset[ foto_mes==202103 , Master_fultimo_cierre_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , Master_fultimo_cierre_pos_rank  :=  (frank(dataset[ foto_mes==202103 , Master_fultimo_cierre_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , Master_fultimo_cierre_neg_rank  := -(frank(-dataset[ foto_mes==202105 , Master_fultimo_cierre_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , Master_fultimo_cierre_pos_rank  :=  (frank(dataset[ foto_mes==202105 , Master_fultimo_cierre_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , Master_fultimo_cierre_rank  := ifelse(Master_fultimo_cierre_neg_rank< 0, Master_fultimo_cierre_neg_rank, Master_fultimo_cierre_pos_rank) ]
dataset[, c("Master_fultimo_cierre", "Master_fultimo_cierre_neg_rank", "Master_fultimo_cierre_pos_rank", "Master_fultimo_cierre_neg", "Master_fultimo_cierre_pos"):=NULL]


#"Visa_msaldototal"

dataset[ foto_mes==202103  , Visa_msaldototal_neg_rank  := -(frank(-dataset[ foto_mes==202103 , Visa_msaldototal_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , Visa_msaldototal_pos_rank  :=  (frank(dataset[ foto_mes==202103 , Visa_msaldototal_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , Visa_msaldototal_neg_rank  := -(frank(-dataset[ foto_mes==202105 , Visa_msaldototal_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , Visa_msaldototal_pos_rank  :=  (frank(dataset[ foto_mes==202105 , Visa_msaldototal_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , Visa_msaldototal_rank  := ifelse(Visa_msaldototal_neg_rank< 0, Visa_msaldototal_neg_rank, Visa_msaldototal_pos_rank) ]
dataset[, c("Visa_msaldototal", "Visa_msaldototal_neg_rank", "Visa_msaldototal_pos_rank", "Visa_msaldototal_neg", "Visa_msaldototal_pos" ):=NULL]


#"Visa_mconsumospesos"

dataset[ foto_mes==202103  , Visa_mconsumospesos_neg_rank  := -(frank(-dataset[ foto_mes==202103 , Visa_mconsumospesos_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , Visa_mconsumospesos_pos_rank  :=  (frank(dataset[ foto_mes==202103 , Visa_mconsumospesos_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , Visa_mconsumospesos_neg_rank  := -(frank(-dataset[ foto_mes==202105 , Visa_mconsumospesos_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , Visa_mconsumospesos_pos_rank  :=  (frank(dataset[ foto_mes==202105 , Visa_mconsumospesos_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , Visa_mconsumospesos_rank  := ifelse(Visa_mconsumospesos_neg_rank< 0, Visa_mconsumospesos_neg_rank, Visa_mconsumospesos_pos_rank) ]
dataset[, c("Visa_mconsumospesos", "Visa_mconsumospesos_neg_rank", "Visa_mconsumospesos_pos_rank", "Visa_mconsumospesos_neg", "Visa_mconsumospesos_pos" ):=NULL]

#"Visa_fultimo_cierre"
dataset[ foto_mes==202103  , Visa_fultimo_cierre_neg_rank  := -(frank(-dataset[ foto_mes==202103 , Visa_fultimo_cierre_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , Visa_fultimo_cierre_pos_rank  :=  (frank(dataset[ foto_mes==202103 , Visa_fultimo_cierre_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , Visa_fultimo_cierre_neg_rank  := -(frank(-dataset[ foto_mes==202105 , Visa_fultimo_cierre_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , Visa_fultimo_cierre_pos_rank  :=  (frank(dataset[ foto_mes==202105 , Visa_fultimo_cierre_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , Visa_fultimo_cierre_rank  := ifelse(Visa_fultimo_cierre_neg_rank< 0, Visa_fultimo_cierre_neg_rank, Visa_fultimo_cierre_pos_rank) ]
dataset[, c("Visa_fultimo_cierre", "Visa_fultimo_cierre_neg_rank", "Visa_fultimo_cierre_pos_rank", "Visa_fultimo_cierre_neg", "Visa_fultimo_cierre_pos"):=NULL]

#"Visa_mconsumototal"
dataset[ foto_mes==202103  , Visa_mconsumototal_neg_rank  := -(frank(-dataset[ foto_mes==202103 , Visa_mconsumototal_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202103  , Visa_mconsumototal_pos_rank  :=  (frank(dataset[ foto_mes==202103 , Visa_mconsumototal_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
dataset[ foto_mes==202105  , Visa_mconsumototal_neg_rank  := -(frank(-dataset[ foto_mes==202105 , Visa_mconsumototal_neg ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[ foto_mes==202105  , Visa_mconsumototal_pos_rank  :=  (frank(dataset[ foto_mes==202105 , Visa_mconsumototal_pos ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)]
dataset[   , Visa_mconsumototal_rank  := ifelse(Visa_mconsumototal_neg_rank< 0, Visa_mconsumototal_neg_rank, Visa_mconsumototal_pos_rank) ]
dataset[, c("Visa_mconsumototal", "Visa_mconsumototal_neg_rank", "Visa_mconsumototal_pos_rank", "Visa_mconsumototal_neg", "Visa_mconsumototal_pos" ):=NULL]


#Elimino variables que se pudo mejorar con rank
dataset <- dataset[, -c("Visa_Finiciomora", "Master_Finiciomora")]
colnames(dataset)
ncol(dataset) #153 variables 

#Guardo el dataset con FE
fwrite(dataset, file = "./datasets/competencia2_2022_FE5_comp2.csv", sep=  ",")

#Funcion Graficar densidades 
graficar_campo  <- function( campo)
{
  
  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202105 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )
  
  densidad_A  <- density( dataset[ foto_mes==202103, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  densidad_B  <- density( dataset[ foto_mes==202105 , get(campo) ],
                          kernel="gaussian", na.rm=TRUE )
  
  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ") 
  )
  
  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202003", "202005"),
           col=c("blue", "red"), lty=c(1,2))
  
}

dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/DR6131/", showWarnings = FALSE )
setwd("./exp/DR6131/")



pdf("densidades_03_05_rank.pdf")


#dd_pos_rank <- colnames(dataset[, c(120:142)])
#dd_neg_rank <- colnames(dataset[, c(142:153)])
dd_rank <- colnames(dataset[, c(120:153)])
colnames(dataset)

#Graficamos distribuciones _rank

for( campo in  dd_rank)
{
  cat( campo, "  " )
  
  graficar_campo( campo)
}

dev.off()








for (col in cols_neg) {
  
  dataset[ foto_mes==202103  , paste(col,"rank", sep = "_")  := -(frank(-dataset[ foto_mes==202103 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
  dataset[ foto_mes==202105  , paste(col,"rank", sep = "_")  :=  (frank(dataset[ foto_mes==202105 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)] }

for (col in cols_pos) {
  
  dataset[ foto_mes==202103  , paste(col,"rank", sep = "_")  := -(frank(-dataset[ foto_mes==202103 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202103 , ])-1)]
  dataset[ foto_mes==202105  , paste(col,"rank", sep = "_")  :=  (frank(dataset[ foto_mes==202105 , get(col) ], ties.method="min", na.last="keep" )-1)/(nrow(dataset[ foto_mes==202105 , ])-1)] }

colnames(dataset)

for (col in cols) {   
  dataset[   , paste(get(col),"rank", sep = "")  := ifelse( paste(col,"neg_rank", sep = "")< 0,  paste(col,"neg_rank", sep = ""),  paste(col,"pos_rank", sep = "")) ]
}

