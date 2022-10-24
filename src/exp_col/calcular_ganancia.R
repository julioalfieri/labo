# limpio la memoria
rm( list=ls() )  
gc()             


require("data.table")
require("dplyr")


# parametros experimento
PARAM <- list()
PARAM$experimento  <- "EXP02_rank_simple_ganancias"

PARAM$input$dataset_original   <- "./exp/FE9250/dataset_rank_simple.csv.gz"

PARAM$input$dataset_prediccion_7000   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_7000.csv"
PARAM$input$dataset_prediccion_7500   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_7500.csv"
PARAM$input$dataset_prediccion_8000   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_8000.csv"
PARAM$input$dataset_prediccion_8500   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_8500.csv"
PARAM$input$dataset_prediccion_9000   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_9000.csv"
PARAM$input$dataset_prediccion_9500   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_9500.csv"
PARAM$input$dataset_prediccion_10000   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_10000.csv"
PARAM$input$dataset_prediccion_10500   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_10500.csv"
PARAM$input$dataset_prediccion_11000   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_11000.csv"
PARAM$input$dataset_prediccion_11500   <- "./exp/EXP02_rank_simple_1/EXP02_rank_simple_1_11500.csv"

#------------------------------------------------------------------------------------------------------------

setwd( "~/buckets/b1/" )

#cargo el dataset original
dataset  <- fread(PARAM$input$dataset_original, stringsAsFactors= TRUE)

#me quedo con el mes de mayo para ver la clase real
clase_real  <- dataset[ foto_mes== 202105  ]

clase_real <- data.frame(clase_real[,c("numero_de_cliente","clase_ternaria")])

#------------------------------------------------------------------------------------------------------------

#cargo las predicciones segun punto de corte
prediccion_7000  <- fread(PARAM$input$dataset_prediccion_7000, stringsAsFactors= TRUE)
prediccion_7500  <- fread(PARAM$input$dataset_prediccion_7500, stringsAsFactors= TRUE)
prediccion_8000  <- fread(PARAM$input$dataset_prediccion_8000, stringsAsFactors= TRUE)
prediccion_8500  <- fread(PARAM$input$dataset_prediccion_8500, stringsAsFactors= TRUE)
prediccion_9000  <- fread(PARAM$input$dataset_prediccion_9000, stringsAsFactors= TRUE)
prediccion_9500  <- fread(PARAM$input$dataset_prediccion_9500, stringsAsFactors= TRUE)
prediccion_10000  <- fread(PARAM$input$dataset_prediccion_10000, stringsAsFactors= TRUE)
prediccion_10500  <- fread(PARAM$input$dataset_prediccion_10500, stringsAsFactors= TRUE)
prediccion_11000  <- fread(PARAM$input$dataset_prediccion_11000, stringsAsFactors= TRUE)
prediccion_11500  <- fread(PARAM$input$dataset_prediccion_11500, stringsAsFactors= TRUE)


#------------------------------------------------------------------------------------------------------------

#me quedo con los casos seleccionados 

prediccion_7000  <- prediccion_7000 [ Predicted== 1  ]
prediccion_7500  <- prediccion_7500 [ Predicted== 1  ]
prediccion_8000  <- prediccion_8000 [ Predicted== 1  ]
prediccion_8500  <- prediccion_8500 [ Predicted== 1  ]
prediccion_9000  <- prediccion_9000 [ Predicted== 1  ]
prediccion_9500  <- prediccion_9500 [ Predicted== 1  ]
prediccion_10000  <- prediccion_10000 [ Predicted== 1  ]
prediccion_10500  <- prediccion_10500 [ Predicted== 1  ]
prediccion_11000  <- prediccion_11000 [ Predicted== 1  ]
prediccion_11500  <- prediccion_11500 [ Predicted== 1  ]

#------------------------------------------------------------------------------------------------------------

#joineo la clase real

prediccion_7000 <- prediccion_7000 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_7500 <- prediccion_7500 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_8000 <- prediccion_8000 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_8500 <- prediccion_8500 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_9000 <- prediccion_9000 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_9500 <- prediccion_9500 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_10000 <- prediccion_10000 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_10500 <- prediccion_10500 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_11000 <- prediccion_11000 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  

prediccion_11500 <- prediccion_11500 %>% 
  left_join(., clase_real, by = "numero_de_cliente")  


#------------------------------------------------------------------------------------------------------------

#agrego una columna que es la de las ganancias

prediccion_7000[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_7500[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_8000[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_8500[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_9000[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_9500[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_10000[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_10500[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_11000[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
prediccion_11500[  , ganancia :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]


#------------------------------------------------------------------------------------------------------------

#armo el dataset con las ganacias segun punto de corte

sum(prediccion_7000$ganancia)


semilla_1 <- data.frame(
  "envios" = c(7000,7500,8000,8500,9000,9500,10000,10500,11000,11500), 
  "ganancia" = c(sum(prediccion_7000$ganancia), sum(prediccion_7500$ganancia), sum(prediccion_8000$ganancia), sum(prediccion_8500$ganancia),
                 sum(prediccion_9000$ganancia), sum(prediccion_9500$ganancia), sum(prediccion_10000$ganancia), sum(prediccion_10500$ganancia),
                 sum(prediccion_11000$ganancia), sum(prediccion_11500$ganancia))
)

#------------------------------------------------------------------------------------------------------------

#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento

dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO


#grabo el dataset
fwrite( semilla_1,
        file= "semilla_1.csv",
        sep= ","  )

