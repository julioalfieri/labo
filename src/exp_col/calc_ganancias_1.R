#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection
options(scipen = 999)
require("data.table")
require("dplyr")
require(ggplot2)

setwd("C:\\Users\\alfie\\OneDrive\\Documentos\\Maestria_DM\\Materias\\DMEyF_22\\")
dataset <- fread("datasets/competencia3_2022.csv.gz")

data_eval  <- dataset[ foto_mes== 202105  ]
data_eval <- data_eval[,c("numero_de_cliente","clase_ternaria")]



semillas <- c("_100019", "_110119", "_111119", "_111919", "_900019", "_688201", "_869293", "_840341", "_550439", "_482861")
cortes <- c('07000','07500','08000','08500','09000','09500','10000','10500','11000')

#############################################################################################################

# Modelo 1: Malo
#cargo cada predicción por número de corte y genero un dataset por cada semilla.
nombre_base <- 'ZZ9910'
BO <- '_01_040_'

BO_idx <- 1
semilla_idx <- 1
temp <- data.table()
for (semilla in semillas){
  data_eval  <- dataset[ foto_mes== 202105,c("numero_de_cliente","clase_ternaria")  ]
  for (corte in cortes){
    df_pred <- fread(paste0('exp/exp_col/',nombre_base,'/',nombre_base,semilla,BO,corte,'.csv'))
    data_eval <- df_pred %>% 
    right_join(., data_eval, by = "numero_de_cliente") 
    data_eval[  , paste0('ganancia_',corte) :=  ifelse( clase_ternaria=="BAJA+2" & Predicted ==1, 78000, -2000 ) ]
    data_eval[  , paste0('ganancia_',corte) :=  ifelse( Predicted == 0, 0, get(paste0('ganancia_',corte)) ) ]
    data_eval[, Predicted:=NULL]
    }
  data_eval[, clase_ternaria:=NULL]
  data_eval[, numero_de_cliente:=NULL]
  temp <- rbind(temp,data_eval%>%
      summarise_all(sum))
  assign(paste0('BO_',BO_idx,'_Semilla_', semilla_idx),data_eval)
  semilla_idx <- semilla_idx + 1
  }
assign(paste0('BO_',BO_idx,'_summary'),temp)

#genero una tabla con las medias y desvíos por corte
BO_1_media <-BO_1_summary %>% summarise_all(mean)
BO_1_media <-rbind(BO_1_media, BO_1_summary %>% summarise_all(sd))

media <- as.numeric(as.vector(BO_1_media[1, ]))
stadesv <- as.numeric(as.vector(BO_1_media[2, ])) 
modelo1 <-data.frame(modelo=as.factor(c("malo")), 
                     Corte=as.numeric(c('7000','7500','8000','8500','9000','9500','10000','10500','11000')),
                    Ganancia=media,
                    sd=stadesv)

#formateo la tabla con nombres de filas y columnas y agrego la media de cada corte
col_semilla <- c("Semilla 1","Semilla 2","Semilla 3","Semilla 4","Semilla 5", "Semilla 6","Semilla 7","Semilla 8","Semilla 9","Semilla 10")# ,"Media" )
#BO_1_summary <- rbind(BO_1_summary, BO_1_summary %>% summarise_all(mean))
BO_1_summary <- cbind(col_semilla, BO_1_summary)
BO_1_summary

colnames(BO_1_summary) <- c('col_semilla','7000','7500','8000','8500','9000','9500','10000','10500','11000')

#cambio estructura para poder graficar
BO_1_melt <-melt(BO_1_summary, id = c('col_semilla'))
BO_1_melt[, Modelo:= "Malo"]


#########################################################################################

# Modelo 2: Bueno
#cargo cada predicción por número de corte y genero un dataset por cada semilla.
nombre_base <- 'ZZ9911'
BO <- '_01_026_'

BO_idx <- 2
semilla_idx <- 1
temp <- data.table()
for (semilla in semillas){
  data_eval  <- dataset[ foto_mes== 202105,c("numero_de_cliente","clase_ternaria")  ]
  for (corte in cortes){
    df_pred <- fread(paste0('exp/exp_col/',nombre_base,'/',nombre_base,semilla,BO,corte,'.csv'))
    data_eval <- df_pred %>% 
      right_join(., data_eval, by = "numero_de_cliente") 
    data_eval[  , paste0('ganancia_',corte) :=  ifelse( clase_ternaria=="BAJA+2" & Predicted ==1, 78000, -2000 ) ]
    data_eval[  , paste0('ganancia_',corte) :=  ifelse( Predicted == 0, 0, get(paste0('ganancia_',corte)) ) ]
    data_eval[, Predicted:=NULL]
  }
  data_eval[, clase_ternaria:=NULL]
  data_eval[, numero_de_cliente:=NULL]
  temp <- rbind(temp,data_eval%>%
                  summarise_all(sum))
  assign(paste0('BO_',BO_idx,'_Semilla_', semilla_idx),data_eval)
  semilla_idx <- semilla_idx + 1
}
assign(paste0('BO_',BO_idx,'_summary'),temp)

#genero una tabla con las medias y desvíos por corte
BO_2_media <-BO_2_summary %>% summarise_all(mean)
BO_2_media <-rbind(BO_2_media, BO_2_summary %>% summarise_all(sd))


media <- as.numeric(as.vector(BO_2_media[1, ]))
stadesv <- as.numeric(as.vector(BO_2_media[2, ])) 
modelo2 <-data.frame(modelo=as.factor(c("bueno")), 
                     Corte=as.numeric(c('7000','7500','8000','8500','9000','9500','10000','10500','11000')),
                     Ganancia=media,
                     sd=stadesv)

#formateo la tabla con nombres de filas y columnas y agrego la media de cada corte
col_semilla <- c("Semilla 1","Semilla 2","Semilla 3","Semilla 4","Semilla 5", "Semilla 6","Semilla 7","Semilla 8","Semilla 9","Semilla 10")# ,"Media" )
#BO_2_summary <- rbind(BO_2_summary, BO_2_summary %>% summarise_all(mean))
BO_2_summary <- cbind(col_semilla, BO_2_summary)


colnames(BO_2_summary) <- c('col_semilla','7000','7500','8000','8500','9000','9500','10000','10500','11000')

#cambio estructura para poder graficar
BO_2_melt <-melt(BO_2_summary, id = c('col_semilla'))

 

#######################################################################################

# Junto resumen de modelos para graficar
modelos1 <- rbind(modelo1, modelo2)

# Standard deviation of the mean
breaks = c(seq(18000000, 22000000, by=500000))
labels = as.character(breaks)

ggplot(modelos1, aes(x=Corte, y=Ganancia, group=modelo, color=modelo)) + 
    geom_line(size=1) + geom_point(size=2.5) + ggtitle("No undersampling") + 
    geom_errorbar(aes(ymin=Ganancia-sd, ymax=Ganancia+sd), width =80,size=0.6, position="Dodge")+
    scale_x_discrete(limits=modelos1$Corte) + xlab("Envíos")+ 
    scale_y_continuous(limits = c(18000000, 22000000), breaks = breaks, labels = labels)+
    theme_light() 
    

#########################################################################################
#Test Wilcox

malo1 <- (BO_1_melt[variable =='8500' |variable =='9000' | variable =='9500', value])
bueno1 <- (BO_2_melt[variable =='7000' |variable =='7500' | variable =='8000', value])


test1 <- data.table(
  modelo = rep(c("malo", "bueno"), each = 30),
  ganancia = c(malo1, bueno1)
)
test1[, undersampling:= "100"]

result1 = wilcox.test(malo1, bueno1, paired = TRUE)
result1
mean(bueno1)
sd(bueno1)
mean(malo1)
sd(malo1)


###########################################################################################
#plot Malo
ggplot(data=BO_1_melt, aes(x=variable, y=value, group=col_semilla)) +
  geom_line(aes(color=col_semilla, size= col_semilla))+
  labs(x="Corte", y="Ganancia", color="Semilla", size="Semilla")+
  scale_size_manual(values = c("Semilla 1" = 0.5,"Semilla 2" = 0.5,"Semilla 3" = 0.5,
                               "Semilla 4" = 0.5,"Semilla 5" = 0.5,"Semilla 6" = 0.5,
                               "Semilla 7" = 0.5,"Semilla 8" = 0.5,"Semilla 9" = 0.5,
                               "Semilla 10" = 0.5,"Media" = 1.5 ))+
  guides(size = "none", label = "none")+theme_light()


#plot Bueno
ggplot(data=BO_2_melt, aes(x=variable, y=value, group=col_semilla)) +
  geom_line(aes(color=col_semilla, size= col_semilla))+
  labs(x="Corte", y="Ganancia", color="Semilla", size="Semilla")+
  scale_size_manual(values = c("Semilla 1" = 0.5,"Semilla 2" = 0.5,"Semilla 3" = 0.5,
                               "Semilla 4" = 0.5,"Semilla 5" = 0.5,"Semilla 6" = 0.5,
                               "Semilla 7" = 0.5,"Semilla 8" = 0.5,"Semilla 9" = 0.5,
                               "Semilla 10" = 0.5,"Media" = 1.5))+
  guides(size = "none", label = "none")+theme_light()