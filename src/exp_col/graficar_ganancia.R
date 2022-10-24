
# Este script genera un gráfico con las ganancias alcanzadas en cada punto de corte para cada una de las 5 semillas.
# Dede correrse para cada uno de los dataset corregidos con los que se esté trabajando.



# limpio la memoria
rm( list=ls() )  
gc()             


require("data.table")
require("dplyr")
require("tidyverse")
require("scales")


# parametros experimento
PARAM <- list()
#PARAM$experimento  <- "EXP02_graficos_ganancias"


PARAM$input$dataset_semilla_1   <- "./exp/EXP02_rank_guess2_ganancias/semilla_1.csv"
PARAM$input$dataset_semilla_2   <- "./exp/EXP02_rank_guess2_ganancias/semilla_2.csv"
PARAM$input$dataset_semilla_3   <- "./exp/EXP02_rank_guess2_ganancias/semilla_3.csv"
PARAM$input$dataset_semilla_4   <- "./exp/EXP02_rank_guess2_ganancias/semilla_4.csv"
PARAM$input$dataset_semilla_5   <- "./exp/EXP02_rank_guess2_ganancias/semilla_5.csv"

#------------------------------------------------------------------------------------------------------------

setwd( "~/buckets/b1/" )

#cargo las ganacias por semilla
semilla_1  <- fread(PARAM$input$dataset_semilla_1, stringsAsFactors= TRUE)
semilla_2  <- fread(PARAM$input$dataset_semilla_2, stringsAsFactors= TRUE)
semilla_3  <- fread(PARAM$input$dataset_semilla_3, stringsAsFactors= TRUE)
semilla_4  <- fread(PARAM$input$dataset_semilla_4, stringsAsFactors= TRUE)
semilla_5  <- fread(PARAM$input$dataset_semilla_5, stringsAsFactors= TRUE)


#------------------------------------------------------------------------------------------------------------

#cambio los nombres de las columnas por el numero de semilla

names(semilla_1)[2]<-"semilla_1"
names(semilla_2)[2]<-"semilla_2"
names(semilla_3)[2]<-"semilla_3"
names(semilla_4)[2]<-"semilla_4"
names(semilla_5)[2]<-"semilla_5"


#------------------------------------------------------------------------------------------------------------

#armo la tabla que voy a usar para graficar

lista <- list(semilla_1, semilla_2, semilla_3, semilla_4, semilla_5)

data <- lista %>% reduce(inner_join, by="envios")

#------------------------------------------------------------------------------------------------------------

#grafico // EXPORTO COMO JPEG: 887X482

ganancia_maxima <- max(data[2:ncol(data)])
envio_minimo <- 7000
envio_maximo <- 11500

data %>%
  pivot_longer(cols = c('semilla_1','semilla_2','semilla_3','semilla_4','semilla_5' )) %>%
  
  ggplot() +
  geom_point(aes(x=envios,y=value,color=name), size=3) +
  geom_line(aes(x=envios,y=value,color=name), size=1) +
  geom_hline(aes(yintercept=ganancia_maxima), linetype='dashed') +
  
  xlim(envio_minimo,envio_maximo) +
  ylim(16000000,max(ganancia_maxima * 1.25)) +
  scale_y_continuous(labels = scales::label_dollar()) + 
  
  ggtitle('Experimiento 02 - Corrección de Data Drifting | Método aplicado: AT A GUESS 2',
          subtitle = '') +  
  ylab('Ganancia [$]') +
  xlab('Envios [-]') +
  labs(color = "") +
  geom_label(aes(x=envio_maximo*.9,y=ganancia_maxima * 1.1,
                 label=paste0('Ganancia máxima: $', round(ganancia_maxima,0))),size=4) +
  theme_bw()

