####################################
# Librerías
####################################

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
    

library(tseries)
library(forecast)

mi_Hosp_UCI <- HospitalizadosUCIEtario %>% pivot_wider(names_from = RangoEtario,values_from = Hosp_UCI) %>% select(-Fecha)
mi_ts_UCI <- ts(mi_Hosp_UCI,start = c(2020,95), frequency = 365)
plot(mi_ts_UCI, main = "Hospitalizados UCI Chile")
adf.test(mi_ts_UCI[,1], alternative = "stationary")
adf.test(mi_ts_UCI[,2], alternative = "stationary")
adf.test(mi_ts_UCI[,3], alternative = "stationary")

dif_mi_ts_UCI <- diff(mi_ts_UCI)
plot(dif_mi_ts_UCI, main = "Hospitalizados UCI Chile")
adf.test(dif_mi_ts_UCI[,1], alternative = "stationary")
adf.test(dif_mi_ts_UCI[,2], alternative = "stationary")
adf.test(dif_mi_ts_UCI[,3], alternative = "stationary")

dif2_mi_ts_UCI <- diff(mi_ts_UCI,differences = 2)
plot(dif2_mi_ts_UCI, main = "Hospitalizados UCI Chile")
adf.test(dif2_mi_ts_UCI[,1], alternative = "stationary")
adf.test(dif2_mi_ts_UCI[,2], alternative = "stationary")
adf.test(dif2_mi_ts_UCI[,3], alternative = "stationary")


par(mfrow = c(2,1), mar = c(4,4,4,1)+.1)
acf(ts(dif2_mi_ts_UCI[,1], frequency = 1))
pacf(ts(dif2_mi_ts_UCI[,1], frequency = 1))
modelo1 <- arima(mi_ts_UCI[,1], c(6,2,2))
mean(residuals(modelo1))
tsdiag(modelo1)
Box.test(residuals(modelo1),type = "Ljung-Box")
pronostico1 <- forecast(modelo1,h = 15)
acf(ts(dif2_mi_ts_UCI[,2], frequency = 1))
pacf(ts(dif2_mi_ts_UCI[,2], frequency = 1))
modelo2 <- arima(mi_ts_UCI[,2], c(6,2,4))
mean(residuals(modelo2))
tsdiag(modelo2)
Box.test(residuals(modelo2),type = "Ljung-Box")
pronostico2 <- forecast(modelo2,h = 15)
acf(ts(dif2_mi_ts_UCI[,3], frequency = 1))
pacf(ts(dif2_mi_ts_UCI[,3], frequency = 1))
modelo3 <- arima(mi_ts_UCI[,3], c(9,2,4))
mean(residuals(modelo3))
tsdiag(modelo3)
Box.test(residuals(modelo2),type = "Ljung-Box")
pronostico3 <- forecast(modelo3,h = 15)

par(mfrow = c(3,1), mar = c(4,4,4,1)+.1)
plot(pronostico1, main = "Hospitalizados UCI 0 a 39 años - Proyección 15días")
plot(pronostico2, main = "Hospitalizados UCI 40 a 59 años - Proyección 15días")
plot(pronostico3, main = "Hospitalizados UCI 60 o más años - Proyección 15días")

##############################
#
##############################
mi_Casos_Etario <-
  CasosGeneroEtario %>% 
  filter(F_Semana >= ymd("20210101")) %>% 
  select(F_Semana,RangoEtario,Casos_Acum) %>% 
  pivot_wider(names_from = RangoEtario, values_from = Casos_Acum) %>% 
  arrange(F_Semana) %>% 
  select(-F_Semana)

mi_ts_Casos <- ts(mi_Casos_Etario,start = c(2021,1), frequency = 53)
plot(mi_ts_Casos, main = "Casos COVID confirmados (acumulado)")
adf.test(mi_ts_Casos[,1], alternative = "stationary")
adf.test(mi_ts_Casos[,2], alternative = "stationary")
adf.test(mi_ts_Casos[,3], alternative = "stationary")
adf.test(mi_ts_Casos[,4], alternative = "stationary")

dif_mi_ts_Casos <- diff(mi_ts_Casos)
plot(dif_mi_ts_Casos, main = "Hospitalizados UCI Chile")
adf.test(dif_mi_ts_Casos[,1], alternative = "stationary")
adf.test(dif_mi_ts_Casos[,2], alternative = "stationary")
adf.test(dif_mi_ts_Casos[,3], alternative = "stationary")
adf.test(dif_mi_ts_Casos[,4], alternative = "stationary")

dif2_mi_ts_Casos <- diff(mi_ts_Casos,differences = 2)
plot(dif2_mi_ts_Casos, main = "Hospitalizados UCI Chile")
adf.test(dif2_mi_ts_Casos[,1], alternative = "stationary")
adf.test(dif2_mi_ts_Casos[,2], alternative = "stationary")
adf.test(dif2_mi_ts_Casos[,3], alternative = "stationary")
adf.test(dif2_mi_ts_Casos[,4], alternative = "stationary")

dif3_mi_ts_Casos <- diff(mi_ts_Casos,differences = 3)
plot(dif3_mi_ts_Casos, main = "Hospitalizados UCI Chile")
adf.test(dif3_mi_ts_Casos[,1], alternative = "stationary")
adf.test(dif3_mi_ts_Casos[,2], alternative = "stationary")
adf.test(dif3_mi_ts_Casos[,3], alternative = "stationary")
adf.test(dif3_mi_ts_Casos[,4], alternative = "stationary")


par(mfrow = c(2,1), mar = c(4,4,4,1)+.1)
acf(ts(dif2_mi_ts_Casos[,1], frequency = 1))
pacf(ts(dif2_mi_ts_Casos[,1], frequency = 1))
modelo1 <- arima(mi_ts_Casos[,1], c(1,2,1))
mean(residuals(modelo1))
tsdiag(modelo1)
Box.test(residuals(modelo1),type = "Ljung-Box")
pronostico1 <- forecast(modelo1,h = 15)
acf(ts(dif2_mi_ts_Casos[,2], frequency = 1))
pacf(ts(dif2_mi_ts_Casos[,2], frequency = 1))
modelo2 <- arima(mi_ts_Casos[,2], c(1,2,2))
mean(residuals(modelo2))
tsdiag(modelo2)
Box.test(residuals(modelo2),type = "Ljung-Box")
pronostico2 <- forecast(modelo2,h = 15)
acf(ts(dif2_mi_ts_Casos[,3], frequency = 1))
pacf(ts(dif2_mi_ts_Casos[,3], frequency = 1))
modelo3 <- arima(mi_ts_Casos[,3], c(1,2,2))
mean(residuals(modelo3))
tsdiag(modelo3)
Box.test(residuals(modelo3),type = "Ljung-Box")
pronostico3 <- forecast(modelo3,h = 15)
acf(ts(dif2_mi_ts_Casos[,4], frequency = 1))
pacf(ts(dif2_mi_ts_Casos[,4], frequency = 1))
modelo4 <- arima(mi_ts_Casos[,4], c(0,2,1))
mean(residuals(modelo4))
tsdiag(modelo4)
Box.test(residuals(modelo4),type = "Ljung-Box")
pronostico4 <- forecast(modelo4,h = 15)

par(mfrow = c(4,1), mar = c(3,3,3,1)+.1)
plot(pronostico1, main = "Hospitalizados UCI 0 a 39 años - Proyección 15días")
plot(pronostico2, main = "Hospitalizados UCI 40 a 59 años - Proyección 15días")
plot(pronostico3, main = "Hospitalizados UCI 60 o más años - Proyección 15días")
plot(pronostico4, main = "Hospitalizados UCI 60 o más años - Proyección 15días")





# CasosGeneroEtario <- read_csv("input/InformeEpidemiologico/CasosGeneroEtario.csv")

mi_url <- URLdecode("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosGeneroEtario.csv")

CasosGeneroEtario <- read.csv(mi_url,encoding = "UTF-8",check.names = F)
#View(CasosGeneroEtario)

####################################
# Algunas constantes
####################################
ETARIO_0_A_19 <-
        paste(paste(
                formatC(seq(
                        from = 0, to = 15, by = 5
                ), width = 2, flag = "0"),
                formatC(seq(
                        from = 4, to = 19, by = 5
                ), width = 2, flag = "0"),
                sep = " - "
        ), "años")

ETARIO_20_A_39 <-
        paste(paste(
                formatC(seq(
                        from = 20, to = 35, by = 5
                ), width = 2, flag = "0"),
                formatC(seq(
                        from = 24, to = 39, by = 5
                ), width = 2, flag = "0"),
                sep = " - "
        ), "años")



ETARIO_40_A_59 <-
        paste(paste(
                formatC(seq(
                        from = 40, to = 55, by = 5
                ), width = 2, flag = "0"),
                formatC(seq(
                        from = 44, to = 59, by = 5
                ), width = 2, flag = "0"),
                sep = " - "
        ), "años")

ETARIO_60_O_MAS <-
        c(paste(paste(
                formatC(seq(
                        from = 60, to = 75, by = 5
                ), width = 2, flag = "0"),
                formatC(seq(
                        from = 64, to = 79, by = 5
                ), width = 2, flag = "0"),
                sep = " - "
        ), "años"), "80 y más años")


CasosGeneroEtario <- CasosGeneroEtario %>% mutate("RangoEtario" = ifelse(`Grupo de edad` %in% ETARIO_60_O_MAS,"60 o más",
                                                                     ifelse(`Grupo de edad` %in% ETARIO_40_A_59,"40 a 59",
                                                                            ifelse(`Grupo de edad` %in% ETARIO_20_A_39,"20 a 39","0 a 19")))) %>% 
        pivot_longer(cols = c(3:length(CasosGeneroEtario)), names_to = "Fecha",values_to = "Casos") %>% 
        mutate(Fecha = as.Date(Fecha))

FECHA_INI <- min(CasosGeneroEtario$Fecha)
FECHA_FIN <- max(CasosGeneroEtario$Fecha)
br_fecha <-  seq(FECHA_INI,FECHA_FIN,by= 7)


CasosGeneroEtario <- CasosGeneroEtario %>%  
        mutate(F_Semana = floor_date(Fecha, unit = "week",week_start = 1)) %>% 
        group_by(RangoEtario,F_Semana) %>%
        summarise(Casos_Acum = sum(Casos, na.rm = TRUE)) %>% 
        arrange(RangoEtario,F_Semana) %>% 
        ungroup()


CasosGeneroEtario <- CasosGeneroEtario %>% 
        mutate(
                Casos_Nuevos = Casos_Acum - lag(Casos_Acum),
                Var_Casos = Casos_Nuevos - lag(Casos_Nuevos),
                "%Var_Casos" = Var_Casos/Casos_Nuevos,
                Num_dias = as.numeric(F_Semana - lag(F_Semana)),
                Tasa_diaria = Casos_Nuevos/Num_dias)

save(CasosGeneroEtario,file = "app/JSP_Data.Rdata")


# Casos Nuevos
CasosGeneroEtario %>% 
        filter((F_Semana)>="2020-09-01") %>% 
        group_by(RangoEtario, F_Semana) %>% 
        summarise("Casos Nuevos" = sum(Casos_Nuevos, na.rm = TRUE)) %>% 
        ungroup() %>% 
        ggplot(aes(x=F_Semana, y=`Casos Nuevos`))+
        geom_line(aes(color = RangoEtario),size = 1,linetype = "dotted") +
        geom_point(aes(color = RangoEtario),size = 4,alpha=0.5) +
        geom_smooth(aes(group = RangoEtario, color = RangoEtario), fill = "lightgrey",size= 1, linetype = "solid")+
        #facet_grid(RangoEtario~., scales = "free_y")+
        scale_x_continuous(minor_breaks = br_fecha,breaks = br_fecha)+
        labs(title = "Casos nuevos Confirmados por Semana",
             subtitle = "Segmentado por Edad",
             x = "Fecha",
             y = "Casos")+
        theme(axis.text.x = element_text(
                angle = 90,
                size = 7,
                vjust = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey")
        )


# Variación de Casos
CasosGeneroEtario %>% 
        filter((F_Semana)>="2020-09-01") %>% 
        #group_by(RangoEtario, F_Semana) %>% 
        #summarise("Variacion Casos" = sum(Var_Casos, na.rm = TRUE)) %>% 
        #ungroup() %>% 
        ggplot(aes(x=F_Semana, y=`%Var_Casos`))+
        geom_line(aes(color = RangoEtario),size = 1,linetype = "solid") +
        geom_point(aes(color = RangoEtario),size = 4,alpha=0.5) +
        #geom_smooth(aes(group = RangoEtario, color = RangoEtario), fill = "lightgrey",size= 1, linetype = "solid")+
        facet_wrap(RangoEtario~., scales = "free_y")+
        scale_x_continuous(minor_breaks = br_fecha,breaks = br_fecha)+
        labs(title = "% Variación Nuevos Casos Confirmados por Semana",
             subtitle = "Segmentado por Edad",
             x = "Fecha",
             y = "Casos")+
        theme(axis.text.x = element_text(
                angle = 90,
                size = 7,
                vjust = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey")
        )



CasosGeneroEtario %>% 
        group_by(RangoEtario, F_Semana) %>% 
        summarise("Total Casos" = sum(Casos, na.rm = TRUE)) %>% 
        ungroup() %>% 
        ggplot(aes(x=paste0(year(F_Semana),formatC(week(F_Semana),width = 2,flag = 0)), y=`Total Casos`))+
        geom_point(aes(color = RangoEtario), size = 2,alpha=0.5) +
        #facet_wrap(.~RangoEtario)+
        scale_y_log10()+
        theme(                axis.text.x = element_text(
                angle = 90,
                size = 7,
                vjust = 0.5
        ))


# Grafica por casos diarios

FECHA_INI <- min(casos_nuevos_acumulados_por_fecha$Fecha)
FECHA_FIN <- max(casos_nuevos_acumulados_por_fecha$Fecha)
br_fecha <-  seq(FECHA_INI,FECHA_FIN,by= 7)

casos_nuevos_acumulados_por_fecha %>% mutate( Fecha = as.Date(Fecha)) %>% 
        filter(Serie == "Casos nuevos") %>% 
        ggplot(aes(x=Fecha, y=Casos))+
        geom_line(size = 1,linetype = "dotted") +
        geom_point(size = 2,alpha=0.5) +
        geom_smooth()+
        scale_x_continuous(breaks = br_fecha)+
        labs(title = "Casos nuevos por día",
             subtitle = "   ",
             x = "Fecha",
             y = "Casos")+
        theme(axis.text.x = element_text(
                angle = 90,
                size = 7,
                vjust = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey")
        )


vacunacion_edad <- read_csv("input/Vacunacion/vacunacion_edad.csv")
vacunacion_edad <- vacunacion_edad %>% pivot_longer(cols = c(3:length(vacunacion_edad)), names_to = "Fecha", values_to = "Vacunados")




####################################
# Plantilla Shiny
####################################
ui <- fluidPage(title = "Análisis COVID19 en Chile",
                lang = "es",
                
                dateRangeInput(inputId = "DAT_RAN_IN_1",
                               label = "Rango de Fechas:",
                               start = "2021-01-01",
                               end = today(),
                               min = "2020-03-01",
                               format = "dd-mm-yyyy",
                               weekstart = 1,
                               language = "es"),
                checkboxGroupInput("CHK_BX_G_IN_1",
                                   label = "Selecciona los rangos etarios:",
                                   choices = list("0 a 19","20 a 39","40 a 59","60 o más"),
                                   selected = list("0 a 19","20 a 39","40 a 59","60 o más"),
                                   inline = TRUE),
                plotOutput("NEW_CASES_1")
)

server <- function(input, output,session) {
  
  output$NEW_CASES_1 <- renderPlot({
    CasosGeneroEtario %>% 
      filter(F_Semana>=input$DAT_RAN_IN_1[1],F_Semana<=input$DAT_RAN_IN_1[2], RangoEtario %in% input$CHK_BX_G_IN_1) %>% 
      group_by(RangoEtario, F_Semana) %>% 
      summarise("Casos Nuevos" = sum(Casos_Nuevos, na.rm = TRUE)) %>% 
      ungroup() %>% 
      ggplot(aes(x=F_Semana, y=`Casos Nuevos`))+
      geom_line(aes(color = RangoEtario),size = 1,linetype = "dotted") +
      geom_point(aes(color = RangoEtario),size = 4,alpha=0.5) +
      geom_smooth(aes(group = RangoEtario, color = RangoEtario), fill = "lightgrey",size= 1, linetype = "solid")+
      #facet_grid(RangoEtario~., scales = "free_y")+
      scale_x_continuous(minor_breaks = br_fecha,breaks = br_fecha)+
      labs(title = "Casos nuevos Confirmados por Semana",
           subtitle = "Segmentado por Edad",
           x = "Fecha",
           y = "Casos")+
      theme(axis.text.x = element_text(
        angle = 90,
        size = 7,
        vjust = 0.5
      ),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "grey")
      )
    
  })  
}

shinyApp(ui, server)
