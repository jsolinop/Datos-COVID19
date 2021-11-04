####################################
# Librerías
####################################

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shiny)

mi_url <- URLdecode("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/InformeEpidemiologico/CasosGeneroEtario.csv")
CasosGeneroEtario <- read.csv(mi_url,encoding = "UTF-8",check.names = F)

mi_url2 <- URLdecode("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/input/ReporteDiario/HospitalizadosUCIEtario.csv")
HospitalizadosUCIEtario <- read.csv(mi_url2,encoding = "UTF-8",check.names = F)

####################################
# Algunas constantes
####################################
ETARIO_0_A_19 <-
  paste(paste(formatC(seq(from = 0, to = 15, by = 5), width = 2, flag = "0"),
    formatC(seq(from = 4, to = 19, by = 5), width = 2, flag = "0"),sep = " - "
  ), "años")

ETARIO_20_A_39 <-
  paste(paste(formatC(seq(from = 20, to = 35, by = 5), width = 2, flag = "0"),
    formatC(seq(from = 24, to = 39, by = 5), width = 2, flag = "0"),sep = " - "
  ), "años")

ETARIO_40_A_59 <-
  paste(paste(formatC(seq(from = 40, to = 55, by = 5), width = 2, flag = "0"),
    formatC(seq(from = 44, to = 59, by = 5), width = 2, flag = "0"),sep = " - "
  ), "años")

ETARIO_60_O_MAS <-
  c(paste(paste(formatC(seq(from = 60, to = 75, by = 5), width = 2, flag = "0"),
    formatC(seq(from = 64, to = 79, by = 5), width = 2, flag = "0"),sep = " - "
  ), "años"),"80 y más años")


UCI_ETARIO_0_A_39 <- c("<=39")
UCI_ETARIO_40_A_59 <- c("40-49","50-59")
UCI_ETARIO_60_O_MAS <- c("60-69",">=70")

################################
# TRANSFORMACIONES CASOS NUEVOS
################################

CasosGeneroEtario <- CasosGeneroEtario %>% 
  mutate("RangoEtario" = ifelse(`Grupo de edad` %in% ETARIO_60_O_MAS,"60 o más",
                                ifelse(`Grupo de edad` %in% ETARIO_40_A_59,"40 a 59",
                                       ifelse(`Grupo de edad` %in% ETARIO_20_A_39,"20 a 39","0 a 19")))) %>% 
  pivot_longer(cols = c(3:length(CasosGeneroEtario)), names_to = "Fecha",values_to = "Casos") %>% 
  mutate(Fecha = as.Date(Fecha))

# Agrupar por Rango y Semana
CasosGeneroEtario <- CasosGeneroEtario %>%  
  filter(Fecha == floor_date(Fecha, unit = "week",week_start = 1)) %>% 
  mutate(F_Semana = Fecha) %>% 
  group_by(RangoEtario,F_Semana) %>%
  summarise(Casos_Acum = sum(Casos, na.rm = TRUE)) %>% 
  arrange(RangoEtario,F_Semana) %>% 
  ungroup()

# Calcular Variaciones y % de Variación
CasosGeneroEtario <- CasosGeneroEtario %>% 
  mutate(
    Casos_Nuevos = Casos_Acum - lag(Casos_Acum),
    Var_Casos = Casos_Nuevos - lag(Casos_Nuevos),
    "%Var_Casos" = Var_Casos/Casos_Nuevos,
    Num_dias = as.numeric(F_Semana - lag(F_Semana)),
    Tasa_diaria = Casos_Nuevos/Num_dias)


################################
# TRANSFORMACIONES UCI
################################
# Normalizar segmentos para gráficas, pivotar tabla y formatear la fecha
HospitalizadosUCIEtario <- HospitalizadosUCIEtario %>% 
  mutate("RangoEtario" = ifelse(`Grupo de edad` %in% UCI_ETARIO_60_O_MAS, "60 o más",
                                ifelse(`Grupo de edad` %in% UCI_ETARIO_40_A_59,"40 a 59","0 a 39"))) %>% 
  pivot_longer(cols = c(3:length(HospitalizadosUCIEtario)), names_to = "Fecha",values_to = "Hosp_UCI") %>% 
  mutate(Fecha = as.Date(Fecha))

# Agrupar por Rango y Semana
HospitalizadosUCIEtario <- HospitalizadosUCIEtario %>%  
  mutate(F_Semana = floor_date(Fecha, unit = "week",week_start = 1)) %>% 
  group_by(RangoEtario,F_Semana,Fecha) %>%
  summarise(Hosp_UCI = sum(Hosp_UCI, na.rm = TRUE),
            Var_Hosp_UCI = Hosp_UCI - lag(Hosp_UCI)) %>% 
  arrange(RangoEtario,F_Semana,Fecha) %>% 
  ungroup()


# Calcular Variaciones y % de Variación
HospitalizadosUCIEtario <- HospitalizadosUCIEtario %>% 
  mutate(
    Var_Hosp_UCI = Hosp_UCI - lag(Hosp_UCI),
    "%Var_Hosp_UCI" = Var_Hosp_UCI/Hosp_UCI)

FECHA_INI <- min(CasosGeneroEtario$F_Semana)
FECHA_FIN <- max(CasosGeneroEtario$F_Semana, HospitalizadosUCIEtario$Fecha)
br_fecha <-  seq(FECHA_INI,FECHA_FIN,by= 7)



####################################
# Plantilla Shiny
####################################
#load(file = "app/JSP_Data.Rdata")

ui <- fluidPage(title = "Análisis COVID19 en Chile",
                lang = "es",
                titlePanel(title = "Análisis COVID19 en Chile",
                           windowTitle = "Análisis COVID19 en Chile"),
                
                sidebarLayout(
                  sidebarPanel(
                    titlePanel("Seleccionar Filtros:"),
                    fluidRow(column(12,
                                    dateRangeInput(
                                      inputId = "DAT_RAN_IN_1",
                                      label = "Rango de Fechas:",
                                      start ="2021-01-01",
                                      end = FECHA_FIN,
                                      min = FECHA_INI,
                                      max = FECHA_FIN,
                                      format = "dd-mm-yyyy",
                                      weekstart = 1,
                                      language = "es"
                                    ),
                                    checkboxGroupInput(
                                      "CHK_BX_G_IN_1",
                                      label = "Selecciona los rangos etarios:",
                                      choices = list("0 a 19", "20 a 39","0 a 39", "40 a 59", "60 o más"),
                                      selected = list("0 a 19", "20 a 39", "0 a 39", "40 a 59", "60 o más"),
                                      inline = TRUE)))),
                  mainPanel(plotOutput("NEW_CASES_1"),
                            plotOutput("ACUM_CASES_1"),
                            plotOutput("HOSP_UCI_1"),
                            plotOutput("HOSP_UCI_2")))
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
        size = 8,
        vjust = 0.5
      ),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "grey"),
      legend.position = "bottom"
      )})
    
  output$ACUM_CASES_1 <- renderPlot({
    CasosGeneroEtario %>% 
      filter(F_Semana>=input$DAT_RAN_IN_1[1],F_Semana<=input$DAT_RAN_IN_1[2], RangoEtario %in% input$CHK_BX_G_IN_1) %>% 
      group_by(RangoEtario, F_Semana) %>% 
      summarise("Casos Nuevos" = sum(Casos_Nuevos, na.rm = TRUE)) %>% 
      ungroup() %>% 
      ggplot(aes(x=F_Semana, y=`Casos Nuevos`))+
      geom_line(aes(color = RangoEtario),size = 1,linetype = "dotted") +
      geom_point(aes(color = RangoEtario),size = 4,alpha=0.5) +
      #geom_smooth(aes(group = RangoEtario, color = RangoEtario), fill = "lightgrey",size= 1, linetype = "solid")+
      #facet_grid(RangoEtario~., scales = "free_y")+
      scale_x_continuous(minor_breaks = br_fecha,breaks = br_fecha)+
      labs(title = "Casos nuevos Confirmados por Semana",
           subtitle = "Segmentado por Edad",
           x = "Fecha",
           y = "Casos")+
      theme(axis.text.x = element_text(
        angle = 90,
        size = 8,
        vjust = 0.5
      ),
      panel.background = element_rect(fill = "white"),
      panel.grid = element_line(color = "grey"),
      legend.position = "bottom"
      )})
  
  
      output$HOSP_UCI_1 <- renderPlot({
      HospitalizadosUCIEtario %>% 
        filter(F_Semana>=input$DAT_RAN_IN_1[1],F_Semana<=input$DAT_RAN_IN_1[2], RangoEtario %in% input$CHK_BX_G_IN_1) %>% 
        #group_by(RangoEtario, F_Semana) %>% 
        ggplot(aes(x=Fecha, y=`Hosp_UCI`))+
        geom_line(aes(color = RangoEtario),size = 1,linetype = "dotted") +
        geom_point(aes(color = RangoEtario),size = 2,alpha=0.35) +
        scale_x_continuous(minor_breaks = br_fecha,breaks = br_fecha)+
        labs(title = "Total Hospitalizados UCI",
             subtitle = "Diario - Segmentado por Edad",
             x = "Fecha",
             y = "Casos")+
        theme(axis.text.x = element_text(
          angle = 90,
          size = 8,
          vjust = 0.5
        ), 
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey"),
        legend.position = "bottom"
        )})  

    output$HOSP_UCI_2 <- renderPlot({
      HospitalizadosUCIEtario %>% 
        filter(F_Semana>=input$DAT_RAN_IN_1[1],F_Semana<=input$DAT_RAN_IN_1[2], RangoEtario %in% input$CHK_BX_G_IN_1) %>% 
        #group_by(RangoEtario, F_Semana) %>% 
        ggplot(aes(x=Fecha, y=Var_Hosp_UCI))+
        geom_line(aes(color = RangoEtario),size = 1,linetype = "dotted") +
        geom_point(aes(color = RangoEtario),size = 1,alpha=0.45) +
        geom_smooth(aes(group = RangoEtario, color = RangoEtario), method = "loess", fill = "lightgrey",size= 1, linetype = "solid")+
        scale_x_continuous(minor_breaks = br_fecha,breaks = br_fecha)+
        labs(title = "Total Hospitalizados UCI",
             subtitle = "Diario - Segmentado por Edad",
             x = "Fecha",
             y = "Casos")+
        theme(axis.text.x = element_text(
          angle = 90,
          size = 8,
          vjust = 0.5
        ),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey"),
        legend.position = "bottom"
        )})
}

shinyApp(ui, server)


