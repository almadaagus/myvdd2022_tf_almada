library(shiny)
library(readr)
library(worldcup)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(shinyWidgets)
library(gapminder)

rsconnect::setAccountInfo(name='almadaagus', token='DE7F34134C70961A3BABDB6BE829CC6C', secret='QwkPl5ARrIV4ceuPQ/CNF7N6k4hesspkZELCQ86u')

partidos <- worldcup::matches
partidos$year <- as.numeric(format(partidos$match_date, "%Y"))
columnas <- c("Anio", "Pais", "P.Ganados", "P.Empatados","P.Perdidos")
partidos_pais <- data.frame(matrix(nrow = 0, ncol = length(columnas)))
colnames(partidos_pais) <- columnas

for(i in 1:nrow(partidos)) {
  partidos_pais <- rbind(partidos_pais, c(partidos$year[i], partidos$home_team_name[i],partidos$home_team_win[i], partidos$draw[i],partidos$away_team_win[i]))
  colnames(partidos_pais) <- columnas
  partidos_pais <- rbind(partidos_pais, c(partidos$year[i], partidos$away_team_name[i],partidos$away_team_win[i], partidos$draw[i],partidos$home_team_win[i]))
  colnames(partidos_pais) <- columnas
}

partidos_pais <-transform(partidos_pais, P.Ganados = as.numeric(P.Ganados))
partidos_pais <-transform(partidos_pais, P.Empatados = as.numeric(P.Empatados))
partidos_pais <-transform(partidos_pais, P.Perdidos = as.numeric(P.Perdidos))
partidos_pais_anio <- partidos_pais %>% group_by(Anio, Pais) %>% summarise(p.ganados = sum(P.Ganados), p.empatados = sum(P.Empatados), p.perdidos = sum(P.Perdidos))
partidos_pais_anio$Anio <- as.character(partidos_pais_anio$Anio)
partidos_pais_anio <- data.frame(partidos_pais_anio)

#listado de paises disponibles
opciones_paises <-  unique(partidos_pais_anio$Pais)


MiInterfaz <- fluidPage( #Estructura general de la web
  titlePanel("Resultados de los partidos jugados en cada uno de los mundiales del pais seleccionado"), #Título de la web
  sidebarLayout( #Función para crear paneles
    sidebarPanel(
      pickerInput(
        inputId = "pais", #ID del widget
        label = "Pais", #Título a mostrar en la app
        choices = sort(opciones_paises), #Opciones disponibles
        selected = "Argentina" #Opción seleccionada inicialmente
      )
    ), #Panel secundario
    mainPanel(
      plotOutput("MiGrafico")
    ) #Panel principal
  )
)


MiServidor <- function(input, output) {
  pais_reactivo <- reactive({input$pais})

  output$MiGrafico <- renderPlot({
    
  grafico <- partidos_pais_anio %>% filter(Pais == pais_reactivo())  %>% group_by(Anio) %>%
  summarise(n=n(),
            won = sum(p.ganados),
            tied = sum(p.empatados),
            lost = sum(p.perdidos)) %>% 
  gather("key", "value", - c(Anio, n)) %>%
  ggplot(aes(x = Anio, y = value, group = key, fill = key)) + 
  geom_col() + 
  coord_flip() +
  scale_fill_manual(values = c("#8b0000","#e6e600" , "#006400")) +
  labs(title =paste("Resultados en todos los mundiales de", pais_reactivo())) +
  theme_bw() +
  scale_x_discrete(name = "Mundiales") +
  scale_y_continuous(name = "Partidos", breaks = seq(0,7,1))

  plot(grafico)
  })
}

shinyApp(ui = MiInterfaz, server = MiServidor)