library(shiny)
library(shinydashboard)
library(tidyverse)
library(patchwork)
library(forecast)

theme_set(theme_classic())
options(scipen = 999)


set <- read.csv2("conso2021.csv")

regions <- c("Latin America & Caribbean", 
                "South Asia",
                "Sub-Saharan Africa" , 
                "Europe & Central Asia",
                "Middle East & North Africa", 
                "East Asia & Pacific",
                "North America" )

gs <- read.csv2("net_save.csv")


ctrys <- unique(gs$Country.Name)



server <- function(input, output, session){
  
  
  data <- reactive({
    req(input$region_id)
    df <- set %>% 
      filter(region == input$region_id)
  })
  
  
  data2 <- reactive({
    req(input$region_id2)
    df <- gs %>% 
      filter(region == input$region_id2,
             anno == 2020)
    
  })
  
  data3 <- reactive({
    
    req(input$country_id)
    df <- gs %>% 
      filter(Country.Name == input$country_id)
    
  })
  
  
  
  output$plot1 <- renderPlot({
    
    data() %>%
      ggplot() +
      aes(pibpc_ppp, co2pc ) +
      geom_line(aes(y = fit), color = "darkred",
                size = 1.2, alpha = .3) +
      geom_ribbon(aes(ymin = lwr, ymax = upr),
                  alpha = .2) +
      geom_text(aes(label = Country.Code), size = 3.2) +
      scale_x_continuous(trans = "log10") +
      scale_y_continuous(trans = "log10") +
      labs(x="PIB percápita PPC",
           y=expression(CO^2~"percápita"),
           subtitle = "Ejes escalados a logaritmo base 10. Año 2021",
           title = expression("Relación PIB y emisión de"~CO^2)) -> g1
    
    
    g1 
    
  })
  
  
  
  output$plot2 <- renderPlot({
    
    data() %>%
      ggplot() +
      aes(reorder(Country.Code, (co2pc - fit)),
          co2pc - fit,
          fill = co2pc - fit) +
      guides(fill = "none") +
      scale_fill_gradient(low = "darkgreen", high ="darkred") +
      geom_col() +
      coord_flip() +
      theme(axis.text.x = element_text(size = 10.5)) +
      labs(x="",
           y="",
           subtitle = "Año 2021",
           title = "Diferencia entre emisión esperada y observada") -> g2
    
    g2
    
    
  })
  
  
  output$plot3 <- renderPlot({
    
    data2() %>%
      ggplot() +
      aes(reorder(Country.Code, diff/100), 
          y= diff,
          fill = diff) +
      guides(fill = "none") +
      scale_fill_gradient(low = "darkred", high ="darkgreen") +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(size = 10.5)) +
      labs(x="",
           y="",
           title = "Diferencia entre ahorro neto observado y media regional",
           subtitle = "Ahorro como porcentaje del Ingreso Nacional Bruto. Año 2020") -> g3
    
    
    g3
    
  })
  
  
  output$plot4 <- renderPlot({
    
    data3() %>% 
      pull(netsav) %>% 
        ts(start = 1990, end = 2020, frequency = 1) %>% 
        {
          tslm(./100 ~ trend)
        } %>% 
        forecast(h = 10) %>% 
        autoplot(., size = 10) +
        scale_y_continuous(labels = scales::percent) +
        labs(x="",
             y="% del Ingreso Nacional Bruto",
             title = "Tendencia y proyección del ahorro neto ajustado",
             subtitle = "En azul proyección por MCO") -> g4
    
    g4
    
  })

  
}



ui <- dashboardPage(

  dashboardHeader(title = "Cambio climático Dashboard",
                  titleWidth = "30%"),
  dashboardSidebar(collapsed = T,
                   sidebarMenu(
                     menuItem("PIB y CO2", tabName = "dashboard1", icon = icon("dashboard")),
                     menuItem("Ahorro neto ajustado", tabName = "dashboard2", icon = icon("dashboard"))
                   )),
  dashboardBody(

    
    tabItems(
      
      tabItem( tabName = "dashboard1",
        
        fluidRow(
          box(selectInput(inputId = "region_id",
                          label = "Elegir región",
                          choices = regions),
              plotOutput("plot1", width = "100%"),
              width = 9)
        ),
        
        
        fluidRow(
          
          box(plotOutput("plot2", width = "100%"),
              width = 9)
          
        ),
        
        
        fluidRow(
          
          box(
            
            p("Se utilizaron los datos de gases de efecto invernadero de Our World in Data, https://github.com/owid/co2-data. Los datos del PIB fueron obtenidos de la base del Banco Mundial"),
            p("El CO2 esperado, es su esperanza condicional en el PIB percápita por país en el 2021. Esta se obtuvo por mínimos cuadrados ordinarios. El área sombreada representa el intérvalo de confianza al 95%, con errores estándares robústos (se utilizó matriz de covarianza HC3)"),
            width = 12
            
          ))
        
      )# fin tab dashboard1
      ,
      
      tabItem(tabName = "dashboard2",
        
              fluidRow(
                box(selectInput(inputId = "region_id2",
                                label = "Elegir región",
                                choices = regions),
                    plotOutput("plot3", width = "100%"),
                    width = 9)
              ),
              
              fluidRow(
                box(selectInput(inputId = "country_id",
                                label = "Elegir País",
                                choices = ctrys),
                    plotOutput("plot4", width = "100%"),
                    width = 9)
                
              ),
              fluidRow(
                
                box(
                  
                  p("Se utilizaron datos del Banco Mundial. El ahorro neto ajustado, también llamado 'Ahorro Genuino', intenta representar cuantos recursos disponibles, se están almacenando para movilizar en un futuro, incluyendo en ello, el desgaste de los recursos naturales. Mientras menor sea el ahorro, más rápido se están consumiendo estos recursos (y por tanto, sacrificando el futuro), y viceversa"),
                  p("La proyección se realizó por Mínimos Cuadrados Ordinarios ajustando por la tendencia anual de la serie"),
                  width = 12
                  
                ))
        
        
        
      ) # fin tab dashboard2
      
      
    ) # fin tabitems
    





)) # fin dashboardPage



shinyApp(ui, server)
