# --- Paquetes del app --- ---------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dygraphs)
library(tidyverse)
library(countrycode)
library(plotly)
library(gtrendsR)
library(lubridate)

source("app_files.R")
# --- Encabezado --- ---------------------------------------------------------------
includeCSS("legend.css")

header <- dashboardHeader(
  title = "Tablero de información para el microempresario",
  titleWidth = 500
  )

# --- Barra lateral --- ------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tab",
    menuItem("Entorno internacional", tabName = "internacional"),
    menuItem("Economía doméstica", tabName = "domestico"),
    menuItem("Google Trends", tabName = "gtrends")
    )
)

# --- Cuerpo del tablero --- -------------------------------------------------------

body <- dashboardBody(
  
  # Tab de datos internacionales  ----
  tabItems(
    
    tabItem(
      tabName = "internacional",
      
      h2("Datos económicos del G7"),
      
      fluidRow(
        
        tabBox(
          width = 8,
          title = "",
          tabPanel("Inflacion", dygraphOutput("inf_g7")),
          tabPanel("PIB", dygraphOutput("gdp_g7")),
          tabPanel("Importaciones", dygraphOutput("import_g7"))
        ),
        
        box( width = 4,
             checkboxGroupInput(
            "paises",
            label = "Páises",
            choices = c("Canada" = "Canada", "Francia" = "France",
                        "Alemania" = "Germany", "Japón" ="Japan",
                        "Italia" = "Italy", "Reino Unido" = "United Kingdom",
                        "Estados Unidos" = "United States"),
            selected = "United States"
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          h2('Datos sobre el precio del barril de petróleo')
        ),
        column(
          width = 6,
          h2('Indicador riesgo país')
        )
      ),
      
      fluidRow(
        tabBox(
          title = "",
          tabPanel("Precio diario", dygraphOutput("wti_dia")),
          tabPanel("Precio mensual", dygraphOutput("wti_mes"))
          # solidHeader = F,
          # title = "Evolución diaria del precio del crudo",
          # status = "success",
          # dygraphOutput("wti_dia")
        ),
        
        box(
          solidHeader = F,
          title = "",
          satatus = "success",
          dygraphOutput("embi")
        )
      )
    ),
    
    # Tab datos domésticos ----
    
    tabItem(
      tabName = "domestico",
      fluidRow(
        column(
          width = 6,
          h2('Inflación')
        ),
        column(
          width = 6,
          h2('Tipo de cambio')
        )
      ),
      
      fluidRow(
        
        tabBox(
          width = 6,
          title = "",
          tabPanel("variación mensual", dygraphOutput("ipc_vm")),
          tabPanel("Variación interanual", dygraphOutput("ipc_vi")),
          tabPanel("Variación con diciembre", dygraphOutput("ipc_vd"))
        ),
        
        tabBox(
          width = 6,
          title = "",
          tabPanel("Tasa de compra y venta", dygraphOutput("plot_tc")),
          tabPanel("Brecha de tasa", dygraphOutput("tc_brecha"))
          )
        ),
      
      h2("Tasas de interés del mercado"),
      fluidRow(
        
        box(
          width = 8,
          title = "",
          dygraphOutput("tasas")
          ),
        
        box(
          width = 4,
          selectInput(
            "tipo_tasa",
            label = "Tipo de tasa",
            choices = c("Activa" = "activas",
                        "Pasiva" = "pasivas"),
            selected = "activas"
        ),
        selectInput(
          "plazo",
          label = "Plazo",
          choices = c(
            "Promeido simple", "Promedio ponderado",
            "90 días", "180 días", "360 días",
            "2 años", "5 años", "Más de 5 años"),
          selected = "Promedio ponderado",
          multiple = TRUE
          ))
        )
      ),
    
    # Tab consulta a Gtrends
    
    tabItem(
      tabName = "gtrends",
      h2("Consulta de términos en google trends"),
      fluidRow(
        box(
          width = 4,
          textInput(
            "termino",
            "Término de consulta"
          ),
          
          selectInput(
            "pais",
             label = "País",
             choices = c("Republica Dominicana" = "DO", "EE.UU." = "US"),
             selected = "DO",
             selectize = TRUE
            ),
          
          selectInput(
             "period",
            label = "Período de tiempo",
            choices = c(
            "1 día" = "now 1-d",
             "7 días" = "now 7-d",
             "30 días" = "today 1-m",
             "90 días" = "Past 90 days",
             "12 meses" = "today 12-m",
             "5 años" = "today+5-y"
            ),
            selected = "today+5-y"
          ),
          actionButton("run", "Consultar")
        ),
        
        box(
          width = 8,
          plotlyOutput("gtrends_plot")
        )
      )
    )
    
    
    
    
  )
)




ui <- dashboardPage(skin = "green", header, sidebar, body)