# Servidor de la aplicación --------------------------------------------------------
server <- function(input, output){
  
  # Gráfico de la inflación de los países del g7
  output$inf_g7 <- renderDygraph({
    
    g7_clean %>% 
      dplyr::filter(country %in% input$paises) %>%
      dplyr::select(country, year, inflation_average_consumer_prices) %>%
      spread(country, inflation_average_consumer_prices) %>%
      dplyr::select(-year) %>%
      ts(start = 2000, frequency = 1) %>%
      dygraph() %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE)
  })
  
  # Gráfico del gdp de los países del g7
  output$gdp_g7 <- renderDygraph({
    g7_clean %>% 
      dplyr::filter(country %in% input$paises) %>%
      dplyr::select(country, year, gross_domestic_product_constant_prices) %>%
      spread(country, gross_domestic_product_constant_prices) %>%
      dplyr::select(-year) %>%
      ts(start = 2000, frequency = 1) %>%
      dygraph() %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE) %>%
      dyRangeSelector(dateWindow = c("2010-01-01", "2019-12-01")) %>%
      dyLegend(show = 'always', width = 600)
  })
  
  # Gráfico de las importaciones de los países del g7
  output$import_g7 <- renderDygraph({
    g7_clean %>% 
      dplyr::filter(country %in% input$paises) %>%
      select(country, year, gross_domestic_product_current_prices) %>%
      spread(country, gross_domestic_product_current_prices) %>%
      select(-year) %>%
      ts(start = 2000, frequency = 1) %>%
      dygraph() %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE) %>%
      dyRangeSelector(dateWindow = c("2010-01-01", "2019-12-01")) %>%
      dyLegend(show = 'always', width = 600)
  })
  
  # Gráfico del precio diario del petróleo
  output$wti_dia <- renderDygraph({
    wti_price_dts %>%
      dygraph() %>%
      dySeries("V1", label = "Precio por barril") %>%
      dyRangeSelector(dateWindow = c("2019-01-01", "2019-12-01"))
  })
  
  # Gráfico del precio mensual del petróleo
  output$wti_mes <- renderDygraph({
    wti_price_mts %>%
      dygraph() %>%
      dySeries("price", label = "Precio por barril") %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2019-12-01"))
  })
  
  # Gráfcio de la variación mensual del IPC
  output$ipc_vm <- renderDygraph({
    ipc_ts[,"ipc_vm"] %>%
      multiplicar() %>%
      dygraph() %>%
      dySeries("V1", label = "Inflación mensual") %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2019-12-01"))
  })
  
  # Gráfico de la variación internaual del IPC
  output$ipc_vi <- renderDygraph({
    ipc_ts[, "ipc_vi"] %>%
      multiplicar() %>%
      dygraph() %>%
      dySeries("V1", label = "Inflación interanual") %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2019-12-01"))
  })
  
  # Gráfico de la variación acumulada del IPC
  output$ipc_vd <- renderDygraph({
    ipc_ts[, "ipc_vd"] %>%
      multiplicar() %>%
      dygraph() %>%
      dySeries("V1", label = "Inflación interanual") %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2019-12-01"))
  })
  
  # Gráfico del tipo de cambio
  output$plot_tc <- renderDygraph({
    tc_ts %>%
      dygraph() %>%
      dySeries("tc_venta_pm", label = "Tasa de venta US$") %>%
      dySeries("tc_compra_pm", label = "Tasa de compra UD$") %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2020-01-01"))
  })
  
  # Gráfico de las brechas entre las tasas compra y venta
  output$plot_tc <- renderDygraph({
    tc_ts %>%
      dygraph() %>%
      dySeries("tc_venta_pm", label = "Tasa de venta US$") %>%
      dySeries("tc_compra_pm", label = "Tasa de compra UD$") %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2020-01-01"))
  })
  
  #
  output$tc_brecha <- renderDygraph({
    (tc_ts[,'tc_venta_pm'] - tc_ts[,'tc_compra_pm']) %>%
      dygraph() %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2019-02-01")) %>%
      dyBarChart()
  })
  
  out <- reactive({
    input$run
    
    if (length(isolate(input$termino)) > 0) {
      unlist(strsplit(input$termino, ","))
    }
  })
  
  #### Eg : gtrends(keyword = NA, geo = "", time = "today+5-y")
  consulta <- reactive({
    input$run
    
    if (length(input$termino) != 0)
      req(input$termino)
    {
    consulta <- gtrends(keyword = out(),
              time = isolate(input$period),
              geo = isolate(input$pais))
      
      consulta <- consulta[["interest_over_time"]] %>%
        ggplot(aes(x = date, y = hits)) +
        geom_line() +
        theme_minimal() +
        labs(x = "", y = "Busquedas")
        }
  })
  
  #Plot the Trend
  output$gtrends_plot <- renderPlotly({
    consulta()
  })
  
  output$tasas <- renderDygraph({
    tasas_clean %>%
      filter(tipo_tasa == input$tipo_tasa,
             plazo %in% input$plazo) %>%
      spread(plazo, tasa) %>%
      select(-fecha, -tipo_tasa) %>%
      ts(start = c(2000, 1), frequency = 12) %>%
      dygraph() %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2019-12-01"))
  })
  
  
  output$embi <- renderDygraph({
    
    embi %>%
      setNames(c("fecha", "EMBI global", "Embi latino", "Rep. Dom.")) %>%
      mutate(periodo = paste( year(fecha), month(fecha), sep = "-")) %>%
      group_by(periodo) %>%
      summarise_at(.vars = vars(-fecha), mean) %>%
      ungroup() %>%
      select(-periodo) %>%
      ts(start = c(2007, 9), frequency = 12) %>%
      dygraph() %>%
      dyRangeSelector(dateWindow = c("2017-01-01", "2020-01-01"))
  })
  
}






