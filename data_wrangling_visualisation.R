# Paquetes -------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(dygraphs)
library(janitor)
library(tseries)
library(magrittr)
library(readxl)
library(plotly)
library(highcharter)

# Utencilios como funciones y cosas así ---- ---------------------------------------

recode_month <- function(x) {
  
  x <- tolower(x)
  
  recode(x,
         'ene' = '01',
         'feb' = '02',
         'mar' = '03',
         'abr' = '04',
         'may' = '05',
         'jun' = '06',
         'jul' = '07',
         'ago' = '08',
         'sep' = '09',
         'oct' = '10',
         'nov' = '11',
         'dic' = '12'
  ) 
}

multiplicar <- function(x){x * 100}

# Importando data ---- -------------------------------------------------------------

# Tipo de cambio -------
url_tc <- paste0(
  "https://cdn.bancentral.gov.do/documents/estadisticas/",
  "mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC",
  ".xls?v=1579789429624"
)

download.file(url_tc, 'datos/tc.xls')

tc <- read_excel(
  'datos/tc.xls',
  sheet = "PromMensual",
  skip = 4,
  col_names = FALSE)

tc <- 
  tc %>%
  setNames(c("year", "mes", "tc_compra_pm", "tc_venta_pm")) %>%
  select(year, mes, tc_compra_pm, tc_venta_pm) %>%
  mutate(
    date = paste(year, recode_month(mes), "01", sep = "-"),
    date = ymd(date),
    date_ym = zoo::as.yearmon(date)
         )

tc_ts <- 
  tc %>%
  dplyr::select(tc_venta_pm, tc_compra_pm) %>%
  ts(start = min(tc$date_ym), frequency = 12) 

  
# Inflacion -----------
url_ipc <- paste0(
  "https://cdn.bancentral.gov.do/documents/",
  "estadisticas/precios/documents/",
  "ipc_base_2010.xls?v=1570116997757")

download.file(url_ipc, "datos/ipc_base_2010.xls")

ipc <- read_excel(
  "datos/ipc_base_2010.xls",
  sheet = 1,
  col_names = FALSE,
  skip = 7
)

ipc <- 
  ipc %>% 
  clean_names() %>%
  select(x1:x7) %>%
  setNames(
    c("year", "mes", "ipc","ipc_vm", "ipc_vd", "ipc_vi", "ipc_p12")
  ) %>%
  filter(!is.na(mes)) %>%
  mutate(
    date = seq(ymd("1984/01/01"), 
                by = "month", 
                length.out = nrow(.)),
    year = year(date),
    date_ym = zoo::as.yearmon(date)
  ) %>%
  select(date, year, mes, everything())


ipc_ts <-
  ipc %>%
  dplyr::select(starts_with("ipc_")) %>%
  ts(start = min(ipc$date_ym), frequency = 12)


# precios del crudo West texas  -----
# FRED data (Descargar la data del siguiente enlace y colocarla en la carpeta datos)
# https://fred.stlouisfed.org/series/DCOILWTICO

precio_wt <- read_csv('datos/DCOILWTICO.csv', na = '.') %>%
  setNames(c("date", "price")) 

wti_price_dts <-
  precio_wt %>%
  mutate(
    date = as.numeric(date) * 24 * 60 * 60
    ) %$%
  #date
  irts(time = date, value = price)


wti_price_mts <-
  precio_wt %>%
  mutate(
    month = month(date),
    year = year(date)) %>%
  group_by(year, month) %>%
  summarise(price = mean(price, na.rm = T)) %>%
  ungroup() %>%
  select(price) %>%
  ts(start = c(2014, 9), frequency = 12)

# Data del g7 -------

g7 <- read_csv('datos/g7_data.csv')

# tasa  -----
tasas <- readRDS("datos/tasas.RDS")

# embi ---- 
url_embi <- paste0("https://bcrdgdcprod.blob.core.windows.net/documents/",
                   "entorno-internacional/documents/Serie_Historica_",
                   "Spread_del_EMBI.xlsx?v=1562875610479")

download.file(url_embi, "datos/embi.xlsx")

embi <- read_excel("datos/embi.xlsx", skip = 1, col_names = T) %>%
  clean_names() %>%
  select(fecha, global, latino, rep_dom)


# Transformando variables y objetos ---- -------------------------------------------

g7_clean <- 
g7 %>%
  select(-(`Units`:`Country/Series-specific Notes`), -`Estimates Start After`) %>%
  clean_names() %>%
  rename( variable = subject_descriptor) %>%
  gather(key = "year", value = "value", -country, -variable) %>%
  mutate(year = parse_number(str_remove(year, "x"))) %>%
  spread(variable, value) %>%
  clean_names()

# --- --- Objeto con las Tasas OSD --- ---
tasas_clean <- tasas %>%
  gather("plazo", "tasa", -tipo_tasa, -fecha) %>%
  filter(!is.na(tasa)) %>%
  mutate(plazo = str_remove(plazo, "ta_|tp_")) %>%
  filter(str_detect(plazo, "90d|180d|360d|2a|5a|ps|pp"), !is.na(plazo)) %>%
  mutate(
    plazo = factor(
      plazo, 
      levels = c("ps", "pp", "90d","180d", "360d", "2a", "5a", "m5a"),
      labels = c("Promeido simple", "Promedio ponderado",
                 "90 días", "180 días", 
                 "360 días", "2 años", "5 años", "Más de 5 años")
      )
    )



# Visualizaciones ------------------------------------------------------------------

# --- --- Graficos sobre el tipo de cambio --- ----

# series de tasa de compra y venta ---
tc_ts %>%
  dygraph() %>%
  dySeries("tc_venta_pm", label = "Tasa de venta US$") %>%
  dySeries("tc_compra_pm", label = "Tasa de compra UD$") %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-02-01"))

# brecha de tasas ---
(tc_ts[,'tc_venta_pm'] - tc_ts[,'tc_compra_pm']) %>%
  dygraph() %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-02-01")) %>%
  dyBarChart()
  

# --- --- Gráficos sobre la inflación --- ----

# Inflacion mensual ---

ipc_ts[,"ipc_vm"] %>%
  multiplicar() %>%
  dygraph() %>%
  dySeries("V1", label = "Inflación mensual") %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-03-01"))
  

# Inlfación interanual ---
ipc_ts[, "ipc_vi"] %>%
  multiplicar() %>%
  dygraph() %>%
  dySeries("V1", label = "Inflación interanual") %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-03-01"))

# Inlfación con diciembre ---
ipc_ts[, "ipc_vd"] %>%
  multiplicar() %>%
  dygraph() %>%
  dySeries("V1", label = "Inflación interanual") %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-03-01"))


# Inlfación promedio ultimos 12 meses ---
ipc_ts[, "ipc_p12"] %>%
  multiplicar() %>%
  dygraph() %>%
  dySeries("V1", label = "Inflación interanual") %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-03-01"))

# Inflación por grupos de bienes y servicios ---

ipc_grupos %>%
  select(ends_with('vi'), -mes, -anio) %>%
  ts(start = c(2001, 1), frequency = 12) %>%
  dygraph()


# --- --- Graficos sobre el G7 --- ----

# Inflación ---
g7_clean %>% 
  dplyr::filter(country %in% c("Canada", "France")) %>%
  select(country, year, inflation_average_consumer_prices) %>%
  spread(country, inflation_average_consumer_prices) %>%
  select(-year) %>%
  ts(start = 2000, frequency = 1) %>%
  dygraph() %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE)
  
# Tasa de crecimiento dle PIB ---

g7_clean %>% 
  select(country, year, gross_domestic_product_constant_prices) %>%
  spread(country, gross_domestic_product_constant_prices) %>%
  select(-year) %>%
  ts(start = 2000, frequency = 1) %>%
  dygraph() %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE)

# Producción en millones de US$ ---
g7_clean %>% 
  select(country, year, gross_domestic_product_current_prices) %>%
  spread(country, gross_domestic_product_current_prices) %>%
  select(-year) %>%
  ts(start = 2000, frequency = 1) %>%
  dygraph() %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE)

# Volumen de exportaciones ---

g7_clean %>% 
  select(country, year, volume_of_exports_of_goods_and_services) %>%
  spread(country, volume_of_exports_of_goods_and_services) %>%
  select(-year) %>%
  ts(start = 2000, frequency = 1) %>%
  dygraph() %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE)


# --- --- Gráficos sobre el precio del petroleo --- ----

# Precios diarios ---
wti_price_dts %>%
  dygraph() %>%
  dySeries("V1", label = "Precio por barril") %>%
  dyRangeSelector(dateWindow = c("2019-01-01", "2019-09-01"))

# Precios promedios mensuales ---
wti_price_mts %>%
  dygraph() %>%
  dySeries("price", label = "Precio por barril") %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-09-01"))

# --- --- Gráficos sobre las tasas activas y pasivas --- ----
tasas_clean %>%
  filter(tipo_tasa == "activas", plazo %in% c("90 días", "180 días")) %>%
  spread(plazo, tasa) %>%
  select(-fecha, -tipo_tasa) %>%
  ts(start = c(2000, 1), frequency = 12) %>%
  dygraph() %>%
  dyRangeSelector(dateWindow = c("2017-01-01", "2019-12-01"))


# Gráfico riesgo país --------------------------------------

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

# Saving workspace
save.image("app/app_lisette_ws")



