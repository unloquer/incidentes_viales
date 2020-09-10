library(tidyverse)
library(readxl)
library(httr)

binary_data <- GET("https://www.medellin.gov.co/movilidad/cifras-estudios/finish/3902-cifras-de-incidentalidad-diaria/334375-victimas-fatales-por-incidentes-viales-2020") %>% httr::content()


excel_file <- tempfile("smtt_med_file")
zz <- file(excel_file, "wb")
writeBin(binary_data, zz)
close(zz)

data <- read_excel(path = excel_file,1)
colnames(data) <- as.character(data[2,])
data <- data[-c(1,2),]

data <- read_tsv("./data/iv_2020_120.csv")

data %>% group_by(lubridate::month(FechaIncidente)) %>% summarise(n())

data %>% group_by(`Dirección`) %>% summarise(cnt=n()) %>% arrange(desc(cnt))

## 3. Geocoder de OSM (nominatim) -> https://nominatim.org/ https://nominatim.openstreetmap.org/ui/search.html
## formato nominatim:
## Medellín, Valle de aburrá, Antioquia, 0500, CL -> Calle 71 CR -> Carrera 65
nominatim_pretext <- "Medellín, Valle de aburrá, Antioquia, 0500, "
data$osm_search_term <- data$`Dirección` %>%
    str_replace("CL","Calle") %>%
    str_replace("CR","Carrera") %>%
    str_replace("TR","Transversal") %>%
    str_replace("DG","Diagonal") %>%
    str_replace("entre","") %>%
    str_replace(" y ","") %>%
    paste0(nominatim_pretext,.)
## API query -> https://nominatim.org/release-docs/develop/api/Search/
nominatim_q_template <- "https://nominatim.openstreetmap.org/search.php?q=%s&format=json&limit=1"

## Hacer map de los todos los elementos y retornar lat y lon 
GET(sprintf(nominatim_q_template,data$osm_search_term[1])) %>% httr::content()

## Opciones para geocodificar las direcciones
## 1. Geocoder de la Alcaldía -> https://www.medellin.gov.co/MAPGISV5_WEB/mapa.jsp?aplicacion=0
## https://www.medellin.gov.co/servicios/GEOCOD_WEBP/processFormField.do?accion=4&nombreArchivoExcel=/pub/aplicaciones/secretaria_ti/tempGeocod/iv_2020_120.xlsx&hoja=iv_2020_120
## https://www.medellin.gov.co/mapas/rest/services/
## https://pro.arcgis.com/es/pro-app/tool-reference/geocoding/geocode-addresses.htm
## https://github.com/cengel/ArcGIS_geocoding
## https://developers.arcgis.com/rest/services-reference/geocode-service.htm

## 2. Geocoder de google



