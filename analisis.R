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

(data$`Fecha\nIncidente`)
