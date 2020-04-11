library(tidyverse)
library(here)
library(lubridate)
library(sf)

setwd(here())

# Read data
covid19_data <- read_csv("dati-province/dpc-covid19-ita-province.csv")  %>%
    select(data, DP = denominazione_provincia, totale_casi, lat, long) %>%
    mutate(data = ymd_hms(data))

# Filter data
co <- covid19_data %>%
    filter(DP != "In fase di definizione/aggiornamento")

# https://www.diva-gis.org/datadown
it <- st_read("Italy_shapefile/ITA_adm/")

# Join
sp <- co %>%
    left_join(it, by = c("DP" = "NAME_2")) %>%
    select(data:totale_casi, geometry)
