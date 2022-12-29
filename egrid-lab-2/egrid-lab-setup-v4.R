library(tidyverse)
library(readxl)
library(sf)
library(tmap)

# sets working directory to R source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
message(paste0("Working directory = ", getwd()))

# loads custom functions
source("./functions/functions1a.R")

egrid1 = read_excel("./data/egrid2018_data_v2.xlsx",
                    sheet="PLNT18",skip=1) %>% 
         tolow2()

egrid2 = egrid1 %>% mutate(StateAbbr = pstatabb,
                           PlantName = pname,
                           NamePlateCapMW = namepcap,
                           Fuel = plfuelct,
                           NetGenGWh = plngenan/10^3,
                           CO2tonsm = replace_na(plco2eqa,0)/10^6) %>% 
  select(StateAbbr, PlantName, Fuel,
         NamePlateCapMW,
         NetGenGWh,
         CO2tonsm,
         lat,lon)

st2 = egrid2 %>% 
  filter(StateAbbr == "GA") %>% 
  filter(lat > 0 & lon < 0)
          
st3 = st2 %>% filter(NamePlateCapMW >= 1000)


st2geo = st_as_sf(st2, coords = c("lon", "lat"), 
                          crs = 4326) %>% 
  select(PlantName, everything())

stsolargeo = st2geo %>% 
  filter(Fuel == "SOLAR")

gacounty = st_read("./gisdata/gacounty.shp") %>% 
  select(NAME, everything())
istates = st_read("./gisdata/interstates.shp")





