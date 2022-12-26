library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(RColorBrewer)
library(viridis)

setwd("c:/temp/class1122")

source("functions.R")

##### Block 1

seds1 = read_csv("Complete_SEDS.csv") %>% 
  tolow3()

msn1 = read_excel("Codes_and_Descriptions.xlsx", sheet="MSN Descriptions", skip=9) %>% 
  tolow3()

stateinfo1 = read_excel("stateinfo6.xls")

##### Block 2

st1 = st_read("cb_2018_us_state_20m.shp") %>% 
  tolow3() %>% 
  mutate(statecode = stusps) %>% 
  filter(statecode != "AK" & 
           statecode != "HI" &
           statefp <= "56") %>% 
  mutate(stateabbr = statecode) %>% 
  left_join(stateinfo1)

##### Block 3

tmap_mode("view")

tm_shape(st1) +
  tm_polygons(col="regname")

#####  Block 4

tm_shape(st1) +
  tm_polygons(col="divname")

##### Block 5

seds2 = seds1 %>% 
  left_join(stateinfo1, 
            by=c("statecode" = "stateabbr")) %>% 
  filter(msn %in% c("CLEIB", "NGEIB", "NUEGB", "TPOPP")) %>% 
  filter(statename != "United States")

#####  Block 6

sedsdiv3 = seds2 %>% 
  group_by(divname, year, msn) %>% 
  summarize(divdata = sum(data))

##### Block 7

sedsdiv4 = sedsdiv3 %>% 
  pivot_wider(names_from=msn,values_from=divdata) %>% 
  tolow() %>% 
  mutate(coalpercap = cleib/tpopp,
         ngaspercap = ngeib/tpopp,
         nuclpercap = nuegb/tpopp)

##### Block 8

ggplot(data=sedsdiv4) +
  geom_line(aes(x=year,y=coalpercap, col=divname),lwd=1.25) +
  scale_color_brewer(palette = "Set3") +
  ggtitle("Coal per capita energy use")

##### Block 9

ggplot(data=sedsdiv4) +
  geom_line(aes(x=year,y=ngaspercap, col=divname),lwd=1.25) +
  scale_color_brewer(palette = "Set3") +
  ggtitle("Natural gas per capita energy use")

##### Block 10

ggplot(data=sedsdiv4) +
  geom_line(aes(x=year,y=nuclpercap, col=divname),lwd=1.25) +
  scale_color_brewer(palette = "Set3") +
  ggtitle("Nuclear per capita electricity generated")

##### Block 11

seds12 = seds1 %>% 
  left_join(stateinfo1, by=c("statecode" = "stateabbr")) %>% 
  filter(msn %in% c("HYEGB", "WYEGB", "SOEGB", "TPOPP")) %>% 
  filter(statename != "United States")

##### Block 12

sedsdiv13 = seds12 %>% 
  group_by(divname, year, msn) %>% 
  summarize(divdata = sum(data))

##### Block 13

sedsdiv14 = sedsdiv13 %>% 
  pivot_wider(names_from=msn,values_from=divdata) %>% 
  tolow() %>% 
  mutate(hydrpercap = hyegb/tpopp,
         solapercap = soegb/tpopp,
         windpercap = wyegb/tpopp)

##### Block 14

sedsdiv14long = sedsdiv14 %>% 
  pivot_longer(hyegb:windpercap,
               names_to="varname",
               values_to="data")
##### Block 15

ggplot(data=sedsdiv14) +
  geom_line(aes(x=year,y=solapercap, col=divname),lwd=1.25) +
  scale_color_brewer(palette = "Set3") +
  ggtitle("Solar per capita electricity generated") 

##### Block 16

ggplot(data=sedsdiv14) +
  geom_line(aes(x=year,y=windpercap, col=divname),lwd=1.25) +
  scale_color_brewer(palette = "Set3") +
  ggtitle("Wind per capita electricity generated")

##### Block 17

ggplot(data=sedsdiv14) +
  geom_line(aes(x=year,y=hydrpercap, col=divname),lwd=1.25) +
  scale_color_brewer(palette = "Set3") +
  ggtitle("Hydroelectric per capita electricity generated")

##### Block 18

imports = seds1 %>% 
  filter(year == 2020 & msn %in% c("ELISP","TPOPP")) %>% 
  pivot_wider(names_from=msn, values_from=data) %>% 
  mutate(importspercap = ELISP/TPOPP)

##### Block 19

st2 = st1 %>% 
  left_join(imports) %>% 
  select(stateabbr,ELISP)

##### Block 20

tmap_mode("view")

tm_shape(st2) +
  tm_polygons("ELISP",palette="BrBG",n=8) +
  tm_layout("Electricity Imports in Millions of kWh") +
  tm_view(view.legend.position = c("right","bottom")) 



