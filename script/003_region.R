library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(gganimate) #Used for animations
library(RColorBrewer) #Used for color scale 

setwd(here())

# Read data
covid19_data <- read_csv("dati-province/dpc-covid19-ita-province.csv")  %>%
    select(data, DP = denominazione_provincia, totale_casi, lat, long) %>%
    mutate(data = ymd_hms(data))

# Filter data
co <- covid19_data %>%
    filter(DP != "In fase di definizione/aggiornamento")

# https://www.diva-gis.org/datadown
it <- st_read("ITA_adm/")

# Join
sp <- co %>%
    left_join(it, by = c("DP" = "NAME_2")) %>%
    select(data:totale_casi, geometry)

sp <- st_as_sf(sp) %>%
    mutate(data = as.character(data), 
           data = str_extract(data, "\\d\\d\\d\\d-\\d\\d-\\d\\d"), 
           data = ymd(data))

################################################################################
# Plot
# Makes plot with ggplot2 and gganimate to animate through the days 
sp_map<-ggplot()+
  geom_sf(data = it,fill = "white")+
  geom_sf(data = sp %>% filter(data == "2020-04-12"),aes(fill=totale_casi))+
  ggtitle("Spread of Covid19 Throughout Italy")+
  xlab("")+
  ylab("")+
  labs(subtitle = "Date: {current_frame}",
       caption = "Date Source: https://github.com/pcm-dpc/COVID-19\nAuthor: @mchiapello")+
  cowplot::background_grid(major = "none", minor = "none") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.background = element_blank(),
        legend.position="bottom",
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(colour="black", size=12, face="bold"),
        plot.title=element_text(size=20, face="bold",hjust =0.5),
        plot.subtitle = element_text(hjust = 0.5,size=12),
        plot.caption = element_text(size = 11,
                                    hjust = .5,
                                    color = "black",
                                    face = "bold"))+
  scale_fill_distiller("Number of Positive Cases",
                       palette ="Reds",type = "div",
                       direction = 1)+
  transition_manual(data)

#   animate(sp_map, nframe=25,fps = 2, end_pause = 15,height = 500, width =500)
  animate(sp_map)


