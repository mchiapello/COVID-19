library(tidyverse)
library(fs)
library(ggthemes)
library(ggrepel)
library(ggsci)
# Font
library(showtext)
# Google fonts: https://fonts.google.com/
font_add("roboto", "~/Downloads/Spartan/static/Spartan-ExtraLight.ttf")
font_add("roboto2", "~/Downloads/Spartan/static/Spartan-Regular.ttf")
showtext_auto()
## turn off if no longer needed
showtext_auto(FALSE)

file <- function(){
file_info(dir_ls()) %>%
select(path, type, size, birth_time, change_time, modification_time, access_time) %>%
arrange(desc(modification_time))}


################################################################################
# Read data procince
f <- dir_ls("../dati-province/", recurse = F, regex = "\\d\\.csv$")

x <- map_df(f, read_csv)

x %>% 
    filter(denominazione_regione == "Lombardia") %>%
#     filter(denominazione_provincia == "Torino",
#            !grepl("2020-03-19", data)) %>%
    ggplot(aes(data, totale_casi)) +
    geom_point() +
    geom_smooth() +
    theme_economist() +
    facet_wrap(~denominazione_provincia)
