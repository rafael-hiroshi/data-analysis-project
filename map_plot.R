detach("package:dplyr", unload=TRUE)
library(dplyr)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

View(df)
df <- read.csv('RDO_3_v2.csv')
df_locations <- df[!is.na(df$LATITUDE) & !is.na(df$LONGITUDE),]

height <- max(df_locations$LATITUDE) - min(df_locations$LATITUDE)
width <- max(df_locations$LONGITUDE) - min(df_locations$LONGITUDE)
coord_map <- c(bottom  = min(df_locations$LATITUDE)  - 0.1 * height, 
                 top     = max(df_locations$LATITUDE)  + 0.1 * height,
                 left    = min(df_locations$LONGITUDE) - 0.1 * width,
                 right   = max(df_locations$LONGITUDE) + 0.1 * width)
map <- get_stamenmap(coord_map, zoom = 10, maptype = "toner-lite")

ggmap(map) +
  geom_point(data = df_locations, mapping = aes(x = LONGITUDE, y = LATITUDE))


violent_crimes <- subset(df_locations, RUBRICA == "Homicídio simples" & !is.na(SEXO_PESSOA))

df_locations %>% count(RUBRICA, sort=TRUE)

height <- max(violent_crimes$LATITUDE) - min(violent_crimes$LATITUDE)
width <- max(violent_crimes$LONGITUDE) - min(violent_crimes$LONGITUDE)
coord_map <- c(bottom  = min(violent_crimes$LATITUDE)  - 0.1 * height, 
               top     = max(violent_crimes$LATITUDE)  + 0.1 * height,
               left    = min(violent_crimes$LONGITUDE) - 0.1 * width,
               right   = max(violent_crimes$LONGITUDE) + 0.1 * width)
map <- get_stamenmap(coord_map, zoom = 10, maptype = "toner-lite")

ggmap(map) + geom_point(aes(x = LONGITUDE, y = LATITUDE, color = DESCR_TIPOLOCAL, fill=RUBRICA),data = violent_crimes)
