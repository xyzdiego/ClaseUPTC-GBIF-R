# Definicion del directorio de trabajo ------------------------------------

setwd("C:/Users/diego/Documents/Clases_R/Hermes/UPTC/")

# Cargado de librerias y archivos -----------------------------------------

if(!require(pacman)) install.packages("pacman")
pacman::p_load(rgbif, protolite, raster, sf, tidyverse, readxl, cartogram, tmap,
               geodata, rnaturalearthdata, rnaturalearth, ggspatial, hrbrthemes, 
               ggthemes, terra, RColorBrewer, glue, fs, dismo, stringr, gtools)

# Exploracion del GBIF ----------------------------------------------------

# Total de registros de presencia en GBIF
occ_count()

# Cantidad de registros georreferenciados
occ_count(georeferenced = TRUE)
occ_count(hasGeospatialIssue = TRUE)

# Cantidad de registros ubicados en Colombia
## Obtencion del c?digo del pais
(col_isocode <- isocodes[grep("Colombia", isocodes$name), "code"])
## Conteo
occ_count(country=col_isocode)

# Cantidad de registros del roble ubicado en Colombia, georreferenciado
## Obtencion del codigo del taxon
name <- name_backbone(name='Tabebuia rosea', rank='species')
name[, c('usageKey', 'scientificName')]

## Conteo
occ_count(taxonKey = name[,("usageKey")], 
          #georeferenced = TRUE
          country = col_isocode
)

# Busqueda de registros de presencia --------------------------------------

## Registros de presencia de ocelote en Costa Rica, georreferenciados y sin 
## problemas detectados de georreferenciacion (ej. coordenadas invertidas)
occ_search(
    scientificName = 'Leopardus pardalis', 
    country = 'CR', 
    hasCoordinate = TRUE, 
    hasGeospatialIssue = FALSE
)

## Consulta con lista de campos para desplegar
occ_search(
    scientificName = 'Leopardus pardalis', 
    country = 'CR', 
    hasCoordinate = TRUE, 
    hasGeospatialIssue = FALSE,
    fields = c('scientificName', 'decimalLongitude', 'decimalLatitude')
)

## Obtencion optima de registros, similar a occ_search, con coordenadas de Col
occ_data(scientificName = 'Leopardus pardalis', 
         hasCoordinate = TRUE, 
         limit = 20000, 
         decimalLongitude = "-76, -74", 
         decimalLatitude = "-3, 8")

# Mapeo de especies -------------------------------------------------------

## Consulta para retornar los puntos en el mapa del canguro
name <- name_backbone(name='Macropus giganteus', rank='species')
m <- mvt_fetch(taxonKey = paste(name[,("usageKey")]))

## Tipo de datos del objeto retornado
class(m)

## Mapeo de los puntos
plot(m$geometry, axes = TRUE, graticule = TRUE)

## Registros de presencia de Canguros en Australia
isocodes[grep("Australia", isocodes$name), "code"]
occ_count(taxonKey = name[,("usageKey")], 
          #georeferenced = TRUE
          country = "AU"
)
Macropus_giganteus_gbif <- 
    occ_search(
        scientificName = 'Macropus giganteus', 
        country = 'AU',
        hasCoordinate = TRUE, 
        hasGeospatialIssue = FALSE,
        fields = c('scientificName', 'decimalLongitude', 'decimalLatitude'),
        limit = 2500
    )

## Clase de los datos retornados por occ_search()
class(Macropus_giganteus_gbif)
class(Macropus_giganteus_gbif$data)
dim(Macropus_giganteus_gbif$data)

## Conversion a objeto sf
Macropus_giganteus_sf <- 
    st_as_sf(Macropus_giganteus_gbif$data, 
             coords = c("decimalLongitude", "decimalLatitude"), 
             crs = 4326
    )

## Capa de altitud
altitude <- getData("worldclim", var="alt", res=10)

## Mapeo
plot(altitude, ext = extent(110, 160, -50, -10), reset = FALSE)
plot(Macropus_giganteus_sf, col = 'black', pch = 20, cex = 0.6, add=TRUE)

# Modelos de presencia-ausencia ------------------------------------------

rm(list = ls())

## Datos de distribucion de la trucha arcoiris
name_backbone(name='Oncorhynchus mykiss', rank='species')
occ_count(taxonKey = 5204019, country = "CO"
          #georeferenced = TRUE
)
trucha <- occ_data(scientificName = "Oncorhynchus mykiss", 
                   limit = 2000,
                   hasCoordinate = TRUE, 
                   hasGeospatialIssue = FALSE)
## Extraer la tabla de datos del objeto GBIF
trucha <- trucha[[2]]

## Mapamundi en shape
wrld <- rnaturalearthdata::countries50
wrld <- st_as_sf(wrld)
wrld <- dplyr::select(wrld, name, region_un, subregion, continent, su_a3)

## Plotting 
plot(st_geometry(wrld))
points(trucha$decimalLongitude, trucha$decimalLatitude, 
       pch = 16, cex = 0.4, col = "royalblue3")

## Subset para Suramerica
came <- filter(wrld, subregion == 'South America')
cntr <- unique(came$name)
trucha_SA <- filter(trucha, country %in% cntr)

## Simple plot --- 
plot(st_geometry(came))
points(trucha_SA$decimalLongitude, trucha_SA$decimalLatitude, 
       pch = 16, cex = 0.4, col = 'red')

## Mapa de altitudes
altitude <- getData("worldclim", var="alt", res=10)
trucha_SA_sf <- 
    st_as_sf(trucha, 
             coords = c("decimalLongitude", "decimalLatitude"), 
             crs = 4326
    )
## Mapeo
plot(altitude, ext = extent(-110, -30, -80, 20), reset = FALSE)
plot(trucha_SA_sf, col = 'black', pch = 20, cex = 0.6, add=TRUE)

## Filtrado para Colombia
#Trucha_Col <- filter(trucha_SA, country == 'Colombia')
Trucha_Col <- occ_data(scientificName = "Oncorhynchus mykiss", 
                   limit = 2000,
                   country = "CO",
                   hasCoordinate = TRUE, 
                   hasGeospatialIssue = FALSE)[[2]]
Col0 <- geodata::gadm(country = 'COL', level = 0, path = '../tmpr')
Colombia <- geodata::gadm(country = 'COL', level = 1, path = '../tmpr')
plot(Colombia)
points(Trucha_Col$decimalLongitude, Trucha_Col$decimalLatitude, 
       pch = 16, cex = 0.4, col = 'red')

## ggplot2 mapa

gmap <- ggplot() + 
    geom_sf(data = st_as_sf(Col0), fill = NA, col = 'grey40') + 
    geom_sf(data = st_as_sf(Colombia), fill = NA, col = 'grey50') +
    geom_sf(data = st_as_sf(wrld), fill = NA, col = 'grey60') +
    geom_point(data = Trucha_Col, 
               aes(x = decimalLongitude, y = decimalLatitude), 
               col = 'brown', size = 2) +
    coord_sf(xlim = ext(Col0)[1:2], ylim = ext(Col0)[3:4]) + 
    labs(x = 'Lon', y = 'Lat', fill = '') +
    ggtitle(label = 'Ubiación de la especie', 
            subtitle = 'Oncorhynchus mykiss') +
    theme_ipsum_es() + 
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(face = 'italic', hjust = 0.5)) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                           style = north_arrow_fancy_orienteering())
gmap

# Localization map
zone <- st_as_sfc(st_bbox(st_as_sf(Col0)))
extn <- extent(as(Col0, 'Spatial'))

gglb <- ggplot() +
    geom_sf(data = st_as_sf(Col0), fill = NA, col = 'grey40') + 
    geom_sf(data = wrld, fill = NA, col = 'grey40') + 
    geom_sf(data = zone, fill = NA, color = 'red', size = 0.5) +
    coord_sf(xlim = c(-82, -66), ylim = c(-6, 18)) + 
    theme_bw() +
    theme(axis.text.y = element_blank(), 
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.spacing = unit(c(0,0,0,0), "cm"),
          plot.margin = unit(c(0,0,0,0), "cm"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none", 
          panel.border = element_rect( color = "grey20", fill = NA, size = 0.4)) +
    labs(x = '', y = '')

gall <- gmap +
    annotation_custom(ggplotGrob(gglb), xmin = -81, xmax = -78, 
                      ymin = -4, ymax = 0)
gall

## Guardar el mapa
ggsave(plot = gall, filename = 'trucha_localization.png', 
       units = 'in', width = 9, height = 7, dpi = 300)

