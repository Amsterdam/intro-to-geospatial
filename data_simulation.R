
#### intro ---------------------------------------------------------------------

# baseer op CBS data van bovenregio's
# 1 regio (A) waar het begon (met de meeste grenzen): exponentiële groei vanaf punt 1, 4 generaties
# punt 1 is punt met hoogste population density + 50 km in random direction 
# de 4 generaties met spatial correlation met afstand 0.5 km. check visually if
# it crosses border to at least 1 other region (regio B) . 
# Ook 2 geïmporteerde cases in regio C, vanwege familiebezoek.

# stappenplan simulatie
# 1) Kies een bovenregio
# 2) Bepaal regio A
# 3) Bepaal regio B & C
# 4) Simuleer besmettingen
# 5) geïmporteerde gevallen toevoegen

#### load data -----------------------------------------------------------------

# check of alle R packages zijn geïnstalleerd en laadt ze vanuit ander script
source("./packages.R")

## polygon data: PDOK gemeente en GGD grenzen
# Haal de kaart met gemeentegrenzen op van PDOK
api <- "https://api.pdok.nl/cbs/gebiedsindelingen/ogc/v1"

ggd_url <- paste0(
  api,
  "/collections/ggdregio_gegeneraliseerd/items",
  "?f=json&jaarcode=2025&limit=1000"
)

ggd_grenzen <- sf::st_read(ggd_url, quiet = TRUE)

# GGD bovenregio definities
Noord <- c("GGD Groningen", "GGD Fryslân", "GGD Drenthe")

Midden_Oost <- c("GGD IJsselland", "GGD Twente", "GGD Noord- en Oost-Gelderland",
                 "Veiligheids- en Gezondheidsregio Gelderland-Midden", 
                 "GGD Gelderland-Zuid", "GGD Regio Utrecht")

Noordwest <- c("GGD Hollands-Noorden", "GGD Zaanstreek/Waterland", "GGD Kennemerland",
               "GGD Amsterdam", "GGD Gooi en Vechtstreek", "GGD Flevoland")

Zuid_Holland <- c("GGD Haaglanden", "GGD Hollands-Midden", "GGD Rotterdam-Rijnmond",
                  "Dienst Gezondheid & Jeugd ZHZ")

ZeeBraLim <- c("GGD Zeeland", "GGD West-Brabant", "GGD Hart voor Brabant", "GGD Brabant-Zuidoost",
               "GGD Limburg-Noord", "GGD Zuid-Limburg")

bovenregios <- list(
  Noord = Noord,
  Midden_Oost = Midden_Oost,
  Noordwest = Noordwest,
  Zuid_Holland = Zuid_Holland,
  ZeeBraLim = ZeeBraLim
)

## raster data: populatie dichtheid
# gebruik terra package
pop_raster <- terra::rast("./data/nld_ppp_2020.tif")

#### simulation ----------------------------------------------------------------

set.seed(5432)

# 1) Kies een bovenregio
regios <- unlist(bovenregios[5]) # pas nummer aan van 1 tot 5 om data te genereren

# Selecteer de GGD-regio's binnen de gekozen bovenregio
regios_sf <- ggd_grenzen %>%
  filter(statnaam %in% regios) %>%
  st_transform(28992)

# 2) Bepaal regio A
# Zorg dat het raster in EPSG:28992 staat
pop_raster_rd <- terra::project(pop_raster, "EPSG:28992")

# crop raster, kies cel met hoogste pop_density, coordinaten centroid
# Combineer de GGD-regio's tot één bovenregio
bovenregio_sf <- regios_sf %>%
  summarise(geometry = st_union(geometry))

# Knip het raster op de bovenregio
pop_bovenregio <- terra::crop(
  pop_raster_rd,
  terra::vect(bovenregio_sf)
)

pop_bovenregio <- terra::mask(
  pop_bovenregio,
  terra::vect(bovenregio_sf)
)

# Uitzondering voor ZeeBraLim
if ("GGD West-Brabant" %in% regios_sf$statnaam) {
  
  # Gebruik alleen GGD West-Brabant als startgebied
  startgebied_sf <- regios_sf %>%
    filter(statnaam == "GGD West-Brabant")
  
  # Crop en mask het raster op West-Brabant
  start_gebied_pop <- terra::crop(
    pop_raster_rd,
    terra::vect(startgebied_sf)
  )
  
  start_gebied_pop <- terra::mask(
    start_gebied_pop,
    terra::vect(startgebied_sf)
  )
  
} else {
  
  # Gebruik de volledige bovenregio
  startgebied_sf <- regios_sf %>%
    summarise(geometry = st_union(geometry))
  
  start_gebied_pop <- terra::crop(
    pop_raster_rd,
    terra::vect(startgebied_sf)
  )
  
  start_gebied_pop <- terra::mask(
    start_gebied_pop,
    terra::vect(startgebied_sf)
  )
}

# Kies de rastercel met de hoogste bevolkingsdichtheid
max_cell <- which.max(terra::values(start_gebied_pop)[, 1])

# Coördinaten van het middelpunt van de rastercel
start_xy <- terra::xyFromCell(start_gebied_pop, max_cell)
# Kies een willekeurige richting
richting <- runif(1, 0, 2 * pi)

# Verplaats het punt 15 km
start_xy_new <- c(
  start_xy[1] + 1500 * cos(richting),
  start_xy[2] + 1500 * sin(richting)
)

# Maak een sf-punt
start_punt <- st_sfc(
  st_point(start_xy_new),
  crs = 28992
) %>%
  st_sf()

# visualisatie check
library(tidyterra)

ggplot() +
  geom_spatraster(data = pop_bovenregio) +
  geom_sf(
    data = regios_sf,
    fill = NA,
    colour = "white",
    linewidth = 0.4
  ) +
  geom_sf(
    data = start_punt,
    colour = "red",
    size = 3
  ) +
  scale_fill_viridis_c(name = "Bevolkingsdichtheid") +
  labs(title = "Startpunt en bevolkingsdichtheid") +
  coord_sf(crs = st_crs(28992)) +
  theme_minimal()

# Bepaal in welke GGD-regio het punt ligt
locatie_A <- st_join(
  start_punt,
  regios_sf,
  join = st_within,
  left = TRUE
)

# Selecteer regio A
regio_A <- regios_sf %>%
  filter(statnaam == locatie_A$statnaam[1])

# Controleer of het punt binnen de gekozen bovenregio ligt
st_within(start_punt, bovenregio_sf, sparse = FALSE)

# 3) Bepaal regio B & C

# Zoek aangrenzende regio's van A (rijindices)
buren <- st_touches(regio_A, regios_sf)[[1]]

# Bereken afstand van start_punt tot elke aangrenzende regio
afstanden <- st_distance(
  start_punt,
  regios_sf[buren, ]
)

# Regio B = aangrenzende regio die het dichtst bij start_punt ligt
index_B <- buren[which.min(afstanden[1, ])]
regio_B <- regios_sf[index_B, ]

# Regio C = willekeurige andere regio uit regios_sf
overige_regios <- setdiff(
  seq_len(nrow(regios_sf)),
  c(which(regios_sf$statnaam == regio_A$statnaam[1]), index_B)
)

index_C <- overige_regios[sample(seq_along(overige_regios), 1)]
regio_C <- regios_sf[index_C, ]

# Toon de gekozen regio's
regio_B$statnaam
regio_C$statnaam

# 4) Simuleer besmettingen in regio A en B

# Gemiddeld aantal nieuwe besmettingen per case
# Normale verdeling met gemiddelde 4.5 en sd 1.5
# functie voor sampling
aantal_besmettingen <- function() {
  max(1, min(8, round(rnorm(1, mean = 4.5, sd = 1.5))))
}

# Generatie 0: eerste besmetting
gevallen <- data.frame(
  generatie = 0,
  x = st_coordinates(start_punt)[1, 1],
  y = st_coordinates(start_punt)[1, 2]
)

# Simuleer nieuwe besmettingen rond één besmetting
simuleer_nieuwe_gevallen <- function(x, y) {
  
  # Aantal nieuwe besmettingen
  n <- aantal_besmettingen()
  
  # Middelpunt van regio B
  centrum_B <- st_coordinates(st_centroid(regio_B))[1, ]
  
  # Voorkeursrichting naar regio B
  richting_B <- atan2(
    centrum_B[2] - y,
    centrum_B[1] - x
  )
  
  # Willekeurige afwijking van de voorkeursrichting
  richting <- richting_B + rnorm(n, mean = 0, sd = pi / 4)
  
  # Afstand vanaf de besmetter (in meters)
  afstand <- rgamma(n, shape = 2, scale = 1500)
  
  # Nieuwe coördinaten
  nieuwe_x <- x + afstand * cos(richting)
  nieuwe_y <- y + afstand * sin(richting)
  
  data.frame(
    x = nieuwe_x,
    y = nieuwe_y
  )
}

# Start met generatie 0
alle_gevallen <- gevallen

for (g in 1:3) {
  
  # Selecteer de vorige generatie
  vorige_generatie <- alle_gevallen %>%
    filter(generatie == g - 1)
  
  # Simuleer nieuwe besmettingen
  nieuwe_gevallen <- bind_rows(
    lapply(seq_len(nrow(vorige_generatie)), function(i) {
      simuleer_nieuwe_gevallen(
        vorige_generatie$x[i],
        vorige_generatie$y[i]
      )
    })
  )
  
  # Ken de nieuwe generatie toe
  nieuwe_gevallen$generatie <- g
  
  # Voeg toe aan alle gevallen
  alle_gevallen <- bind_rows(
    alle_gevallen,
    nieuwe_gevallen
  )
}

# 5) geïmporteerde gevallen toevoegen voor regio C
# op een plek waar wel mensen wonen

# Crop en mask het raster op regio C
pop_C <- terra::crop(pop_raster_rd, terra::vect(regio_C))
pop_C <- terra::mask(pop_C, terra::vect(regio_C))

# Zoek rastercellen met populatiedichtheid < 5
waarden <- terra::values(pop_C)[, 1]
geschikte_cellen <- which(!is.na(waarden) & waarden > 5)

# Kies willekeurig één geschikte cel
gekozen_cel <- sample(geschikte_cellen, 1)

# Coördinaten van de gekozen cel
cluster_xy <- terra::xyFromCell(pop_C, gekozen_cel)

# Drie locaties in het cluster
cluster_C <- data.frame(
  x = rnorm(3, mean = cluster_xy[1], sd = 150),
  y = rnorm(3, mean = cluster_xy[2], sd = 150),
  generatie = 2
)

# Voeg cluster toe aan bestaande gevallen
alle_gevallen <- bind_rows(
  alle_gevallen,
  cluster_C
)

# Omzetten naar sf
mazelen_sf <- st_as_sf(
  alle_gevallen,
  coords = c("x", "y"),
  crs = 28992
)

# Maak de kaart
library(leaflet)

# Transformeer naar WGS84
regios_leaflet <- st_transform(regios_sf, 4326)
mazelen_leaflet <- st_transform(mazelen_sf, 4326)

# Interactieve kaart
leaflet() %>%
  addPolygons(
    data = regios_leaflet,
    fill = FALSE,
    color = "black",
    weight = 1,
    opacity = 0.8
  ) %>%
  addCircleMarkers(
    data = mazelen_leaflet,
    radius = 4,
    color = ~c("#440154", "#31688e", "#35b779", "#fde725")[generatie + 1],
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste("Generatie:", generatie)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#440154", "#31688e", "#35b779", "#fde725"),
    labels = c("Generatie 0", "Generatie 1", "Generatie 2", "Generatie 3"),
    title = "Besmettingsgeneratie"
  )

st_write(
  mazelen_sf,
  "./data/sim_mazelen_zomer_ZBL.gpkg",
  delete_dsn = TRUE
)
