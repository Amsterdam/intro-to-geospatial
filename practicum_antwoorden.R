
#### intro ---------------------------------------------------------------------

# auteur:   Dr. Remy Hoek Spaans
# datum:    02/11/2026
# context:  Deze practica zijn onderdeel van de epi netwerkdag 3 nov 2026
# doel:     Kennis maken met geo-data - en analyse + toepassingen in volksgezondheid

# Scenario:

# In de zomervakantie zijn een paar ongevaccineerde kinderen teruggekeerd
# van vakantie en hebben mazelen opgelopen. Ze hebben met hun vriendjes en 
# vriendinnetjes in de buurt gespeeld en soms bij familie gelogeerd. 
# De uitbraak verspreid zich langzaam over de boven-regio. 

# Jij bent als epidemioloog gevraagd de mazelen gevallen van de hele boven-regio
# te visualiseren en het risico van spill-over naar andere regio's te beoordelen.
# De scholen gaan ook weer bijna open. Het team wil weten in welke scholen extra
# gevaccineerd moet worden. 

#### PRACTICUM 1 ---------------------------------------------------------------

# leerdoel:  Verschillende datatypes laden en visualiseren in 1 kaartje

#### opdracht 1: data verkennen ------------------------------------------------

# NOTA BENE: geografische data is wat "zwaarder" dan een dataframe of tibble
# hierdoor moet je soms even geduld hebben terwijl R je data laadt, vooral
# als je veel datapunten hebt en/of een groot gebied inlaadt. 
# Het vergt ook wat van je R geheugen 
# (zie kleine ronde schijfje rechtsboven bij je Environment)
# Je kunt gc() runnen om "op te ruimen" en sluit je browser tabs / andere programma's!
# Gelukkig wonen we in een klein kikkerlandje :)

# check of alle R packages zijn geïnstalleerd en laadt ze vanuit ander script
source("./packages.R")
library(leaflet)

# data laden
## point data: gesimuleerde mazelen gevallen per bovenregio
# gebruik sf package
mazelen <- st_read("./data/sim_mazelen_zomer_NW.gpkg") # kies hier jouw dataset
# we voegen een extra kolom toe die aantal gevallen per locatie/huishouden is
set.seed(472)
mazelen$cluster_grootte <- sample(
  1:3,
  size = nrow(mazelen),
  replace = TRUE,
  prob = c(0.5, 0.3, 0.2)
)
# en wat nep huishoudgegevens toe zodat je wat keuze hebt om te plotten
# voel je vrij dit aan te passen

# Aantal kinderen per huishouden (minimaal 1, maximaal 6)
mazelen$aantal_kinderen <- sample(
  1:6,
  size = nrow(mazelen),
  replace = TRUE,
  prob = c(0.15, 0.30, 0.25, 0.15, 0.10, 0.05)
)

# Reisgeschiedenis huishouden (vooral geen, enkele USA)
mazelen$reisgeschiedenis <- sample(
  c("geen", "USA", "Belgie", "Duitsland", "Singapore"),
  size = nrow(mazelen),
  replace = TRUE,
  prob = c(0.80, 0.12, 0.04, 0.03, 0.01)
)

## polygon data: PDOK gemeente en GGD grenzen
# Haal de kaart met gemeentegrenzen op van PDOK
api <- "https://api.pdok.nl/cbs/gebiedsindelingen/ogc/v1"

collecties <- fromJSON(
  paste0(api, "/collections?f=json")
)

collectie_tabel <- collecties$collections

gemeente_url <- paste0(
  api,
  "/collections/gemeente_niet_gegeneraliseerd/items", 
  "?f=json&jaarcode=2025&limit=1000" # jaar 2025 omdat RIVM data van 2025 is
)

ggd_url <- paste0(
  api,
  "/collections/ggdregio_gegeneraliseerd/items",
  "?f=json&jaarcode=2025&limit=1000"
)
  
gemeentegrenzen_2025 <- sf::st_read(gemeente_url, quiet = TRUE)
ggd_grenzen <- sf::st_read(ggd_url, quiet = TRUE)

## raster data: populatie dichtheid
# gebruik terra package
pop_raster <- terra::rast("./data/nld_ppp_2020.tif")

## RIVM vaccinatiegraad
# via CBS
# check metadata zodat je weet wat je wilt opvragen i.p.v. de hele dataset laden
vaccinaties.meta <- cbs_get_meta("50141NED", catalog = "RIVM")
# in ons geval zijn we geïnteresseerd in BMR onder schoolkinderen
vac_codes <- vaccinaties.meta$Vaccinaties

bmr_10y_2025 <- cbs_get_data(
  "50141NED",
  catalog = "RIVM",
  Perioden = "2025JJ00", # laad alleen meest recente data, meetjaar 2025
  Vaccinaties = "A028779", # kies juiste vaccinatiecode uit tabel
  typed = TRUE
)

#### opdracht 2: data verkennen ------------------------------------------------

# verken je data
# je kunt b.v. base R, ggplot, tmap of leaflet gebruiken om geodata te visualiseren
# in de de eerste instantie bekijken we elke data los van elkaar, later combineren we ze

# Als je niet zeker weet welk package je moet gebruiken, kun je altijd
# een snelle, simpele visualisatie doen met base R
plot(pop_raster)
plot(ggd_grenzen)
plot(gemeentegrenzen_2025)
plot(mazelen)
plot(bmr_10y_2025)
# oeps! het lijkt erop dat onze BMR vaccinatiegraad data nog niet gevisualiseerd 
# kan worden als kaartlaag... laten we eens kijken hoe dat komt.

# gebruik de class() functie om meer informatie te krijgen over het data type
class(pop_raster)
class(ggd_grenzen)
class(mazelen)
class(bmr_10y_2025)
# je ziet nu dat onze BMR data nog geen geografische data gelinkt heeft
# dit moeten we zometeen dus zelf gaan doen (practicum 2)
# laten we eerst verder gaan met de data die we wel hebben!

# inspecteer de data... waar staan de coordinaten opgeslagen?
# gebruik: str() en/of view()

# voordat je deze data op dezelfde kaart gaat plotten, check of ze wel hetzelfde
# coördinate reference system (crs) gebruiken. 
# Hint: gebruik terra::crs() of sf::st_crs()
# Hint: gebruik X <- sf::set_crs() en X <- terra::crs() om ontbrekende crs toe te wijzen
st_crs(pop_raster)
st_crs(ggd_grenzen)
st_crs(gemeentegrenzen_2025)
st_crs(mazelen)
# gebruikt elke dataset dezelfde crs?

# bij rasters kun je ook de resolutie checken
terra::res(pop_raster)
# gebaseerd op wat je weet van het CRS, welke eenheid staat dit in?

# met ggplot of tmap heb je wat meer controle over de visualisatie
# met leaflet (proberen we later) heb je ingebouwde achtergrondkaarten
# en kun je interactieve elementen toevoegen + opslaan als html widget
# we doen eerst 1 laag tegelijk

# voor punt en polygon data kun je geom_sf() gebruiken

# polygon data
ggplot() +
  geom_sf(data = ggd_grenzen) +
  labs(title = "GGD-regio's") +
  theme_minimal()

# punt data
ggplot() +
  geom_sf(data = mazelen) +
  labs(title = "Mazelen") +
  theme_minimal()

# laten we nu de visualisatie wat uitbreiden
# voorbeeld: kleuren gebaseerd op naam van de GGD
ggplot(ggd_grenzen) +
  geom_sf(aes(fill = statnaam),
          colour = "white",
          linewidth = 0.3) +
  labs(title = "GGD regions",
       fill = "GGD region") +
  theme_minimal()

# voorbeeld: grootte van punt gebaseerd op aantal cases
ggplot() +
  geom_sf(
    data = mazelen,
    aes(size = cluster_grootte),
    colour = "red",
    alpha = 0.6,
    shape = 16
  ) +
  scale_size_continuous(
    range = c(2, 8),
    breaks = 1:3,
    name = "Aantal cases"
  ) +
  labs(title = "Mazelen gevallen per locatie") +
  theme_minimal()

# meer voorbeelden hoe je de opmaak kunt aanpassen:
# https://r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

# voorbeelden in tmap
# https://www.epirhandbook.com/en/new_pages/gis.html

# ggplot werkt nog niet 100% vlekkeloos met rasters via ingebouwde syntax
# er zijn een paar opties:
# 1: tidyterra package gebruiken, dat integreerd terra met tidyverse
# 2: je raster omzetten naar een dataframe en dat plotten in ggplot
# dan kun je dus geen berekeningen etc. meer doen, alleen voor visualisatie
# 3: stars package heeft geom_stars(), wat wel geïntegreerd is met ggplot
# 4: gebruik tmap of leaflet voor de visualisatie, die werken wel met rasters

# voor dit practicum gebruiken we optie 1, om het aantal packages te beperken
install.packages("tidyterra")
library(tidyterra)
ggplot() +
  tidyterra::geom_spatraster(data = pop_raster) +
  labs(title = "Bevolkingsdichtheid") +
  theme_minimal()

#### opdracht 2: data bewerken -------------------------------------------------

# Nu onze vaccinatiegraad data nog...
# Wat we wel hebben:
head(bmr_10y_2025$RegioS)
# meer informatie vind je in de metadata
cbs_regios <- vaccinaties.meta$RegioS
# bekijk de tabel.. via de Key kun je dus dat van RegioS verbinden met de vaccinatiedata
# maar ook met bijvoorbeeld de gemeentegrenzen, deze data komt immers ook van
# het CBS en zij gebruiken dezelfde logica hierin
head(gemeentegrenzen_2025$statcode)
head(ggd_grenzen$statcode)

# we kunnen dit dus gebruiken om vaccinatiegraad data aan onze geografische
# data te koppelen
# in dit geval willen we vaccinatiegraad koppelen aan gemeente niveau
gemeente_vg_25 <- gemeentegrenzen_2025 %>% 
                    left_join(bmr_10y_2025, by = c("statcode" = "RegioS"))

# ik gebruik meestal leftjoin en dan het sf object eerst, dan behoud je class sf
class(gemeente_vg_25)

# misschien heb jij van het RIVM ook vaccinatiegraad data op PC4 niveau ontvangen
# daar zit geen metadat bij, maar hier vind je een koppeltabel:
# https://www.cbs.nl/nl-nl/maatwerk/2026/38/buurt-wijk-en-gemeente-2026-voor-postcode-huisnummer
# en de geodata:
# https://www.pdok.nl/introductie/-/article/cbs-postcode4

# Je wilt graag alle data op 1 kaart zetten voor jouw bovenregio. 
# Later combineren we de data in ggplot (statisch) of leaflet (interactief)
# We moeten eerst wat checks en aanpassingen doen in onze data voordat dat kan.
# Om tijd te besparen hebben we de bovenregio's al gedefinieerd voor je:

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

# Je zult eerst de data moeten filteren voor jouw bovenregio
GGD_NW <- ggd_grenzen %>% 
                filter(statnaam %in% Noordwest)

# Nu willen we ook de gemeentegrenzen filteren ... maar we hebben geen zin om 
# weer alles uit te schrijven. Hier kunnen we het sf package voor gebruiken
# We gaan eerst alle GGD regio's samenvoegen tot 1 polygon met st_union
NW_bovenregio <- GGD_NW %>% 
  st_union() %>% 
  st_as_sf()

# visualiseer wat je gedaan hebt:
plot(GGD_NW)
plot(NW_bovenregio)
# Let op: stel je had numerieke data gelinkt aan deze polygons, dan had je
# die of moeten laten vallen, of aggregeren, afhankelijk van je doel.

# daarna kunnen we kijken welke gemeentes binnen dit polygon vallen
# Selecteer alleen gemeenten die binnen NW_bovenregio vallen
gemeente_NW1 <- gemeente_vg_25 %>%
  st_filter(NW_bovenregio, .predicate = st_intersects)

gemeente_NW2 <- gemeente_vg_25 %>%
  st_filter(NW_bovenregio, .predicate = st_within)

# Visualiseer het resultaat
plot(st_geometry(gemeente_NW1))

# eigenlijk is het preciezer om gewoon te filteren op basis van koppeltabel
# https://www.cbs.nl/nl-nl/cijfers/detail/86059NED

cbs_koppel_regios <- read_csv2(
  "./data/86059NED-202509300000/Observations.csv",
  show_col_types = FALSE
)

# filter om wat meer duidelijkheid te krijgen
ggd_data <- cbs_koppel_regios %>%
  filter(grepl("GG", Measure))

# vul hier je bovenregio naar keuze in
koppel_GGDNW <- ggd_data %>% filter(StringValue %in% Noordwest)

# filter nu je spatial gemeente data op basis van de koppeltabel
gemeente_br <- gemeente_vg_25 %>% 
                filter(statcode %in% koppel_GGDNW$RegioS)

# De raster data die we nu hebben is voor heel Nederland
# We willen alleen jouw boven-regio laten zien als achtergrond

# Mask raster zodat alleen cellen binnen Noordwest overblijven
pop_raster_NW <- terra::mask(pop_raster, NW_bovenregio)
# Visualiseer het resultaat
plot(pop_raster_NW)

pop_raster_NW <- terra::crop(pop_raster, NW_bovenregio)
# Visualiseer het resultaat
plot(pop_raster_NW)

# voor iets meer over rasters: https://emlab-ucsb.github.io/intro-terra/#Spatial_data

#### opdracht 3: data visualiseren in ggplot -----------------------------------

# Voordat je de data combineert moet alles wel in hetzelfde crs staan
# Hint: leaflet wil alles in WGS84, want de achtergrond kaart staat daarin
# Als je afstanden wilt meten gebruik projected, want eenheid = meters.

# Zodra je kaartlagen gaat combineren, maakt het wel uit in welke volgorde
# jij deze op de kaart zet. 
# Analogie: stel je hebt een tafelkleed (raster) met een puzzel erop (polygon) en je strooit
# er nog wat confetti (punt data) over. Je kunt natuurlijk nooit de confetti zien
# als je eerst de confetti strooit en daarna het kleed eroverheen legt. 

# optie 1: gebruik bevolkingsdichtheid als achtergrond
ggplot() +
  tidyterra::geom_spatraster(data = pop_raster_NW) +
  theme_minimal() +
  geom_sf(
    data = gemeente_NW1,
    fill = NA,
    colour = "lightgrey",
    linewidth = 0.3,
  ) +
  geom_sf(
    data = GGD_NW,
    fill = NA,
    colour = "white",
    linewidth = 1
  ) +
  geom_sf(
    data = mazelen,
    aes(size = cluster_grootte),
    colour = "red",
    alpha = 0.6,
    shape = 16
  ) +
  scale_size_continuous(
    range = c(2, 4),
    breaks = 1:3,
    name = "Aantal cases"
  )

# optie 2: gebruik aantal ongevaccineerden als achtergrond

# bereken eerst ongevaccineerden
gemeente_br$ongevac <- gemeente_br$Populatie_1 - gemeente_br$GevaccineerdenZonderLeeftijdsgrens_4

ggplot(data = gemeente_br) +
  geom_sf(aes(fill = ongevac), colour = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours = c(
      "#FEE08B", "#FDAE61",
      "#F46D43", "#D73027", "#A50026"
    ),
    name = "Aantal ongevaccineerden",
    na.value = "grey80"
  ) +
  labs(
    title = "BMR-vaccinatiegraad schoolkinderen",
    subtitle = "Geschat aantal ongevaccineerden - per gemeente in 2025"
  ) +
  theme_minimal()

#### opdracht 4: data visualiseren in leaflet ----------------------------------

# Leaflet gebruikt WGS84 (EPSG:4326)
# Projecteer naar WGS84 indien nodig
mazelen_leaflet  <- st_transform(mazelen, 4326)

# optie 1: 1 statische kaart met achtergrond (zonder aantal gevaccineerden)
leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  
  # Gemeentegrenzen
  addPolygons(
    data = gemeente_br,
    fill = FALSE,
    color = "lightgrey",
    weight = 1
  ) %>%
  
  # GGD-grenzen
  addPolygons(
    data = GGD_NW,
    fill = FALSE,
    color = "white",
    weight = 3
  ) %>%
  
  # Mazelenclusters
  addCircleMarkers(
    data = mazelen_leaflet,
    radius = ~cluster_grootte * 2,
    color = "red",
    fillColor = "red",
    fillOpacity = 0.6,
    stroke = FALSE,
    popup = ~paste0(
      "Aantal cases: ", cluster_grootte
    )
  )

# optie 2: met klikbare kaartlagen en meetlatje
# Kleurenpalet voor aantal ongevaccineerden
pal <- colorNumeric(
  palette = c(
    "#FEE08B", "#FDAE61",
    "#F46D43", "#D73027", "#A50026"
  ),
  domain = gemeente_br$ongevac,
  na.color = "grey80"
)
# voor meer info over paletten: 
# https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
# https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

kaart2 <- leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  
  # Bevolkingsraster
  addRasterImage(
    pop_raster_NW,
    opacity = 0.6,
    group = "Bevolkingsraster",
  ) %>%
  
  # Aantal ongevaccineerden per gemeente
  addPolygons(
    data = gemeente_br,
    fillColor = ~pal(ongevac),
    fillOpacity = 0.75,
    color = "white",
    weight = 0.5,
    group = "Ongevaccineerden",
    popup = ~paste0(
      "Aantal ongevaccineerden: ",
      round(ongevac)
    )
  ) %>%
  
  # GGD-grenzen
  addPolygons(
    data = GGD_NW,
    fill = FALSE,
    color = "black",
    weight = 2,
    group = "GGD-grenzen"
  ) %>%
  
  # Mazelenclusters
  addCircleMarkers(
    data = mazelen_leaflet,
    radius = ~cluster_grootte * 2,
    color = "red",
    fillColor = "red",
    fillOpacity = 0.7,
    stroke = FALSE,
    group = "Mazelenclusters",
    popup = ~paste0(
      "Aantal cases: ", cluster_grootte
    )
  ) %>%
  
  # Legenda
  addLegend(
    position = "bottomright",
    pal = pal,
    values = gemeente_br$ongevac,
    title = "Aantal ongevaccineerden"
  ) %>%
  
  # Kaartlagen aan- en uitzetten
  addLayersControl(
    baseGroups = "Achtergrondkaart",
    overlayGroups = c(
      "Bevolkingsraster",
      "Ongevaccineerden",
      "Gemeentegrenzen",
      "GGD-grenzen",
      "Mazelenclusters"
    ),
    options = layersControlOptions(
      collapsed = FALSE
    )
  ) %>%
  
  # Meetlat (afstand en oppervlakte meten)
  addMeasure(
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "red",
    completedColor = "blue"
  ) %>%
  
  # Begin met het bevolkingsraster verborgen
  hideGroup("Bevolkingsraster")

kaart2

# opslaan als html widget - pas de naam aan

htmlwidgets::saveWidget(
  widget = kaart2,
  file = "mazelen_noordwest.html",
  selfcontained = TRUE
)

#### PRACTICUM 2 ---------------------------------------------------------------

# leerdoel: koppelen van geografische data met data uit andere bronnen

#### opdracht 1: gebruik pdok geoding service ----------------------------------

# we gaan een nieuwe databron toevoegen: locaties van basisscholen in NL
# deze is al gedownload en staat in de data map
scholen <- st_read("./data/02.-alle-schoolvestigingen-basisonderwijs.csv")

# deze data bevat geen geografische gegevens, maar wel adressen
# het zou ontzettend veel werk kosten om deze met de hand op te zoeken
# gelukkig heeft pdok een geocoding service

# we doen eerst 1 adres als voorbeeld, daarna zetten we dit in een functie
# voorbeeldje
adres <- "Neude 11 Utrecht"

response <- request(
  "https://api.pdok.nl/bzk/locatieserver/search/v3_1/free"
) |>
  req_url_query(
    q = adres,
    fl = "straatnaam,huisnummer,woonplaatsnaam,centroide_ll"
  ) |>
  req_perform()

data_met_coords <- resp_body_json(response)

# je ziet in de code hierboven dat we de coordinaten krijgen in "ll"
# ll = longitude, latitude
# centroide = geometrisch middelpunt van een ruimtelijk gebied

# inspecteer de data.. hoe krijg je hier de coordinaten uit?

# het valt je misschien op dat er meerdere matches zijn, dat klopt. 
# voor het gemak van dit practicum pakken we nu steeds de eerste match,
# maar het kan zijn dat er extra handmatige data cleaning stappen nodig zijn als
# er alleen een gedeeltelijke match is voor bepaalde adressen.

# laten we eerst een duidelijke adres kolom maken
names(scholen)
scholen <- scholen %>% 
            mutate(PC6 = gsub(" ", "", POSTCODE)) %>% 
            mutate(adres = paste0(STRAATNAAM, " ", HUISNUMMER.TOEVOEGING, ", ",
                                  PC6, " ", PLAATSNAAM))

# wij hebben een hele dataset, dus ik heb alvast een functie die je kunt gebruiken
# om een grotere hoeveelheid in 1 keer terug te krijgen van de API
get_address <- function(adres) {
  
  response <- request(
    "https://api.pdok.nl/bzk/locatieserver/search/v3_1/free"
  ) |>
    req_url_query(
      q = adres,
      fq = "type:adres",
      rows = 1,
      fl = "straatnaam, huisnummer, postcode, woonplaatsnaam, centroide_ll"
    ) |>
    req_perform()
  
  docs <- resp_body_json(response, simplifyVector = TRUE)$response$docs
  
  if (length(docs) == 0) {
    return(data.frame(
      adres_match = NA_character_,
      centroide_ll = NA_character_
    ))
  }
  
  data.frame(
    adres = adres,
    adres_match = paste(
      docs$straatnaam[1], " ",
      docs$huisnummer[1], ", ",
      docs$postcode[1], " ",
      docs$woonplaatsnaam[1]
    ),
    centroide_ll = docs$centroide_ll[1]
  )
}

# laten we eerst een kleine check doen om te kijken hoe de data eruit komt:
check <- bind_rows(lapply(scholen$adres[1:10], get_address))

# nu doen we het voor de hele dataset (duurt wat langer ,dus eerst filteren op regio)

# filter op jouw regio, gebruik gemeentenummer. 
# Eerst "GM" ervoor zetten voor CBS match
scholen$statcode <- paste0("GM", scholen$GEMEENTENUMMER)
gemeente_codes <- unique(gemeente_NW1$statcode)
scholen_regio <- scholen %>% 
                  filter(statcode %in% gemeente_codes)

# functie toepassen (even geduld, ongeveer 3 minuten)
school_locaties <- bind_rows(lapply(scholen_regio$adres, get_address))
class(school_locaties)

# nu hebben we de geometrie in een kolom, maar we moeten R nog laten weten
# welk datatype dit is, door het een sf object te maken 
# ook moeten we een crs toewijzen
school_sf <- school_locaties %>%
  st_as_sf(
    wkt = "centroide_ll",
    crs = 4326
  )

## nog een paar handige bewerkingen voor omzetten van normale naar geodata en vice versa
test <- school_sf[1:10, ]
# je kunt ook de coordinaten matrix uit de data halen door de st_coordinates functie:
st_coordinates(test)
# je kunt ook weer terug naar dataframe door:
st_drop_geometry(test)
# of
as.data.frame(test)

# laten we onze school locaties combineren met vaccinatiegraad data in een kaartje

leaflet() %>%
  addProviderTiles("Esri.WorldTopoMap") %>%
  
  # Gemeenten coloured by vaccination rate
  addPolygons(
    data = gemeente_NW1,
    fillColor = ~colorNumeric(
      palette = "viridis",
      domain = gemeente_NW1$VaccinatiegraadZonderLeeftijdsgrens_5
    )(VaccinatiegraadZonderLeeftijdsgrens_5),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~paste0(
      "<b>Gemeente:</b> ", statnaam,
      "<br><b>Vaccinatiegraad:</b> ",
      VaccinatiegraadZonderLeeftijdsgrens_5, "%"
    )
  ) %>%
  
  # School locations
  addCircleMarkers(
    data = school_sf,
    color = "red",
    radius = 4,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~adres
  ) %>%
  
  addLegend(
    position = "bottomright",
    pal = colorNumeric(
      palette = "viridis",
      domain = gemeente_NW1$VaccinatiegraadZonderLeeftijdsgrens_5
    ),
    values = gemeente_NW1$VaccinatiegraadZonderLeeftijdsgrens_5,
    title = "Vaccinatiegraad (%)",
    opacity = 0.7
  )

## so far so good
# wel is het lastig om de achtergrond kaart goed te zien met deze polygondata
# misschien handiger om de vaccinatiegraad data te koppelen aan 
# de schoollocaties.. hier gaan we volgend practicum mee verder

#### PRACTICUM 3 ---------------------------------------------------------------

# Leerdoel: paar simpele berekeningen kunnen uitvoeren om antwoord te geven
# op de originele vraag: waar moet gevaccineerd worden?

# laten we eerst onze vaccinatiegraad data van polygon naar punt data halen
# hier kun je st_join voor gebruiken
# LET OP: je moet wel zeker weten dat beide datasets hetzelfde CRS hebben
school_sf <- school_sf %>%
  st_join(
    gemeente_NW1 %>%
      select(statnaam, VaccinatiegraadZonderLeeftijdsgrens_5)
  )

# nu heb je een extra kolom in je school data

# geef de data weer in leaflet

# Nu kun je wat simpele berekeningen gaan doen!

# Hiervoor moeten we onze dat omzetten naar NL coordinaten stelsel
# Rijksdriehoekscoördinatenstelsel (EPSG:28992)

school_sf_R3 <- school_sf %>% st_transform(28992)

# Wat wordt jouw strategie voor vaccineren?

# Voor verdere ideeën zie ook:
# https://www.epirhandbook.com/en/new_pages/gis.html#spatial-statistics




