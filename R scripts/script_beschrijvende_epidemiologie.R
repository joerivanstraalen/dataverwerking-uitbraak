#script voor beschrijvende epidemiologie van uitbraakdata in HP Zone 
#de dummy data zijn gebaseerd op een HP Zone export van mazelen cases (Format Options: Excel, Output Options: All core values), niet alle kolommen zijn meegenomen

#benodigde packages laden
library(pacman)
pacman::p_load(readxl, tidyverse, lubridate, rlang, gtsummary, scales, leaflet, janitor, RColorBrewer, pdokr, sf)

#data inladen en opschonen
df <- read_excel("C:/Users/.../dummy_data_hp_zone.xlsx") #voer hier de locatie van het databestand in

df <- df %>%
  clean_names() %>%
  mutate_at(c("confidence", "status", "principal_contextual_setting", "municipality",
              "hospitalised", "vaccinated_in_respect_to_the_diagnosis", 
              "oorspronkelijke_bron_van_de_melding", "status_van_de_melding", "gender"), as.factor) %>% #verander variabelen naar type factor
  mutate_at(c("age_in_years_at_date_of_onset", "age_in_months_if_a_baby"), as.numeric) %>% #verander variabelen naar type numeric
  mutate_at(c("date_of_onset"), list(ymd)) #verander variabelen naar type Date

df <- df %>%
  mutate(gender = fct_recode(gender, "Vrouw" = "Female", "Man" = "Male", "Onbekend" = "Unknown"),
         hospitalised = fct_recode(hospitalised, "Nee" = "No", "Ja" = "Yes", "Onbekend" = "Not known")) #factor levels hernoemen

cases <- df %>%
  filter(confidence == "Probable" |
           confidence == "Confirmed") #filter op probable en confirmed cases

#tabel met karakteristieken van cases
variabelen <- c("gender", "age_in_years_at_date_of_onset",
                "principal_contextual_setting", "hospitalised", 
                "vaccinated_in_respect_to_the_diagnosis",
                "oorspronkelijke_bron_van_de_melding", 
                "municipality") #selecteer alle kolommen met variabelen om weer te geven in een tabel

tabel <- cases %>%
  select(all_of(variabelen)) %>%
  mutate(across(where(is.factor), fct_drop)) %>% #factor categorieën met nul observaties niet laten zien in de tabel
  tbl_summary(label = list(
    gender ~ "Geslacht",
    hospitalised ~ "Ziekenhuisopname",
    vaccinated_in_respect_to_the_diagnosis ~ "Vaccinatiestatus",
    principal_contextual_setting ~ "Setting besmetting opgelopen",
    oorspronkelijke_bron_van_de_melding ~ "Melder",
    age_in_years_at_date_of_onset ~ "Leeftijd",
    municipality ~ "Gemeente"),
    missing = "no") %>%
  add_n() %>%
  bold_labels() %>%
  modify_header(label = "**Variabele**") %>%
  modify_caption("**Tabel 1. Karakteristieken van cases.**")

print(tabel) #tabel weergeven

#aantal cases over de tijd (epicurve)
epicurve <- cases %>%
  ggplot(aes(x = date_of_onset, #selecteer datum variabele
             fill = vaccinated_in_respect_to_the_diagnosis)) + #aparte kleuren voor vaccinatiestatus
  geom_bar(col = "black") +
  theme_classic(base_size = 14) +
  coord_cartesian(expand = FALSE) +
  labs(y = "Aantal cases", x = "Eerste ziektedag", fill = "Gevaccineerd") + 
  scale_fill_brewer(palette = "Dark2")

plot(epicurve) #epicurve plotten

#geografische kaart van cases

geo_results <- pdok_geocode(query = cases$postcode) #coördinaten van postcodes verkrijgen via de PDOK locatieserver
geo_results <- st_transform(geo_results, crs = 4326) #transformeer coördinaten naar WGS84
geo_results <- st_centroid(geo_results) #van elk resultaat de centrale coördinaat te pakken

coordinates <- st_coordinates(geo_results) #extraheer latitude en longitude

#voeg de coördinaten samen met geo_results en schoon de data op
geo_results <- geo_results %>%
  bind_cols(as.data.frame(coordinates)) %>%
  rename(longitude = X, latitude = Y) %>%
  select(query, latitude, longitude) %>% #behoud alleen de relevante kolommen
  st_drop_geometry() #verwijder de sf-geometrie kolom om een gewone dataframe te krijgen

rm(coordinates)

#koppel de coördinaten aan de originele dataset
cases <- cases %>%
  left_join(geo_results, by = c("postcode" = "query"))

rm(geo_results)

#geografische kaart maken
map <- leaflet()%>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>% 
  addCircles(data = cases,
             ~longitude, 
             ~latitude, 
             radius = 100, 
             color = "red", 
             fillOpacity = 0.4) %>% 
  addLegend(position = "bottomright",
            colors = "red",
            labels = "Casus met 100m radius",
            group = "circles",
            title = "Kaart van cases") #kaart plotten

print(map) #kaart weergeven
