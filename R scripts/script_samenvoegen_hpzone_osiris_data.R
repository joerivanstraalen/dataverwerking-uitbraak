#script voor het samenvoegen van data uit HP Zone en Osiris 

#de dummy data zijn gebaseerd op een HP Zone en Osiris export van mazelen cases, niet alle kolommen zijn meegenomen
#HP Zone export --> Format Options: Excel, Output Options: All core values
#Osiris export --> tabblad "Exporteren", Export gecodeerde antwoorden: Alleen omschrijving

#benodigde packages laden
library(pacman)
pacman::p_load(readxl, tidyverse, lubridate, janitor)

#HP Zone data inladen en opschonen
hp_zone <- read_excel("C:/Users/.../dummy_data_hp_zone.xlsx") #voer hier de locatie van het databestand in

hp_zone <- hp_zone %>%
  clean_names() %>%
  mutate_at(c("confidence", "status", "principal_contextual_setting", "municipality",
              "hospitalised", "vaccinated_in_respect_to_the_diagnosis", 
              "oorspronkelijke_bron_van_de_melding", "status_van_de_melding", "gender"), as.factor) %>% #verander variabelen naar type factor
  mutate_at(c("age_in_years_at_date_of_onset", "age_in_months_if_a_baby"), as.numeric) %>% #verander variabelen naar type numeric
  mutate_at(c("date_of_onset"), list(ymd)) #verander variabelen naar type Date

hp_zone <- hp_zone %>%
  mutate(gender = fct_recode(gender, "Vrouw" = "Female", "Man" = "Male", "Onbekend" = "Unknown"),
         hospitalised = fct_recode(hospitalised, "Nee" = "No", "Ja" = "Yes", "Onbekend" = "Not known")) #factor levels hernoemen

hp_zone <- hp_zone %>%
  filter(confidence == "Probable" |
           confidence == "Confirmed") #filter op probable en confirmed cases

#Osiris data inladen en opschonen
osiris <- read_excel("C:/Users/.../dummy_data_osiris.xlsx") #voer hier de locatie van het databestand in

osiris <- osiris %>%
  clean_names() %>%
  mutate(epi_vacc_ziekte_v2 = ifelse(is.na(epi_vacc_ziekte_v2), "Onbekend",
                                     ifelse(epi_vacc_ziekte_v2 == "Ja", "Gevaccineerd",
                                            ifelse(epi_vacc_ziekte_v2 == "Nee", "Niet gevaccineerd", epi_vacc_ziekte_v2)))) %>% #verander categorieën voor vaccinatiestatus
  mutate_at(c("osirisnr", "zie_diagnose_morb5_os"), as.character) %>% #verander variabele naar type character
  mutate_at(c("status", "pat_geslacht", "pat_geboorte_jaar", "pa_tmigratie",
              "zie_opn_zh", "zie_comp_morb_jno", "zie_compl_morb", "epi_vacc_ziekte_v2",
              "epi_vacc_status_check_v2", "epi_risico_groep_rvp", "zi_eklinbevlab",
              "zie_labwelk", "zi_eklinbevhoe", "zie_diagnose_morb5ja", "zie_overleden2013"), as.factor) %>% #verander variabelen naar type factor
  mutate_at(c("morb1zdt_exan", "epi_labuitslag_dt"), list(dmy)) #verander variabelen naar type Date

osiris <- osiris %>%
  filter(status != "Gewist" &
           status != "GewistGGD" &
           status != "GewistGGDLCI" &
           status != "GewistGGDEPI") #verwijder gewiste cases

#selecteer velden uit Osiris data om toe te voegen aan HP Zone data
osiris <- osiris %>%
  select(osirisnr,
         pa_tmigratie, #hebben de ouders/verzorgers of grootouders een migratieachtergrond
         zie_comp_morb_jno, #zijn er complicaties van de mazelen opgetreden
         zie_compl_morb, #welke complicaties van de mazelen zijn er opgetreden
         epi_vacc_status_check_v2, #is de vaccinatiestatus geverifieerd
         epi_risico_groep_rvp, #behoort de patiënt of de ouders/verzorgers tot een van de volgende groepen
         zi_eklinbevlab, #werd de klinische diagnose bevestigd met laboratoriumonderzoek
         zi_eklinbevhoe, #hoe werd de klinische diagnose bevestigd
         epi_labuitslag_dt) #datum laboratoriumuitslag

#Osiris velden toevoegen aan HP Zone data
df <- left_join(hp_zone, osiris,
                by = c("osirisnummer" = "osirisnr"))

summary(df) #inspecteer samengevoegde dataset

#samengevoegde dataset exporteren
setwd("C:/Users/...") #voer hier de locatie in om de samengevoegde dataset naar te exporteren

write.csv2(df, file = "samengevoegde_dataset_hp_zone_osiris.csv", row.names = F, na = "") #exporteer dataset


