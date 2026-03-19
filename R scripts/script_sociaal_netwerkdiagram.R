#script voor sociaal netwerkdiagram van uitbraakdata in Osiris
#de dummy data zijn gebaseerd op een Osiris export van mazelen cases --> tabblad "Exporteren", Export gecodeerde antwoorden: Alleen omschrijving

#benodigde packages laden
library(pacman)
pacman::p_install_gh("reconhub/epicontacts@timeline") #deze regel hoeft maar 1 keer uitgevoerd te worden om de package "epicontacts" te installeren, in het vervolg hoeft de package alleen maar te worden geladen met de regel hieronder
pacman::p_load(readxl, tidyverse, lubridate, remotes, janitor, epicontacts)

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

#sociaal netwerkdiagram
#voor meer informatie: https://www.epirhandbook.com/en/new_pages/transmission_chains.html

#epicontacts object maken
epic <- make_epicontacts(
  linelist = osiris,
  contacts =  osiris[!is.na(osiris$zie_diagnose_morb5_os),], #contacten van cases aangeven
  id = "osirisnr",
  from = "zie_diagnose_morb5_os",
  to = "osirisnr",
  directed = T,
  na_rm_linelist = T,
  na_rm_contacts = T)

print(tibble(epic$contacts), n = Inf) #inspecteer alle contacten

#plot sociaal netwerkdiagram
plot(epic, 
     node_color = "epi_vacc_ziekte_v2", #aparte kleuren voor vaccinatiestatus
     col_pal = c(`Niet gevaccineerd` = "firebrick", 
                 Gevaccineerd = "darkgreen",
                 Onbekend = "gray"),
     selector = F,
     label = F,
     title = "Sociaal netwerkdiagram van gelinkte mazelen cases naar vaccinatiestatus",)

#plot sociaal netwerkdiagram op tijd-as
plot(epic,
     x_axis = "morb1zdt_exan", #eerste ziektedag op de x-as
     node_color = "epi_vacc_ziekte_v2", #aparte kleuren voor vaccinatiestatus
     col_pal = c(`Niet gevaccineerd` = "firebrick", 
                 Gevaccineerd = "darkgreen",
                 Onbekend = "gray"),
     network_shape = "rectangle",
     parent_pos = "bottom", 
     selector = F,
     label = F,
     title = "Sociaal netwerkdiagram van gelinkte mazelen cases naar vaccinatiestatus")

?vis_epicontacts #voor meer opties om te plotten

