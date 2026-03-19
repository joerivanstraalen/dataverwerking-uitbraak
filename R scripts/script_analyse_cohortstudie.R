#script voor analyse cohortstudie 
#de dummy data zijn grotendeels gebaseerd op de EPIET case study "Outbreak of gastroenteritis after a high school dinner in Copenhagen, Denmark, November 2006", link: https://github.com/EPIET/OutbreakInvestigation/tree/master/Copenhagen

#benodigde packages laden
library(pacman)
pacman::p_load(readxl, tidyverse, lubridate, rlang, gtsummary, gt, epitools, broom, rio,
               here, janitor, scales) #benodigde R packages laden

#linelist bestand inladen en opschonen
linelist <- read_excel("C:/Users/.../dummy_data_linelist.xlsx") #voer hier de locatie van het linelist bestand in

linelist <- linelist %>%
  clean_names() %>%
  mutate(case = ifelse(symptomen == "Ja", "Case", "Non-case")) %>%
  mutate_at(c("geslacht", "symptomen", "waterdunne_ontlasting", "braken", "buikpijn", "misselijkheid", "koorts", "hoofdpijn",
              "tonijn", "tonijn_hoeveelheid", "garnalen", "garnalen_hoeveelheid", "groente", "groente_hoeveelheid",
              "kalfsvlees",  "kalfsvlees_hoeveelheid", "pasta", "pasta_hoeveelheid", "rucola", "rucola_hoeveelheid",
              "saus", "saus_hoeveelheid", "brood", "brood_hoeveelheid", "champagne", "champagne_hoeveelheid", 
              "bier", "bier_hoeveelheid", "rode_wijn", "rode_wijn_hoeveelheid", "witte_wijn", "witte_wijn_hoeveelheid",
              "water", "water_hoeveelheid", "case"), as.factor) %>% #verander variabelen naar type factor
  mutate_at(c("leeftijd"), as.numeric) %>% #verander variabelen naar type numeric
  mutate_at(c("ezd_datum"), list(ymd)) #verander variabelen naar type Date (let op: in dit voorbeeld zijn de datums geregistreerd in ymd formaat, andere formaten kunnen zijn dmy, mdy, etc.)

linelist <- linelist %>%
  mutate(case = relevel(case, ref = "Non-case"),
         tonijn = relevel(tonijn, ref = "Nee"),
         garnalen = relevel(garnalen, ref = "Nee"),
         groente = relevel(groente, ref = "Nee"),
         kalfsvlees = relevel(kalfsvlees, ref = "Nee"),
         pasta = relevel(pasta, ref = "Nee"),
         rucola = relevel(rucola, ref = "Nee"),
         saus = relevel(saus, ref = "Nee"),
         brood = relevel(brood, ref = "Nee"),
         bier = relevel(bier, ref = "Nee"),
         champagne = relevel(tonijn, ref = "Nee"),
         rode_wijn = relevel(rode_wijn, ref = "Nee"),
         witte_wijn = relevel(witte_wijn, ref = "Nee")) #referentie categoriën van variabelen aangeven

cases <- linelist %>%
  filter(case == "Case") #filter op cases

#Karakteristieken cases en non-cases
variabelen_cases_non_cases <- c("case", "geslacht", "leeftijd", 
                                "tonijn", "garnalen", "kalfsvlees", "pasta", "rucola", "saus", "brood",
                                "champagne", "bier", "rode_wijn", "witte_wijn", "water") #selecteer relevante variabelen

tabel <- linelist %>%
  select(all_of(variabelen_cases_non_cases)) %>%
  tbl_summary(by = case,
              missing = "no",
              statistic = all_categorical() ~ "{n} ({p}%)",
              value = c(tonijn, garnalen, kalfsvlees, pasta, rucola,
                        saus, brood, champagne, bier, rode_wijn, witte_wijn, water) ~ "Ja") %>% #alleen de categorie "Ja" weergeven voor deze variabelen
  add_n(by = case) %>%
  bold_labels() %>%
  modify_header(label = "**Variabele**") %>%
  modify_caption("**Tabel 1. Karakteristieken van cases en non-cases.**")

print(tabel) #tabel weergeven

#Attack rates
voedsel_vars <- c("tonijn", "garnalen", "kalfsvlees", "pasta", "rucola", "saus", 
                  "brood", "champagne", "bier", "rode_wijn", "witte_wijn", "water") #selecteer relevante variabelen om attack rates en RRs te berekenen

attack_rates <- linelist %>%
  pivot_longer(cols = voedsel_vars,
               names_to = "Variabele",
               values_to = "Categorie") %>% 
  group_by(Variabele, Categorie) %>%
  summarise(Aantal = n(),
            Cases = sum(case == "Case", na.rm = TRUE), #selecteer cases op basis van de registratie in de kolom "Case"
            `Attack rate (%)` = round((Cases / Aantal) * 100, 1),
            .groups = "drop") %>%
  filter(!is.na(Categorie) &
           Categorie == "Ja") %>% #filter op blootstelling aan de variabelen
  select(-Categorie) %>%
  arrange(`Attack rate (%)`) #sorteer op oplopende attack rate

print(attack_rates) #attack rates weergeven

#Relative risks

#functie maken voor voor het uitvoeren van deze analyse voor alle items van voedsel_vars
extract_rr_ci <- function(var) {
  if (!var %in% names(linelist)) return(NULL)
  
  model <- try(glm(as.formula(paste("case ~", var)), family = binomial(link = "log"), data = linelist), silent = TRUE)
  if (inherits(model, "try-error")) return(NULL)
  
  rr_row <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(str_detect(term, var)) %>%
    mutate(voedsel = var)
  
  if (nrow(rr_row) == 0) return(NULL)
  
  #n toevoegen (alle niet-NA waarden)
  rr_row$n <- sum(!is.na(linelist[[var]]))
  return(rr_row)
}

#dataframe maken met RR en CI voor voedsel_vars
rr_data <- map_dfr(voedsel_vars, extract_rr_ci)

#kolom maken met RR (95% CI)
rr_data <- rr_data %>%
  mutate(
    product = str_to_title(voedsel),
    rr_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    label = sprintf("%-15s  %5s   %15s", product, n, rr_ci)
  ) %>%
  arrange(estimate)

#kopregel voor dataframe maken
header_row <- tibble(
  estimate = NA,  
  conf.low = NA,
  conf.high = NA,
  label = sprintf("%-15s  %5s   %15s", "Variabele", "n", "RR (95% C.I.)")
)

#kopregel toevoegen aan dataframe en factor volgorde goed zetten
rr_data <- bind_rows(header_row, rr_data) %>%
  mutate(label = factor(label, levels = rev(label))) #voor juiste volgorde in plot

#plot de RRs
rr_plot <- ggplot(rr_data, aes(x = estimate, y = label)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_log10() +
  labs(x = "Relative Risk", y = NULL)+
  theme_minimal(base_size = 13)

plot(rr_plot)

#blootstelling analyse
tabel_blootstelling <- linelist %>%
  tbl_summary(include = c(water_hoeveelheid), #geef de blootstelling variabele aan om te analyseren
              by = case,
              missing = "no") %>%
  add_n() %>%
  bold_labels() %>%
  modify_header(label = "**Variabele**") %>%
  modify_caption("**Tabel 2. Blootstelling analyse.**")

print(tabel_blootstelling) #tabel weergeven
