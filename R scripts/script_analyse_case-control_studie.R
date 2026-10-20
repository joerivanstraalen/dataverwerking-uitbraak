#script voor analyse case-control studie 
#de dummy data zijn gebaseerd op een export van gegevens verzameld met de template vragenlijst voor het softwareprogramma Enalyzer (niet alle kolommen zijn meegenomen)
#de template vragenlijst voor Enalyzer is één van de eindproducten van het regioproject "Dataverwerking tijdens een uitbraak"  

#benodigde packages laden
library(pacman)
pacman::p_load(readxl, tidyverse, lubridate, rlang, gtsummary, gt, epitools, broom, rio,
               here, janitor, scales) #benodigde R packages laden

#Enalyzer export inladen en opschonen
#acties voor het maken van de Export: 
#Respondenten > Acties > Downloaden > Alle … respondenten > Downloaden (…) > Bestandsformaat: Excel, Variabel-labels: Datalabels, Dataselectie: “Systeemachtergrondvariabelen” en “Open antwoorden” > Download (vervolgens wachten tot het Excel-bestand is gegenereerd in de Downloadbibliotheek en dan downloaden naar de pc/laptop)
df <- read_excel("C:/Users/.../dummy_data_enalyzer.xlsx") #voer hier de locatie van het databestand in

df <- df %>%
  clean_names() %>%
  rename(geslacht = wat_is_je_geslacht_1,
         leeftijd = hoe_oud_ben_je_1,
         postcode = wat_is_je_postcode_1,
         verblijf = waar_verblijf_je_op_dit_moment_1,
         klachten = heeft_u_in_de_afgelopen_tijdsperiode_klachten_gehad_die_passen_bij_ziektebeeld,
         ezd_datum = wanneer_begonnen_de_klachten,
         zorgverlener = ben_je_met_de_klachten_naar_een_zorgverlener_geweest_zoals_de_huisarts_of_het_ziekenhuis,
         opgenomen = ben_je_hiervoor_opgenomen_geweest_in_een_ziekenhuis,
         buitenland = ben_je_in_de_afgelopen_tijdsperiode_in_het_buitenland_geweest,
         diarree = heb_je_last_gehad_van_de_volgende_klachten_waterdunne_ontlasting_of_diarree,
         bloed_ontlasting = heb_je_last_gehad_van_de_volgende_klachten_bloed_in_de_ontlasting,
         buikpijn = heb_je_last_gehad_van_de_volgende_klachten_buikpijn_of_buikkramp,
         braken = heb_je_last_gehad_van_de_volgende_klachten_braken_of_overgeven,
         misselijkheid = heb_je_last_gehad_van_de_volgende_klachten_misselijkheid,
         koorts = heb_je_last_gehad_van_de_volgende_klachten_koorts_van_38_graden_of_hoger,
         vermoeidheid = heb_je_last_gehad_van_de_volgende_klachten_vermoeidheid,
         groenten_fruit_salade = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_rauwe_groenten_fruit_of_salade_gegeten,
         hoeveel_groenten_fruit_salade = hoeveel_heb_je_hiervan_gegeten_12,
         kip = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_kip_gegeten,
         ei = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_ei_gegeten,
         rauwe_melk = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_rauwe_melk_gedronken,
         rauwmelkse_kaas = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_rauwmelkse_kaas_gegeten,
         geitenkaas = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_geitenkaas_gegeten,
         franse_kaas = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_franse_kaas_gegeten,
         rauw_vlees = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_ongaarrauw_vlees_gegeten,
         viszeeproducten = heb_je_in_de_tijdsperiode_voordat_je_ziek_werd_viszeeproducten_gegeten,
         werk = welk_werk_doe_je,
         contact_dieren = heb_je_in_periode_contact_gehad_met_dieren,
         bijeenkomst = heb_je_in_periode_een_grote_bijeenkomst_bezocht_zoals_bijv_een_feest_congres_religieuze_bi,
         bijeenkomst_soort = wat_voor_soort_bijeenkomst_was_dit_meerdere_antwoorden_mogelijk,
         immunosuppresiva = gebruikt_u_medicijnen_die_uw_weerstand_verlagen_bijv_tegen_reuma_ziekte_van_crohn_of_bij_org) %>%
  mutate(case = ifelse(klachten == "Ja", "Case", "Non-case")) %>%
  mutate_at(c("geslacht", "verblijf", "klachten", "zorgverlener", "opgenomen", "buitenland", "diarree", "bloed_ontlasting", "buikpijn",
              "braken", "misselijkheid", "koorts", "vermoeidheid", "groenten_fruit_salade", "hoeveel_groenten_fruit_salade",
              "kip",  "ei", "rauwe_melk", "rauwmelkse_kaas", "geitenkaas", "franse_kaas",
              "rauw_vlees", "viszeeproducten", "werk", "contact_dieren", "bijeenkomst", "bijeenkomst_soort", 
              "immunosuppresiva", "case"), as.factor) %>% #verander variabelen naar type factor
  mutate_at(c("leeftijd"), as.numeric) %>% #verander variabelen naar type numeric
  mutate_at(c("ezd_datum"), list(dmy)) #verander variabelen naar type Date (let op: in dit voorbeeld zijn de datums geregistreerd in dmy formaat, andere formaten kunnen zijn ymd, mdy, etc.)

df <- df %>%
  mutate(
    across(c(diarree, bloed_ontlasting, buikpijn, braken, misselijkheid, koorts, vermoeidheid, groenten_fruit_salade,
             kip, ei, rauwe_melk, rauwmelkse_kaas, geitenkaas, franse_kaas, rauw_vlees, viszeeproducten),
           ~ factor(., 
                    levels = c(1, 2, 3),
                    labels = c("Ja", "Nee", "Nee")))) #labels geven aan antwoordcategorieën, let op: categorie 2 (= Mogelijk) is in dit voorbeeld omgezet naar "Nee"

df <- df %>%
  mutate(case = relevel(case, ref = "Non-case"),
         groenten_fruit_salade = relevel(groenten_fruit_salade, ref = "Nee"),
         kip = relevel(kip, ref = "Nee"),
         ei = relevel(ei, ref = "Nee"),
         rauwe_melk = relevel(rauwe_melk, ref = "Nee"),
         rauwmelkse_kaas = relevel(rauwmelkse_kaas, ref = "Nee"),
         geitenkaas = relevel(geitenkaas, ref = "Nee"),
         franse_kaas = relevel(franse_kaas, ref = "Nee"),
         rauw_vlees = relevel(rauw_vlees, ref = "Nee"),
         viszeeproducten = relevel(viszeeproducten, ref = "Nee")) #referentie categoriën van variabelen aangeven

cases <- df %>%
  filter(case == "Case") #filter op cases

#Karakteristieken cases en non-cases
variabelen_cases_non_cases <- c("case", "geslacht", "leeftijd", 
                                "groenten_fruit_salade", "kip",  "ei", "rauwe_melk", "rauwmelkse_kaas", "geitenkaas", "franse_kaas",
                                "rauw_vlees", "viszeeproducten") #selecteer relevante variabelen

tabel <- df %>%
  select(all_of(variabelen_cases_non_cases)) %>%
  tbl_summary(by = case,
              missing = "no",
              statistic = all_categorical() ~ "{n} ({p}%)",
              value = c(groenten_fruit_salade, kip,  ei, rauwe_melk, rauwmelkse_kaas, geitenkaas, franse_kaas,
                        rauw_vlees, viszeeproducten) ~ "Ja") %>% #alleen de categorie "Ja" weergeven voor deze variabelen
  add_n(by = case) %>%
  bold_labels() %>%
  modify_header(label = "**Variabele**") %>%
  modify_caption("**Tabel 1. Karakteristieken van cases en non-cases.**")

print(tabel) #tabel weergeven

#Odds ratios
voedsel_vars <- c("groenten_fruit_salade", "kip",  "ei", "rauwe_melk", "rauwmelkse_kaas", "geitenkaas", "franse_kaas",
                  "rauw_vlees", "viszeeproducten") #selecteer relevante variabelen om ORs te berekenen

#functie maken voor voor het uitvoeren van deze analyse voor alle items van voedsel_vars
extract_or_ci <- function(var) {
  if (!var %in% names(df)) return(NULL)
  
  model <- try(glm(as.formula(paste("case ~", var)), family = binomial, data = df), silent = TRUE)
  if (inherits(model, "try-error")) return(NULL)
  
  or_row <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(str_detect(term, var)) %>%
    mutate(voedsel = var)
  
  if (nrow(or_row) == 0) return(NULL)
  
  # n toevoegen (alle niet-NA waarden)
  or_row$n <- sum(!is.na(df[[var]]))
  return(or_row)
}

#dataframe maken met OR en CI voor voedsel_vars
or_data <- map_dfr(voedsel_vars, extract_or_ci)

#kolom maken met OR (95% CI)
or_data <- or_data %>%
  mutate(
    product = str_to_title(voedsel),
    or_ci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high),
    label = sprintf("%-15s  %5s   %15s", product, n, or_ci)
  ) %>%
  arrange(estimate)

#kopregel voor dataframe maken
header_row <- tibble(
  estimate = NA,  
  conf.low = NA,
  conf.high = NA,
  label = sprintf("%-15s  %5s   %15s", "Variabele", "n", "OR (95% C.I.)")
)

#kopregel toevoegen aan dataframe en factor volgorde goed zetten
or_data <- bind_rows(header_row, or_data) %>%
  mutate(label = factor(label, levels = rev(label))) #voor juiste volgorde in plot

#plot de ORs
or_plot <- ggplot(or_data, aes(x = estimate, y = label)) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 orientation = "y",
                 width = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_log10() +
  labs(x = "Odds Ratio", y = NULL)+
  theme_minimal(base_size = 13)

plot(or_plot)


