# Dataverwerking tijdens een uitbraak
Een toolkit met bestanden voor het verzamelen, registreren, analyseren en presenteren van uitbraakdata door teams Infectieziektebestrijding van de GGD.
## Inhoud
### 10 stappen van uitbraakonderzoek
Deze handleiding is toegespitst op de GGD-praktijk en vertaalt bestaande richtlijnen naar concrete werkafspraken voor GGD'en. Voor iedere stap wordt verwezen naar direct toepasbare hulpmiddelen en de kaders waarbinnen deze toegepast kunnen worden.
### Generieke uitbraakvragenlijst
Een generieke mastervragenlijst ontwikkeld voor het verzamelen van gegevens bij uitbraakonderzoek. Deze vragenlijst bundelt, actualiseert en standaardiseert bestaande materialen. Tijdens een uitbraak kan worden geselecteerd welke vragen relevant zijn voor de specifieke situatie.
### DPIA's
Data Privacy Impact Assessments (DPIA's) voor het veilig verwerken van gegevens verzameld tijdens uitbraakonderzoek. De DPIA's zijn opgesteld in samenwerking met, en goedgekeurd door, een Privacy Officer en Functionaris Gegevensbescherming. Ze kunnen als voorbeeld worden gebruikt en worden aangepast aan de situatie van de eigen GGD.
### R-scripts
Voor het analyseren en presenteren van verzamelde uitbraakdata. De scripts sluiten aan op gegevens zoals geregistreerd in HP Zone, Osiris, Enalyzer of een zelf opgestelde linelist in Excel. Voor elk script is een dummydataset ontwikkeld.
| R-script | Dataset |
|-----------|----------|
| `script_beschrijvende_epidemiologie` | `dummy_data_hp_zone` |
| `script_samenvoegen_hpzone_osiris_data` | `dummy_data_hp_zone`, `dummy_data_osiris` |
| `script_sociaal_netwerkdiagram` | `dummy_data_osiris` |
| `script_analyse_cohortstudie` | `dummy_data_enalyzer` |
| `script_analyse_case-control_studie` | `dummy_data_enalyzer` |
| `script_uitbraakrapport` | `dummy_data_linelist` |

