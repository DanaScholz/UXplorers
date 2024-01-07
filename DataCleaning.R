# Data Cleaning
#install.packages("ez")
#install.packages("tidyverse")
#install.packages("readxl")
#remotes::install_github("statisticsforsocialscience/hcictools")
#install.packages("careless")
#install.packages("jmv")
#install.packages("rstatix")

library(hcictools)
library(tidyverse)
library(psych)
library(readxl)
library(careless)
source("qualtricshelpers.R")
library(jmv)
library(dplyr)
library(rstatix)
library(car)
library(ez)

# Daten einlesen

Rohdaten <- "Daten/Rohdaten_07.01.csv"
raw <- load_qualtrics_csv(Rohdaten)

# Zeilen entfernen
raw <- filter(raw, Progress >= 99)

# Spalten entfernen
raw.short <- raw[,c(-1:-4, -7:-8,-10:-17)]

# Codebook generieren
generate_codebook(raw.short, Rohdaten, "Daten/codebook.csv")
codebook <- read_codebook("Daten/codebook_final.csv")
names(raw.short) <- codebook$variable

# Richtige Datentypen zuordnen ----

raw.short$age <- as.numeric(raw.short$age)


raw.short$gender %>%
  dplyr::recode_factor(`1`= "männlich", `2` = "weiblich", `3`="divers", `4`="keine Angabe") %>%
  as.factor() -> raw.short$gender

raw.short$education %>%
  ordered(levels = c(1:5),
          labels = c(`1`="(noch) kein Schulabschluss",
                     `2`="Hauptschulabschluss / Volksschulabschluss",
                     `3`= "Realschulabschluss (Mittlere Reife)",
                     `4`="(Fach-)Abitur",
                     `5`="(Fach-)Hochschulabschluss")) -> raw.short$education

# Umformung der 'region'-Variable
raw.short$region %>%
  dplyr::recode_factor(`1` = "Land", 
                       `2` = "Stadtrand", 
                       `3` = "Stadt",
                       `4` = "Nicht in der Städteregion Aachen") %>%
  as.factor() -> raw.short$region

raw.short$own_vehicle %>%
  dplyr::recode_factor(`1` = "Ja", `2` = "Nein") %>%
  as.factor() -> raw.short$own_vehicle
raw.short$license %>%
  dplyr::recode_factor(`1` = "Ja", `2` = "Nein") %>%
  as.factor() -> raw.short$license

# Ranking-Umformung und Datentyp zuordnen ----
# Umkehrung der Ränge für jedes Verkehrsmittel vor der Mittelwertberechnung
for(i in 1:9) {
  raw.short[[paste0("preferences_mobility_", i)]] <- 9 - raw.short[[paste0("preferences_mobility_", i)]] + 1
}

# Umwandeln der umgekehrten Ränge in Faktoren
for(i in 1:9) {
  raw.short[[paste0("preferences_mobility_", i)]] <- factor(raw.short[[paste0("preferences_mobility_", i)]], levels = c(1:9))
}

# Berechnung der mittleren umgekehrten Ränge für jedes Verkehrsmittel
mean_rankings_inverted <- sapply(raw.short[, grep("preferences_mobility_", names(raw.short))], function(x) mean(as.numeric(x), na.rm = TRUE))

# Zuordnen der Namen der Verkehrsmittel zu den berechneten umgekehrten mittleren Rängen
names(mean_rankings_inverted) <- c("Auto", "E-Auto", "Roller/Mofa/Motorrad", "Fahrrad", "E-Fahrrad", "E-Roller", "Bus", "Bahn", "Taxi")

# Anzeigen der mittleren umgekehrten Ränge in absteigender Reihenfolge (höhere Präferenz zuerst)
mean_rankings_inverted_sorted <- sort(mean_rankings_inverted, decreasing = TRUE)


###habe hier mal aufgehört, wir können ja schauen, welche wir brauchen

# Umcodierung der negativ formulierten Items mit 6 Antwortmöglichkeiten ----
raw.short$driving_climate_2_n <- 7 - raw.short$driving_climate_2_n
raw.short$driving_climate_4_n <- 7 - raw.short$driving_climate_4_n
raw.short$technology_scared_n <- 7 - raw.short$technology_scared_n
raw.short$technology_overload_n <- 7 - raw.short$technology_overload_n

#Qualitätskontrolle ----
nrow(raw.short)
raw.short <- careless_indices(raw.short,
                              speeder_analysis = "median/3",
                              likert_vector = c(22:62))

raw.short <- raw.short[!(raw.short$livesinaachen %in% c("2")), ]


raw.short %>%
  filter(speeder_flag == FALSE) -> raw.noSpeeder #notwendig, da sonst die Funktion careless nicht funktioniert


raw.short %>%
  filter(speeder_flag == FALSE) %>%
  filter(careless_longstr < 20) %>%
  filter(careless_psychsyn > 0) %>%
  filter(careless_psychant < 0) %>%
  filter(careless_mahadflag == FALSE) -> raw.short


# Skalen berechnen ----

names(raw.short)

schluesselliste <- list(
  Driving_Climate = c("driving_climate_1", "driving_climate_2_n", "driving_climate_3", "driving_climate_4_n"),
  Techchnology_Commitment = c( "technology_curios", "technology_favor","technology_new","technology_moreoften","technology_scared_n","technology_overload_n","technology_destroy","technology_complex","technology_succesful","technology_me","technology_solution","technology_happening"),
  Performance_Expectancy = c("utaut_performance1", "utaut_performance2", "utaut_performance3", "utaut_performance 4"),
  Effort_Expectancy = c("utaut_effort1", "utaut_effort2", "utaut_effort3", "utaut_effort4"),
  Social_Influence = c("utaut_social1", "utaut_social2", "utaut_social3"),
  Facilitating_Conditions = c("utaut_fascilitating1", "utaut_fascilitating2", "utaut_fascilitating3", "utaut_fascilitating4"),
  Hedonic_Motivation = c("utaut_hedonic1", "utaut_hedonic2", "utaut_hedonic3"),
  Price_Value = c("utaut_pricevalue1", "utaut_pricevalue2", "utaut_pricevalue3"),
  Behavioral_Intention = c("utaut_behavioral1", "utaut_behavioral2", "utaut_behavioral3"),
  Use_Intention = c("utaut_useintention"),
  Semantic_Differential = c(
                      "Sem_Diff_Ziefle_1", "Sem_Diff_Ziefle_2", "Sem_Diff_Ziefle_3",
                      "Sem_Diff_Ziefle_4", "Sem_Diff_Ziefle_5", "Sem_Diff_Ziefle_6",
                      "Sem_Diff_Ziefle_7", "Sem_Diff_Ziefle_8", "Sem_Diff_Ziefle_9",
                      "Sem_Diff_Ziefle_10", "Sem_Diff_Ziefle_11", "Sem_Diff_Ziefle_12",
                      "Sem_Diff_Ziefle_13", "Sem_Diff_Ziefle_14", "Sem_Diff_Ziefle_15",
                      "Sem_Diff_Ziefle_16", "Sem_Diff_Ziefle_17", "Sem_Diff_Ziefle_18",
                      "Sem_Diff_Ziefle_19", "Sem_Diff_Ziefle_20", "Sem_Diff_Ziefle_21",
                      "Sem_Diff_Ziefle_22", "Sem_Diff_Ziefle_23"))


scores <- scoreItems(schluesselliste, items = raw.short, missing = TRUE, min = 1, max = 6)
data <- bind_cols(raw.short, as_tibble(scores$scores))
#CronbachsAlpha und RDS abspeichern----
scores$alpha
warnings()
saveRDS(data, "Daten/dataFromNumeric.rds")

