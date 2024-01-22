#install.packages("gt")
#install.packages("esquisse")
install.packages("ggplot2")
install.packages("reshape2")
library(gt)
library(psych)
library(dplyr)
library(esquisse)
library(ggplot2)
library(reshape2)
mydata <- readRDS("Daten/dataFromNumeric.rds")



#Mediakanalnutzung Häufigkeit----
# Zerlegen der 'translated_media_channels' Spalte in einzelne Medienkanal-Namen
media_channels_list <- strsplit(raw.short$translated_media_channels, ", ")
media_channels_unlisted <- unlist(media_channels_list)

# Häufigkeitsauszählung für jeden Medienkanal
media_frequency <- table(media_channels_unlisted)

# Umwandlung in ein Dataframe für die Visualisierung
media_frequency_df <- as.data.frame(media_frequency)
names(media_frequency_df) <- c("MediaChannel", "Frequency")

# Balkendiagramm erstellen
ggplot(media_frequency_df, aes(x = reorder(MediaChannel, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "#112446") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Häufigkeit der Nutzung von Medienkanälen",
       x = "Medienkanal",
       y = "Häufigkeit")


#Verkehrsmittel_Nutzung Präferenzen ----
# Dataframe aus umgekehrten mittleren Rängen für ggplot2 erstellen
mean_rankings_inverted_df <- data.frame(
  Verkehrsmittel = names(mean_rankings_inverted_sorted),
  Mittlerer_umgekehrter_Rang = mean_rankings_inverted_sorted
)

# Erstellung des Balkendiagramms mit ggplot2
library(ggplot2)

# Erstellung des Balkendiagramms mit ggplot2 
mean_rankings_inverted_df %>%
  ggplot(aes(x = reorder(Verkehrsmittel, Mittlerer_umgekehrter_Rang), y = Mittlerer_umgekehrter_Rang)) +
  geom_bar(stat = "identity", fill = "#112446") +
  labs(x = "Verkehrsmittel", 
       y = "Präferenzscore", 
       title = "Präferenz für Verkehrsmittel", 
       subtitle = "Höherer Score zeigt stärkere Präferenz", 
       caption = "Datenquelle: Befragung") +
  theme_minimal() +
  coord_flip()  # Balken horizontal anzeigen

# Erstellung eines Dataframes mit der Anzahl der Beobachtungen für jede Region
region_counts <- raw.short %>%
  count(region) %>%
  arrange(desc(n))

# Erstellung des Balkendiagramms mit ggplot2 mit einheitlicher Farbe
ggplot(region_counts, aes(x = region, y = n)) +
  geom_bar(stat = "identity", fill = "#112446") +
  labs(x = "Siedlungsstruktur innerhalb der Städteregion Aachen", 
       y = "Anzahl der Befragten", 
       title = "Verteilung der Befragten nach Siedlungsstruktur", 
       caption = "Datenquelle: Befragung") +
  theme_minimal() +
  coord_flip() # Für horizontale Balken



sapply(mydata, function(x) sum(is.na(x)))



data_means <- mydata %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Umschmelzen des Dataframes von einem breiten zu einem langen Format für ggplot2
data_long <- pivot_longer(
  data_means, 
  cols = starts_with("Sem_Diff_Ziefle_"), # Nur Spalten, die mit "Sem_Diff_Ziefle_" beginnen
  names_to = "item", 
  values_to = "rating"
)

# Den Namen der Variablen umformen, um die Adjektivpaare darzustellen
data_long$item <- factor(data_long$item, levels = c(
  "Sem_Diff_Ziefle_1", "Sem_Diff_Ziefle_2", "Sem_Diff_Ziefle_3",
  "Sem_Diff_Ziefle_4", "Sem_Diff_Ziefle_5", "Sem_Diff_Ziefle_6",
  "Sem_Diff_Ziefle_7", "Sem_Diff_Ziefle_8", "Sem_Diff_Ziefle_9",
  "Sem_Diff_Ziefle_10", "Sem_Diff_Ziefle_11", "Sem_Diff_Ziefle_12",
  "Sem_Diff_Ziefle_13", "Sem_Diff_Ziefle_14", "Sem_Diff_Ziefle_15",
  "Sem_Diff_Ziefle_16", "Sem_Diff_Ziefle_17", "Sem_Diff_Ziefle_18",
  "Sem_Diff_Ziefle_19", "Sem_Diff_Ziefle_20", "Sem_Diff_Ziefle_21",
  "Sem_Diff_Ziefle_22", "Sem_Diff_Ziefle_23"
), labels = c(
  "nicht verkehrstauglich:verkehrstauglich",
  "feindlich:freundlich",
  "unübersichtlich:übersichtlich",
  "nicht vertrauenswürdig:vertrauenswürdig",
  "riskant:berechenbar",
  "unkontrollierbar:kontrollierbar",
  "angreifbar:beschützt",
  "beängstigend:beruhigend",
  "spionierend:abgeschirmt",
  "datenunsicher:datensicher",
  "unbequem:bequem",
  "zeitaufwendig:zeitsparend",
  "stressig im Straßenverkehr:entspannend im Straßenverkehr",
  "traditionell:neuartig",
  "langweilig:faszinierend",
  "sperrig:elegant",
  "ungewohnt:gewohnt",
  "teuer:erschwinglich",
  "kurzlebig:nachhaltig",
  "umweltschädlich:umweltfreundlich",
  "schlecht angebunden:gut angebunden",
  "ineffizient:effizient",
  "unflexibel:flexibel"))

# Erstellen Sie das semantische Differential-Diagramm
ggplot(data_long, aes(x = item, y = rating)) +
  geom_line(group = 1) + # Nur eine Gruppe, da es sich um Mittelwerte handelt
  geom_point() + # Fügt Punkte für jedes Adjektivpaar hinzu
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Rating (Mittelwert)", limits = c(1, 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank()) +
  coord_flip()
