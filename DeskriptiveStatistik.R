#install.packages("gt")
#install.packages("esquisse")
install.packages("ggplot2")
library(gt)
library(psych)
library(dplyr)
library(esquisse)
library(ggplot2)







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

mean_rankings_inverted_df %>%
  ggplot(aes(x = reorder(Verkehrsmittel, -Mittlerer_umgekehrter_Rang), y = Mittlerer_umgekehrter_Rang)) +
  geom_bar(stat = "identity", fill = "#112446") +
  labs(x = "Verkehrsmittel", 
       y = "Mittlerer umgekehrter Rang", 
       title = "Mittlerer umgekehrter Rang der Verkehrsmittel-Nutzung", 
       subtitle = "Basierend auf Präferenzen der Befragten", 
       caption = "Datenquelle: Befragung") +
  theme_minimal() +
  coord_flip()  # Balken horizontal anzeigen