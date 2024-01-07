#install.packages("gt")
#install.packages("esquisse")
library(gt)
library(psych)
library(dplyr)
library(esquisse)







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