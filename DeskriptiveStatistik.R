#install.packages("gt")
#install.packages("esquisse")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("knitr")
install.packages("kableExtra")

# Lade die Pakete
library(knitr)
library(kableExtra)

library(gt)
library(psych)
library(dplyr)
library(esquisse)
library(ggplot2)
library(reshape2)
library(tidyr)
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

mean_rankings_inverted_df %>%
  ggplot(aes(x = reorder(Verkehrsmittel, Mittlerer_umgekehrter_Rang), y = Mittlerer_umgekehrter_Rang)) +
  geom_bar(stat = "identity", fill = "#112446") +
  labs(x = "Verkehrsmittel", 
       y = "Präferenzscore", 
       title = "Präferenz für Verkehrsmittel", 
       subtitle = "Höherer Score zeigt stärkere Präferenz", 
       caption = paste("n=107", mydata$Anzahl_Teilnehmer)) +
  theme_minimal() +
  coord_flip()








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
       caption = "n=107") +
  theme_minimal() +
  coord_flip() # Für horizontale Balken



sapply(mydata, function(x) sum(is.na(x)))



data_means <- mydata %>%
  summarise(across(everything(), mean, na.rm = TRUE))
#SemDiff

# Umschmelzen des Datensatzes für ggplot2
mydata_melted <- melt(mydata, variable.name = "adjektivpaar", value.name = "bewertung")


n <- nrow(mydata) / length(positive_poles)

# Erstellen Sie wiederholte Vektoren für die Pole
mydata_melted$positive <- rep(positive_poles, each = n)
mydata_melted$negative <- rep(negative_poles, each = n)

# Adjektivpaare, hier müssen Sie Ihre spezifischen Paare einfügen
positive_poles <- c(
  "verkehrstauglich", "freundlich", "übersichtlich", "vertrauenswürdig", 
  "berechenbar", "kontrollierbar", "beschützt", "beruhigend", 
  "abgeschirmt", "datensicher", "bequem", "zeitsparend", 
  "entspannend im Straßenverkehr", "neuartig", "faszinierend", 
  "elegant", "gewohnt", "erschwinglich", "nachhaltig", 
  "umweltfreundlich", "gut angebunden", "effizient", "flexibel"
)

negative_poles <- c(
  "nicht verkehrstauglich", "feindlich", "unübersichtlich", 
  "nicht vertrauenswürdig", "riskant", "unkontrollierbar", "angreifbar", 
  "beängstigend", "spionierend", "datenunsicher", "unbequem", 
  "zeitaufwendig", "stressig im Straßenverkehr", "traditionell", 
  "langweilig", "sperrig", "ungewohnt", "teuer", "kurzlebig", 
  "umweltschädlich", "schlecht angebunden", "ineffizient", "unflexibel"
)

# Fügen Sie die Pole Ihrem umgeschmolzenen Datensatz hinzu
mydata_melted$positive <- factor(positive_poles, levels = positive_poles)
mydata_melted$negative <- factor(negative_poles, levels = positive_poles)

# Erstellen des Plots
p <- ggplot(mydata_melted, aes(x = bewertung, y = positive)) +
  geom_point(aes(color = adjektivpaar)) +
  geom_text(aes(label = negative, x = -0.5), hjust = 1, color = "grey50") +
  geom_line(aes(group = adjektivpaar)) +
  scale_x_continuous(limits = c(-1, 5), breaks = 1:5) +
  labs(x = "Bewertung", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none")

# Drucken Sie das ggplot-Objekt, um den Plot zu erzeugen
print(p)







# Boxplot für 'technology_commitment'
if("technology_commitment" %in% names(mydata)) {
  ggplot(mydata, aes(y = technology_commitment)) +
    geom_boxplot() +
    labs(title = "Boxplot für Technology Commitment",
         y = "Technology Commitment",
         x = "")
}

# Boxplot für 'Behavioral Intention'
if("Behavioral_Intention" %in% names(mydata)) {
  ggplot(mydata, aes(y = Behavioral_Intention)) +
    geom_boxplot() +
    labs(title = "Boxplot für Behavioral Intention",
         y = "Behavioral Intention",
         x = "")
}



# Histogramm für die Altersverteilung mit Anzahl der Beobachtungen
hist(mydata$age, 
     main = "Altersverteilung der Teilnehmer",
     xlab = "Alter",
     ylab = "Anzahl der Teilnehmer",
     col = "lightgrey",
     border = "black")
mtext(paste("n =", nrow(mydata)), side = 1, line = 4, adj = 1)


# Berechnen des Mittelwerts und der Standardabweichung für das Alter
age_mean <- mean(mydata$age, na.rm = TRUE)
age_sd <- sd(mydata$age, na.rm = TRUE)

# Auszählen der Häufigkeiten für das Geschlecht
gender_counts <- table(mydata$gender)
gender_proportions <- prop.table(gender_counts)

# Auszählen der Häufigkeiten für den Bildungsabschluss
education_counts <- table(mydata$education)
education_proportions <- prop.table(education_counts)

# Ausgabe der Ergebnisse
cat("Alter - Mittelwert (M):", age_mean, "\n")
cat("Alter - Standardabweichung (SD):", age_sd, "\n")
cat("Geschlecht - Anzahlen und Proportionen:\n")
print(gender_counts)
print(gender_proportions)
cat("Bildungsabschluss - Anzahlen und Proportionen:\n")
print(education_counts)
print(education_proportions)


summary_table <- data.frame(
  Attribute = c("Geschlecht", "Alter", "Schulabschluss"),
  Weiblich = c(sum(mydata$gender == "weiblich"), NA, NA),
  Männlich = c(sum(mydata$gender == "männlich"), NA, NA),
  Alter_M = c(NA, age_mean, NA),
  Alter_SD = c(NA, age_sd, NA),
  Kein_Schulabschluss = c(NA, NA, sum(mydata$education == "(noch) kein Schulabschluss")),
  Hauptschulabschluss = c(NA, NA, sum(mydata$education == "Hauptschulabschluss / Volksschulabschluss")),
  Realschulabschluss = c(NA, NA, sum(mydata$education == "Realschulabschluss (Mittlere Reife)")),
  Abitur = c(NA, NA, sum(mydata$education == "(Fach-)Abitur")),
  Hochschulabschluss = c(NA, NA, sum(mydata$education == "(Fach-)Hochschulabschluss"))
)

# Die NAs werden verwendet, um Zellen leer zu lassen

# Anzeigen der Tabelle
print(summary_table)

knitr::kable(summary_table, format = "html")






