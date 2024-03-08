library(boot)
install.packages("NSM3")
library(NSM3)
library(MASS)

mydata <- readRDS("Daten/dataFromNumeric.rds")



#Hypothese1 ----

# Umwandlung der 'education' Variable in eine geordnete Faktorvariable
mydata$education <- factor(mydata$education,
                           levels = c("(noch) kein Schulabschluss",
                                      "Hauptschulabschluss / Volksschulabschluss",
                                      "Realschulabschluss (Mittlere Reife)",
                                      "(Fach-)Abitur",
                                      "(Fach-)Hochschulabschluss"),
                           ordered = TRUE)

# Umwandlung der geordneten Faktorvariable 'education' in einen numerischen Vektor
mydata$education_numeric <- as.numeric(mydata$education)

# Durchführung des Spearman-Korrelationstests
cor.test(mydata$education_numeric, mydata$Behavioral_Intention, method = "spearman")

mydata$education <- factor(mydata$education,
                           levels = c("(noch) kein Schulabschluss",
                                      "Hauptschulabschluss / Volksschulabschluss",
                                      "Realschulabschluss (Mittlere Reife)",
                                      "(Fach-)Abitur",
                                      "(Fach-)Hochschulabschluss"),
                           ordered = TRUE)

# Durchführung des Kruskal-Wallis-Tests
kruskal.test(Behavioral_Intention ~ education, data = mydata)

#Hypothese2 ----
mydata$generation <- cut(mydata$age,
                         breaks = c(-Inf, 24, 40, 56, 76, Inf),
                         labels = c("Gen Z", "Millennials", "Gen X", "Babyboomer", "Stille Generation"),
                         right = FALSE)

generations <- levels(mydata$generation)

for (gen in generations) {
  print(paste("Shapiro-Wilk-Test für Generation:", gen))
  print(shapiro.test(mydata$Behavioral_Intention[mydata$generation == gen]))
}

kruskal.test(Behavioral_Intention ~ generation, data = mydata)

library(NSM3)

# Durchführen des Kruskal-Wallis-Tests
kw_test <- kruskal.test(Behavioral_Intention ~ generation, data = mydata)

# Durchführen des Dwass-Steel-Critchlow-Fligner Tests, falls Kruskal-Wallis signifikant ist
if(kw_test$p.value < 0.05) {
  dscf_test <- dscfTest(Behavioral_Intention ~ generation, data = mydata)
  print(dscf_test)
} else {
  print("Kruskal-Wallis-Test ist nicht signifikant; DSCF-Test nicht notwendig.")
}




#Hypothese3 ----
mydata <- subset(mydata, gender != "keine Angabe")
mydata$gender <- as.factor(mydata$gender)

# Lineares Modell mit Interaktion zwischen 'Effort_Expectancy' und 'gender'
model <- lm(Behavioral_Intention ~ Effort_Expectancy * gender, data = mydata)

summary(model)
anova(model)

#Hypothese 4 -----

# Definieren älterer Teilnehmer, z.B. 50 Jahre und älter
mydata$older <- mydata$age >= 50

# Umwandeln von Geschlecht in eine binäre Variable (0 für Männer, 1 für Frauen)
mydata$gender_binary <- ifelse(mydata$gender == "weiblich", 1, 0)

# Erstellen des Interaktionsterms
mydata$interaction <- mydata$Facilitating_Conditions * mydata$gender_binary * mydata$older

# Lineares Regressionsmodell
model <- lm(Behavioral_Intention ~ Facilitating_Conditions + gender_binary + older + interaction, data = mydata)

# Zusammenfassung des Modells
summary(model)

# Berechnung der Residuen aus dem Modell
residuals <- residuals(model)

# Shapiro-Wilk-Test auf Normalverteilung der Residuen
shapiro.test(residuals)

# Erstellen eines Q-Q-Plots
qqnorm(residuals)
qqline(residuals)

#Scheißenichtnormalverteilt 

boot_fn <- function(data, indices) {
  d <- data[indices, ] # Stichproben mit Zurücklegen
  fit <- lm(Behavioral_Intention ~ Facilitating_Conditions + gender_binary + older + interaction, data = d)
  return(coef(fit)) # Gibt die Koeffizienten des Modells zurück
}

# Durchführung des Bootstrapping mit 1000 Wiederholungen
results <- boot(data = mydata, statistic = boot_fn, R = 1000)

# Anzeigen der Ergebnisse des Bootstrappings
results

# Berechnung der Bootstrap-Konfidenzintervalle
boot_ci <- boot.ci(results, type = c("perc", "bca"))
print(boot_ci)

#Hypothese 5 ----- Kendalls Tau eigentlich 



# Funktion zur Durchführung der Regression, die wir bootstrappen wollen
reg_fn <- function(data, indices) {
  d <- data[indices, ] # Stichproben mit Zurücklegen
  fit <- lm(Behavioral_Intention ~ Techchnology_Commitment, data = d)
  return(coef(fit)) # Gibt die Koeffizienten des Modells zurück
}

# Durchführung des Bootstrapping mit 1000 Wiederholungen
set.seed(123) # Für reproduzierbare Ergebnisse
reg_results <- boot(data = mydata, statistic = reg_fn, R = 1000)

# Anzeigen der Ergebnisse des Bootstrappings
print(reg_results)

# Berechnung der Bootstrap-Konfidenzintervalle für 'Technology_Commitment'
boot_ci_tech <- boot.ci(reg_results, type = c("perc", "bca"), index = 2)

# Anzeigen der Konfidenzintervalle
print(boot_ci_tech)



#Hypothese 6 ----
# Shapiro-Wilk-Test für Sem_Diff
shapiro.test(mydata$Sem_Diff)

# Shapiro-Wilk-Test für Behavioral Intention to Use
shapiro.test(mydata$Behavioral_Intention)

# Q-Q-Plot für Sem_Diff
qqnorm(mydata$Sem_Diff)
qqline(mydata$Sem_Diff)

# Q-Q-Plot für Behavioral Intention to Use
qqnorm(mydata$Behavioral_Intention)
qqline(mydata$Behavioral_Intention)

cor.test(mydata$Sem_Diff, mydata$Behavioral_Intention, method = "spearman")

robust_model <- rlm(Behavioral_Intention ~ Sem_Diff, data = mydata)

# Zusammenfassung des robusten Modells anzeigen
summary(robust_model)

robust_model <- rlm(Behavioral_Intention ~ Sem_Diff, data = mydata)

####Visualisierung



# Daten für die Regressionslinie vorbereiten
regression_data <- data.frame(
  Sem_Diff = range(mydata$Sem_Diff),
  Behavioral_Intention = predict(robust_model, newdata = data.frame(Sem_Diff = range(mydata$Sem_Diff)))
)




  ggplot(mydata, aes(x = Sem_Diff, y = Behavioral_Intention)) +
  geom_point(size = 2) +  # Größe der Punkte anpassen
  geom_line(data = regression_data, aes(x = Sem_Diff, y = Behavioral_Intention), color = "blue") + # Regressionslinie hinzufügen
  labs(
    title = "Einfluss der Wahrnehmung des upBUS auf die Nutzungsabsicht", 
    x = "Wahrnehmung des upBUS", 
    y = "Nutzungsabsicht (Behavioral Intention to Use)",
    caption = "Robustes Regressionsmodell"
  ) +
  theme_minimal() + # Minimalistisches Thema verwenden
  coord_cartesian(xlim = c(min(mydata$Sem_Diff), max(mydata$Sem_Diff)), 
                  ylim = c(min(mydata$Behavioral_Intention), max(mydata$Behavioral_Intention))) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


#Hypothese 7 -----
shapiro.test(mydata$Behavioral_Intention)

cor.test(mydata$Performance_Expectancy, mydata$Behavioral_Intention, method = "spearman")
cor.test(mydata$Effort_Expectancy, mydata$Behavioral_Intention, method = "spearman")
cor.test(mydata$Social_Influence, mydata$Behavioral_Intention, method = "spearman")
cor.test(mydata$Hedonic_Motivation, mydata$Behavioral_Intention, method = "spearman")
cor.test(mydata$Price_Value, mydata$Behavioral_Intention, method = "spearman")
cor.test(mydata$Facilitating_Conditions, mydata$Behavioral_Intention, method = "spearman")

#Hypothese 8 ----

mydata$Use_Intention <- factor(mydata$Use_Intention, ordered = TRUE)

# Durchführung der ordinalen logistischen Regression
model <- polr(Use_Intention ~ Behavioral_Intention + Facilitating_Conditions, data = mydata, Hess = TRUE)

# Anzeigen der Zusammenfassung des Modells
summary(model)


#Visualisierung --> ganz komisch, ist nicht das was wir wollen
library(effects)

# Erstelle das effect object für Behavioral Intention
effect_plot_behav <- Effect(c("Behavioral_Intention", "Facilitating_Conditions"), model)

# Plot für Behavioral Intention
plot(effect_plot_behav, 
     main = "Effekt von Behavioral Intention auf Use Intention",
     xlab = "Behavioral Intention", ylab = "Wahrscheinlichkeit", 
     type = "probability")  # Verwende "probability" oder "logit" für den 'type'-Parameter

# Erstelle das Effekt-Objekt für Behavioral Intention
effect_plot_behav <- allEffects(model)

# Plots für alle Effekte im Modell
plot(effect_plot_behav, 
     main = "Effekte auf Use Intention",
     xlab = "Prädiktoren", ylab = "Wahrscheinlichkeit", 
     type = "probability")  # 'type' muss "probability" oder "logit" sein

#Hypothese 9
# Robuste Regression
robust_model <- rlm(Behavioral_Intention ~ Sem_Diff_Sicherheit + Sem_Diff_Ziefle_18 + Sem_Diff_Ziefle_21 + Sem_Diff_Zuverlässigkeit, data = mydata)

# Zusammenfassung des robusten Modells anzeigen
summary(robust_model)

# Berechnen Sie die Mittelwerte für jedes Adjektivpaar
mean_values <- sapply(mydata[Sem_Diff], mean, na.rm = TRUE)

# Erstellen Sie einen Datenrahmen für die exportierten Mittelwerte
mean_df <- mydata(Adjektivpaar = Sem_Diff, Mittelwert = mean_values)

# Exportieren Sie die Mittelwerte in eine CSV-Datei
write.csv(mean_df, file = "Semantische_Differentiale_Mittelwerte.csv", row.names = FALSE)
summary(model)

write.csv(mydata, "Daten/final_data.csv")