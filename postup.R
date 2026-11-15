# Semestralni prace: analyza dat o kosmickych startech
# Zdrojovy soubor: launch_data_original.csv
# Projektova slozka: C:/Users/revel/OneDrive/Desktop/R_cosmic
#
# DULEZITE: Do tohoto souboru zapisujeme postupne vsechny prikazy spustene v RGUI.
# Vysledky a jejich interpretaci doplnime do Dokumentace.docx po kontrole vystupu.

# KROK 1: Nacteni ulozene tabulky a kontrola zakladni struktury
setwd("C:/Users/revel/OneDrive/Desktop/R_cosmic")

launch <- read.csv(
  "launch_data_original.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

dim(launch)
names(launch)
str(launch[c("Launch_JD", "OrbMass", "OrbPay", "Rok")])
summary(launch[c("Launch_JD", "OrbMass", "OrbPay", "Rok")])
