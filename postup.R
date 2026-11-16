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

# ZAZNAM VYSLEDKU KROKU 1 (kontrola provedena v RGUI 15. 11. 2026)
# dim(launch): 76032 radku a 34 sloupcu.
# Zdrojovych 33 sloupcu doplnuje odvozeny sloupec Rok.
# Rozsah roku: 1942-2026.
# OrbMass: prumer 2.762e-07, median 0, maximum 0.021.
# OrbPay: prumer 0.6528, median 0, maximum 132.025.
# U obou hmotnostnich promennych jsou i horni kvartily nulove; pred analyzou
# proverime, kolik je nul, skutecnych nenulovych hodnot a chybejicich udaju.

# KROK 2: Rozdeleni zaznamu podle kodu startu a kontrola OrbPay
# Prvni pismeno LaunchCode oznacuje typ startu; O oznacuje orbitalni start.
# Druhe pismeno S oznacuje uspesny start.
sort(table(launch$LaunchCode))

orbital <- grepl("^O", trimws(launch$LaunchCode))
orbital_uspesny <- grepl("^OS", trimws(launch$LaunchCode))

c(
  celkem = nrow(launch),
  orbitalni = sum(orbital, na.rm = TRUE),
  orbitalni_uspesne = sum(orbital_uspesny, na.rm = TRUE),
  orbitalni_uspesne_s_kladnym_OrbPay = sum(orbital_uspesny & launch$OrbPay > 0, na.rm = TRUE),
  orbitalni_uspesne_s_nulovym_OrbPay = sum(orbital_uspesny & launch$OrbPay == 0, na.rm = TRUE),
  orbitalni_uspesne_s_chybejicim_OrbPay = sum(orbital_uspesny & is.na(launch$OrbPay), na.rm = TRUE)
)

summary(launch$OrbPay[orbital_uspesny & launch$OrbPay > 0])

# ZAZNAM VYSLEDKU KROKU 2 (kontrola provedena v RGUI 15. 11. 2026)
# Kody zacinajici O: 7218 orbitalnich startu.
# Kody zacinajici OS: 6694 zaznamu; 6691 ma OrbPay > 0, 3 ma OrbPay = 0,
# zadny nema chybejici OrbPay.
# Pro OrbPay > 0 v teto skupine: min 0.002, Q1 1.157, median 3.092,
# prumer 6.913, Q3 6.300 a maximum 132.025 tuny.
# Kody OS s ciselne pripojenou hodnotou mohou oznacovat dilci uspesnost;
# pred analyzou proto oddelime presny kod OS.

# KROK 3: Vytvoreni vyberu plne uspesnych orbitalnich startu a souhrn po dekadach
table(launch$LaunchCode[orbital_uspesny])

# LaunchCode obsahuje u nekterych zaznamu okrajove mezery, proto pouzijeme trimws().
orbital_data <- subset(launch, trimws(LaunchCode) == "OS" & OrbPay > 0)
orbital_data$Desetileti <- floor(orbital_data$Rok / 10) * 10

c(
  pocet_zaznamu = nrow(orbital_data),
  pocet_chybejicich_OrbPay = sum(is.na(orbital_data$OrbPay)),
  pocet_kladnych_OrbPay = sum(orbital_data$OrbPay > 0, na.rm = TRUE)
)

table(orbital_data$Desetileti)
aggregate(OrbPay ~ Desetileti, data = orbital_data, FUN = median)
aggregate(OrbPay ~ Desetileti, data = orbital_data, FUN = mean)

# ZAZNAM VYSLEDKU KROKU 3 (kontrola provedena v RGUI 15. 11. 2026)
# Presny kod OS a kladny OrbPay: 6659 zaznamu, bez chybejicich hodnot.
# Pocty podle desetileti: 1950=18, 1960=760, 1970=1108, 1980=1122,
# 1990=813, 2000=597, 2010=816, 2020=1425.
# Median OrbPay (t): 0.0655, 1.5955, 1.8050, 2.9200, 3.1770, 3.6200,
# 3.5000, 5.3060 (postupne pro desetileti 1950 az 2020).
# Prumer OrbPay (t): 0.9918, 2.4532, 3.3030, 6.7696, 12.2169, 10.7123,
# 5.4358, 8.6229 (postupne pro desetileti 1950 az 2020).
# Desetileti 2020 je neukoncene; prumery jsou citlive na vysoke hodnoty.

# KROK 4: Histogram OrbPay bez logaritmicke transformace
# Kazdy graf ukladame do samostatneho PNG souboru.
hist(
  orbital_data$OrbPay,
  breaks = 50,
  main = "Rozdeleni OrbPay u uspesnych orbitalnich startu",
  xlab = "OrbPay (tuny)",
  ylab = "Pocet startu",
  col = "lightblue",
  border = "white"
)
dev.copy(png, filename = "histogram_OrbPay.png", width = 1400, height = 900, res = 150)
dev.off()
file.info("histogram_OrbPay.png")$size

# ZAZNAM VYSLEDKU KROKU 4
# Soubor histogram_OrbPay.png byl vytvoren (11397 bajtu).
# Vetsina hodnot OrbPay je soustredena priblizne mezi 0 a 20 tunami,
# rozdeleni je vyrazne pravostranně nesymetricke a obsahuje nekolik hodnot
# nad 100 tun; proto histogram na puvodni ose zobrazuje dlouhy pravy ocas.

# KROK 5: Krabicovy graf OrbPay podle jednotlivych roku
boxplot(
  OrbPay ~ factor(Rok),
  data = orbital_data,
  main = "OrbPay podle roku uspesneho orbitalniho startu",
  xlab = "Rok startu",
  ylab = "OrbPay (tuny)",
  col = "lightgreen",
  las = 2,
  cex.axis = 0.6,
  outcex = 0.35,
  outcol = "gray40"
)

dev.copy(
  png,
  filename = "boxplot_OrbPay_rok.png",
  width = 3200,
  height = 1500,
  res = 200
)
dev.off()
file.info("boxplot_OrbPay_rok.png")$size

# POZNAMKA K PRVNI VERZI GRAFU:
# Nekolik odlehlych hodnot nad 100 t roztahlo osu a stlacilo boxy pro vetsinu
# startu. Nasledujici verze priblizi osu na 0-25 t; hodnoty nad 25 t zustanou
# v datech, pouze nebudou v tomto detailnim obrazku videt.

# KROK 5B: Detailni krabicovy graf po jednotlivych letech (rozsah 0-25 t)
boxplot(
  OrbPay ~ factor(Rok),
  data = orbital_data,
  main = "OrbPay podle roku startu (detail 0-25 tun)",
  xlab = "Rok startu",
  ylab = "OrbPay (tuny)",
  col = "lightgreen",
  las = 2,
  cex.axis = 0.65,
  outcex = 0.35,
  outcol = "gray40",
  ylim = c(0, 25)
)

dev.copy(
  png,
  filename = "boxplot_OrbPay_rok_detail.png",
  width = 3600,
  height = 1700,
  res = 200
)
dev.off()
file.info("boxplot_OrbPay_rok_detail.png")$size

# VYSLEDEK DETAILNIHO BOXPLOTU
# Soubor boxplot_OrbPay_rok_detail.png byl vytvoren (52868 bajtu).
# Osa 0-25 t zviditelnuje mediany a rozptyl vetsiny roku; hodnoty nad 25 t
# zustavaji v datech, ale v tomto detailnim pohledu nejsou zobrazeny.
# Rozsah boxu a mediany se mezi roky meni; graf sam o sobe netestuje vyznamnost.

# KROK 6: Bodovy graf OrbPay podle jednotliveho roku, bez transformace
plot(
  OrbPay ~ Rok,
  data = orbital_data,
  main = "OrbPay podle roku startu",
  xlab = "Rok startu",
  ylab = "OrbPay (tuny)",
  pch = 16,
  cex = 0.45,
  col = rgb(0.1, 0.3, 0.7, 0.25)
)

dev.copy(
  png,
  filename = "bodovy_graf_OrbPay_rok.png",
  width = 2200,
  height = 1400,
  res = 180
)
dev.off()
file.info("bodovy_graf_OrbPay_rok.png")$size

# POZNAMKA K PUVODNIMU CELOPERIODICKEMU BODOVEMU GRAFU:
# Soubor bodovy_graf_OrbPay_rok.png byl vytvoren (35574 bajtu), ale uzivateli
# se v nem kvuli mnozstvi let a rozsahu hodnot spatne orientovalo. Dalsi
# bodove grafy proto vytvarime zvlast po desetiletich a s osou 0-25 t.
# Hodnoty nad 25 t zustavaji v datech, pouze nebudou v detailu zobrazeny.

# KROK 6A: Bodovy graf pro desetileti 1950-1959
data_1950 <- subset(orbital_data, Rok >= 1950 & Rok <= 1959)

plot(
  OrbPay ~ Rok,
  data = data_1950,
  main = "OrbPay podle roku startu (1950-1959)",
  xlab = "Rok startu",
  ylab = "OrbPay (tuny; detail do 25 t)",
  pch = 16,
  cex = 0.8,
  col = "steelblue",
  xlim = c(1950, 1959),
  ylim = c(0, 25),
  xaxt = "n"
)
axis(1, at = 1950:1959)

dev.copy(
  png,
  filename = "bodovy_graf_OrbPay_1950_1959.png",
  width = 1800,
  height = 1100,
  res = 160
)
dev.off()
file.info("bodovy_graf_OrbPay_1950_1959.png")$size

# ZPETNA VAZBA K GRAFU 1950-1959:
# V tomto desetileti je jen 18 startu ve trech letech, proto samostatny
# bodovy graf pusobi prazdne a neprinasi prehledny obraz.
# Misto serie grafu po desetiletich zobrazime rocni mediany za cele obdobi.

# KROK 6B: Rocni median OrbPay za cele sledovane obdobi
rocni_mediany <- aggregate(OrbPay ~ Rok, data = orbital_data, FUN = median)
rocni_pocty <- aggregate(OrbPay ~ Rok, data = orbital_data, FUN = length)
names(rocni_mediany)[2] <- "Median_OrbPay"
names(rocni_pocty)[2] <- "Pocet_startu"
rocni_prehl <- merge(rocni_mediany, rocni_pocty, by = "Rok")

head(rocni_prehl, 10)
tail(rocni_prehl, 10)

plot(
  Median_OrbPay ~ Rok,
  data = rocni_prehl,
  type = "o",
  pch = 16,
  col = "steelblue",
  lwd = 2,
  main = "Typicka hmotnost nakladu podle roku",
  xlab = "Rok startu",
  ylab = "Rocni median OrbPay (tuny)"
)

dev.copy(
  png,
  filename = "median_OrbPay_rocne.png",
  width = 2200,
  height = 1300,
  res = 180
)
dev.off()
file.info("median_OrbPay_rocne.png")$size

# VYSLEDEK ROCNIHO MEDIANU
# Soubor median_OrbPay_rocne.png byl vytvoren (27609 bajtu); obsahuje 70 roku.
# Priklady z kontrolniho vystupu: 1957 median 4.196 t (n=2), 1958 median
# 0.014 t (n=7), 1959 median 0.788 t (n=9); 2025 median 9.945 t (n=312)
# a 2026 median 6.000 t (n=256; rok jeste neni uplny).
# Median v case kolisa a neni rovnomerne rostouci; pocet startu se mezi roky lisi.

# KROK 7: Popisna statistika OrbPay pro analyticky vyber
x <- orbital_data$OrbPay
prumer_x <- mean(x)
sd_pop <- sqrt(mean((x - prumer_x)^2))

popisna_OrbPay <- data.frame(
  Pocet = length(x),
  Minimum = min(x),
  Maximum = max(x),
  Prumer = prumer_x,
  Median = median(x),
  Rozptyl = var(x),
  Smerodatna_odchylka = sd(x),
  Sikmost = mean((x - prumer_x)^3) / sd_pop^3,
  Spicatost_exces = mean((x - prumer_x)^4) / sd_pop^4 - 3
)

round(popisna_OrbPay, 4)

# VYSLEDEK POPISNE STATISTIKY OrbPay
# n=6659, minimum=0.002 t, maximum=132.025 t, prumer=6.9362 t,
# median=3.112 t, vyberovy rozptyl=286.3207, vyberova smerodatna odchylka=16.921.
# Momentalova sikmost=5.9402 a exces spicatosti=36.1923; rozdeleni je silne
# pravostranně nesymetricke s tezkymi chvosty.

# KROK 8: Welchuv dvouvyberovy t-test - prumerny OrbPay pred a od roku 2000
orbital_data$Obdobi <- ifelse(orbital_data$Rok < 2000, "1957-1999", "2000-2026")
table(orbital_data$Obdobi)

# H0: Prumerny kladny OrbPay je v obou obdobich stejny.
# H1: Prumerny kladny OrbPay se mezi obdobi odlisuje.
t.test(
  OrbPay ~ Obdobi,
  data = orbital_data,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

# VYSLEDEK WELCHOVA T-TESTU
# Skupiny: 1957-1999 n=3821, 2000-2026 n=2838.
# Prumery: 6.037645 t a 8.146028 t; rozdil starsi-minus-novejsi=-2.108383 t.
# t=-5.0779, df=6281.5, p=3.926e-07; 95% CI rozdilu [-2.922331,-1.294434].
# Na hladine 0.05 zamitame H0; prumer je vyssi v obdobi 2000-2026.
# Vysledek se tyka prumeru kladnych OrbPay a je citlivy na vysoke hodnoty.

# KROK 9: Welchova ANOVA podle desetileti a parova porovnani s Holm korekci
orbital_data$Desetileti <- factor(floor(orbital_data$Rok / 10) * 10)
table(orbital_data$Desetileti)

# H0: Prumery OrbPay jsou ve vsech desetiletich stejne.
# H1: Alespon jedno desetileti ma jiny prumer.
oneway.test(OrbPay ~ Desetileti, data = orbital_data, var.equal = FALSE)

pairwise.t.test(
  orbital_data$OrbPay,
  orbital_data$Desetileti,
  p.adjust.method = "holm",
  pool.sd = FALSE
)

# VYSLEDEK WELCHOVY ANOVA A HOLMOVYCH POROVNANI
# Skupiny podle desetileti: 1950=18, 1960=760, 1970=1108, 1980=1122,
# 1990=813, 2000=597, 2010=816, 2020=1425.
# Welch ANOVA: F(7, 338.39)=80.114, p<2.2e-16; H0 o shodnych prumerech zamitame.
# Po Holmove korekci jsou vyznamne 25 z 28 dvojic. Nevyznamne:
# 1980-2010 (p=0.12340), 1990-2000 (p=0.32947), 2000-2020 (p=0.15272).
# Interpretaci omezujeme s ohledem na sikme rozdeleni a n=18 v 50. letech.

# KROK 10: Korelace roku startu a OrbPay
# Pearsonova korelace meri linearni vztah; Spearmanova poradi-based monotonicitu.
cor.test(orbital_data$Rok, orbital_data$OrbPay, method = "pearson")
cor.test(orbital_data$Rok, orbital_data$OrbPay, method = "spearman", exact = FALSE)

# VYSLEDEK KORELACNI ANALYZY
# Pearson r=0.1094169, 95% CI [0.08562277, 0.13308631], p<2.2e-16:
# slaba kladna linearni souvislost.
# Spearman rho=0.2826391, p<2.2e-16: kladna, ale ne silna monotoni souvislost.
# Velmi male p-hodnoty pri n=6659 neznamenaji velky prakticky efekt.

# KROK 11: Linearni regrese OrbPay podle roku startu
model_linear <- lm(OrbPay ~ Rok, data = orbital_data)
summary(model_linear)
confint(model_linear)

# VYSLEDEK LINEARNI REGRESE
# Sklon Rok=0.08942 t/rok (95% CI 0.06990 az 0.10894; p<2.2e-16).
# R-squared=0.01197; model vysvetluje priblizne 1.2 % variability OrbPay.
# Residual standard error=16.82 t. Vztah je vyznamny, ale linearni model
# ma malou vysvetlovaci silu a vysoke reziduum kvuli rozptylu a odlehlym hodnotam.

# KROK 12: Kvadraticky trend - nelinearni prubeh v case
# Rok centrujeme kolem 2000, aby byly koeficienty citelnejsi.
orbital_data$Rok_c <- orbital_data$Rok - 2000
model_quad <- lm(OrbPay ~ Rok_c + I(Rok_c^2), data = orbital_data)
summary(model_quad)

# Porovnani, zda kvadraticky clen zlepsil linearni model
anova(model_linear, model_quad)

# VYSLEDEK KVADRATICKE REGRESE
# Koeficienty: intercept=9.4148304, Rok_c=0.0517339,
# Rok_c^2=-0.0050393. Kvadraticky model ma R2=0.02191 oproti
# linearniho R2=0.01197; rezidualni chyba klesla z 16.82 na 16.74 t.
# Porovnani vnoreneho modelu: F=67.602, p=2.385e-16; zlepseni je
# statisticky vyznamne, ale celkova vysvetlovaci sila zustava mala.

# KROK 13: Vypocet vrcholu odhadnute kvadraticke krivky
koef_quad <- coef(model_quad)
rok_vrchol <- 2000 - koef_quad["Rok_c"] / (2 * koef_quad["I(Rok_c^2)"])
predikce_vrcholu <- predict(model_quad, newdata = data.frame(Rok_c = rok_vrchol - 2000))
c(rok_vrchol = rok_vrchol, predikovany_prumerny_OrbPay = predikce_vrcholu)

# VRCHOL KVADRATICKEHO MODELU Z JEHO VYPSANYCH KOEFICIENTU:
# Rok priblizne 2005.13; predikovany prumerny OrbPay priblizne 9.55 t.

# KROK 14: Kontrola dalsich ciselnych promennych pro popis a ANOVA
# Nektera pole jsou v CSV textova kvuli pomlckam; prevedeme je na cisla.
sloupce_kontroly <- c("Apogee", "Perigee", "Inc", "Azimuth", "Range", "OrbMass", "OrbPay")
cisla_kontrola <- as.data.frame(lapply(orbital_data[sloupce_kontroly], function(z) {
  z <- trimws(as.character(z))
  z[z %in% c("", "-")] <- NA
  suppressWarnings(as.numeric(z))
}))

audit_cisel <- data.frame(
  Promenna = names(cisla_kontrola),
  Platnych = sapply(cisla_kontrola, function(z) sum(!is.na(z))),
  Chybejicich = sapply(cisla_kontrola, function(z) sum(is.na(z))),
  Minimum = sapply(cisla_kontrola, function(z) if (all(is.na(z))) NA else min(z, na.rm = TRUE)),
  Maximum = sapply(cisla_kontrola, function(z) if (all(is.na(z))) NA else max(z, na.rm = TRUE))
)
audit_cisel
summary(cisla_kontrola$Apogee)
summary(cisla_kontrola$Perigee)
summary(cisla_kontrola$Inc)
table(trimws(orbital_data$Apoflag), useNA = "ifany")

# KROK 15: Provzit Apogee a provest ANOVA podle desetileti
apogee_data <- data.frame(
  Desetileti = orbital_data$Desetileti,
  Apogee = cisla_kontrola$Apogee,
  Apoflag = trimws(orbital_data$Apoflag)
)

table(apogee_data$Apoflag == "?", !is.na(apogee_data$Apogee))

# Nejasne hodnoty s priznakem ? vynechame, ostatni platne hodnoty ponechame.
apogee_potvrzene <- subset(apogee_data, !is.na(Apogee) & Apoflag != "?")
table(apogee_potvrzene$Desetileti)
summary(apogee_potvrzene$Apogee)

oneway.test(Apogee ~ Desetileti, data = apogee_potvrzene, var.equal = FALSE)

pairwise.t.test(
  apogee_potvrzene$Apogee,
  apogee_potvrzene$Desetileti,
  p.adjust.method = "holm",
  pool.sd = FALSE
)
