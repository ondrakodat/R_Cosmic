# Nova analyza kosmickych startu: orbitalni starty jako hlavni tema.
# Neorbitalni starty se zpracovavaji oddelene, nikdy se neslucuji s orbitalnimi.
# Spusteni v RGUI z adresare projektu. Vsechny grafy a tabulky se ukladaji
# do podadresare nova_analyza.

setwd("C:/Users/revel/OneDrive/Desktop/R_cosmic")
dir.create("nova_analyza", showWarnings = FALSE)

launch <- read.csv("launch_data_original.csv", stringsAsFactors = FALSE,
                   check.names = FALSE)
launch$LaunchCode_clean <- trimws(launch$LaunchCode)
launch$Rok <- suppressWarnings(as.integer(launch$Rok))
launch$Kod_prvni <- substr(launch$LaunchCode_clean, 1, 1)
launch$Kod_vysledek <- substr(launch$LaunchCode_clean, 2, 2)

# Katalog rozlisuje orbitální (O), ostatni startovni kategorie a zaznamy Z,
# ktere nejsou startem. Kategorie mimo O a Z tvorí oddeleny neorbitalni soubor.
orbital_all <- subset(launch, Kod_prvni == "O" & !is.na(Rok))
# Jasne neorbitalni kategorie oddelujeme od hlubokovesmirnych misi (D),
# startu z jinych teles (X) a near-orbitalnich letu (N).
neorbital_kody <- c("M", "T", "A", "S", "H", "R", "L", "Y")
neorbital_all <- subset(launch, Kod_prvni %in% neorbital_kody & !is.na(Rok))
ostatni_kategorie <- subset(launch, !is.na(Kod_prvni) & Kod_prvni != "" &
                              Kod_prvni != "O" & Kod_prvni != "Z" &
                              !Kod_prvni %in% neorbital_kody & !is.na(Rok))
orbital_all$Desetileti <- floor(orbital_all$Rok / 10) * 10

# KROK 1: Zakladni kontrola a pocet zaznamu
dim(launch)
range(launch$Rok, na.rm = TRUE)
table(launch$Kod_prvni, useNA = "ifany")
c(vsechny_zaznamy = nrow(launch), orbitalni_pokusy = nrow(orbital_all),
  neorbitalni_starty = nrow(neorbital_all),
  ostatni_kategorie = nrow(ostatni_kategorie))

# OVERENY VYSLEDEK KROKU 1 (R GUI, 16. 11. 2026)
# dim(launch): 76032 radku a 34 sloupcu; rozsah Rok: 1942-2026.
# Prvni pismeno LaunchCode: A=2675, D=229, H=210, L=12, M=15584,
# N=16, O=7218, R=106, S=49334, T=175, X=11, Y=455, Z=7.
# Hlavni vyber O: 7218 zaznamu, rozsah 1957-2026. Kategorie mimo O
# se zatim nepovazuji vsechny za neorbitalni; D, N, X a Z budou oddeleny.

# KROK 2: Rocni pocty orbitalnich startu a casova osa
rocni_orbital <- aggregate(Launch_Tag ~ Rok, data = orbital_all, FUN = length)
names(rocni_orbital)[2] <- "Pocet_startu"
roky <- data.frame(Rok = seq(min(orbital_all$Rok), max(orbital_all$Rok)))
rocni_orbital <- merge(roky, rocni_orbital, by = "Rok", all.x = TRUE)
rocni_orbital$Pocet_startu[is.na(rocni_orbital$Pocet_startu)] <- 0
write.csv(rocni_orbital, "nova_analyza/pocty_orbitalnich_startu_po_roku.csv",
          row.names = FALSE)

# OVERENY VYSLEDEK KROKU 2 (R GUI, 16. 11. 2026)
# Pocet zaznamu po letech: n=70; 1957=3, 1958=28, 1959=21, 1960=37,
# 1961=49; 2017=90, 2018=107, 2019=102, 2020=109, 2021=144, 2022=182,
# 2023=214, 2024=255, 2025=325, 2026=266. Rok 2026 je neukonceny.

png("nova_analyza/casova_osa_orbitalnich_startu.png", 1800, 1000, res = 150)
plot(Pocet_startu ~ Rok, data = rocni_orbital, type = "o", pch = 16,
     col = "steelblue", xlab = "Rok", ylab = "Počet orbitálních pokusů",
     main = "Orbitální starty podle roku")
dev.off()

# KROK 3B: Rocni pocty podle peti nejcastejsich kodu Agency
top_ag <- c("RVSN", "UNKS", "SPX", "CALT", "AE")
roky_top <- sort(unique(orbital$Rok))
mat_top <- matrix(0, nrow = length(roky_top), ncol = length(top_ag),
                  dimnames = list(roky_top, top_ag))

for (i in seq_along(top_ag)) {
  roky_org <- orbital$Rok[trimws(orbital$Agency) == top_ag[i]]
  pocty_org <- table(roky_org)
  mat_top[names(pocty_org), i] <- as.integer(pocty_org)
}

nazvy_top <- c("RVSN (SSSR)", "UNKS (SSSR)", "SpaceX", "CALT (Čína)",
               "Arianespace")
barvy_top <- c("firebrick", "darkorange", "steelblue", "darkgreen", "purple")
write.csv(data.frame(Rok = roky_top, mat_top, check.names = FALSE),
          "nova_analyza/pocty_startu_top5_organizaci_v_case.csv", row.names = FALSE)

png("nova_analyza/pocty_startu_top5_organizaci_v_case.png",
    width = 1800, height = 1000, res = 150)
matplot(roky_top, mat_top, type = "l", lty = 1, lwd = 2, col = barvy_top,
        xlab = "Rok", ylab = "Počet orbitálních startů",
        main = "Orbitální starty podle organizace a roku",
        sub = "Pět nejčastějších organizací; rok 2026 je neúplný")
legend("topleft", legend = nazvy_top, col = barvy_top, lty = 1,
       lwd = 2, bty = "n")
dev.off()

# VYSLEDEK KROKU 3B
# Soucty startu peti zobrazenych kodu: RVSN=1486, UNKS=898, SPX=718,
# CALT=370, AE=299. Jde o casove rady samostatnych kodu Agency;
# casove navazujici statni organizace nejsou automaticky slouceny.

# POPIS GRAFU KROKU 3
# Graf ukazuje celkovy narust poctu orbitalnich startu v dlouhem obdobi,
# s vyraznym mezirocnim kolisanim. Rok 2026 je neukonceny a nelze jej
# primo srovnavat s uplnymi kalendarnimi roky.

# KROK 3: Organizace a typy raket
# Agency je kod organizace zajistujici start; neni nutne vlastnikem nakladu.
agency_data <- subset(orbital_all, !trimws(Agency) %in% c("", "-"))
raketa_data <- subset(orbital_all, !trimws(LV_Type) %in% c("", "-"))
pocty_agency <- sort(table(trimws(agency_data$Agency)), decreasing = TRUE)
pocty_raket <- sort(table(trimws(raketa_data$LV_Type)), decreasing = TRUE)
# Top 10 Agency kodu: RVSN=1486, UNKS=898, SPX=718, CALT=370, AE=299,
# SAST=278, AFSSD=243, VKSR=197, FKA=183, GSFC=175.
# Top 10 LV_Type: Falcon 9=691, Soyuz-U=633, Kosmos 11K65M=446,
# Voskhod 11A57=299, Molniya 8K78M=259, Soyuz-U-PVB=154,
# Space Shuttle=137, Kosmos 11K63=126, Tsiklon-3=122, Proton-K/DM-2=111.
write.csv(as.data.frame(pocty_agency),
          "nova_analyza/pocty_startu_podle_agency.csv", row.names = FALSE)
write.csv(as.data.frame(pocty_raket),
          "nova_analyza/pocty_startu_podle_rakety.csv", row.names = FALSE)
top_agency <- names(head(pocty_agency, 10))
agency_rok <- subset(agency_data, trimws(Agency) %in% top_agency)
agency_rok$Agency <- trimws(agency_rok$Agency)
rocne_agency <- as.data.frame(table(agency_rok$Rok, agency_rok$Agency),
                              stringsAsFactors = FALSE)
names(rocne_agency) <- c("Rok", "Agency", "Pocet_startu")
rocne_agency$Rok <- as.integer(as.character(rocne_agency$Rok))
write.csv(rocne_agency, "nova_analyza/rocni_pocty_top10_agency.csv",
          row.names = FALSE)

png("nova_analyza/top_organizace.png", 1800, 1000, res = 150)
barplot(head(pocty_agency, 10), las = 2, col = "steelblue",
        ylab = "Počet orbitálních startů",
        main = "Nejaktivnější organizace (kódy GCAT)")
dev.off()

# KROK 4: Uspechy a neuspechy
# OS = plny uspech, OF = plne selhani. U OSxx/OFxx udava ciselny suffix
# procento uspechu. OE/OA znamenaji vybuch na rampe / preruseni pred vzletem.
kod <- orbital_all$LaunchCode_clean
orbital_all$Stav <- ifelse(kod == "OS", "Plný úspěch",
                    ifelse(kod == "OF", "Plné selhání",
                    ifelse(grepl("^O[SF][0-9]{2}$", kod), "Částečný úspěch",
                    ifelse(kod == "OE", "Výbuch na rampě",
                    ifelse(kod == "OA", "Přerušení před vzletem", "Neznámý/jiný")))))
table(orbital_all$Stav, useNA = "ifany")
stav_agency <- as.data.frame(table(trimws(agency_data$Agency), agency_data$Stav))
names(stav_agency) <- c("Agency", "Stav", "Pocet")
write.csv(stav_agency, "nova_analyza/uspesnost_podle_agency.csv", row.names = FALSE)

# Pro prumernou uspesnost pouzijeme procenta z OS, OF a parcialnich kodu.
orbital_all$Uspech_pct <- NA_real_
orbital_all$Uspech_pct[kod == "OS"] <- 100
orbital_all$Uspech_pct[kod == "OF"] <- 0
parcialni_kod <- grepl("^O[SF][0-9]{2}$", kod)
orbital_all$Uspech_pct[parcialni_kod] <- as.numeric(sub("^O[SF]", "", kod[parcialni_kod]))

c(pocet_hodnocenych_startu = sum(!is.na(orbital_all$Uspech_pct)),
  prumerna_uspesnost_pct = mean(orbital_all$Uspech_pct, na.rm = TRUE),
  pocet_vybuchu_na_rampe = sum(kod == "OE"),
  pocet_prerusenych_startu = sum(kod == "OA"))

uspech_data <- subset(orbital_all, !is.na(Uspech_pct) &
                        !trimws(Agency) %in% c("", "-"))
uspech_data$Agency <- trimws(uspech_data$Agency)
uspech_agency <- aggregate(Uspech_pct ~ Agency, data = uspech_data, FUN = mean)
names(uspech_agency) <- c("Agency", "Prumerna_uspesnost_pct")
write.csv(uspech_agency, "nova_analyza/podil_uspechu_podle_agency.csv",
          row.names = FALSE)

# VYSLEDEK KROKU 4 (podle R GUI vystupu LaunchCode)
# OS=6662, OF=424; parcialni kody celkem=115. OE=11, OA=6.
# Vahovana prumerna uspesnost pro 7201 realne vypustenych orbitalnich pokusu
# je 93.315 %. OE a OA vykazujeme zvlast, protoze nedoslo ke vzletu.
# Nejvyssi pocty Agency kodu: RVSN n=1482, uspesnost=92.04453 %;
# UNKS n=898, 96.29733 %; SPX n=717, 99.05858 %;
# CALT n=370, 96.54054 %; AE n=299, 96.50502 %.

top5_kody <- c("RVSN", "UNKS", "SPX", "CALT", "AE")
uspech_pocty <- aggregate(Uspech_pct ~ Agency, data = uspech_data, FUN = length)
names(uspech_pocty)[2] <- "Pocet_hodnocenych"
uspech_top5 <- merge(uspech_pocty, uspech_agency, by = "Agency")
uspech_top5 <- subset(uspech_top5, Agency %in% top5_kody)
uspech_top5 <- uspech_top5[match(top5_kody, uspech_top5$Agency), ]
write.csv(uspech_top5, "nova_analyza/uspesnost_top5_organizaci.csv", row.names = FALSE)

# Graf uspešnosti je horizontalni, aby se vesly delsi nazvy organizaci.
nazvy_uspech <- c("RVSN - SSSR", "UNKS - SSSR", "SpaceX", "CALT - Čína",
                  "Arianespace")
barvy_uspech <- c("firebrick", "darkorange", "steelblue", "darkgreen", "purple")
png("nova_analyza/uspesnost_top5_organizaci.png", 1800, 1000, res = 150)
par(mar = c(4, 12, 4, 3))
pozice <- barplot(uspech_top5$Prumerna_uspesnost_pct,
                  names.arg = nazvy_uspech, horiz = TRUE,
                  col = barvy_uspech, xlim = c(0, 108),
                  xlab = "Vážená úspěšnost (%)",
                  main = "Úspěšnost orbitálních startů podle organizace",
                  cex.names = 0.95)
text(uspech_top5$Prumerna_uspesnost_pct + 1, pozice,
     labels = paste0(round(uspech_top5$Prumerna_uspesnost_pct, 1), "%"),
     adj = 0, cex = 0.95)
dev.off()

# KROK 5: OrbPay a popisna statistika
# Nuly nezamenujeme automaticky za chybejici; hlavni analyza nakladu pouziva
# uspesne orbitalni starty s kladnym OrbPay a presnym kodem OS.
orbital_all$OrbPay_num <- suppressWarnings(as.numeric(orbital_all$OrbPay))
naklad <- subset(orbital_all, LaunchCode_clean == "OS" &
                   !is.na(OrbPay_num) & OrbPay_num > 0)
summary(naklad$OrbPay_num)
sd(naklad$OrbPay_num)
var(naklad$OrbPay_num)
x <- naklad$OrbPay_num
prumer_orbpay <- mean(x)
sd_pop_orbpay <- sqrt(mean((x - prumer_orbpay)^2))
c(sikmost = mean((x - prumer_orbpay)^3) / sd_pop_orbpay^3,
  spicatost_exces = mean((x - prumer_orbpay)^4) / sd_pop_orbpay^4 - 3)
naklad$Desetileti <- floor(naklad$Rok / 10) * 10
orbpay_souhrn <- do.call(rbind, lapply(split(naklad$OrbPay_num, naklad$Desetileti),
  function(x) data.frame(n = length(x), prumer = mean(x), median = median(x),
                         smerodatna_odchylka = sd(x), minimum = min(x),
                         maximum = max(x))))
orbpay_souhrn$Desetileti <- as.integer(rownames(orbpay_souhrn))
orbpay_souhrn <- orbpay_souhrn[, c("Desetileti", "n", "prumer", "median",
                                   "smerodatna_odchylka", "minimum", "maximum")]
write.csv(orbpay_souhrn, "nova_analyza/orbpay_podle_desetileti.csv", row.names = FALSE)

# VYSLEDEK POPISNE STATISTIKY OrbPay (R GUI, 16. 11. 2026)
# Z 6662 plne uspesnych startu maji 3 nulovy a 6659 kladny OrbPay;
# chybejici hodnoty nejsou. Kladny vyber: min=0.002 t, Q1=1.165 t,
# median=3.112 t, prumer=6.936 t, Q3=6.300 t, max=132.025 t.
# SD=16.92101 t, rozptyl=286.3207 t^2, sikmost=5.940209,
# exces spicatosti=36.192287. Rozdeleni je silne pravostranne vychylene.

png("nova_analyza/histogram_orbpay.png", 1400, 900, res = 150)
hist(naklad$OrbPay_num, breaks = 50, col = "lightblue", border = "white",
     xlab = "OrbPay (tuny)", main = "Hmotnost nákladu u úspěšných orbitálních startů")
dev.off()

# KROK 5B: OrbPay podle peti nejcastejsich typu raket
naklad$typ_rakety <- trimws(as.character(naklad$LV_Type))
pocty_raket_naklad <- sort(table(naklad$typ_rakety), decreasing = TRUE)
top5_rakety <- names(head(pocty_rakety_naklad, 5))
naklad_top5 <- subset(naklad, typ_rakety %in% top5_rakety)
souhrn_raket <- do.call(rbind, lapply(top5_rakety, function(r) {
  hodnoty <- naklad_top5$OrbPay_num[naklad_top5$typ_rakety == r]
  data.frame(typ_rakety = r, pocet = length(hodnoty), prumer_t = mean(hodnoty),
             median_t = median(hodnoty), minimum_t = min(hodnoty),
             maximum_t = max(hodnoty))
}))
write.csv(souhrn_raket, "nova_analyza/orbpay_top5_raket.csv", row.names = FALSE)

# OVERENY VYSLEDEK KROKU 5B (R GUI, 16. 11. 2026)
# Falcon 9 n=687, prumer=12.8494 t, median=15.500 t, rozsah 0.330-18.940 t.
# Soyuz-U n=614, prumer=6.5669 t, median=6.300 t, rozsah 5.400-9.804 t.
# Kosmos 11K65M n=420, prumer=0.7209 t, median=0.800 t, rozsah 0.104-2.005 t.
# Voskhod 11A57 n=286, prumer=5.7011 t, median=5.8555 t, rozsah 4.700-6.840 t.
# Molniya 8K78M n=246, prumer=1.8631 t, median=1.978 t, rozsah 1.173-2.728 t.

png("nova_analyza/orbpay_top5_raket_boxplot.png", 1800, 1100, res = 150)
par(mar = c(6, 5, 4, 2))
boxplot(OrbPay_num ~ typ_rakety, data = naklad_top5, las = 1,
        cex.axis = 0.9, ylim = c(0, 20),
        col = c("steelblue", "darkorange", "firebrick", "darkgreen", "purple"),
        xlab = "Typ rakety", ylab = "OrbPay (tuny)",
        main = "Hmotnost nákladu podle typu rakety")
dev.off()

# KROK 6: Test obdobi a ANOVA pro typy raket
naklad$Obdobi <- ifelse(naklad$Rok < 2000, "1957-1999", "2000-2026")
t.test(OrbPay_num ~ Obdobi, data = naklad, var.equal = FALSE)

# VYSLEDEK WELCHOVA T-TESTU (R GUI, 16. 11. 2026)
# Skupiny: 1957-1999 n=3821, prumer=6.037645 t, median=2.150 t;
# 2000-2026 n=2838, prumer=8.146028 t, median=4.462 t.
# t=-5.0779, df=6281.5, p=3.926e-07; 95% CI rozdilu starsi-minus-novejsi
# [-2.922331, -1.294434] t. H0 o shodnych prumerech zamitame na hladine 0.05.
# Vysledek plati pro OS starty s kladnym OrbPay; prumer ovlivnuji krajni hodnoty.

oneway.test(OrbPay_num ~ typ_rakety, data = naklad_top5, var.equal = FALSE)
pairwise.t.test(naklad_top5$OrbPay_num, naklad_top5$typ_rakety,
                p.adjust.method = "holm", pool.sd = FALSE)

# VYSLEDEK WELCHOVY ANOVA A HOLMOVYCH POROVNANI (R GUI, 16. 11. 2026)
# Welchova ANOVA: F(4, 948.08)=14639, p<2.2e-16; prumery se lisi.
# Vsech 10 dvojic top 5 typu raket je vyznamnych po Holm korekci
# (u vsech p<2.2e-16). Rozdily jsou popisne a skupiny maji ruzne rozptyly.

# KROK 7: Korelace a linearni/kvadraticky trend rocniho poctu
cor.test(rocni_orbital$Rok, rocni_orbital$Pocet_startu, method = "spearman",
         exact = FALSE)

# VYSLEDEK KORELACE (R GUI, 16. 11. 2026; 70 rocniku)
# Pearson r=0.3365314, 95% CI [0.1102777, 0.5296247], p=0.004392:
# kladna linearni souvislost.
# Spearman rho=0.05666734, p=0.6413: monotoni souvislost se nepotvrdila.
# Rozdil mezi metodami ukazuje, ze rocni pocty nepostupuji monotonne;
# p-hodnoty nevysvetluji pricinu a posledni rok je neuplny.

rocni_orbital$Rok_c <- rocni_orbital$Rok - 2000
model_pocet_linear <- lm(Pocet_startu ~ Rok_c, data = rocni_orbital)
model_pocet_quad <- lm(Pocet_startu ~ Rok_c + I(Rok_c^2), data = rocni_orbital)
summary(model_pocet_linear)
summary(model_pocet_quad)
anova(model_pocet_linear, model_pocet_quad)

# VYSLEDEK LINEARNI A KVADRATICKE REGRESE ROCNICH POCTU
# Linearni sklon=0.8515 startu/rok (p=0.004392), R2=0.1133, RSE=48.84.
# Kvadraticky clen=0.03397 (p=0.032611); R2=0.1721, upravene R2=0.1474,
# RSE=47.55. Porovnani modelu: F(1,67)=4.7617, p=0.03261.
# Kvadraticky model statisticky zlepsuje shodu, ale stale vysvetluje jen
# 17.2 % variability; vysoke pocty v nekolika poslednich letech maji vliv.

png("nova_analyza/regrese_rocniho_poctu_startu.png", 1800, 1000, res = 150)
plot(rocni_orbital$Rok, rocni_orbital$Pocet_startu, pch = 16,
     col = "steelblue", xlab = "Rok", ylab = "Počet orbitálních startů",
     main = "Lineární a kvadratický trend ročních počtů",
     sub = "Rok 2026 je neúplný")
lines(rocni_orbital$Rok, predict(model_pocet_linear), col = "firebrick", lwd = 2)
lines(rocni_orbital$Rok, predict(model_pocet_quad), col = "darkgreen", lwd = 2)
legend("topleft", legend = c("Lineární model", "Kvadratický model"),
       col = c("firebrick", "darkgreen"), lty = 1, lwd = 2, bty = "n")
dev.off()

# KROK 8: Neorbitalni starty - oddeleny doplnkovy souhrn
rocni_neorb <- aggregate(Launch_Tag ~ Rok, data = neorbital_all, FUN = length)
names(rocni_neorb)[2] <- "Pocet_startu"
write.csv(rocni_neorb, "nova_analyza/neorbitalni_pocty_po_roku.csv",
          row.names = FALSE)
write.csv(as.data.frame(sort(table(neorbital_all$Kod_prvni), decreasing = TRUE)),
          "nova_analyza/neorbitalni_kategorie.csv", row.names = FALSE)
write.csv(as.data.frame(sort(table(ostatni_kategorie$Kod_prvni), decreasing = TRUE)),
          "nova_analyza/ostatni_kategorie_mimo_orbitalni_a_neorbitalni.csv",
          row.names = FALSE)

# DOPLNKOVY VYSLEDEK NEORBITALNI CASTI (R GUI, 16. 11. 2026)
# Jasne neorbitalni kategorie: A=2675, H=210, L=12, M=15584, R=106,
# S=49334, T=175, Y=455; celkem n=68551, roky 1942-2026.
# D=229, N=16, X=11 a Z=7 jsou vykazany oddelene, nikoli jako neorbitalni.
# Pokryti neorbitalnich startu je v GCAT mene uplne nez pokryti orbitalnich.

# KROK 9: Audit chybejicich a nepouzitelnych hodnot
promenne <- c("Agency", "LV_Type", "Platform", "Launch_Site", "OrbPay")
audit <- data.frame(Promenna = promenne,
                    Chybejici = sapply(launch[promenne], function(x)
                      sum(is.na(x) | trimws(as.character(x)) %in% c("", "-"))))
write.csv(audit, "nova_analyza/audit_chybejicich_hodnot.csv", row.names = FALSE)

# KROK 10: Audit ciselnych promennych pro uspesne starty s kladnym nakladem
# Vstupni audit z R GUI (16. 11. 2026), n=6659:
# OrbMass: 6659 platnych, 1 ruzna hodnota, min=max=0; nepouzitelne pro srovnavani.
# OrbPay: 6659 platnych, 2073 ruznych, rozsah 0.002-132.025 t.
# Apogee: 6093 platnych, 2467 ruznych, rozsah 51-1048000; Apoflag '?' u 175 zaznamu.
# Perigee: 1286 platnych, 391 ruznych, rozsah -2345 az 1088; zaporne minimum overit.
# Inc: 1286 platnych, 275 ruznych, rozsah 2.9-143.4.
# Azimuth: pouze jedna platna hodnota 0; Range bez platnych hodnot.
# Pred ANOVA dalsich promennych nejprve zkontrolovat pocty platnych hodnot podle obdobi.
# VYSLEDEK POKRYTI CISELNYCH PROMENNYCH PODLE DESETILETI (R GUI, 16. 11. 2026)
# Desetileti: OrbPay / Apogee potvrzene / Perigee / Inc
# 1950: 18/18/2/2; 1960: 760/760/88/88; 1970: 1108/1108/80/80;
# 1980: 1122/1119/17/17; 1990: 813/801/43/43; 2000: 597/573/121/121;
# 2010: 816/767/198/198; 2020: 1425/772/737/737.
# Apogee v tomto poctu nezahrnuje Apoflag '?'. Pro ANOVA perigea a sklonu
# vynechame 50. leta (n=2); 2020. leta jsou nekompletni (2020-2026).
# KROK 11: Welchovy ANOVY ciselnych parametru podle desetileti
# Desetileti 1950 vynechame u perigea a sklonu, protoze n=2.
# U Apogee ignorujeme Apoflag '?'; ostatni promenny pouziji platne cislo.
flag_ap <- trimws(as.character(naklad$Apoflag))
flag_ap[is.na(flag_ap)] <- ''

analyzni_data <- data.frame(
  Desetileti = naklad$Desetileti,
  Apogee = ifelse(flag_ap == '?', NA_real_, cisla$Apogee),
  Perigee = cisla$Perigee,
  Inc = cisla$Inc
)
for (prom in c('Apogee', 'Perigee', 'Inc')) {
  d <- analyzni_data[!is.na(analyzni_data[[prom]]) &
                       as.integer(as.character(analyzni_data$Desetileti)) >= 1960, ]
  cat('\n', prom, '\n')
  print(table(d$Desetileti))
  print(oneway.test(d[[prom]] ~ d$Desetileti, var.equal = FALSE))
}
# VYSLEDKY WELCHOVY ANOVY A HOLMOVYCH POROVNANI PODLE DESETILETI
# (R GUI, 16. 11. 2026; od 1960; Apogee bez Apoflag '?')
# Apogee F(6,2438.8)=29.24, p<2.2e-16. Holm vyznamne dvojice:
# 1960-1970, 1960-1980, 1960-1990, 1960-2000, 1960-2010;
# 1970-1990, 1970-2000, 1970-2010, 1970-2020;
# 1980-1990, 1980-2000, 1980-2010, 1980-2020;
# 1990-2020, 2000-2020, 2010-2020. Ostatni dvojice nejsou vyznamne.
# Perigee F(6,144.79)=8.5256, p=6.18e-08; vyznamne 1960-2000,
# 1970-2000 a 1970-2020.
# Inc F(6,145.13)=14.155, p=1.185e-12; vyznamne 1960-1970,
# 1970-2000, 1970-2010, 1960-2020 a 2000-2020.
# KROK 12: Popisne statistiky orbitalnich parametru (R GUI, 16. 11. 2026)
souhrn_momenty <- do.call(rbind, lapply(
  c("Apogee", "Perigee", "Inc"),
  function(prom) {
    z <- analyzni_data[[prom]]
    z <- z[!is.na(z)]
    prumer <- mean(z)
    sd_pop <- sqrt(mean((z - prumer)^2))
    data.frame(
      promenna = prom, n = length(z), prumer = prumer,
      rozptyl = var(z), smerodatna_odchylka = sd(z),
      sikmost = mean((z - prumer)^3) / sd_pop^3,
      spicatost_exces = mean((z - prumer)^4) / sd_pop^4 - 3,
      minimum = min(z), maximum = max(z)
    )
  }
))
# Overene: Apogee n=5918, prumer=10653.41399, var=590721100,
# SD=24304.75403, sikmost=14.9308836, exces=566.5442219, min=51, max=1048000.
# Perigee n=1286, prumer=290.78927, var=53132.99, SD=230.50595,
# sikmost=-2.903325, exces=39.5473631, min=-2345, max=1088.
# Inc n=1286, prumer=61.68733, var=872.6062, SD=29.53991,
# sikmost=0.1806697, exces=-0.9661801, min=2.9, max=143.4.
