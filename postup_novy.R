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

# KROK 5: OrbPay a popisna statistika
# Nuly nezamenujeme automaticky za chybejici; hlavni analyza nakladu pouziva
# uspesne orbitalni starty s kladnym OrbPay a presnym kodem OS.
orbital_all$OrbPay_num <- suppressWarnings(as.numeric(orbital_all$OrbPay))
naklad <- subset(orbital_all, LaunchCode_clean == "OS" &
                   !is.na(OrbPay_num) & OrbPay_num > 0)
summary(naklad$OrbPay_num)
sd(naklad$OrbPay_num)
naklad$Desetileti <- floor(naklad$Rok / 10) * 10
orbpay_souhrn <- do.call(rbind, lapply(split(naklad$OrbPay_num, naklad$Desetileti),
  function(x) data.frame(n = length(x), prumer = mean(x), median = median(x),
                         smerodatna_odchylka = sd(x), minimum = min(x),
                         maximum = max(x))))
orbpay_souhrn$Desetileti <- as.integer(rownames(orbpay_souhrn))
orbpay_souhrn <- orbpay_souhrn[, c("Desetileti", "n", "prumer", "median",
                                   "smerodatna_odchylka", "minimum", "maximum")]
write.csv(orbpay_souhrn, "nova_analyza/orbpay_podle_desetileti.csv", row.names = FALSE)

png("nova_analyza/histogram_orbpay.png", 1400, 900, res = 150)
hist(naklad$OrbPay_num, breaks = 50, col = "lightblue", border = "white",
     xlab = "OrbPay (tuny)", main = "Hmotnost nákladu u úspěšných orbitálních startů")
dev.off()

# KROK 6: Test obdobi a ANOVA pro typy raket
naklad$Obdobi <- ifelse(naklad$Rok < 2000, "1957-1999", "2000-2026")
t.test(OrbPay_num ~ Obdobi, data = naklad, var.equal = FALSE)

naklad <- subset(naklad, !trimws(LV_Type) %in% c("", "-"))
raketa_pocty <- sort(table(trimws(naklad$LV_Type)), decreasing = TRUE)
top_rakety <- names(raketa_pocty[raketa_pocty >= 30])
naklad_top_rakety <- subset(naklad, trimws(LV_Type) %in% top_rakety)
naklad_top_rakety$LV_Type <- trimws(naklad_top_rakety$LV_Type)
oneway.test(OrbPay_num ~ LV_Type, data = naklad_top_rakety,
            var.equal = FALSE)
pairwise.t.test(naklad_top_rakety$OrbPay_num, naklad_top_rakety$LV_Type,
                p.adjust.method = "holm", pool.sd = FALSE)

# KROK 7: Korelace a linearni/kvadraticky trend rocniho poctu
cor.test(rocni_orbital$Rok, rocni_orbital$Pocet_startu, method = "spearman",
         exact = FALSE)
rocni_orbital$Rok_c <- rocni_orbital$Rok - 2000
model_pocet_linear <- lm(Pocet_startu ~ Rok_c, data = rocni_orbital)
model_pocet_quad <- lm(Pocet_startu ~ Rok_c + I(Rok_c^2), data = rocni_orbital)
summary(model_pocet_linear)
summary(model_pocet_quad)
anova(model_pocet_linear, model_pocet_quad)

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

# KROK 9: Audit chybejicich a nepouzitelnych hodnot
promenne <- c("Agency", "LV_Type", "Platform", "Launch_Site", "OrbPay")
audit <- data.frame(Promenna = promenne,
                    Chybejici = sapply(launch[promenne], function(x)
                      sum(is.na(x) | trimws(as.character(x)) %in% c("", "-"))))
write.csv(audit, "nova_analyza/audit_chybejicich_hodnot.csv", row.names = FALSE)

