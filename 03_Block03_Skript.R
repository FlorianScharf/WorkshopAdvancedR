# =======================
# Hilfsfunktionen
# =======================

install_and_load <- function(pakete) {
  install.packages(setdiff(pakete, rownames(installed.packages())))
  invisible(lapply(pakete, library, character.only = TRUE))
  print(paste("Pakete geladen:", paste(pakete, collapse = ", ")))
}

berechne_skalenwert <- function(daten, id_var = "id", zeit_var = "zeitpunkt", wert_var = "wert") {
  gruppiert <- group_by(daten, .data[[id_var]], .data[[zeit_var]])
  summarise(gruppiert, skalenwert = mean(.data[[wert_var]]))
}

deskriptiv_mit_ci <- function(daten, gruppe, wert) {
  gruppiert <- group_by(daten, .data[[gruppe]])
  summarise(gruppiert,
            M = round(mean(.data[[wert]]), 2),
            SD = round(sd(.data[[wert]]), 2),
            n = n(),
            CI_low = round(M - 1.96 * SD / sqrt(n), 2),
            CI_up  = round(M + 1.96 * SD / sqrt(n), 2)
  )
}

nice.time <- function() {
  format(Sys.time(), "%d_%m_%Y_%H-%M-%S")
}
# Beispiel: nice.time() -> "23_03_2025_14-45-12"

# =======================
# Pakete laden
# =======================
install_and_load(c("tidyverse", "gt", "afex", "emmeans"))

# =======================
# Daten simulieren
# =======================
set.seed(123)
daten_long <- tibble(
  id = rep(1:100, each = 6),
  zeitpunkt = rep(rep(c("t1", "t2"), each = 3), times = 100),
  item = rep(paste0("item", 1:3), times = 200),
  wert = rnorm(600, mean = 3.5, sd = 1) + rep(c(0, 0.3), each = 300)
)

# =======================
# Skalen bilden
# =======================
skala <- berechne_skalenwert(daten_long)

# =======================
# Long zu Wide
# =======================
daten_wide <- pivot_wider(skala,
                          names_from = "zeitpunkt",
                          values_from = "skalenwert",
                          names_prefix = "skala_"
)

# =======================
# Statistische Analysen
# =======================

# t-Test
t_test <- t.test(daten_wide$skala_t1, daten_wide$skala_t2, paired = TRUE)

# Regressionsanalyse
lm_fit <- lm(skala_t2 ~ skala_t1, data = daten_wide)

# ANOVA mit afex
skala$zeitpunkt <- factor(skala$zeitpunkt, levels = c("t1", "t2"))
anova_afex <- aov_ez(
  id = "id",
  dv = "skalenwert",
  data = skala,
  within = "zeitpunkt",
  type = 3
)

# =======================
# Grafiken
# =======================

# Verlauf je Person + Mittelwert (korrigiert: linewidth statt size)
ggplot(skala, aes(x = zeitpunkt, y = skalenwert, group = id)) +
  geom_line(alpha = 0.2, linewidth = 0.5) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "blue", linewidth = 1.5) +
  labs(title = "Verlauf der Skalenwerte", y = "Skalenwert", x = "Messzeitpunkt") +
  theme_minimal()

# Boxplot je Zeitpunkt
ggplot(skala, aes(x = zeitpunkt, y = skalenwert)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", color = "darkblue", size = 3) +
  labs(title = "Verteilung der Skalenwerte nach Messzeitpunkt") +
  theme_minimal()

# =======================
# Tabelle
# =======================
deskriptiv <- deskriptiv_mit_ci(skala, gruppe = "zeitpunkt", wert = "skalenwert")

# Tabelle klassisch anzeigen
tabelle_export <- select(deskriptiv, zeitpunkt, M, SD, CI_low, CI_up)
print(tabelle_export)

# =======================
# Ergebnisse anzeigen
# =======================
print(t_test)
print(summary(lm_fit))
print(anova_afex)