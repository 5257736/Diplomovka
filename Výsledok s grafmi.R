library("ggpubr")
library("dplyr")
library(car)
library(ggplot2)
library(tidyr)
library(readxl)
library(tidyverse)
install.packages("dplyr") 
library(dplyr)

file_path <- "D:/One Drive Škola/OneDrive - UPJŠ/BodyMTIa_2023-2024.xlsx"  # Uprav, ak máš iný názov alebo cestu
data <- read_excel(file_path)
head(data)  # Prvých pár riadkov
str(data)   # Štruktúra dát


shapiro_P1_2023 <- shapiro.test(data$P1_2023)
shapiro_P1_2024 <- shapiro.test(data$P1_2024)  
shapiro_P2_2023 <- shapiro.test(data$P2_2023)  
shapiro_P2_2024 <- shapiro.test(data$P2_2024)  
shapiro_SP_2023 <- shapiro.test(data$SP_2023) # Tu je p-hodnota <0,05
shapiro_SP_2024 <- shapiro.test(data$SP_2024) #Z výstupu je p-hodnota > 0,05, čo znamená, že distribúcia údajov sa významne nelíši od normálnej distribúcie. Inými slovami, môžeme predpokladať normálnosť.


shapiro_P1_2023$p.value
shapiro_P1_2024$p.value
shapiro_P2_2023$p.value
shapiro_P2_2024$p.value
shapiro_SP_2023$p.value
shapiro_SP_2024$p.value

# Leveneho test pre P1_2023 vs P1_2024
levene_result <- leveneTest(data$P1_2023, data$P1_2024)

# Výstup
cat("Leveneho test: p =", levene_result$`Pr(>F)`[1], "\n")


if (shapiro_P1_2023$p.value > 0.05 & shapiro_P1_2024$p.value > 0.05) { 
  # Normálne rozdelenie
  if (levene_result$`Pr(>F)`[1] > 0.05) {
    t_test <- t.test(data$P1_2023, data$P1_2024, var.equal = TRUE)
    test_name <- "Studentov t-test (rovnaké rozptyly)"
  } else {
    t_test <- t.test(data$P1_2023, data$P1_2024, var.equal = FALSE)
    test_name <- "Welchov t-test (rôzne rozptyly)"
  }
} else {
  t_test <- wilcox.test(data$P1_2023, data$P1_2024)
  test_name <- "Mann-Whitney U test (neparametrický)"
}

cat(test_name, ": p =", t_test$p.value, "\n") # reálne rozdiely neexistujú namerali rovnaké výsledky (teda že platí nulová hypotéza), je až 18,65% (p = 0,1865).



# Leveneho test pre P2_2023 vs P2_2024
levene_result <- leveneTest(data$P2_2023, data$P2_2024)

# Výstup
cat("Leveneho test: p =", levene_result$`Pr(>F)`[1], "\n")


if (shapiro_P2_2023$p.value > 0.05 & shapiro_P2_2024$p.value > 0.05) { 
  # Normálne rozdelenie
  if (levene_result$`Pr(>F)`[1] > 0.05) {
    t_test <- t.test(data$P2_2023, data$P2_2024, var.equal = TRUE)
    test_name <- "Studentov t-test (rovnaké rozptyly)"
  } else {
    t_test <- t.test(data$P2_2023, data$P2_2024, var.equal = FALSE)
    test_name <- "Welchov t-test (rôzne rozptyly)"
  }
} else {
  t_test <- wilcox.test(data$P2_2023, data$P2_2024)
  test_name <- "Mann-Whitney U test (neparametrický)"
}

cat(test_name, ": p =", t_test$p.value, "\n") # reálne rozdiely existujú namerali rôzne výsledky (teda že platí alternatívna hypotéza), je 1,13% (p = 0,0112).



# Leveneho test pre SP_2023 vs SP_2024
levene_result <- leveneTest(data$SP_2023, data$SP_2024)

# Výstup
cat("Leveneho test: p =", levene_result$`Pr(>F)`[1], "\n")


if (shapiro_SP_2023$p.value > 0.05 & shapiro_SP_2024$p.value > 0.05) { 
  # Normálne rozdelenie
  if (levene_result$`Pr(>F)`[1] > 0.05) {
    t_test <- t.test(data$SP_2023, data$SP_2024, var.equal = TRUE)
    test_name <- "Studentov t-test (rovnaké rozptyly)"
  } else {
    t_test <- t.test(data$SP_2023, data$SP_2024, var.equal = FALSE)
    test_name <- "Welchov t-test (rôzne rozptyly)"
  }
} else {
  t_test <- wilcox.test(data$SP_2023, data$SP_2024)
  test_name <- "Mann-Whitney U test (neparametrický)"
}

cat(test_name, ": p =", t_test$p.value, "\n")  # reálne rozdiely existujú namerali rôzne výsledky (teda že platí alternatívna hypotéza), je 1,1% (p = 0,0107).


# Transformácia dát (opravený výber stĺpcov)
data_long <- data %>%
  pivot_longer(
    cols = matches("^(P1|P2|SP)_"),  # Zahrnie všetky písomky vrátane SP
    names_to = c("Pisomka", "Rok"),
    names_sep = "_",
    values_to = "Body",
    values_drop_na = TRUE  # Automaticky odstráni riadky s chýbajúcimi hodnotami
  ) %>%
  mutate(
    Rok = as.factor(Rok),
    Pisomka = factor(Pisomka, levels = c("P1", "P2", "SP"))
  )

# Kontrola dát
print(table(data_long$Pisomka, data_long$Rok))

# Vykreslenie grafu
ggplot(data_long, aes(x = Rok, y = Body, fill = Rok)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") + # modrý priemer
  stat_summary(fun = median, geom = "point", shape = 95, size = 6, color = "yellow") + # žltý medián
  facet_wrap(~Pisomka, ncol = 1) +
  labs(title = "Porovnanie výsledkov P1, P2 a SP v rokoch 2023 a 2024",
       x = "Rok", y = "Počet bodov") +
  theme_minimal()



# Načítanie dát zo súboru
file_path <- "D:/One Drive Škola/OneDrive - UPJŠ/BodyMTIa_2023-2024.xlsx"
data <- read_excel(file_path)

# Zoznam stĺpcov, ktoré chceme analyzovať
columns <- c("P1_2023", "P1_2024", "P2_2023", "P2_2024", "SP_2023", "SP_2024")

# Vytvorenie histogramov pre každý stĺpec
for (col in columns) {
  if (col %in% colnames(data)) {  # Skontroluje, či stĺpec existuje v dátach
    values <- na.omit(data[[col]])  # Odstránenie chýbajúcich hodnôt (NA)
    mean_val <- mean(values)
    median_val <- median(values)
    
    # Vykreslenie histogramu s čiarami pre priemer a medián
    p <- ggplot(data.frame(values), aes(x = values)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed", size = 1.2, show.legend = TRUE) +
      geom_vline(aes(xintercept = median_val), color = "green", linetype = "solid", size = 1.2, show.legend = TRUE) +
      labs(title = paste("Histogram pre", col),
           x = "Hodnoty",
           y = "Frekvencia") +
      annotate("text", x = mean_val, y = max(table(cut(values, breaks = seq(min(values), max(values), by = 5)))), 
               label = paste("Priemer =", round(mean_val, 2)), color = "red", vjust = -1.5) +
      annotate("text", x = median_val, y = max(table(cut(values, breaks = seq(min(values), max(values), by = 5)))) * 0.9, 
               label = paste("Medián =", round(median_val, 2)), color = "green", vjust = -1.5) +
      theme_minimal()
    
    print(p)
  }
}





# Zoznam stĺpcov pre analýzu
columns <- c("P1_2023", "P1_2024", "P2_2023", "P2_2024", "SP_2023", "SP_2024")

# Výpočet globálneho rozsahu pre os x
all_values <- unlist(lapply(columns, function(col) {
  if (col %in% colnames(data)) {
    na.omit(data[[col]])
  }
}))
x_min <- floor(min(all_values, na.rm = TRUE))  # Minimálna hodnota zaokrúhlená nadol
x_max <- ceiling(max(all_values, na.rm = TRUE))  # Maximálna hodnota zaokrúhlená nahor

# Vytvorenie histogramov s rovnakou osou x
for (col in columns) {
  if (col %in% colnames(data)) {
    values <- na.omit(data[[col]])
    mean_val <- mean(values)
    median_val <- median(values)
    
    p <- ggplot(data.frame(values), aes(x = values)) +
      geom_histogram(
        binwidth = 5,
        fill = "lightgreen",
        color = "black",
        alpha = 0.7
      ) +
      geom_vline(
        aes(xintercept = mean_val),
        color = "red",
        linetype = "dashed",
        size = 1.2
      ) +
      geom_vline(
        aes(xintercept = median_val),
        color = "blue",
        linetype = "solid",
        size = 1.2
      ) +
      annotate(
        "text",
        x = mean_val,
        y = Inf,
        label = paste("Priemer =", round(mean_val, 2)),
        color = "red",
        vjust = 2,
        hjust = -0.1
      ) +
      annotate(
        "text",
        x = median_val,
        y = Inf,
        label = paste("Medián =", round(median_val, 2)),
        color = "blue",
        vjust = 4,
        hjust = -0.1
      ) +
      labs(
        title = paste("Histogram pre", col),
        x = "Počet bodov",
        y = "Frekvencia"
      ) +
      xlim(x_min, x_max) +  # Nastavenie spoločného rozsahu
      theme_minimal()
    
    print(p)
  }
}
