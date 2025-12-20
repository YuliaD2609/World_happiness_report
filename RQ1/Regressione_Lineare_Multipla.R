df <- read.csv(file.choose(), header = TRUE, sep = ",")



# Suddividiamo le variabili in numeriche e categoriche
#Tutte sono numeriche eccetto country e cntry_code

# Variabili numeriche
numeric_vars <- names(df)[sapply(df, is.numeric)]
numeric_vars

# Variabili categoriche (fattori o caratteri)
categorical_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
categorical_vars


# Numero totale di missing values per paese
na_mat <- is.na(df[, numeric_vars])

missing_by_country_initial <- aggregate(na_mat,
                                        by=list(country=df$country),
                                        FUN=sum)
cat("Numero di missing values per paese")
missing_by_country_initial

# Somma di missing values su tutte le variabili numeriche
missing_by_country_initial$total_missing <- rowSums(missing_by_country_initial[numeric_vars])

# Ordine decrescente eliminando i paesi con 0 missing values
missing_by_country_initial <- missing_by_country_initial[missing_by_country_initial$total_missing > 0, ]
missing_by_country_initial <- missing_by_country_initial[order(-missing_by_country_initial$total_missing), ]

colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5"))(nrow(missing_by_country_initial))
var_labels <- c(
  log_gdp = "PIL pro capite",
  social_support = "Supporto sociale",
  positive_affect = "Emozioni positive",
  negative_affect = "Emozioni negative",
  healthy_life = "Aspettativa di vita sana",
  freedom = "Libertà di scelta nella vita",
  generosity = "Generosità",
  corruption = "Percezione della corruzione"
)

library(dplyr)
#medie per ogni variabile
df_mean <- df %>%
  group_by(country) %>%
  summarise(
    happiness_score = mean(happiness_score, na.rm = TRUE),
    log_gdp = mean(log_gdp_per_capita_sc, na.rm = TRUE),
    social_support = mean(social_support_sc, na.rm = TRUE),
    healthy_life = mean(healthy_life_expectancy_at_birth_sc, na.rm = TRUE),
    freedom = mean(freedom_to_make_life_choices_sc, na.rm = TRUE),
    generosity = mean(generosity_sc, na.rm = TRUE),
    corruption = mean(perceptions_of_corruption_sc, na.rm = TRUE),
    positive_affect = mean(positive_affect_sc, na.rm = TRUE),
    negative_affect = mean(negative_affect_sc, na.rm = TRUE)
  )

model <- lm(happiness_score ~ 
              log_gdp +
              social_support +
              healthy_life +
              freedom +
              generosity +
              corruption +
              positive_affect +
              negative_affect,
            data = df_mean)

summary(model)

# grafico dei coefficienti
coef <- summary(model)$coefficients[-1,1]
# Sostituzione dei nomi tecnici con le label testuali
print(coef)

names(coef) <- var_labels[names(coef)]
print(coef)

par(mar = c(10, 5, 4, 2))


barplot(coef,
        las=2,
        ylim= c(-0.2,0.4),
        col=colors_green,
        main="Effetto delle variabili sulla felicità",
        ylab="Coefficiente stimato",
        cex.names = 0.7,)
abline(h=0, col="black")

confint(model)



