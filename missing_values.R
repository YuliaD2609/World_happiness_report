df <- read.csv(file.choose(), header = TRUE, sep = ",")

numeric_vars <- c("log_gdp_per_capita",
                  "social_support",
                  "healthy_life_expectancy_at_birth",
                  "freedom_to_make_life_choices",
                  "generosity",
                  "perceptions_of_corruption",
                  "positive_affect",
                  "negative_affect"
                  )

# numero massimo consentito di NA per riga
sum(rowSums(is.na(df)) > 3)

df <- df[rowSums(is.na(df)) <= 3, ]

# numero di osservazioni per paese
country_counts <- table(df$country)

# si tolgono i paesi con meno di due righe
valid_countries <- names(country_counts[country_counts >= 2])
setdiff(names(country_counts), valid_countries)

df <- df[df$country %in% valid_countries, ]

# paesi
countries <- unique(df$country)

for (v in numeric_vars) {
  
  # media globale per fallback
  global_mean <- mean(df[[v]], na.rm = TRUE)
  
  country_means <- tapply(
    df[[v]],
    df$country,
    function(x){
      n_total  <- length(x)
      n_non_na <- sum(!is.na(x))
      perc_non_na <- n_non_na / n_total
      
      # se meno del 50% dei valori è presente → NON imputare
      if (perc_non_na < 0.50){
        return(NA_real_)
      }
      
      return(mean(x, na.rm = TRUE))
    }
  )
  
  for (i in seq_len(nrow(df))) {
    if (is.na(df[i, v])) {
      cty <- df[i, "country"]
      cmean <- country_means[[as.character(cty)]]
      
      if (is.na(cmean)) {
        df[i, v] <- global_mean     # fallback globale
      } else {
        df[i, v] <- cmean
      }
    }
  }
}


post_n_missing <- sapply(df_imputed[numeric_vars], function(x) sum(is.na(x)))
post_missing_df <- data.frame(var = names(post_n_missing), n_missing = as.integer(post_n_missing))
print(post_missing_df)

#ricostruzione sc
df$log_gdp_per_capita_sc <- scale(df$log_gdp_per_capita)
df$social_support_sc <- scale(df$social_support)
df$healthy_life_expectancy_at_birth_sc <- scale(df$healthy_life_expectancy_at_birth)
df$freedom_to_make_life_choices_sc <- scale(df$freedom_to_make_life_choices)
df$generosity_sc <- scale(df$generosity)
df$perceptions_of_corruption_sc <- scale(df$perceptions_of_corruption)
df$positive_affect_sc <- scale(df$positive_affect)
df$negative_affect_sc <- scale(df$negative_affect)

sapply(df[c(numeric_vars)], function(x) sum(is.na(x)))
