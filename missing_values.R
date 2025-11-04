df <- read.csv(file.choose(), header = TRUE, sep = ",")

numeric_vars <- c("log_gdp_per_capita",
                  "social_support",
                  "healthy_life_expectancy_at_birth",
                  "freedom_to_make_life_choices",
                  "generosity",
                  "perceptions_of_corruption",
                  "positive_affect",
                  "negative_affect",
                  "log_gdp_per_capita_sc",
                  "social_support_sc",
                  "healthy_life_expectancy_at_birth_sc",
                  "freedom_to_make_life_choices_sc",
                  "generosity_sc",
                  "perceptions_of_corruption_sc",
                  "positive_affect_sc",
                  "negative_affect_sc"
                  )

df_imputed <- df

# paesi
countries <- unique(df$country)

for (v in numeric_vars) {
  global_mean <- mean(df[[v]], na.rm = TRUE)
  
  country_means <- tapply(df[[v]], df$country, function(x) if(all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
  
  for (i in seq_len(nrow(df_imputed))) {
    if (is.na(df_imputed[i, v])) {
      cty <- df_imputed[i, "country"]
      cmean <- country_means[[as.character(cty)]]
      if (is.na(cmean)) {
        # fallback: global mean
        df_imputed[i, v] <- global_mean
      } else {
        df_imputed[i, v] <- cmean
      }
    }
  }
}

post_n_missing <- sapply(df_imputed[numeric_vars], function(x) sum(is.na(x)))
post_missing_df <- data.frame(var = names(post_n_missing), n_missing = as.integer(post_n_missing))
print(post_missing_df)