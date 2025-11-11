df_gen <- read.csv(file.choose(), header = TRUE, sep = ",")

post_n_missing <- sapply(df_gen[numeric_vars], function(x) sum(is.na(x)))
post_missing_df_gen <- data.frame(var = names(post_n_missing), n_missing = as.integer(post_n_missing))
print(post_missing_df_gen)
View(df_gen)

#ricostruzione sc
df_gen$log_gdp_per_capita_sc <- as.numeric(scale(df_gen$log_gdp_per_capita))
df_gen$social_support_sc <- as.numeric(scale(df_gen$social_support))
df_gen$healthy_life_expectancy_at_birth_sc <- as.numeric(scale(df_gen$healthy_life_expectancy_at_birth))
df_gen$freedom_to_make_life_choices_sc <- as.numeric(scale(df_gen$freedom_to_make_life_choices))
df_gen$generosity_sc <- as.numeric(scale(df_gen$generosity))
df_gen$perceptions_of_corruption_sc <- as.numeric(scale(df_gen$perceptions_of_corruption))
df_gen$positive_affect_sc <- as.numeric(scale(df_gen$positive_affect))
df_gen$negative_affect_sc <- as.numeric(scale(df_gen$negative_affect))


sapply(df_gen[c(numeric_vars)], function(x) sum(is.na(x)))

