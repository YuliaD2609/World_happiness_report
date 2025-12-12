df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc")

apply(df[vars_sc], 2, function(x) sum(abs(scale(x)) > 3, na.rm = TRUE))

outliers_list <- lapply(vars_sc, function(v) which(abs(scale(df[[v]])) > 3))
names(outliers_list) <- vars_sc
outliers_list

lapply(vars_sc, function(v) df[which(abs(scale(df[[v]])) > 3), c("country","year",v)])
