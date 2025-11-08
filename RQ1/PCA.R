df <- read.csv(file.choose(), header = TRUE, sep = ",")

vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc"
)

X <- df[vars_sc]

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# calcolo PCA
pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
#per determinare il contributo delle componenti
round(pca$rotation, 3)

summary(pca)
# screeplot
plot(pca, type = "l")

