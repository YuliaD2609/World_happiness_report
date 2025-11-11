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


# CLUSTERING
# Si considerano PC1-PC3
scores <- pca$x[,1:3]

# matrice delle distanze
dist_euclidea <- dist(X_complete, method = "euclidean")
dist_euclidea[1:10] #si stampano solo le prime 10 per avere una visione iniziale dei valori

scores <- pca$x[, 1:3]

df$cluster_pc <- NA
df$cluster_pc[complete.cases(X)] <- cutree(hc_pc, k = 3)

#scatterplot con tutti i cluster
pairs(scores[,1:3], col=df$cluster_pc[complete.cases(X)], pch=19, cex = 0.4)

# scatterplot dei cluster
plot(scores[,1], scores[,2], col=df$cluster_pc[complete.cases(X)],
     pch=19, cex = 0.4, xlab="PC1", ylab="PC2", main="Cluster su PC")

