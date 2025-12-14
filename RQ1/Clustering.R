df <- read.csv(file.choose(), header = TRUE, sep = ",")

library(pheatmap)
all_sc <- c("log_gdp_per_capita_sc",
            "social_support_sc",
            "positive_affect_sc",
            "negative_affect_sc",
            "healthy_life_expectancy_at_birth_sc",
            "freedom_to_make_life_choices_sc",
            "generosity_sc",
            "perceptions_of_corruption_sc",
            "happiness_score_sc"
)

vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc"
)
df_country <- aggregate(df[, all_sc],
                        by = list(country = df$country),
                        FUN = mean)
df_country <- df_country[complete.cases(X), ]
X <- df_country[all_sc]

# Rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# Matrice di correlazione
mat_cor <- cor(X_scaled)
round(mat_cor, 3)
pheatmap(mat_cor,
         main = "Matrice di correlazione",
         fontsize = 10,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

# Senza la variabile happiness score
X <- df_country[vars_sc]
rownames(X) <- df_country$country
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)
mat_cor <- cor(X_scaled)

# Calcolo PCA
pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
summary(pca)
# Per determinare il contributo delle componenti
round(pca$rotation, 3)
# Screeplot
plot(pca, type = "l",  main="Screeplot PCA")

# Si prendono in considerazione i primi
scores <- pca$x[,1:3]
# Matrice delle distanze
cat("Matrice delle distanze: ")
dist_euclidea <- dist(scores, method = "euclidean")
dist_euclidea[1:10] # Si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)
rownames(mat_dist) <- rownames(X_scaled)
colnames(mat_dist) <- rownames(X_scaled)
round(mat_dist[1:10, 1:10], 3)

# Matrice di similarità
cat("Matrice di similarità: ")
mat_similarity <- 1 - mat_dist / max(mat_dist)
rownames(mat_similarity) <- rownames(X_scaled)
colnames(mat_similarity) <- rownames(X_scaled)
round(mat_similarity[1:10, 1:10], 3)

# Matrice di covarianza
mat_cov <- cov(X_scaled)
round(mat_cov, 3)
pheatmap(mat_cov,
         main = "Matrice di covarianza",
         fontsize = 10,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

# Matrice di non omogeneità
non_omogeneity <- apply(X_scaled, 1, var)
summary(non_omogeneity)

# Clustering gerarchico
hc <- hclust(dist_euclidea, method="complete")
str(hc, list.len = nrow(hc$merge)*2, max.level = 5)

# Dendrogramma
plot(hc, main="Dendrogramma clutering per paese",
     xlab="Variabili", ylab="Distanza", cex=0.4)
rect.hclust(hc, k=2, border="red")

# Calcolo cluster
clusters <- cutree(hc, k=2)
clusters

plot(scores[,1], scores[,2],
     col=clusters, pch=19, cex=0.6,
     xlab="PC1", ylab="PC2",
     main="Clusters")

# Aggiunta cluster
df_country$cluster <- clusters
# Raggruppamento dei paesi in base al cluster di appartenenza
# Media di felicità per ogni cluster
aggregate(df_country$happiness_score_sc,
          by = list(cluster = df_country$cluster),
          FUN = mean)

# Kmeans a 2 cluster
kmeans_res <- kmeans(scores, centers = 2, nstart = 1)
kmeans_res
clusters_km <- kmeans_res$cluster

par(mfrow = c(1,2))
plot(scores[,1], scores[,2],
     col = clusters, pch = 19,cex=0.6,
     main = "Clustering gerarchico",
     xlab = "PC1", ylab = "PC2")
plot(scores[,1], scores[,2],
     col = clusters_km, pch = 19, cex=0.6,
     main = "k-means",
     xlab = "PC1", ylab = "PC2")
par(mfrow = c(1,1))

# Within-Cluster Sum of Squares
wcss <- kmeans_res$tot.withinss
cat("Within-Cluster Sum of Squares: ", wcss)
# Between-Cluster Sum of Squares
bcss <- kmeans_res$betweenss
cat("Between-Cluster Sum of Squares: ", bcss)
bcss
n <- nrow(scores)
k <- 2
# Calinski–Harabasz
ch <- (bcss / (k - 1)) / (wcss / (n - k))
cat("Calinski-Harabasz: ", ch)
