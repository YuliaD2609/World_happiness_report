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
             #"generosity_sc",
             "perceptions_of_corruption_sc"
)

var_labels <- c(
  log_gdp_per_capita_sc = "PIL pro capite",
  social_support_sc = "Supporto sociale",
  positive_affect_sc = "Emozioni positive",
  negative_affect_sc = "Emozioni negative",
  healthy_life_expectancy_at_birth_sc = "Aspettativa di vita sana",
  freedom_to_make_life_choices_sc = "Libertà di scelta nella vita",
  #generosity_sc = "Generosità",
  perceptions_of_corruption_sc = "Percezione della corruzione"
)

all_var_labels <- c(
  log_gdp_per_capita_sc = "PIL pro capite",
  social_support_sc = "Supporto sociale",
  positive_affect_sc = "Emozioni positive",
  negative_affect_sc = "Emozioni negative",
  healthy_life_expectancy_at_birth_sc = "Aspettativa di vita sana",
  freedom_to_make_life_choices_sc = "Libertà di scelta nella vita",
  generosity_sc = "Generosità",
  perceptions_of_corruption_sc = "Percezione della corruzione",
  happiness_score_sc = "Felicità"
)

df_country <- aggregate(df[, all_sc],
                        by = list(country = df$country),
                        FUN = mean)
df_country <- df_country[complete.cases(df_country), ]
X <- df_country[all_sc]

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# matrice di correlazione
mat_cor <- cor(X_scaled)
round(mat_cor, 3)
colnames(mat_cor) <- all_var_labels
rownames(mat_cor) <- all_var_labels
pheatmap(mat_cor,
         main = "Matrice di correlazione",
         fontsize = 10,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

# senza la variabile happiness score
X <- df_country[vars_sc]
rownames(X) <- df_country$country
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)
mat_cor <- cor(X_scaled)

# calcolo PCA
pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
summary(pca)
# per determinare il contributo delle componenti
round(pca$rotation, 3)
# screeplot
plot(pca, type = "l",  main="Screeplot PCA")


# matrice di correlazione
mat_cor <- cor(X_scaled)
round(mat_cor, 3)
colnames(mat_cor) <- var_labels
rownames(mat_cor) <- var_labels
pheatmap(mat_cor,
         main = "Matrice di correlazione",
         fontsize = 10,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

# si prendono in considerazione i primi
scores <- pca$x[,c(1,3)]
# matrice delle distanze
dist_euclidea <- dist(scores, method = "euclidean")
dist_euclidea[1:10] # si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)
rownames(mat_dist) <- rownames(X_scaled)
colnames(mat_dist) <- rownames(X_scaled)
round(mat_dist[1:10, 1:10], 3)

# matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
rownames(mat_similarity) <- rownames(X_scaled)
colnames(mat_similarity) <- rownames(X_scaled)
round(mat_similarity[1:10, 1:10], 3)

# matrice di covarianza
mat_cov <- cov(X_scaled)
colnames(mat_cov) <- var_labels
rownames(mat_cov) <- var_labels
round(mat_cov, 3)
pheatmap(mat_cov,
         main = "Matrice di covarianza",
         fontsize = 10,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

# matrice di non omogeneità
#non_omogeneity <- apply(X_scaled, 1, var)
#summary(non_omogeneity)

# clustering gerarchico
hc <- hclust(dist_euclidea, method="complete")
str(hc, list.len = nrow(hc$merge)*2, max.level = 5)

# dendrogramma
plot(hc, main="Dendrogramma clutering per paese",
     xlab="Variabili", ylab="Distanza", cex=0.4)
rect.hclust(hc, k=2, border="red")

# calcolo cluster
clusters <- cutree(hc, k=2)
clusters

plot(scores[,1], scores[,2],
     col=clusters, pch=19, cex=0.6,
     xlab="PC1", ylab="PC2",
     main="Clusters")

# aggiunta cluster
df_country$cluster <- clusters
# raggruppamento dei paesi in base al cluster di appartenenza
# media di felicità per ogni cluster
aggregate(df_country$happiness_score_sc,
          by = list(cluster = df_country$cluster),
          FUN = mean)

# kmeans a 2 cluster
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
wcss
# Between-Cluster Sum of Squares
bcss <- kmeans_res$betweenss
bcss
n <- nrow(scores)
k <- 2
# Calinski–Harabasz
ch <- (bcss / (k - 1)) / (wcss / (n - k))
ch

wcss <- numeric()
for (k in 1:8) {
  km <- kmeans(scores, centers = k, nstart = 25)
  wcss[k] <- km$tot.withinss
}

# Elbow method
plot(1:8, wcss, type="b", pch=19, frame=FALSE,
     xlab="Numero di cluster",
     ylab="WCSS",
     ylim = c(1, 700),
     main="Metodo del gomito")

# analisi felicità nei cluster
happiness_cluster <- aggregate(df_country$happiness_score_sc,
                               by = list(cluster = df_country$cluster),
                               FUN = mean)

colnames(happiness_cluster) <- c("cluster", "mean_happiness")
happiness_cluster

barplot(happiness_cluster$mean_happiness,
        names.arg = paste("Cluster", happiness_cluster$cluster),
        col = c("tomato", "#238B45"),
        ylab = "Livello di felicità medio",
        xlab = "Cluster",
        main = "Confronto del livello medio di felicità tra cluster",
        ylim = range(c(-0.5, 1.5)))
abline(h = 0, lty = 1, lwd = 2)

