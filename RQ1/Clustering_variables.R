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


var_labels <- c(
  log_gdp_per_capita_sc = "PIL pro capite",
  social_support_sc = "Supporto sociale",
  positive_affect_sc = "Emozioni positive",
  negative_affect_sc = "Emozioni negative",
  healthy_life_expectancy_at_birth_sc = "Aspettativa di vita sana",
  freedom_to_make_life_choices_sc = "Libertà di scelta nella vita",
  generosity_sc = "Generosità",
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

X <- df[all_sc]

# Rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# Matrice di correlazione
cat("Matrice di correlazione: ")
mat_cor <- cor(X_scaled)
colnames(mat_cor) <- all_var_labels
rownames(mat_cor) <- all_var_labels
round(mat_cor, 3)
pheatmap(mat_cor,
         main = "Matrice di correlazione",
         fontsize = 7,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

X <- df[vars_sc]

# Rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)
# Calcolo PCA
pca <- prcomp(X_scaled, center = TRUE, scale. = TRUE)
cat("PCA: ")
summary(pca)
# Per determinare il contributo delle componenti
cat("Contributo componenti: ")
round(pca$rotation, 3)
# Screeplot
plot(pca, type = "l",  main="Screeplot PCA")

# Si prendono in considerazione i primi
scores <- pca$x[,c(1,3)]
# Matrice delle distanze
cat("Matrice distanze: ")
dist_euclidea <- dist(scores, method = "euclidean")
dist_euclidea[1:10] # si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)
rownames(mat_dist) <- rownames(X_scaled)
colnames(mat_dist) <- rownames(X_scaled)
round(mat_dist[1:10, 1:10], 3)

# Matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
rownames(mat_similarity) <- rownames(X_scaled)
colnames(mat_similarity) <- rownames(X_scaled)
round(mat_similarity[1:10, 1:10], 3)

# Matrice di covarianza
mat_cov <- cov(X_scaled)
colnames(mat_cov) <- var_labels
rownames(mat_cov) <- var_labels
round(mat_cov, 3)
pheatmap(mat_cov,
         main = "Matrice di covarianza",
         fontsize = 8,
         angle_col = 45,
         display_numbers = TRUE,
         number_format = "%.2f")

# Matrice di non omogeneità
non_omogeneity <- apply(X_scaled, 1, var)
summary(non_omogeneity)

# Clustering gerarchico
dist_vars <- dist(1 - mat_cor) 
hc <- hclust(dist_vars, method="complete")

par(mfrow = c(1, 2)) 
# Dendrogramma
plot(hc, main="Dendrogramma",
     xlab="Variabili", ylab="Distanza", cex=1)
rect.hclust(hc, k=2, border="red")
plot(hc, main="Dendrogramma",
     xlab="Variabili", ylab="Distanza", cex=1)
rect.hclust(hc, k=3, border="green")
par(mfrow = c(1, 1))

# Dendrogramma
plot(hc, main="Dendrogramma",
     xlab="Variabili", ylab="Distanza", cex=1)

# Metodo della silhouette
library(cluster)
sil_width <- numeric()
for (k in 2:8) {
  km <- kmeans(scores, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist(scores))
  sil_width[k] <- mean(sil[, 3])
}

summary(sil)

plot(2:8, sil_width[2:8], type = "b",
     xlab = "Numero di cluster (k)",
     ylab = "Silhouette media",
     main = "Silhouette Method")


# taglio a 2 cluster
clusters <- cutree(hc, k=2)
clusters

plot(scores[,1], scores[,2],
     col=clusters, pch=19, cex=0.7,
     xlab="PC1", ylab="PC2",
     main="Clusters")



extract_clusters <- function(hc, step) {
  cutree(hc, k = nrow(hc$merge) + 1 - step)
}
n <- length(hc$labels)

results <- data.frame(
  n_cluster = integer(),
  clusters = character(),
  height = numeric(),
  stringsAsFactors = FALSE
)

for (i in 0:(n - 1)) {
  
  k <- n - i
  
  cl <- cutree(hc, k = k)
  
  # Costruzione rappresentazione testuale dei cluster
  cluster_list <- split(names(cl), cl)
  cluster_str <- paste(
    sapply(cluster_list, function(x) {
      paste0("{", paste(x, collapse = ", "), "}")
    }),
    collapse = ", "
  )
  
  h <- if (k == n) 0 else hc$height[n - k]
  
  results <- rbind(
    results,
    data.frame(
      n_cluster = k,
      clusters = cluster_str,
      height = round(h, 4)
    )
  )
}
results
