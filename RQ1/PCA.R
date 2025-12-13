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

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# matrice di correlazione
mat_cor <- cor(X_scaled)
round(mat_cor, 3)
pheatmap(mat_cor,
         main = "Matrice di correlazione",
         fontsize = 10,
         angle_col = 45)

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

# si prendono in considerazione i primi
scores <- pca$x[,1:3]
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
round(mat_cov, 3)
pheatmap(mat_cov,
         main = "Matrice di covarianza",
         fontsize = 10,
         angle_col = 45)

# matrice di non omogeneità
non_omogeneity <- apply(X_scaled, 1, var)
summary(non_omogeneity)

# clustering gerarchico
hc <- hclust(dist_euclidea, method="complete")
str(hc, list.len = nrow(hc$merge)*2, max.level = 5)

# dendrogramma
plot(hc, main="Dendrogramma clutering per paese",
     xlab="Variabili", ylab="Distanza", cex=0.7)
rect.hclust(hc, k=3, border="red")

# calcolo cluster
clusters <- cutree(hc, k=3)
clusters

plot(scores[,1], scores[,2],
     col=clusters, pch=19, cex=0.9,
     xlab="PC1", ylab="PC2",
     main="Clusters")

# aggiunta cluster
df_country$cluster <- clusters
# raggruppamento dei paesi in base al cluster di appartenenza
aggregate(df_country$happiness_score_sc,
          by = list(cluster = df_country$cluster),
          FUN = mean)




cluster_means <- aggregate(
  df_country[, vars_sc],
  by = list(cluster = df_country$cluster),
  FUN = mean
)

round(cluster_means, 3)

overall_means <- colMeans(df_country[, vars_sc])
cluster_diff <- sweep(cluster_means[, -1], 2, overall_means)

round(cluster_diff, 3)

clusters <- unique(df_country$cluster)

for (k in clusters) {
  cat("\nCluster", k, "\n")
  
  df_k <- df_country[df_country$cluster == k, ]
  
  cors <- cor(df_k[, vars_sc], df_k$happiness_score_sc)
  print(round(cors, 3))
}

non_omogeneity_cluster <- aggregate(
  apply(df_country[, vars_sc], 1, var),
  by = list(cluster = df_country$cluster),
  FUN = mean
)

non_omogeneity_cluster

















# Clustering tra paesi
X <- df[vars_sc]
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# PCA sui paesi
pca <- prcomp(X_scaled, scale.=TRUE)
scores <- pca$x[,1:3]

# distanza sui paesi
dist_p <- dist(scores, method="euclidean")

# clustering gerarchico
hc_p <- hclust(dist_p, method="complete")

plot(hc_p, main="Dendrogramma dei Paesi")
rect.hclust(hc_p, k=3, border="red")


clusters_paesi <- cutree(hc_p, k=3)
country_clusters <- factor(country_clusters)
plot(scores,
     col = country_clusters,
     pch = 19, cex = 0.6,
     xlab = "PC1", ylab = "PC2",
     main = "Cluster dei paesi nello spazio PCA")
legend("topright", legend=levels(country_clusters),
       col=1:3, pch=19)












# funzione per calcolare la dimensione di ogni cluster durante l'agglomerazione
cluster_sizes <- function(hc) {
  n <- length(hc$order)
  sizes <- rep(1, n)
  size_vec <- numeric(n - 1)
  
  for (i in 1:(n - 1)) {
    left <- hc$merge[i, 1]
    right <- hc$merge[i, 2]
    
    size_left <- ifelse(left < 0, 1, size_vec[left])
    size_right <- ifelse(right < 0, 1, size_vec[right])
    
    size_vec[i] <- size_left + size_right
  }
  
  return(size_vec)
}

# calcolo dimensioni dei cluster
sizes <- cluster_sizes(hc)

# costruzione tabella
aggl_table <- data.frame(
  Passo = 1:(nrow(hc$merge)),
  Unione = apply(hc$merge, 1, paste, collapse = "  "),
  Distanza = round(hc$height, 4),
  Dimensione_Cluster = sizes
)

print(aggl_table)





# scatterplot dei cluster
plot(scores[,1], scores[,2], col=df$cluster_pc[complete.cases(X)],
     pch=19, cex = 0.4, xlab="PC1", ylab="PC2", main="Clusters")









wcss <- sapply(1:10, function(k){kmeans(X_complete, center=k, nstart=10)$tot.withinss})
plot(1:10, wcss, type="b", pch=19, frame=FALSE,
     main = "Elbow method",
     ylab="wcss",
     xlab="clusters")


















# CLUSTERING
# Si considerano PC1-PC3
scores <- pca$x[,1:3]

# matrice delle distanze
dist_euclidea <- dist(X_complete, method = "euclidean")
dist_euclidea[1:10] #si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)

# matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
round(mat_similarity[1:5, 1:5], 3)

library(pheatmap)
pheatmap(mat_similarity,
         main = "Matrice delle similarità tra paesi",
         color = colorRampPalette(c("white", "#74c476", "#00441b"))(100))



df$cluster_pc <- NA
df$cluster_pc[complete.cases(X)] <- cutree(hc_pc, k = 3)

#scatterplot con tutti i cluster
pairs(scores[,1:3], col=df$cluster_pc[complete.cases(X)], pch=19, cex = 0.4)

# scatterplot dei cluster
plot(scores[,1], scores[,2], col=df$cluster_pc[complete.cases(X)],
     pch=19, cex = 0.4, xlab="PC1", ylab="PC2", main="Cluster su PC")



library(dplyr)
library(ggplot2)

# Calcola la media della felicità per cluster
mean_happiness_cluster <- df %>%
  filter(!is.na(cluster_pc)) %>%
  group_by(cluster_pc) %>%
  summarise(mean_happiness = mean(happiness_score, na.rm = TRUE))
mean_happiness_cluster

# Grafico a barre
ggplot(mean_happiness_cluster, aes(x = as.factor(cluster_pc), y = mean_happiness, fill = as.factor(cluster_pc))) +
  geom_bar(stat = "identity") +
  labs(title = "Media della felicità per cluster", x = "Cluster", y = "Media Happiness Score") +
  theme_minimal() +
  theme(legend.position = "none")

library(dplyr)

# Media delle variabili per cluster
cluster_summary <- df %>%
  filter(!is.na(cluster_pc)) %>%
  group_by(cluster_pc) %>%
  summarise(across(c(happiness_score,
                     log_gdp_per_capita,
                     social_support,
                     healthy_life_expectancy_at_birth,
                     freedom_to_make_life_choices,
                     generosity,
                     perceptions_of_corruption,
                     positive_affect,
                     negative_affect),
                   mean, na.rm = TRUE))

print(cluster_summary)
