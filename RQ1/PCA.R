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

X <- df[all_sc]

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)

# matrice di correlazione
mat_cor <- cor(X_scaled)
round(mat_cor, 3)
pheatmap(mat_cor,
         main = "Matrice di correlazione",
         fontsize = 7,
         angle_col = 45)

X <- df[vars_sc]

# rimozione di righe con NA
X_complete <- X[complete.cases(X), ]
X_scaled <- scale(X_complete)
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

pheatmap(mat_dist,
         main = "Matrice delle distanze",
         color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100),
         fontsize = 7,
         angle_col = 45)

# matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
rownames(mat_similarity) <- rownames(X_scaled)
colnames(mat_similarity) <- rownames(X_scaled)
round(mat_similarity[1:10, 1:10], 3)

pheatmap(mat_similarity,
         main = "Matrice delle similarità",
         color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100),
         fontsize = 7,
         angle_col = 45)

# matrice di covarianza
mat_cov <- cov(X_scaled)
round(mat_cov, 3)
pheatmap(mat_cov,
         main = "Matrice di covarianza",
         fontsize = 7,
         angle_col = 45)

# matrice di non omogeneità
non_omogeneity <- apply(X_scaled, 1, var)
summary(non_omogeneity)

# clustering gerarchico
dist_vars <- dist(1 - mat_cor) 
hc <- hclust(dist_vars, method="complete")

# dendrogramma
plot(hc, main="Dendrogramma",
     xlab="Variabili", ylab="Distanza", cex=0.6)

# taglio a 3 cluster
clusters <- cutree(hc, k=3)
clusters

#scatterplot con tutti i cluster
pairs(scores[,1:3], col=df$cluster_pc[complete.cases(X)], pch=19, cex = 0.4)

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