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
plot(pca, type = "l",  main="Screeplot PCA")




























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