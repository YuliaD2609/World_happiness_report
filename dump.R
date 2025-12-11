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

#selezione delle variabili standardizzate senza valori nulli
X <- df[vars_sc]
X_complete <- X[complete.cases(X), ]

# matrice delle distanze
dist_euclidea <- dist(X_complete, method = "euclidean")

dist_manhattan <- dist(X_complete, method = "manhattan")

dist_chebyshev <- dist(X_complete, method = "maximum")

dist_minkowski <- dist(X_complete, method = "minkowski", p = 3)

#install.packages("vegan")
library(vegan)
dist_canberra <- vegdist(X_complete, method = "canberra")

list(
  Euclidea = dist_euclidea[1:10],
  Manhattan = dist_manhattan[1:10],
  Chebyshev = dist_chebyshev[1:10],
  Minkowski = dist_minkowski[1:10],
  Canberra = dist_canberra[1:10]
)



set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(scores, k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type="b", pch=19,
     xlab="Numero di cluster", ylab="Within-Cluster Sum of Squares")

library(cluster)
sil <- sapply(2:8, function(k){
  km <- kmeans(scores, centers=k, nstart=25)
  mean(silhouette(km$cluster, dist(scores))[,3])
})

plot(2:8, sil, type="b", pch=19,
     xlab="k", ylab="Silhouette media")

set.seed(123)
km <- kmeans(scores, centers = 3, nstart = 50)
df$cluster <- km$cluster

plot(scores[,1], scores[,2],
     col = km$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Cluster dei Paesi su PC1 e PC2")
legend("topright", legend=unique(km$cluster),
       col=unique(km$cluster), pch=19)

aggregate(X_complete, by = list(cluster = df$cluster), mean)















X <- df[vars_sc]
X <- X[complete.cases(X), ]

# K means
set.seed(123)
k <- 4

PC <- pca$x[,1:4]
res_km <- kmeans(PC, centers=k, nstart=50)


#aggiunta al dataframe
df$cluster <- res_km$cluster

pca <- prcomp(X, scale = TRUE)
plot(pca$x[,1], pca$x[,2],
     col=res_km$cluster,
     pch=19,
     xlab="PC1", ylab="PC2",
     main="Cluster dei Paesi sulla PCA")
text(pca$x[,1], pca$x[,2], labels=df$country, cex=0.6, pos=3)

#install.packages("cluster")
library(cluster)
sil <- silhouette(res_km$cluster, dist(PC))
mean(sil[,3])


aggregate(df$happiness_score, by=list(df$year), mean)

dist_mat <- dist(PC)
hc <- hclust(dist_mat, method="ward.D2")
plot(hc)




set.seed(123)
km <- kmeans(scores, centers = 3, nstart = 50)

# Aggiunta al dataframe originale in modo sicuro
df$cluster <- NA
complete_idx <- complete.cases(X)
df$cluster[complete_idx] <- km$cluster

plot(scores[,1], scores[,2], col=km$cluster, pch=19, 
     cex = 0.4, xlab="PC1", ylab="PC2", main="Cluster dei Paesi (PC1 vs PC2)")
legend("topright", legend=paste("Cluster",1:3), col=1:3, pch=19)


# heatmap
library(pheatmap)
sample_idx <- sample(1:nrow(X_complete), 60)
mat_euclidea <- as.matrix(dist_euclidea)
rownames(mat_euclidea) <- paste(df$cntry_code[complete_idx], df$year[complete_idx], sep="_")
colnames(mat_euclidea) <- paste(df$cntry_code[complete_idx], df$year[complete_idx], sep="_")

pheatmap(mat_euclidea[sample_idx, sample_idx],
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         main = "Heatmap delle distanze tra Paesi",
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize = 6,
         color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100))



# variabili socio-economiche standardizzate
vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc")

# lista per salvare risultati annuali
cluster_per_year <- list()

years <- sort(unique(df$year))

set.seed(123)

for (yr in years) {
  
  # sotto–dataset annuale
  df_y <- df[df$year == yr, vars_sc]
  
  # rimozione NA
  df_y <- df_y[complete.cases(df_y), ]
  
  # PCA scores (PC1-PC3)
  pca_y <- prcomp(df_y, center = TRUE, scale. = TRUE)
  scores_y <- pca_y$x[, 1:3]
  
  # k-means (3 cluster)
  km_y <- kmeans(scores_y, centers = 3, nstart = 30)
  
  # salva risultati
  cluster_per_year[[as.character(yr)]] <- km_y$cluster
}



df$cluster_year <- NA

for (yr in years) {
  idx <- df$year == yr & complete.cases(df[,vars_sc])
  df$cluster_year[idx] <- cluster_per_year[[as.character(yr)]]
}
install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = year, y = happiness_score, color = factor(cluster_year))) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Evoluzione annuale dei cluster",
       color = "Cluster") +
  theme_minimal()


library(pheatmap)

mat <- table(df$country, df$cluster_year)

pheatmap(mat,
         main = "Persistenza dei cluster nel tempo",
         color = colorRampPalette(c("white", "green", "darkgreen"))(100))







# Loop su tutti gli anni
unique_years <- sort(unique(df$year))

for (yr in unique_years) {
  
  # prendo solo il dato di quell'anno
  df_year <- subset(df, year == yr)
  
  # estraggo la matrice delle variabili
  X_year <- df_year[, vars_sc]
  
  # matrice delle distanze
  dist_mat <- dist(scale(X_year), method = "euclidean")
  dist_mat <- as.matrix(dist_mat)
  
  # creo rownames UNICHE concatenando nome + anno + indice
  rownames(dist_mat) <- paste(df_year$cntry_code)
  colnames(dist_mat) <- paste(df_year$cntry_code)
  
  # plot heatmap
  pheatmap(
    dist_mat,
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean",
    main = paste("Heatmap Distanze tra Paesi - Anno", yr),
    show_rownames = TRUE,    # puoi mettere TRUE se vuoi
    show_colnames = TRUE,
    color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100)
  )
}

library(ggplot2)

ggplot(df, aes(x = year, y = happiness_score, color = factor(cluster_year))) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Evoluzione annuale dei cluster",
       color = "Cluster") +
  theme_minimal()






#install.packages("plotly")
library(plotly)

plot_ly(x = scores[,1], y = scores[,2], z = scores[,3],
        color = as.factor(df$cluster_pc[complete.cases(X)]),
        colors = c("black","red","green"),
        type = "scatter3d", mode = "markers", marker = list(size = 2, opacity = 0.7)) %>%
  layout(scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "PC3")
  ))




#CLUSTERING

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

#selezione delle variabili standardizzate senza valori nulli
X <- df[vars_sc]
X_complete <- X[complete.cases(X), ]

# matrice delle distanze
dist_euclidea <- dist(X_complete, method = "euclidean")

dist_manhattan <- dist(X_complete, method = "manhattan")

dist_chebyshev <- dist(X_complete, method = "maximum")

dist_minkowski <- dist(X_complete, method = "minkowski", p = 3)

#install.packages("vegan")
library(vegan)
dist_canberra <- vegdist(X_complete, method = "canberra")

list(
  Euclidea = dist_euclidea[1:10],
  Manhattan = dist_manhattan[1:10],
  Chebyshev = dist_chebyshev[1:10],
  Minkowski = dist_minkowski[1:10],
  Canberra = dist_canberra[1:10]
)



set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(scores, k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type="b", pch=19,
     xlab="Numero di cluster", ylab="Within-Cluster Sum of Squares")

library(cluster)
sil <- sapply(2:8, function(k){
  km <- kmeans(scores, centers=k, nstart=25)
  mean(silhouette(km$cluster, dist(scores))[,3])
})

plot(2:8, sil, type="b", pch=19,
     xlab="k", ylab="Silhouette media")

set.seed(123)
km <- kmeans(scores, centers = 3, nstart = 50)
df$cluster <- km$cluster

plot(scores[,1], scores[,2],
     col = km$cluster, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Cluster dei Paesi su PC1 e PC2")
legend("topright", legend=unique(km$cluster),
       col=unique(km$cluster), pch=19)

aggregate(X_complete, by = list(cluster = df$cluster), mean)















X <- df[vars_sc]
X <- X[complete.cases(X), ]

# K means
set.seed(123)
k <- 4

PC <- pca$x[,1:4]
res_km <- kmeans(PC, centers=k, nstart=50)


#aggiunta al dataframe
df$cluster <- res_km$cluster

pca <- prcomp(X, scale = TRUE)
plot(pca$x[,1], pca$x[,2],
     col=res_km$cluster,
     pch=19,
     xlab="PC1", ylab="PC2",
     main="Cluster dei Paesi sulla PCA")
text(pca$x[,1], pca$x[,2], labels=df$country, cex=0.6, pos=3)

#install.packages("cluster")
library(cluster)
sil <- silhouette(res_km$cluster, dist(PC))
mean(sil[,3])


aggregate(df$happiness_score, by=list(df$year), mean)

dist_mat <- dist(PC)
hc <- hclust(dist_mat, method="ward.D2")
plot(hc)




set.seed(123)
km <- kmeans(scores, centers = 3, nstart = 50)

# Aggiunta al dataframe originale in modo sicuro
df$cluster <- NA
complete_idx <- complete.cases(X)
df$cluster[complete_idx] <- km$cluster

plot(scores[,1], scores[,2], col=km$cluster, pch=19, 
     cex = 0.4, xlab="PC1", ylab="PC2", main="Cluster dei Paesi (PC1 vs PC2)")
legend("topright", legend=paste("Cluster",1:3), col=1:3, pch=19)


# heatmap
library(pheatmap)
sample_idx <- sample(1:nrow(X_complete), 60)
mat_euclidea <- as.matrix(dist_euclidea)
rownames(mat_euclidea) <- paste(df$cntry_code[complete_idx], df$year[complete_idx], sep="_")
colnames(mat_euclidea) <- paste(df$cntry_code[complete_idx], df$year[complete_idx], sep="_")

pheatmap(mat_euclidea[sample_idx, sample_idx],
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         main = "Heatmap delle distanze tra Paesi",
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize = 6,
         color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100))



# variabili socio-economiche standardizzate
vars_sc <- c("log_gdp_per_capita_sc",
             "social_support_sc",
             "positive_affect_sc",
             "negative_affect_sc",
             "healthy_life_expectancy_at_birth_sc",
             "freedom_to_make_life_choices_sc",
             "generosity_sc",
             "perceptions_of_corruption_sc")

# lista per salvare risultati annuali
cluster_per_year <- list()

years <- sort(unique(df$year))

set.seed(123)

for (yr in years) {
  
  # sotto–dataset annuale
  df_y <- df[df$year == yr, vars_sc]
  
  # rimozione NA
  df_y <- df_y[complete.cases(df_y), ]
  
  # PCA scores (PC1-PC3)
  pca_y <- prcomp(df_y, center = TRUE, scale. = TRUE)
  scores_y <- pca_y$x[, 1:3]
  
  # k-means (3 cluster)
  km_y <- kmeans(scores_y, centers = 3, nstart = 30)
  
  # salva risultati
  cluster_per_year[[as.character(yr)]] <- km_y$cluster
}



df$cluster_year <- NA

for (yr in years) {
  idx <- df$year == yr & complete.cases(df[,vars_sc])
  df$cluster_year[idx] <- cluster_per_year[[as.character(yr)]]
}
install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = year, y = happiness_score, color = factor(cluster_year))) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Evoluzione annuale dei cluster",
       color = "Cluster") +
  theme_minimal()


library(pheatmap)

mat <- table(df$country, df$cluster_year)

pheatmap(mat,
         main = "Persistenza dei cluster nel tempo",
         color = colorRampPalette(c("white", "green", "darkgreen"))(100))







# Loop su tutti gli anni
unique_years <- sort(unique(df$year))

for (yr in unique_years) {
  
  # prendo solo il dato di quell'anno
  df_year <- subset(df, year == yr)
  
  # estraggo la matrice delle variabili
  X_year <- df_year[, vars_sc]
  
  # matrice delle distanze
  dist_mat <- dist(scale(X_year), method = "euclidean")
  dist_mat <- as.matrix(dist_mat)
  
  # creo rownames UNICHE concatenando nome + anno + indice
  rownames(dist_mat) <- paste(df_year$cntry_code)
  colnames(dist_mat) <- paste(df_year$cntry_code)
  
  # plot heatmap
  pheatmap(
    dist_mat,
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean",
    main = paste("Heatmap Distanze tra Paesi - Anno", yr),
    show_rownames = TRUE,    # puoi mettere TRUE se vuoi
    show_colnames = TRUE,
    color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100)
  )
}

library(ggplot2)

ggplot(df, aes(x = year, y = happiness_score, color = factor(cluster_year))) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Evoluzione annuale dei cluster",
       color = "Cluster") +
  theme_minimal()














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
