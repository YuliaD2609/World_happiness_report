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



