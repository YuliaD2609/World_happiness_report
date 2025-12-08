library(pheatmap)

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

# medie per paese
df_media <- aggregate(df[vars_sc],
                      by = list(country = df$country, cntry_code=df$cntry_code),
                      FUN = mean, na.rm = TRUE)

# matrice dei dati
X <- scale(df_media[, vars_sc])
rownames(X) <- df_media$cntry_code

# matrice delle distanze
dist_euclidea <- dist(X, method = "euclidean")
dist_euclidea[1:10] #si stampano solo le prime 10 per avere una visione iniziale dei valori
mat_dist <- as.matrix(dist_euclidea)

pheatmap(mat_similarity,
         main = "Matrice delle distanze tra Paesi",
         color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100),
         fontsize_row = 4, fontsize_col = 4)

# matrice di similarità
mat_similarity <- 1 - mat_dist / max(mat_dist)
round(mat_similarity[1:5, 1:5], 3)

pheatmap(mat_similarity,
         main = "Matrice delle dimilarità tra Paesi",
         color = colorRampPalette(c("white", "#a1d99b", "#006d2c"))(100),
         fontsize_row = 4, fontsize_col = 4)

# calcolo PCA
pca <- prcomp(X, center = TRUE, scale. = TRUE)
#per determinare il contributo delle componenti
round(pca$rotation, 3)
summary(pca)

# screeplot
plot(pca, type = "l", main = "Screeplot PCA (medie per Paese e anno)")
