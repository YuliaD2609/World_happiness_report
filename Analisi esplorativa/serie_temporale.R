df <- read.csv(file.choose(), header = TRUE, sep = ",")

# media annuale della felicità
media_annuale <- aggregate(happiness_score ~ year, df, mean)

ts_media <- ts(media_annuale$happiness_score,
               start = min(media_annuale$year),
               end   = max(media_annuale$year),
               frequency = 1)

plot(ts_media,
     type = "o",
     pch = 19,
     col = "#238B45",
     xlab = "Anno",
     ylab = "Felicità (media annuale)",
     main = "Serie Temporale della Felicità")


trend <- lm(media_annuale$happiness_score ~ media_annuale$year)
summary(trend)