df <- read.csv(file.choose(), header = TRUE, sep = ",")

#frequenza assoluta
freq_ass <- table(cut(df$happiness_score, breaks = 40, right = FALSE))
freq_ass
#frequenza relativa
freq_rel <- prop.table(freq_ass)
freq_rel
#frequenza relativa cumulata
freq_rel_cum <- cumsum(freq_rel)
freq_rel_cum

media_annuale_happy <- aggregate(happiness_score_sc ~ year, df, mean)
media_annuale_gdp   <- aggregate(log_gdp_per_capita_sc ~ year, df, mean)
media_annuale_support  <- aggregate(social_support_sc ~ year, df, mean)
media_annuale_gener    <- aggregate(generosity_sc ~ year, df, mean)
media_annuale_pos      <- aggregate(positive_affect_sc ~ year, df, mean)
media_annuale_neg      <- aggregate(negative_affect_sc ~ year, df, mean)
media_annuale_free     <- aggregate(freedom_to_make_life_choices_sc ~ year, df, mean)
media_annuale_health   <- aggregate(healthy_life_expectancy_at_birth_sc ~ year, df, mean)
media_annuale_corr     <- aggregate(perceptions_of_corruption_sc ~ year, df, mean)


#Minimo, media, mediana, quantili
summary(df$happiness_score)

par(mgp = c(4, 0, -1))

barplot(freq_ass,
        main = "Distribuzione di frequenza assoluta del punteggio di felicità",
        xlab = "Classi di Happiness Score",
        ylab = "Frequenza assoluta",
        las = 2,
        ylim = c(-10,150),
        col = "#56B117")

barplot(freq_rel,
        main = "Distribuzione di frequenza relativa del punteggio di felicità",
        xlab = "Classi di Happiness Score",
        ylab = "Frequenza relativa",
        las = 2,
        col = "#56B117")



plot(media_annuale_happy$year,
     media_annuale_happy$happiness_score_sc,
     type = "o",
     pch = 19,
     cex = 0.5,
     col = "#238B45",
     xlab = "Anno",
     ylab = "Valori medi (standardizzati)",
     main = "Serie temporali delle variabili (2005–2022)",
     ylim = range(
       media_annuale_happy$happiness_score_sc,
       media_annuale_gdp$log_gdp_per_capita_sc,
       media_annuale_support$social_support_sc,
       media_annuale_gener$generosity_sc,
       media_annuale_pos$positive_affect_sc,
       media_annuale_neg$negative_affect_sc,
       media_annuale_free$freedom_to_make_life_choices_sc,
       media_annuale_health$healthy_life_expectancy_at_birth_sc,
       media_annuale_corr$perceptions_of_corruption_sc
     )
)

# Linee aggiuntive
lines(media_annuale_gdp$year,    media_annuale_gdp$log_gdp_per_capita_sc,           type="o", cex = 0.5,pch=19, col="blue")
lines(media_annuale_support$year, media_annuale_support$social_support_sc,           type="o", cex = 0.5,pch=19, col="orange")
lines(media_annuale_gener$year,   media_annuale_gener$generosity_sc,                type="o", cex = 0.5,pch=19, col="purple")
lines(media_annuale_pos$year,     media_annuale_pos$positive_affect_sc,             type="o", cex = 0.5,pch=19,  col="red")
lines(media_annuale_neg$year,     media_annuale_neg$negative_affect_sc,             type="o", cex = 0.5,pch=19,  col="brown")
lines(media_annuale_free$year,    media_annuale_free$freedom_to_make_life_choices_sc, type="o", cex = 0.5,pch=19, col="darkgreen")
lines(media_annuale_health$year,  media_annuale_health$healthy_life_expectancy_at_birth_sc, cex = 0.5,type="o", pch=19, col="cyan4")
lines(media_annuale_corr$year,    media_annuale_corr$perceptions_of_corruption_sc,  type="o", cex = 0.5,pch=19, col="gray40")

# Legenda
legend("topright",
       legend = c("Happiness Score", "Log GDP", "Social Support", "Generosity",
                  "Positive Affect", "Negative Affect", "Freedom of Choice",
                  "Healthy Life Expectancy", "Perception of Corruption"),
       col = c("#238B45", "blue", "orange", "purple", "red", "brown",
               "darkgreen", "cyan4", "gray40"),
       pch = c(19,19,19,19,19,19,19,19,19),
       lty = 1,
       bty = "n",
       cex = 0.8)


par(mgp = c(3, 0.5, 0))

plot(freq_rel_cum,
     type = "b",
     pch = 19,
     col = "#238B45",
     xaxt = "n",
     xlab = "Classi di Happiness Score",
     ylab = "Frequenza relativa cumulata",
     main = "Funzione di distribuzione empirica continua della Felicità")



axis(1, at = 1:length(freq_rel_cum), labels = names(freq_rel_cum), las = 2, cex.axis = 0.6)


#Istogramma
hist_data <- hist(df$happiness_score,
                  breaks = 40,                         
                  col = "#56B117",                     
                  border = "white",                    
                  main = "Distribuzione della variabile Happiness Score",
                  xlab = "Punteggio di felicità",
                  ylab = "Frequenza",
                  cex.main = 1.3,
                  cex.lab = 1,
                  cex.axis = 0.9)

#Aggiunta curva 
dens <- density(df$happiness_score)
scale_factor <- max(hist_data$counts) / max(dens$y)     

lines(dens$x, dens$y * scale_factor, 
      col = "red", 
      lwd = 2)

legend("topright",
       legend = c("Istogramma", "Densità"),
       fill = c("#56B117", NA),
       border = c("white", NA),
       lty = c(NA, 1),
       col = c("black", "red"),
       bty = "n",
       cex = 0.9)

