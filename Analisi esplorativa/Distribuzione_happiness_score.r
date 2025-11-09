df <- read.csv(file.choose(), header = TRUE, sep = ",")

#frequenza assoluta
freq_ass <- table(cut(df$happiness_score, breaks = 40, right = FALSE))
freq_ass
#frequenza relativa
freq_rel <- prop.table(freq_abs)
freq_rel

#Minimo, media, mediana, quantili
summary(df$happiness_score)

barplot(freq_ass,
        main = "Distribuzione di frequenza assoluta del punteggio di felicità",
        xlab = "Classi di Happiness Score",
        ylab = "Frequenza assoluta",
        las = 2,
        col = "#56B117")

barplot(freq_rel,
        main = "Distribuzione di frequenza assoluta relativa del punteggio di felicità",
        xlab = "Classi di Happiness Score",
        ylab = "Frequenza relativa",
        las = 2,
        col = "#56B117")


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
