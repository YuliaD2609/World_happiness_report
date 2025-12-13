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


# kernel density plot

dens <- density(df$happiness_score)

plot(dens,
     main = "Stima kernel density plot del Happiness Score",
     xlab = "Happiness Score",
     ylab = "Densità",
     lwd = 2,
     col = "#238B45")

polygon(dens,
        col = rgb(35/255, 139/255, 69/255, 0.3),
        border = "#238B45")


