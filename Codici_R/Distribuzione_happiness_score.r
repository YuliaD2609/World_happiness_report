df <- read.csv(file.choose(), header = TRUE, sep = ",")

#Controllo con summary
summary(df$happiness_score)

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

