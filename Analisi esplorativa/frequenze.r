df <- read.csv(file.choose(), header = TRUE, sep = ",")

# Frequenza assoluta
freq_ass <- table(cut(df$happiness_score, breaks = 40, right = FALSE))
cat("Frequenza assoluta: ", freq_ass)
# Frequenza relativa
freq_rel <- prop.table(freq_ass)
cat("Frequenza relativa: ", freq_rel)
# Frequenza relativa cumulata
freq_rel_cum <- cumsum(freq_rel)
cat("Frequenza relativa cumulata: ", freq_rel_cum)

# Minimo, media, mediana, quantili
summary(df$happiness_score)


# Classi usate nei barplot
breaks <- attr(freq_ass, "dimnames")[[1]]

# Estrazione dei limiti numerici delle classi
break_limits <- do.call(rbind,
                        lapply(strsplit(gsub("\\[|\\)|\\]", "", breaks), ","), as.numeric))

class_centers <- rowMeans(break_limits)

# Kernel density
dens <- density(df$happiness_score)



par(mgp = c(4, 0, -1))

bp_abs <- barplot(freq_ass,
                  main = "Distribuzione di frequenza assoluta del punteggio di felicità",
                  xlab = "Classi di Happiness Score",
                  ylab = "Frequenza assoluta",
                  las = 2,
                  ylim = c(0, max(freq_ass) * 1.2),
                  col = "#56B117")

# Scala la densità
scale_abs <- max(freq_ass) / max(dens$y)
dens_centers <- approx(x = dens$x,
                       y = dens$y,
                       xout = class_centers)$y
lines(bp_abs,
      dens_centers * scale_abs,
      col = "red",
      lwd = 2)

legend("topright",
       legend = c("Frequenza assoluta", "Densità"),
       fill = c("#56B117", NA),
       lty = c(NA, 1),
       col = c("black", "red"),
       bty = "n")



bp_rel <- barplot(freq_rel,
                  main = "Distribuzione di frequenza relativa del punteggio di felicità",
                  xlab = "Classi di Happiness Score",
                  ylab = "Frequenza relativa",
                  las = 2,
                  ylim = c(0, max(freq_rel) * 1.2),
                  col = "#56B117")

# Scala densità
scale_rel <- max(freq_rel) / max(dens_centers)

lines(bp_rel,
      dens_centers * scale_rel,
      col = "red",
      lwd = 2)

legend("topright",
       legend = c("Frequenza relativa", "Densità"),
       fill = c("#56B117", NA),
       lty = c(NA, 1),
       col = c("black", "red"),
       bty = "n")


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


# Istogramma
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

# Aggiunta curva 
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


# Kernel density plot

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


