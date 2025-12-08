df <- read.csv(file.choose(), header = TRUE, sep = ",")

colors_green <- colorRampPalette(c("#00441b", "#238b45", "#74c476", "#c7e9c0", "#f7fcf5"))(nrow(missing_by_country_initial))

library(dplyr)
#medie per ogni variabili
df_mean <- df %>%
  group_by(country) %>%
  summarise(
    happiness_score = mean(happiness_score, na.rm = TRUE),
    log_gdp = mean(log_gdp_per_capita_sc, na.rm = TRUE),
    social_support = mean(social_support_sc, na.rm = TRUE),
    healthy_life = mean(healthy_life_expectancy_at_birth_sc, na.rm = TRUE),
    freedom = mean(freedom_to_make_life_choices_sc, na.rm = TRUE),
    generosity = mean(generosity_sc, na.rm = TRUE),
    corruption = mean(perceptions_of_corruption_sc, na.rm = TRUE),
    positive_affect = mean(positive_affect_sc, na.rm = TRUE),
    negative_affect = mean(negative_affect_sc, na.rm = TRUE)
  )

model <- lm(happiness_score ~ 
              log_gdp +
              social_support +
              healthy_life +
              freedom +
              generosity +
              corruption +
              positive_affect +
              negative_affect,
            data = df_mean)

summary(model)

# grafico dei coefficienti
coef <- summary(model)$coefficients[-1,1]
barplot(coef,
        las=2,
        col=colors_green,
        main="Effetto delle variabili sulla felicità",
        ylab="Coefficiente stimato",
        cex.names = 0.7,)
abline(h=0, col="black")

confint(model)



