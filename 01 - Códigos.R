library(tidyverse)
library(GGally)
library(mixtools)

dados <- read_csv("./penguins.csv")
dados <- dados %>% filter(dados$body_mass_g != "NA" & dados$bill_depth_mm != "NA")

#Correlações e diagramas de dispersão
ggpairs(dados)

# Profundidade do Bico por Massa Corporal
image <- "pinguim2.png"
ggplot(dados, aes(x = dados$body_mass_g, y = dados$bill_depth_mm))+
  geom_image(aes(image = image), size = 0.1)+
  geom_abline(intercept = modelo$beta[1,1], slope = modelo$beta[2,1], color = "red", size = 1)+
  geom_abline(intercept = modelo$beta[1,2], slope = modelo$bet[2,2], color = "blue", size =1)+
  labs(x = "Massa Corporal (em gramas)", y = "Profundidade do Bico (em mm)", title = "Profundidade do Bico por Massa Corporal")+
  theme_bw()

summary(lm(dados$body_mass_g ~ dados$bill_depth_mm, dados))

#Modelo
modelo <- lm(dados$body_mass_g ~ dados$bill_depth_mm, dados)

#Aplicação do Algoritmo
set.seed(568)
beta <- matrix(c(22, 0.002, -15, 0.002),2,2)
modelo <- regmixEM(dados$bill_depth_mm, dados$body_mass_g, k = 2, beta = beta)
modelo$beta

#Separar em grupos
dados$grupo <- ifelse(modelo$posterior[,1] > 0.5, "Grupo1", "Grupo2")

#Grupo1
g1 <- dados %>% filter(grupo == "Grupo1")
modelo_g1 <- lm(bill_depth_mm ~ body_mass_g, g1)
anova(modelo_g1)
confint(modelo_g1)

dados_g1 <- data.frame(
  Valores_Preditos = fitted(modelo_g1),
  Residuos = residuals(modelo_g1)
)

dados_g1$student <- (dados_g1$Residuos - mean(dados_g1$Residuos)) / var(dados_g1$Residuos)

#Resíduos Studentizados por Valores Preditos no Grupo 1
ggplot(dados_g1, aes(x = Valores_Preditos, y = student)) +
  geom_point(color = "blue", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Resíduos Studentizados por Valores Preditos no Grupo 1",
    x = "Valores Preditos",
    y = "Resíduos Studentizados"
  ) +
  theme_bw()

residuos <- residuals(modelo_g1)

dados_g11 <- data.frame(
  Quantis_Teoricos = qqnorm(residuos, plot.it = FALSE)$x,
  Quantis_Amostrais = qqnorm(residuos, plot.it = FALSE)$y
)

#Normalidade dos Resíduos do Grupo 1 (QQ-Plot)
ggplot(dados_g11, aes(sample = residuos)) +
  stat_qq_line(color = "red", size = 1) + 
  stat_qq(color = "blue", alpha = 0.8) +
  labs(
    title = "Normalidade dos Resíduos no Grupo 1 (QQ-Plot)",
    x = "Quantis Teóricos",
    y = "Quantis Amostrais"
  ) +
  theme_bw()

#Grupo 2
g2 <- dados %>% filter(grupo == "Grupo2")

modelo_g2 <- lm(bill_depth_mm ~ body_mass_g, g2)
anova(modelo_g2)
confint(modelo_g2)

dados_g2 <- data.frame(
  Valores_Preditos = fitted(modelo_g2),
  Residuos = residuals(modelo_g2)
)
dados_g2$student <- (dados_g2$Residuos - mean(dados_g2$Residuos)) / var(dados_g2$Residuos)

# Resíduos Studentizados por Valores Preditos no Grupo 2
ggplot(dados_g2, aes(x = Valores_Preditos, y = student)) +
  geom_point(color = "blue", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Resíduos Studentizados por Valores Preditos no Grupo 2",
    x = "Valores Preditos",
    y = "Resíduos Studentizados"
  ) +
  theme_bw()

residuos <- residuals(modelo_g2)

dados_g22 <- data.frame(
  Quantis_Teoricos = qqnorm(residuos, plot.it = FALSE)$x,
  Quantis_Amostrais = qqnorm(residuos, plot.it = FALSE)$y
)

#Normalidade dos Resíduos do Grupo 2 (QQ-Plot)
ggplot(dados_g22, aes(sample = residuos)) +
  stat_qq_line(color = "red", size = 1) +
  stat_qq(color = "blue", alpha = 0.8) +
  labs(
    title = "Normalidade dos Resíduos (QQ-Plot) no Grupo 2",
    x = "Quantis Teóricos",
    y = "Quantis Amostrais"
  ) +
  theme_bw()
