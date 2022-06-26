library(readr)
library(ggplot2)
library(ggthemes)
library(devtools)
devtools::install_github("mkyou/glmTests")
library(glmTests)

rolos = read_table2("https://raw.githubusercontent.com/Gabriel4210/Lista-5---MLG/main/rolos.dat", 
                     col_names = FALSE)
base = data.frame(comprimento = c(rolos$X1,rolos$X3),
                  falhas = c(rolos$X2, rolos$X4))
base

#Grafico de dispersão
plot1 = ggplot(base, aes(x=comprimento, y=falhas)) +
  geom_point() +
  geom_smooth(method="loess", se = F) +
  labs(subtitle = "Comprimento da peça por falhas",
       y = "Número de falhas", x = "Comprimento da peça de tecido (metros)",
       caption = "https://www.ime.usp.br/~giapaula/texto_2013.pdf")+
  theme_economist()

plot(plot1)

#Ajuste modelo
fit1 = glm(falhas ~ comprimento,data=base, family = poisson(link = "log"))
summary(fit1)
log(0.0019297)
#O aumento de 1 unidade no comprimento tem efeito multiplicativo no número de 
#falhas igual a 0.0019297
res_envelope(fit1)
res_vs_fitted(fit1)
res_vs_index(fit1)
influence(fit1)
local_influence(fit1)
#Não parece bem ajustado

#Como o Residual deviance é o dobro dos graus de liberdade existe indicios 
#de uma possivel superdispersão
install.packages("AER")
library(AER)
dispersiontest(fit1)

#Binomial negativa
library(MASS)
fit2 = glm.nb(falhas ~ comprimento, data = base)
summary(fit2)
#O aumento de 1 unidade no comprimento tem efeito multiplicativo no número de 
#falhas igual a 0.0018827

#Quase-verossimilhança
fit3 = glm(falhas ~ comprimento,data=base, family = quasi(variance = "mu", link = "log"))
summary(fit3)
#O aumento de 1 unidade no comprimento tem efeito multiplicativo no número de 
#falhas igual a 0.0019297
fit4 = glm(falhas ~ comprimento,data=base, family = quasi(variance = "mu^2", link = "log"))
summary(fit4)
#O aumento de 1 unidade no comprimento tem efeito multiplicativo no número de 
#falhas igual a 0.001877