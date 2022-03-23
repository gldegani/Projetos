#instalação dos pacotes necessários
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#listando os arquivos do project
list.files()

#carregando a base de dados (arquivo pode ser encontrado em: https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009)
wine <- read.csv("winequality-red.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


#################################################################################
#                               PROBLEMA DE PESQUISA                            #
#################################################################################

#Com as informações dos vinhos, é possível analisar e  prever a qualidade da bebida?
#Quais são as principais características que levam um vinho a ser considerado de "qualidade"?
#É possível classificar os vinhos em grupos apenas pelos seus atributos?
#################################################################################


#observando os dados carregados
wine %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "bordered",
                full_width = FALSE,
                font_size = 20)


#Visualizando as observações e as especificações referentes às variáveis do dataset
glimpse(wine)

#Analisando de maneira simples as principais estatísticas do dataset
summary(wine) ##verificando as informações, parecem existir alguns outliers na base

#Estudando a correlação das variáveis

##1° forma
chart.Correlation(wine,, histogram = T)

##2°forma
library(corrplot)
corrplot(cor(wine), order = "hclust", method = "number")

#Verificando graficamente o comportamento da Y

ggplot(wine, aes(x = quality)) +
  geom_bar(color = "black", fill = "palegreen1") +
  labs(title = "Qualidade do vinho",
       x = "Qualidade",
       y = "Total") + theme_bw()


#Criação do modelo de regressão linear (OLS)

modelo_ols_wine <- lm(formula = quality ~ .,
                      data = wine)

#Verificando os parâmetros do modelo
summary(modelo_ols_wine) #p-value < 0.05, há modelo estatisticamente significante
                         #contudo, existem variáveis que não são estatisticamente significantes

#Stepwise
step_wine <- step(modelo_ols_wine, k = 3.841459)
##De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F),7)

summary(step_wine) #com o stepwise, algumas variáveis foram removidas do modelo

#outra forma de verificar os resultados do modelo stepwise
summ(step_wine, confint = T, digits = 4, ci.width = .95)


#verificando como ficou o modelo de regressão após stepwise
step_wine$call

#salvando os fitted values e residuals na base de dados
wine_estimado <- wine
wine_estimado$yhat <- step_wine$fitted.values
wine_estimado$erro <- step_wine$residuals

#histograma da normalidade dos resíduos
plot_norm <- wine_estimado %>% 
  mutate(residuos = wine_estimado$erro) %>% 
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "grey50",
                 fill = "grey90",
                 bins = 30,
                 alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean(wine_estimado$erro),
                            sd = sd(wine_estimado$erro)),
                aes(color = "Curva normal teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(title = "Normalidade dos resíduos (com stepwise)",
       x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#verificando graficamente o modelo de regressão linear (após stepwise)
plot(step_wine)

#verificando o ajuste dos resíduos a normalidade
sf.test(step_wine$residuals) #com esse teste, é possível notar que o modelo de regressão linear não é o melhor modelo

#transformação de box-cox
##calculando o lambda de box-cox (ajustar a Y para uma dist normal):
lambda_BC <- powerTransform(wine_estimado$quality)
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
wine_estimado$bcretorno <- (((wine_estimado$quality^lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

#Visualizando a nova variável na base de dados
wine_estimado %>% 
  select(quality, bcretorno, everything()) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

#Estimando um novo modelo (transformado pela box-cox)
wine_box_cox <- lm(formula = bcretorno ~ .-quality - yhat - erro,
                   data = wine_estimado)
summary(wine_box_cox)

#comparando o modelo "normal" e com box-cox (ambos sem stepwise)
export_summs(wine_box_cox, modelo_ols_wine, scale = F, digits = 4)

#stepwise na box-cox
step_wine_box_cox <- step(wine_box_cox,  k = 3.841459)

#verificando os resultados do modelo
summary(step_wine_box_cox)

#análise dos resíduos à normalidade (após box-cox)
sf.test(step_wine_box_cox$residuals)

#histograma da normalidade dos resíduos - com box-cox e stepwise

wine_regression <- wine
wine_regression$yhat <- step_wine_box_cox$fitted.values
wine_regression$erro <- step_wine_box_cox$residuals


plot_regression <- wine_regression %>%
  mutate(residuos = wine_regression$erro) %>% 
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "grey50",
                 fill = "grey90",
                 bins = 30,
                 alpha = 0.7) +
  stat_function(fun = dnorm,
                args = list(mean = mean(wine_regression$erro),
                            sd = sd(wine_regression$erro)),
                aes(color = "Curva normal teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(title = "Normalidade dos resíduos (com stepwise e box-cox)",
       x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

plot_regression
ggsave("plot_regression.png") #salvando o gráfico em um arquivo png



#comparando os gráficos de ajuste a normalidade (com box-cox e stepwise X somente stepwise)
library(gridExtra)
grid.arrange(plot_norm,
             plot_regression)

#################################################################################
#                                  OFF TOPIC                                    #
#################################################################################

#adicionando um gif no gráfico

gif <- image_read("joey_gif.gif")
plot <- image_read("plot_regression.png")

frames <- image_composite(plot, gif, offset = "+750+30")
animation <- image_animate(frames, fps = 10)
image_scale(animation, "x550")
beep("treasure")
