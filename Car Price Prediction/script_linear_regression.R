#Pacotes utilizados
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

#Carregando a base de dados:
car_df <- read.csv("CarPrice_Assignment.csv", header = T, sep = ",", stringsAsFactors = F)

#Observando os valores da base de dados:
car_df %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "basic",
                full_width = F,
                font_size = 12)

#Visualizando as especificações de cada variável:
glimpse(car_df)

#Alterando o tipo da variável symboling para categórica:
car_df$symboling <- as.character(car_df$symboling)

#Tabela de frequência das variáveis qualitativas:
table(car_df$fueltype)

#Estatísticas univariadas:
summary(car_df)

#Nome das variáveis:
names(car_df)

#Correlação das variáveis quanti:
num.cols <- sapply(car_df, is.numeric)
car_df_quanti <- car_df[, num.cols]  #criando um dataframe apenas com variáveis quanti

chart.Correlation(car_df_quanti[2:15], histogram = T)

###outra forma de visualizar as correlações###
install.packages("corrplot")
library(corrplot)

corrplot(cor(car_df_quanti[2:15]), order = "hclust", method = "number")

###outra forma de visualizar as correlações###
car_df_quanti[2:15] %>% 
  correlation(method = "pearson") %>% #não é muito bom com grande quantidade de variáveis
  plot()

#Gráficos de pontos das variáveis numéricas:

##Price x Wheelbase
wheelbase <- car_df_quanti %>% 
  ggplot(aes(y = price, x= wheelbase)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x car length
car_length <- car_df_quanti %>% 
  ggplot(aes(y = price, x= carlength)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x car width
car_width <- car_df_quanti %>% 
  ggplot(aes(y = price, x= carwidth)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x car  height
car_heigth <- car_df_quanti %>% 
  ggplot(aes(y = price, x= carheight)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x curb weight
curb_weigth <- car_df_quanti %>% 
  ggplot(aes(y = price, x= curbweight)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x Engine Size
engine_size <- car_df_quanti %>% 
  ggplot(aes(y = price, x= enginesize)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x Bore Ratio
bore_ratio <- car_df_quanti %>% 
  ggplot(aes(y = price, x= boreratio)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x Stroke
stroke <- car_df_quanti %>% 
  ggplot(aes(y = price, x= stroke)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()


##Price x Compression Ratio
compression_ratio <- car_df_quanti %>% 
  ggplot(aes(y = price, x= compressionratio)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()


##Price x Horse Power
horse_power <- car_df_quanti %>% 
  ggplot(aes(y = price, x= horsepower)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x Peak RPM
peak_rpm <- car_df_quanti %>% 
  ggplot(aes(y = price, x= peakrpm)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##Price x City MPG
city_mpg <- car_df_quanti %>% 
  ggplot(aes(y = price, x= citympg)) +
  geom_point(color = "royalblue4", size = 4, alpha = 0.5) +
  theme_bw()

##agrupando os gráficos acima
library(gridExtra)
grid.arrange(
  wheelbase,
  car_length,
  car_width,
  car_heigth,
  curb_weigth,
  engine_size,
  bore_ratio,
  stroke,
  compression_ratio,
  horse_power,
  peak_rpm,
  city_mpg,
  nrow = 4, ncol = 3
)


#Verificando o comportamento da variável Y:
ggplot(data = car_df) +
  aes(x = price) +
  geom_histogram(color = "black", fill = "lightblue", bins = 30) +
  labs(title = "Histograma do preços dos veículos",
       x = "Preço do veículos",
       y = "Frequência") + theme_bw()

#KDE da variável Y
ggplotly(
ggplot(car_df, aes(x = price)) +
  geom_density(aes(x = price),
               position = "identity", color = "black", size = 2) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue", bins =  30) +
  theme_bw()
)


#Procedimento N-1 Dummies:
car_dummies <- dummy_columns(.data = car_df,
                             select_columns = c("symboling", "fueltype", "aspiration",
                                                "doornumber", "carbody", "drivewheel",
                                                "enginelocation", "enginetype",
                                                "cylindernumber", "fuelsystem"),
                             remove_selected_columns = T,
                             remove_most_frequent_dummy = T)

#Visualizando a base dummizada:
car_dummies %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 12)

#ESTIMAÇÃO DO MODELO DE REGRESSÃO:
modelo_car_dummiess <- lm(formula = price ~ .-car_ID - CarName,
                          data = car_dummies)

#PARÂMETROS DO MODELO CAR_DUMMIES:
summary(modelo_car_dummiess)
summ(modelo_car_dummiess, confint = T, digits = 4, ci.width = 0.95)

#SALVANDO OS FITTED VALUES E RESIDUALS NO DATASET
car_df_estimado <- car_df
car_df_estimado$yhat <- modelo_car_dummiess$fitted.values
car_df_estimado$erro <- modelo_car_dummiess$residuals

#VERIFICAÇÃO DOS RESÍDUOS À NORMALIDADE:
sf.test(modelo_car_dummiess$residuals) #com este teste, é possível observar que a regressão linear não é o melhor ajuste possível

#STEPWISE:
step_car <- step(modelo_car_dummiess, k = 3.841459)
#De onde vem o argumento k = 3.841459?
qchisq(p = 0.05, df = 1, lower.tail = F)
round(pchisq(3.841459, df = 1, lower.tail = F),7)

summary(step_car)

#VERIFICAÇÃO DOS RESÍDUOS À NORMALIDADE (após stepwise):
sf.test(step_car$residuals) #mesmo após o stepwise, o modelo linear não é a melhor opção

#HISTOGRAMA DOS RESÍDUOS À NORMALIDADE
car_dummies %>% 
  mutate(residuos = step_car$residuals) %>% 
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "grey50",
                 fill = "grey90",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm,
                args = list(mean = mean(step_car$residuals),
                            sd = sd(step_car$residuals)),
                aes(color = "Curva normal teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#TRANSFORMAÇÃO DE BOX-COX:

##calculando o lambda de box-cox (ajustar a Y para uma dist normal):
lambda_BC <- powerTransform(car_dummies$price)
lambda_BC


#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
car_dummies$bcretorno <- (((car_dummies$price^lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

#Visualizando a nova variável na base de dados
car_dummies %>% 
  select(price, bcretorno, everything()) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

#ESTIMANDO UM NOVO MODELO MÚLTIPLO COM VARIÁVEL DEPENDENTE (Y) TRANSFORMADA POR BOX-COX
modelo_box_cox_car <- lm(formula = bcretorno ~ .-car_ID - CarName,
                         data = car_dummies)

#PARÂMETROS DO MODELO BOX-COX
summary(modelo_box_cox_car)

#STEPWISE BOX-COX
step_box_cox <- step(modelo_box_cox_car, k = 3.841459)

summary(step_box_cox)

#VERIFICAÇÃO DOS RESÍDUOS À NORMALIDADE (após stepwise e transformação box-cox):
sf.test(step_box_cox$residuals) #mesmo após a box-cox, o modelo não se mostrou a melhor opção

#RESUMO DOS  MODELOS LINEAR E BOX-COX - com stepwise
export_summs(step_car, step_box_cox, scale = F, digits = 6)


##MESMO APÓS STEPWIS E BOX-COX, EVIDENCIOU-SE QUE O MODELO DE REGRESSÃO NÃO É O MELHOR!!!