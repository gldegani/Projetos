################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "fastDummies","lmtest","splines","jtools","questionr","MASS",
             "pscl","overdisp","magick","cowplot","beepr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library(gridExtra)
library(grid)
library(correlation)
library(ggraph)
library(PerformanceAnalytics)
################################################################################
#                           LENDO O DATAFRAME                                  #       
################################################################################
loan <- read.csv("Loan.csv", header = TRUE, sep = ",", stringsAsFactors = F)

head(loan)

################################################################################
#                           OBSERVAÇÃO DA BASE DE DADOS                        #       
################################################################################

#apresentação dos dados
loan %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 15)

#visualização das observações e das especificações das variáveis
glimpse(loan)

#Estatísticas descritivas univariadas e tabela de frequência
summary(loan)

#Tabela de frequência do gênero das pessoas que fazem empréstimo (pacote questionr)
freq(loan$Gender, total = T) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 15)

#verificando quantos valores na existem no data frama
table(is.na(loan))

#Verificou-se que existem valores NA no df. Neste caso, realizou-se um cálculo para identificar
#as porcentagem de valores NA no df, visando ações posteriores em cada coluna.
print(colMeans(is.na(loan) | loan == "") * 100)

################################################################################
#           ANÁLISE GRÁFICA DA BASE DE DADOS - VARIÁVEIS QUALITATIVAS          #       
################################################################################

#verificando os genêros
gender_basic_plot <- ggplot(data = loan) +
  geom_bar(aes(x = Gender), fill = "darkslategray3") +
  geom_text(aes(x = Gender, label = ..count..), stat = "count", vjust = 2) +
  labs(title = "Solicitação de empréstimo por genêro",
       x = "Genêro",
       y = "Quantidade") +
  theme_bw()

gender_basic_plot

#verificando os genêros - incrementado
gender_updated_plot <- ggplot(data = loan) +
  aes(x = Gender, fill = Gender) +
  geom_bar(position = "dodge", aes(y = (..count..)/sum(..count..))) +
  labs(title = "Solicitação de empréstimo por genêro (%)",
       x = "Genêro",
       y = "Porcentagem") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

#apresentado ambos os gráficos
grid.arrange(gender_basic_plot,
             gender_updated_plot)

#verificando o status civil da base de dados
ggplot(data = loan) +
  aes(x = Married) +
  geom_bar(position = "dodge", fill = "palegreen4") +
  labs(title = "Solicitantes casados",
       x = "Status Civil",
       y = "Qtde") +
  theme_classic()

#verificando o número de dependentes
ggplot(data = loan) +
  aes(x = Dependents) +
  geom_bar(position = "dodge", fill = "slateblue2") +
  labs(title = "Qtde de dependentes",
       x = "Status Civil",
       y = "Qtde") +
  theme_classic()

#verificação das pessoas casadas e a quantidade de dependentes
ggplot(data = loan) +
  aes(x = Married, fill = Dependents) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::percent)
  labs(title = "Solicitantes casados",
       x = "Status Civil",
       y = "Qtde") +
  theme_classic()
  
#grau de instrução dos solicitantes
ggplot(data = loan) +
  aes(x = Education, fill = Education) +
  geom_bar(position = "dodge") +
  labs(title = "Grau de instrução dos solicitantes",
       x = "Grau de instrução",
       y = "Qtde") +
  theme_gray() +
  coord_flip()


#Verificando quais são self-employees
ggplot(data = loan) +
  aes(x = Self_Employed) +
  geom_bar(position = "dodge", fill = "pink") +
  labs(title = "Auto-empregados",
       x = "Auto-empregados: sim ou não",
       y = "Quantidade") +
  theme_dark()

#Analisando a área da propriedade
ggplot(data = loan) +
  aes(x = Property_Area, fill = Property_Area) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(title = "Tipo da propriedade",
       x = "Propriedade",
       y = "Quantidade")

#Verificando o tipo da propriedade + Grau de instrução
ggplot(data = loan) +
  aes(x = Property_Area, fill = Education) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(title = "Tipo da propriedade + Graduação",
       y = "Qtde")

################################################################################
#           ANÁLISE GRÁFICA DA BASE DE DADOS - VARIÁVEIS QUANTITATIVAS         #       
################################################################################

#Analisando a correlação dos valores quantitativos da base de dados
loan %>% 
  correlation(method = "pearson") %>% 
  plot()

chart.Correlation((loan[7:10]), histogram = T)

#aplication income
aplication_income_plot <- ggplot(data = loan) +
  aes(x = ApplicantIncome) +
  geom_histogram(color = "black", fill = "lightblue", bins = 50) +
  labs(title = "Histograma dos valores de aplicação",
       x = "Valor aplicado") +
  theme_bw()

aplication_income_plot


#Loan_Amount_Term
Loan_Amount_Term_plot <- ggplot(data = loan) +
  aes(x = Loan_Amount_Term) +
  geom_histogram(color = "black", fill = "lightgreen", bins = 10) +
  labs(title = "Histograma dos valores dos termos de aplicação",
       x = "Valor aplicado") +
  theme_bw()

Loan_Amount_Term_plot

grid.arrange(aplication_income_plot,
             Loan_Amount_Term_plot)

################################################################################
#                           REGRESSÃO LINEAR SIMPLES                           #       
################################################################################

#Gráfico de dispersão entre o valor aplicado e o valor do empréstimo
ggplot(data = loan) +
  aes(x = ApplicantIncome, y = LoanAmount) +
  geom_point(color = "green", size = 2.5) +
  geom_smooth(aes(color = "Valores ajustados"),
              method = "lm", se = T, size = 2)
theme_bw() +
  labs(title = "Gráfico de dispersão",
       x = "Aplicação $",
       y = "Empréstimo $")

names(loan)

#Regressão Linear simples
modelo_loan <- lm(formula = LoanAmount ~ ApplicantIncome,
                  data = loan)

#observando os parâmetros do modelo
summary(modelo_loan)
summ(modelo_loan, confint = T, digits = 4, ci.width = 0.95)
export_summs(modelo_loan, scale = F, digits = 4)

#devida a alta dispersão dos dados e a presença de outliers, os resultados da regressão linear
#não foram satisfatórios!!!