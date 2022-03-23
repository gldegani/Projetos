#packages
pacotes <- c("plotly","tidyverse","knitr","kableExtra",
             "see","ggraph","nortest","rgl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#loading database
books <- read.csv("bestsellers with categories.csv", header = T, sep = ",", stringsAsFactors = F)


#################################################################################################################################
#                                              PROBLEMA DE PESQUISA                                                             #
#################################################################################################################################
#os livros mais vendidos são de ficção ou não?
#existe correlação entre as variáveis, de modo que é possível verificar os fatores que mais influenciam na venda de um livro?
#o preço dos livros é um fator determinante nos mais vendidos?
#livros mais caros são mais bem avaliados?
#################################################################################################################################

#verificando a base de dados carregada (50 livros mais vendidos na Amazon entre 2009 e 2019 -> total de 500 livros)
books %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "bordered",
                full_width = F,
                font_size = 14)

#visualizando as observações do dataset
glimpse(books) #verificado que a variável "Year" está como inteiro. Será feita a alteração para um dado tipo "string"

books$Year <- as.character(books$Year)

#analisando as principais estatísticas do dataset (rating, reviews e price)
summary(books) #nota-se que a nota mais baixa é 3.3

#################################################################################
#                                ANÁLISE GRÁFICA                                #
#################################################################################

#gênero dos livros mais vendidos
genero <- ggplot(data = books) +
  aes(Genre) + geom_bar(position = "dodge", fill = "aquamarine4") + 
  labs(title = "Classificação em ficção ou não ficção dos livros mais vendidos",
      x = "Gênero", 
      y = "Quantidade") + theme_bw()

ggplotly(genero) #nota-se que a maioria dos livros mais vendidos são de não-ficção


#gênero dos livros mais vendidos em porcentagem
genero_percent <- ggplot(data = books) +
  aes(Genre, fill = Genre) + geom_bar(position = "dodge", aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) + 
  labs(title = "Porcentagem dos livros mais vendidos, agrupados conforme seu genêro %",
       x = " ",
       y = "Porcentagem")

ggplotly(genero_percent) #56% dos livros mais vendidos foram de não ficção

glimpse(books)

#verificando a nota dada pelos usuários dos livros mais vendidos
review_hist <- ggplot(books) +
  aes(x = User.Rating) + geom_histogram(color = "black", fill = "springgreen3", bins = 15) +
  labs(title = "Histograma com as notas dos leitores",
       x = "Nota (0 - 5)",
       y = "Frequência simples") + theme_bw()

ggplotly(review_hist) #a nota mais baixa é 3.3

#buscando qual o nome do livro que possui a nota mais baixa
subset(books, books$User.Rating == min(books$User.Rating), select = "Name")
books[books$User.Rating == min(books$User.Rating), "Name"]

#verificando quais os títulos que possuem as notas mais altas
books[books$User.Rating == max(books$User.Rating), c("Name","User.Rating", "Year")]
count(books[books$User.Rating == max(books$User.Rating), c("Name","User.Rating", "Year")]) #existem 52 títulos com a nota máxima.
                                                                                           #Pode ocorrer do mesmo livro estar em mais de um ano

n_distinct(books[books$User.Rating == max(books$User.Rating), "Name"]) #analisando mais a fundo, existem 28 livros

#verificando o preço dos livros mais vendidos
price <- ggplot(data = books) +
  aes(x = Price) + geom_histogram(color = "black", fill = "slateblue2", bins = 30) +
  labs(title = "Histograma dos preços dos livros mais vendidos entre 2009 - 2019",
       x = "Preço ($)",
       y = "Frequência") + theme_classic()

ggplotly(price)

#Notou-se que existem alguns livros com o preço igual a zero. A seguir os mesmos serão apresentados
books[books$Price == 0, c("Name", "Year", "Price")]

#verificando o preços dos livros mais vendidos (separados por anos)
price_year <- ggplot(books) +
  aes(x = Price, fill = Year) + geom_histogram(alpha = 0.6, binwidth = 5) +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~Year)
  
ggplotly(price_year)

#verificando o preços dos livros mais vendidos (separados por anos em um violin plot)
violin_price <- ggplot(books) +
  aes(x = Year, y = Price, fill = Year) +
  geom_violin(width = 2.2, size = 0.2) +
  stat_summary(fun = "mean",            #incluindo a média dos valores de cada ano
               geom = "crossbar",
               color = "red") +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis(discrete = T) +
  labs(title = "Preço dos livros mais vendidos por ano (2009 - 2019) [formato violino]",
       y = "Preço ($)") + theme_bw() + coord_flip()

ggplotly(violin_price)

#Verificando os livros com mais reviews e sua respetiva nota
revisados <- subset(books, select = c(Name, Reviews, User.Rating))
revisados <- revisados[order(revisados$Reviews, decreasing = TRUE), ]
revisados <- unique(revisados)
revisados[1:5,]

#Verificando os livros mais populares (baseados na quantidade de reviews)
books_popular <- books[order(books$Reviews, decreasing = T), ]
books_popular <- books_popular[1:10, ]

popular_books <- books_popular %>% 
  mutate(Name = fct_reorder(Name, Reviews)) %>% 
  ggplot(aes(x = Reviews, y = Name)) +
  geom_col(position = "dodge", fill = "firebrick") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Livros mais vendidos entre 2009 e 2019",
       x = "Número de reviews",
       y = "Livro") + coord_flip()

ggplotly(popular_books)

#Organizando os livros conforme sua popularidade (baseado na qtde de reviews)
books_reviews <- books %>% mutate(popularidade = cut(Reviews,
                                                     c(-Inf, quantile(books$Reviews,
                                                                      type = 5,
                                                                      probs = c(0.25,0.50,0.75),
                                                                      TRUE), Inf),
                                                     c("1° Quartil",
                                                       "2° Quartil",
                                                       "3° Quartil",
                                                       "4° Quartil")))
books_reviews <- select(books_reviews, Name:Reviews, popularidade)

glimpse(books_reviews)

#Somando o número de revisões totais de cada livro
books_vendidos <- books %>% 
  group_by(Name) %>% 
  summarise(Total_Reviews = sum(Reviews)) %>% 
  top_n(n = 10)

books_vendidos <- books_vendidos[order(books_vendidos$Total_Reviews, decreasing = TRUE), ]

livros_mais_vendidos_total <- books_vendidos %>% 
  mutate(Name = fct_reorder(Name, Total_Reviews)) %>% 
  ggplot(aes(x = Total_Reviews, y = Name)) +
  geom_col(position = "dodge", fill = "chartreuse2") +
  labs(title = "Livros mais vendidos no total",
       x = " ",
       y = " ") + theme_classic()

ggplotly(livros_mais_vendidos_total)

#Tabela organizando os livros por número de reviews e popularidade
books_reviews %>% 
  mutate(books_reviews[order(books_reviews$Reviews, decreasing = T), ]) %>% 
  select(Name, Author, Reviews, popularidade) %>% 
  kbl(caption = "Popularidade dos livros por n° de reviews") %>% 
  kable_classic(full_width = FALSE, html_font = "Cambria")

#verificando qual o livro mais caro na lista dos mais vendidos
books[books$Price == max(books$Price), c("Name", "Year")] #observa-se que o livro mais vendido é um livro técnico

#Verificando se os livros mais caros são os que possuem as maiores notas
dispersao <- ggplot(books) +
  aes(x = Price, y = User.Rating, fill = Genre) + geom_point(size = 4) +
  labs(title = "Gráfico de dispersão com o preço dos livros e as notas dos usuários, separado por gênero",
       x = "Preço ($)",
       y = "Nota dos usuários") + theme_bw()

ggplotly(dispersao) #claramente não existe uma correlação entre o preço do livro com a nota atribuída pelo leitor 


#agrupando os gráficos para facilitar a visualização
library(gridExtra)
grid.arrange(
  genero_percent,
  price,
  review_hist,
  livros_mais_vendidos_total,
  nrow = 2, ncol = 2)

#verificando a correlação das variáveis do dataset
chart.Correlation(books[3:5], histogram = TRUE) #nenhuma correlação forte
