#packages
packages <- c("plotly","tidyverse","knitr","kableExtra",
             "see","ggraph","nortest","rgl")

if(sum(as.numeric(!packages %in% installed.packages())) != 0){
  instalador <- packages[!packages %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(packages, require, character = T) 
} else {
  sapply(packages, require, character = T) 
}


#loading database
books <- read.csv("bestsellers with categories.csv", header = T, sep = ",", stringsAsFactors = F)


#chaecking database (amazon top 50 bestselling books 2009 - 2019)
books %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "bordered",
                full_width = F,
                font_size = 14)


glimpse(books)


#checking statistics (rating, reviews e price)
summary(books)

#################################################################################
#                               GRAPHICS ANALYSIS                               #
#################################################################################


#Books Genre
books_genre <- ggplot(data = books) +
  aes(Genre, fill = Genre) + geom_bar(position = "dodge", aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) + 
  labs(title = "best-selling books by genre",
       x = " ",
       y = "%")

ggplotly(books_genre)


#ratings
books_rating <- ggplot(books) +
  aes(x = User.Rating) + geom_histogram(color = "black", fill = "springgreen3", bins = 15) +
  labs(title = "Books rating",
       x = "Rating",
       y = " ") + theme_bw()

ggplotly(books_rating)

#checking the lowest rating
subset(books, books$User.Rating == min(books$User.Rating), select = "Name")

#checking the highest rating
books[books$User.Rating == max(books$User.Rating), c("Name","User.Rating", "Year")]
count(books[books$User.Rating == max(books$User.Rating), c("Name","User.Rating", "Year")]) #52 books

#Pode ocorrer do mesmo livro estar em mais de um ano
n_distinct(books[books$User.Rating == max(books$User.Rating), "Name"]) #28 distinct books

#price
price <- ggplot(data = books) +
  aes(x = Price) + geom_histogram(color = "black", fill = "slateblue2", bins = 30) +
  labs(title = "Price",
       x = "Preço ($)",
       y = "Frequência") + theme_classic()

ggplotly(price)

#It was noticed that there are some books with the price of zero. They will be presented below.
books[books$Price == 0, c("Name", "Year", "Price")]

#price by year
price_year <- ggplot(books) +
  aes(x = Price, fill = Year) + geom_histogram(alpha = 0.6, binwidth = 5) +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis(discrete = T) +
  facet_wrap(~Year)

ggplotly(price_year)

#price by year (violin plot)
violin_price <- ggplot(books) +
  aes(x = Year, y = Price, fill = Year) +
  geom_violin(width = 2.2, size = 0.2) +
  stat_summary(fun = "mean",            #with mean
               geom = "crossbar",
               color = "red") +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis(discrete = T) +
  labs(title = "Price by Year - violin plot",
       y = "Preço ($)") + theme_bw() + coord_flip()

ggplotly(violin_price)



#10 most popular books based on total number of reviews
books_sold <- books %>% 
  group_by(Name) %>% 
  summarise(Total_Reviews = sum(Reviews)) %>% 
  top_n(n = 10)

books_sold <- books_sold[order(books_sold$Total_Reviews, decreasing = TRUE), ]

books_most_sold <- books_sold %>% 
  mutate(Name = fct_reorder(Name, Total_Reviews)) %>% 
  ggplot(aes(x = Total_Reviews, y = Name)) +
  geom_col(position = "dodge", fill = "chartreuse2") +
  labs(title = "Best selling books",
       x = " ",
       y = " ") + theme_classic()

ggplotly(books_most_sold)


#most expensive book
books[books$Price == max(books$Price), c("Name", "Year")]
books %>% filter(Price == max(Price)) %>% 
  select(Name, Year, Price)

#Checking if the most expensive books are the ones with the highest ratings
scatter <- ggplot(books) +
  aes(x = Price, y = User.Rating, fill = Genre) + geom_point(size = 4) +
  labs(title = "Scatter plot -  price and reviews, by genre",
       x = "Price ($)",
       y = "Nota dos usuários") + theme_bw()

ggplotly(scatter) #claramente não existe uma correlação entre o preço do livro com a nota atribuída pelo leitor 


#agrupando os gráficos para facilitar a visualização
library(gridExtra)
grid.arrange(
  books_genre,
  books_rating,
  price,
  price_year,
  violin_price,
  scatter,
  nrow = 3, ncol = 2)

#looking for correlation
chart.Correlation(books[3:5], histogram = TRUE) #no correlation found

#END
