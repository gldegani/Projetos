#instalação de pacotes

pacotes <- c("rgdal","raster","tmap","maptools","tidyverse","broom","knitr",
             "kableExtra","RColorBrewer")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#carregando um arquivo shapefile
shp_aprendizado <- readOGR(dsn = "C:\\Users\\GABRIEL\\Documents\\USP\\Análise Espacial\\aprendizado\\BR_Mesorregioes_2020",
                           layer = "BR_Mesorregioes_2020")

#verificando os atributos do shapefile
summary(shp_aprendizado)

#analisando a classe e o tipo do objeto
class(shp_aprendizado)
typeof(shp_aprendizado)

#plotando o objeto
plot(shp_aprendizado)

#acessando a base de dados do shapefile
shp_aprendizado@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "bordered",
                full_width = T,
                font_size = 12)

#verificando outros componentes do shapefile
shp_aprendizado@polygons #Posições geográficas dos polígonos
shp_aprendizado@plotOrder #Ordem de plotagem dos polígonos
shp_aprendizado@bbox #Eixo X (Longitude Oeste e Leste; Latitude Norte e Sul)
shp_aprendizado@proj4string@projargs #Sistema de projeção geográfica do shapefile

#acessando os dados de modo específico
shp_aprendizado@data[135,]

#analisando as áreas das mesoregiões
raster::area(shp_aprendizado)

#calculando a área de cada região
shp_aprendizado@data$area <- raster::area(shp_aprendizado) / 1000000

shp_aprendizado@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "bordered",
                full_width = T,
                font_size = 12)

###UNINDO DADOS EXTERNOS

#carregando a base de dados (retirada do site: https://www.ipea.gov.br/ipeageo/bases.html)
library(readxl)
df <- read_excel(path = "C:\\Users\\GABRIEL\\Documents\\USP\\Análise Espacial\\aprendizado\\IBGE_CIDADES_MESOREGIAO.xls",
                 sheet = 1, col_names = TRUE)
head(df)

df %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 12,
                full_width = T)

#data wrangling
df <- df %>% 
  rename("CD_MESO" = "Código da Mesoregião")

df %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                font_size = 12,
                full_width = T)

#combinando os dados
shp_dados <- merge(x = shp_aprendizado,
                   y = df,
                   by.x = "CD_MESO",
                   by.y = "CD_MESO")

shp_dados@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

###VISUALIZAÇÃO DE DADOS ESPACIAIS

#utilizando o ggplot2 (grande pode computacional, não recomendado)

##passo 1: Transformar o shapefile num objeto do tipo data frame e, depois,
# importar os dados que já estavam no shapefile para o novo objeto data frame
shp_dados_ggplot <- tidy(shp_dados, region = "CD_MESO")

shp_dados_ggplot <- shp_dados_ggplot %>% rename(CD_MESO = id)

shp_dados_ggplot <- shp_dados_ggplot %>% 
  left_join(shp_dados@data,
            by =  "CD_MESO")

##passo 2: plotando o gráfico
shp_dados_ggplot %>%
  rename("qtde_sus" = "Estabelecimentos de Saúde SUS" ) %>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = qtde_sus),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "N° de estab SUS") +
  scale_color_viridis_c() +
  theme_bw()

#utilizando a biblioteca tmap

shp_dados@data <- shp_dados@data %>% 
  rename(qtde_sus = 7)

tm_shape(shp = shp_dados) +
  tm_fill(col = "qtde_sus", palette = "Reds")

##com mais detalhes
tm_shape(shp = shp_dados) +
  tm_fill(col = "qtde_sus",
          style = "quantile",
          n = 4,
          palette = "Greens")

##adicionando ainda mais detalhes e deixando completo
tm_shape(shp = shp_dados) + 
  tm_fill(col = "qtde_sus",
          style = "quantile",
          n = 4,
          palette = "Reds",
          legend.hist = T) +
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            legend.hist.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.width = 0.3,
            frame = F,
            main.title = "Quantidade de estabelecimentos da Rede SUS por mesorregião") +
  tm_borders(alpha = 0.8) + 
  tm_compass(type = "8star",
             show.labels = 3)


#mais um mapa de exemplo
shp_dados@data <- shp_dados@data %>% 
  rename(populacao = 12)

tm_shape(shp = shp_dados) + 
  tm_fill(col = "populacao",
          style = "quantile",
          n = 4,
          palette = "Set1") +
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            legend.hist.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.width = 0.3,
            frame = T,
            main.title = "População brasileira por mesorregião") +
  tm_borders(alpha = 0.8) + 
  tm_compass(type = "radar",
             show.labels = 3)

display.brewer.all()

#com base no mapa, obteve-se o interesse em verificar qual a mesorregião com a menor e a maior qtde de estabelecimentos
shp_dados@data %>% filter(qtde_sus == min(qtde_sus)) %>% 
  select(NM_MESO, SIGLA_UF, qtde_sus)

shp_dados@data %>% filter(qtde_sus == max(qtde_sus)) %>% 
  select(NM_MESO, SIGLA_UF, qtde_sus)

#salvando o shapefile
writeOGR(obj = shp_dados,
         layer = "shapefile_estudo",
         driver = "ESRI Shapefile",
         dsn = "shapefile_estudo_salvo")


###DESMEBRANDO SHAPEFILES

#carregando um novo shapefile
shp_mapa_mundi <- readOGR(dsn = "mundo_shp", layer = "mundo")

#visualizando o shapefile carregado
tm_shape(shp = shp_mapa_mundi) + tm_borders()

#observando as variáveis das base de dados do objeto
shp_mapa_mundi@data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

#neste exemplo, será criado um novo shapefile somente com os países da américa do norte
shp_north_america <- shp_mapa_mundi[shp_mapa_mundi@data$FIPS_CNTRY == "CA" | 
                                      shp_mapa_mundi@data$FIPS_CNTRY == "MX" | 
                                      shp_mapa_mundi@data$FIPS_CNTRY == "US", ]

tm_shape(shp = shp_north_america) + 
  tm_borders()

