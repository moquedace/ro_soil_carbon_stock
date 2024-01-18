---
Título: "Tutorial - Δ Normalized Burn Ratio (NBR)"
Autores: "Cássio Moquedace, Khaterine Martinez, Lara Lima, Rugana Imbana"
---


## **Objetivo**
Calcular e espacializar o nível de severidade de queimadas com o Δ *Normalized Burn Ratio* (NBR) em ambiente R

## ***Normalized Burn Ratio* (NBR)**
É um índice derivado das bandas de infravermelho próximo (NIR) e infravermelho médio (SWIR) (Figura 1), que constitui uma técnica de estimar o nível de severidade do fogo, potencialmente capaz de quantificar alterações que ocorrem na vegetação e no solo causadas pelo fogo.

<p align="center">
<img src="https://i.ibb.co/zPWvvJQ/grafico.jpg" width="700">
</p>

**Figura 1.** Curvas de resposta espectral. (Adaptado do Serviço Florestal dos EUA)

As mudanças decorrentes após um incêndio provocam modificações no espectro eletromagnético, pelo fato do fogo consumir a vegetação e destruir a clorofila, deixando solo descoberto, além de carbonizar as raízes e alterar a umidade relativa do solo. As transformações no espectro consistem em aumento na região do visível e do infravermelho médio e, na diminuição na região próxima ao infravermelho.

Nem sempre é fácil mapear essas mudanças usando métodos tradicionais, principalmente quando áreas afetadas são grandes, tendo topografia complexa e encostas íngremes, dificultando acessibilidade. Diante disso o emprego de ferramentas de sensoriamento remoto, como o NBR, facilitam o monitoramento de áreas queimadas e avaliação da severidade do fogo.

Salienta-se que, a determinação do perímetro do incêndio e a distribuição dos níveis de severidade em seu interior são importantes para o planejamento da restauração de áreas afetadas, assim como para análise dos efeitos do fogo na sucessão da vegetação pós-fogo. O índice é calculado conforme a equação NBR = (NIR - SWIR) / (NIR + SWIR).

### Área de exemplo
Utilizaremos como exemplo para este tutorial o bioma brasileiro Pantanal, que este ano (2020) a intensidade de focos de calor observada é sem precedentes. Quase que a totalidade desses focos se tratam de queimadas criminosas, especialmente para limpeza de pastagens oriundas de desmatamento ilegal ([PrevFogo](https://ecoa.org.br/as-6-causas-principais-da-tragedia-dos-incendios-no-pantanal/)).

Devido a grande extensão territorial do bioma Pantanal, utilizaremos imagens oriundas do produto do espectrorradiômetro de imagem de resolução moderada (MODIS) Terra MOD09A1 Versão 6 com resolução espacial de 500 metros. Para o cálculo do NBR serão utilizadas as bandas 2 e 6 de acordo com a equação NBR MODIS = (B2 - B6) / (B2 + B6)

### Δ *Normalized Burn Ratio* (NBR)
O índice é sensível a umidade, dessa forma, para evitar que áreas com baixa umidade como pastagens degradadas sejam contabilizadas como áreas queimadas, utilizar o delta Δ NBR anula esse efeito. A utilização de imagens capturadas em situação pré-fogo e situação pós fogo, atenua a precisão de inferência do índice, para áreas que de fato sofreram queimadas. O cálculo do índice será realizado conforme equação Δ NBR = (NBR 2019) - (NBR 2020)

<p>&nbsp;</p>

## **Execução dos códigos**
### Carregando pacotes necessários e definindo local de trabalho
Limpando memória não utilizada no R
```{r message=FALSE}
gc()
```

Carregando pacotes
```{r message=FALSE}
pkg <- c("raster", "sf", "sp", "rgdal", "dplyr", "MODIStsp", "tmap","MODIS", "tmaptools", "gdalUtilities", "gdalUtils", "rgeos", "kableExtra")

sapply(pkg, require, character.only = T)
```

Limpando todos os objetos gravados no R
```{r message=F}
rm(list = ls())
```

Definindo local de trabalho
```{r message=FALSE}
setwd("D:/OneDrive/Cássio/R/sol 793/tuto_3")
```

<p>&nbsp;</p>

### Baixando imagens MODIS
Maiores detalhes de como baixar imagens usando o pacote `MODIStsp` acesse [MODIStsp v2.0.2](https://docs.ropensci.org/MODIStsp/articles/interactive_execution.html)

Baixando dados 2019
```{r message=FALSE, eval=FALSE, echo=TRUE}
MODIStsp()
```

Baixando dados 2020
```{r message=FALSE, eval=FALSE, echo=TRUE}
MODIStsp()
```

<p>&nbsp;</p>

### Preparando dados
#### Carregando `shapefile` com os limites do pantanal
Lendo arquivo com todos os biomas, filtrando somente o pantanal e reprojetando para `SIRGAS 2000`

Limites dos biomas baixados no IBGE no link [IBGE Biomas](https://docs.ropensci.org/MODIStsp/articles/interactive_execution.html)

Datum e projeção que utilizaremos no mapa, mais informações das projeções de classe CRS PROJ.4, compatível com pacote `raster` do R acesse [EPSG.io](https://epsg.io/)
```{r message=FALSE}
pantanal_lim <- st_read(dsn = "../dados/shp/pantanal",
                        layer = "lm_bioma_250") %>%
  filter(Bioma == "Pantanal") %>%
  st_transform(crs = "EPSG:5641")
```

Visualizando os limites
```{r message=FALSE, fig.width=10, fig.height=4, fig.align='center'}
tm_shape(pantanal_lim) +
  tm_sf()
```
<p align="center">
<img src="limite_pantanal.jpg" width="400">
</p>

<p>&nbsp;</p>

#### Lendo imagens
Lendo arquivo `hdf` com as imagens baixadas
```{r message=FALSE}
nome_hdf <- list.files(path = "../dados/modis/hdf/", pattern = ".hdf$", full.names = T)
```

Extraindo `rasters` do arquivo `hdf`
```{r message=FALSE}
hdf_list <- list()

for (i in seq_along(nome_hdf)) {
  
  hdf_list[[i]] <- getSds(nome_hdf[i])
  
}
```


Separando as bandas dos `rasters` atribuindo a uma lista e renomeando
```{r message=FALSE, warning=FALSE}
list_raster <- list()

list_raster[[1]] <- raster(readGDAL(hdf_list[[1]][["SDS4gdal"]][2], as.is = TRUE))
list_raster[[2]] <- raster(readGDAL(hdf_list[[2]][["SDS4gdal"]][2], as.is = TRUE))
list_raster[[3]] <- raster(readGDAL(hdf_list[[1]][["SDS4gdal"]][6], as.is = TRUE))
list_raster[[4]] <- raster(readGDAL(hdf_list[[2]][["SDS4gdal"]][6], as.is = TRUE))
list_raster[[5]] <- raster(readGDAL(hdf_list[[3]][["SDS4gdal"]][2], as.is = TRUE))
list_raster[[6]] <- raster(readGDAL(hdf_list[[4]][["SDS4gdal"]][2], as.is = TRUE))
list_raster[[7]] <- raster(readGDAL(hdf_list[[3]][["SDS4gdal"]][6], as.is = TRUE))
list_raster[[8]] <- raster(readGDAL(hdf_list[[4]][["SDS4gdal"]][6], as.is = TRUE))

nomes <- c("2019.b2.1", "2019.b2.2", "2019.b6.1", "2019.b6.2", "2020.b2.1", "2020.b2.2", "2020.b6.1", "2020.b6.2")

names(list_raster) <- nomes
```

<p>&nbsp;</p>

#### Mosaicos
Construindo um mosaico para cada banda 
```{r message=FALSE}
b2_2019_mosaico <- mosaic(list_raster[["2019.b2.1"]],
                          list_raster[["2019.b2.2"]], fun = mean)

b6_2019_mosaico <- mosaic(list_raster[["2019.b6.1"]],
                          list_raster[["2019.b6.2"]], fun = mean)

b2_2020_mosaico <- mosaic(list_raster[["2020.b2.1"]],
                          list_raster[["2020.b2.2"]], fun = mean)

b6_2020_mosaico <- mosaic(list_raster[["2020.b6.1"]],
                          list_raster[["2020.b6.2"]], fun = mean)
```

<p>&nbsp;</p>

#### *Normalized Burn Ratio* (NBR)

Criando função para cálculo da diferença normalizada
```{r message=FALSE}
f.nbr <- function(x, y){
  nbr <- (x - y) / (x + y)
  return(nbr)
}
```

Calculando *Normalized Burn Ratio* (NBR) para 2019

NBR = (NIR - SWIR) / (NIR + SWIR) ou NBR MODIS = (B2 - B6) / (B2 + B6)
```{r message=FALSE}
nbr_2019 <- f.nbr(b2_2019_mosaico, b6_2019_mosaico)
```

Calculando *Normalized Burn Ratio* (NBR) para 2020

NBR = (NIR - SWIR) / (NIR + SWIR) ou NBR MODIS = (B2 - B6) / (B2 + B6)
```{r message=FALSE}
nbr_2020 <- f.nbr(b2_2020_mosaico, b6_2020_mosaico)
```

Visualizando *Normalized Burn Ratio* (NBR) para 2019 e 2020
```{r message=FALSE, fig.width=10, fig.height=4, fig.align='center'}
nbr_2019_fig <- tm_shape(nbr_2019, raster.downsample = T) +
  tm_raster(midpoint = NA, style = "fisher") +
  tm_layout(legend.outside = T, main.title = "NBR 2019")

nbr_2020_fig <- tm_shape(nbr_2020, raster.downsample = T) +
  tm_raster(midpoint = NA, style = "fisher") +
  tm_layout(legend.outside = T, main.title = "NBR 2020")

tmap_arrange(nbr_2019_fig, nbr_2020_fig)
```
<p align="center">
<img src="nbr_sinu.jpg" width="500">
</p>


Calculando Δ *Normalized Burn Ratio* (NBR) Δ NBR = (NBR 2019) - (NBR 2020)
```{r message=FALSE}
delta_nbr <- nbr_2019 - nbr_2020
```

Visualizando o Δ NBR
```{r message=FALSE, fig.width=10, fig.height=4, fig.align='center'}
tm_shape(delta_nbr, raster.downsample = T) +
  tm_raster(midpoint = NA, style = "fisher") +
  tm_layout(legend.outside = T)
```
<p align="center">
<img src="delta_nbr.fig.jpg" width="500">
</p>

#### Ajustando Δ NBR
Reprojetando Δ NBR
```{r message=FALSE, warning=FALSE}
delta_nbr <- projectRaster(delta_nbr, crs = crs(pantanal_lim))
```

Recortando Δ NBR para extensão do Pantanal
```{r message=FALSE, warning=FALSE}
delta_nbr_mask <- delta_nbr %>% 
  crop(pantanal_lim) %>% 
  mask(pantanal_lim)
```

Visualizando o Δ NBR recortado
```{r message=FALSE, fig.width=10, fig.height=4, fig.align='center'}
tm_shape(delta_nbr_mask, raster.downsample = F) +
  tm_raster(midpoint = NA, style = "fisher", palette = "-RdYlGn") +
  tm_layout(legend.outside = T)
```
<p align="center">
<img src="delta_nbr_mask.fig.jpg" width="450">
</p>

#### Reclassificando valores Δ NBR
Categorias propostas pelo [USGS](https://burnseverity.cr.usgs.gov/pdfs/LAv4_BR_CheatSheet.pdf)

```{r echo=FALSE, warning=FALSE, message=FALSE}
categ_queima <- data.frame("NBR" = c("< -0,25", "-0,25 a -0,1", "-0,1 a +0,1", "0,1 a 0,27", "0,27 a 0,44", "0,44 a 0,66", "> 0,66"), "classe" = c("Alta regeneração pós-fogo", "Baixo crescimento pós-fogo", "Não queimado", "Queimada de baixa gravidade", "Queimada de gravidade moderada-baixa", "Queimada de gravidade moderada-alta", "Queimada de alta gravidade" ))

categ_queima %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
  }) %>%
  mutate(classe = cell_spec(
    classe, color = "white", bold = T,
    background = spec_color(1:7, end = 0.9, option = "B", direction = -1)
  )) %>%
  kable(escape = F, align = "c", col.names = c("$\\Delta$ NBR", "Categoria"), caption = "**Tabela 1 - Distribuição dos intervalos $\\Delta$ NBR de acordo com a categoria de severidade da queimada **") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "bordered"), full_width = T)
```

<p align="center">
<img src="tabela.jpeg" width="900">
</p>

Fonte: Adaptado de [KARL, 2012](https://wiki.landscapetoolbox.org/doku.php/remote_sensing_methods:normalized_burn_ratio)


Criando `matrix` de classificação
```{r message=FALSE, warning=FALSE}
m_rec <- matrix(c(-Inf, -0.25, 1,
                  -0.25, -0.1, 2,
                  -0.1, 0.1, 3,
                  0.1, 0.27, 4,
                  0.27, 0.44, 5,
                  0.44, 0.66, 6,
                  0.66, Inf, 7),
                ncol = 3, byrow = T)
```

Aplicando reclassificação
```{r message=FALSE, warning=FALSE}
reclas_nbr <- raster::reclassify(delta_nbr_mask, rcl = m_rec)
```

Visualizando distribuição das classes
```{r message=FALSE, fig.width=10, fig.height=5, fig.align='center'}
hist(reclas_nbr)
```
<p align="center">
<img src="histograma.jpg" width="600">
</p>

Calculando área das classes
```{r message=FALSE, warning=FALSE}
area_classe <- raster::zonal(reclas_nbr, reclas_nbr, fun = "count") *
  (prod(res(reclas_nbr))) / 1e+6
```

Atribuindo nome as classes
```{r message=FALSE, warning=FALSE}
n_classes <- c("Alta regeneração pós-fogo",
               "Baixo crescimento pós-fogo",
               "Não queimado",
               "Queimada de baixa gravidade",
               "Queimada de gravidade moderada-baixa", 
               "Queimada de gravidade moderada-alta",
               "Queimada de alta gravidade")

categ <- data.frame("categoria" = n_classes, "area_km_2" = area_classe[, 2])
```

<p>&nbsp;</p>

### Criando mapa final
Definindo extensão do mapa
```{r message=FALSE, warning=FALSE}
bbox.pantanal <- st_bbox(reclas_nbr)

xrange <- bbox.pantanal$xmax - bbox.pantanal$xmin
yrange <- bbox.pantanal$ymax - bbox.pantanal$ymin

bbox.pantanal[1] <- bbox.pantanal[1] - (0.3 * xrange) 
bbox.pantanal[3] <- bbox.pantanal[3] + (0.3 * xrange) 
bbox.pantanal[2] <- bbox.pantanal[2] - (0.05 * yrange) 
bbox.pantanal[4] <- bbox.pantanal[4] + (0.05 * yrange)

bbox.pantanal <- bbox.pantanal %>% st_as_sfc()
```

Criando legenda de texto para o mapa
```{r message=FALSE, warning=FALSE}
legend_p <- "Dados: MODIS Terra MOD09A1\nLimites bioma: IBGE\nDatum: SIRGAS 2000\nResolução espacial: 500m"
```

Gerando mapa
```{r message=FALSE, fig.width=12, fig.height=6.75, fig.align='center'}
mapa_pronto <- tm_shape(reclas_nbr, raster.downsample = F, bbox = bbox.pantanal) +
  tm_graticules(lines = F, n.x = 3, n.y = 4, labels.rot = c(0, 90),
                labels.size = 1) + # Adicionando e configurando grid de coordenadas
  tm_raster(style = "cat", palette = "-RdYlGn", # Definindo cor e estilo da paleta
            labels = paste0(categ$categoria, " (", # Atribuindo texto e área das categorias na legenda
                            trimws(format(round(categ$area_km_2, 1),
                                          nsmall = 1, big.mark = ".",
                                          decimal.mark = ",")), " km²)"),
            title = expression(bold(paste(Delta, " NBR", " (USGS)")))) + # Adicionando título da legenda
  tm_layout(legend.title.fontface = "bold", # Título da legenda em negrito
            legend.text.size = 0.65, # Tamanho do texto da legenda
            legend.position = c("RIGHT", "bottom"), # Posição da legenda
            legend.format = list(text.align = "left"), # Alinhando texto da legenda
            legend.width = 0.5) + # Largura da legenda
  tm_credits(text = legend_p, align = "center", fontface = "bold",
             position = c("left", "bottom")) + # Adicionando texto dos dados e configurando estilo
  tm_scale_bar(text.size = 0.5, width = 0.2, position = c("left", "bottom")) + # Adicionando escala ao mapa
  tm_compass(type ="8star", size = 4, position = c("right", "top")) + # Adicionando rosa dos ventos ao mapa
  tm_shape(pantanal_lim) + # Inserindo limites do Pantanal
  tm_borders(lwd = 0.75, col = "black") + # Cor e espessura da linha de borda
  tm_add_legend(type = "fill", col = "transparent", # Adicionando o limite do bioma a legenda do mapa
                border.col = 'black',
                labels =  "Limites Pantanal")

mapa_pronto
```

<p align="center">
<img src="mapa_nbr_pantanal.jpg" width="700">
</p>

<p>&nbsp;</p>

### Salvando mapa final
```{r error=TRUE, message=FALSE, echo=T, eval=F}
tmap_save(mapa_pronto, "../mapa/mapa_nbr_pantanal.jpg", dpi = 600, width = 12, height = 6.75, units = "in")
```

<p>&nbsp;</p>

### Vídeo tutorial
[<img src="thumb_git.jpg" width="600">](https://youtu.be/PINuGoRUPC4)

<p>&nbsp;</p>