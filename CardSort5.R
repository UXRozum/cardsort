#Автор скрипта - Сережа розум, https://www.uxrozum.com/
#Если вы в первый раз видите R, то этот материал будет очень полезен -  https://thecode.media/rrrrr/ , https://www.youtube.com/playlist?list=PLRDJTUEth2lErO5xbwoTyl9-X3H7lokZI

#Шаг 0 установка библиотек (запускается только один раз после установки R)
install.packages(c('openxlsx','igraph', 'factoextra', "ggwordcloud", 'rstudioapi'))

# Шаг 1; запуск библиотек
library(igraph)
library(openxlsx)
library(factoextra)
library(ggwordcloud)
library(rstudioapi)


#Шаг 2: выбираем папку
setwd(
  rstudioapi::selectDirectory()) #это должна быть отдельная папка на компе 
                                 #с латинским названием 
                                 #в папке должен находиться файл с данными - Card.xlsx

#Шаг 2.5: загрузка данных
Raw <- read.xlsx('Card.xlsx') #В файле должно быть три столбка минимум
                              #Card - названия карточек
                              #Group_id - номер группы (уникальный для КАЖДОЙ группы)
                              #Group_name - названия групп, которое давали респонденты


#Шаг 3: создаем таблицу сопряжений 
Adj <- crossprod(table(Raw$Group_id, Raw$Card))
diag(Adj) = 0

#Шаг 4: сохраняем таблицу сопряжений
write.xlsx(as.data.frame(Adj), 'Adjacency.xlsx',
           overwrite = T, col.names = T, row.names=T)

#Шаг 5 кластеризация

#Алгоритм edge betweenness -  https://en.wikipedia.org/wiki/Girvan%E2%80%93Newman_algorithm
Net <-  graph_from_adjacency_matrix(Adj, mode='undirected') #формируем структуру сетей на основе таблицы совпадений
Clust <- as.dendrogram(cluster_edge_betweenness(Net)) #строим кластеры
fviz_dend(Clust, k=6 #количество групп, которые нам нужно получить
             ,horiz=T) #выводим график


#Алгоритм k-means - https://habr.com/ru/post/67078/
fviz_nbclust(Adj, kmeans) #Вычисляем оптимальное количество кластеров
km <- kmeans(Adj , centers = 6 #строим кластеры
             , nstart = 25)
fviz_cluster(km, data= Adj) #выводим график

#Алгоритм hierarchical clusterisation (Ward) - https://en.wikipedia.org/wiki/Ward%27s_method

plot(hclust(dist(Adj), method='ward.D')) #тут сразу и кластеры формируем и график строим


#Шаг 6: Строим сетевой график
plot.new()
plot.igraph(
  graph_from_adjacency_matrix(ifelse(Adj-10 < 0, 0, Adj-10)
 #это количество связей, которое отсекаем, 
                                    #чем больше число, тем более сильне связи
                                    # если на графике каша, то стоит его увеличить
                                    # ориентируйся на количество респондентов
                            
                              , mode='undirected'), 
  vertex.label.color= "black", vertex.color= "gray", 
  vertex.size= 20, vertex.frame.color='gray',asp = 0.7,
  layout = layout.kamada.kawai,
  vertex.label.cex = 0.9,
  width=1, height=1)

#Шаг 7: облака слов для названий групп


cardgroup <- c('Грифон', 'Василиск', 'Кит') #сюда пишем названия карточек

gnames <- as.data.frame(table(Raw$Group_name[Raw$Card %in%  cardgroup])) #вытаскиваем названия карточек
ggplot(gnames, aes(label = Var1, size= Freq, color = Freq)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 10 #мксимальный размер слова, если все слова не помещаются, делаем его меньше
                  ) +
  theme_minimal() 


#Быстрый вариант: выделяем все, что между началом и концом, и жмем ctrl+enter
# Важно - файл с карточками должен называться cards.xlsx

# Если не установили, установите 
install.packages(c('openxlsx','igraph', 'factoextra', "ggwordcloud", 'rstudioapi'))



#Начало
library(igraph)
library(openxlsx)
library(factoextra)
library(ggwordcloud)
library(rstudioapi)

setwd(rstudioapi::selectDirectory())
Raw <- read.xlsx('Card.xlsx')
Adj <- crossprod(table(Raw$Group_id, Raw$Card))
diag(Adj) = 0
write.xlsx(as.data.frame(Adj), 'Adjacency.xlsx',
           overwrite = T, col.names = T, row.names=T)
Net <-  graph_from_adjacency_matrix(Adj, mode='undirected')
Clust <- as.dendrogram(cluster_edge_betweenness(Net))
png(filename="Dendro_%02d.png",
    width = 1000, height = 1000, units = "px")
for(i in 1:length(unique(Raw$Card))) {
  myPlot <- fviz_dend(Clust, k=i,horiz=T,
                      main=paste("Дендрограмма распределение карточек по группам \n",
                                 "Количество групп:", i))
  print(myPlot)
}
dev.off()
for(i in 1:19) {
  Adj2 <- Adj-max(Adj)*i/20
  png(filename=paste('Net graph ', i ,'.png'),
      width = 1000, height = 1000, units = "px")
  myPlot <- plot.igraph(
    graph_from_adjacency_matrix(ifelse(Adj2 < 0, 0, Adj2), mode='undirected'),
    vertex.label.color= "black", vertex.color= "gray", 
    vertex.size= 20, vertex.frame.color='gray',asp = 0.7,
    layout = layout.kamada.kawai,
    vertex.label.cex = 0.9,
    width=1, height=1)
  print(myPlot+title(paste('Сетевой граф распределения карточек по группам \n',
                           100-i*5, '% связей'),cex.main=2))
  dev.off()
}

for(i in 1:length(table(Raw$Card))) {
    png(filename=paste('Wordcloud ', i ,'.png'),
        width = 1000, height = 1000, units = "px")
    gnames <- as.data.frame(table(Raw$Group_name[Raw$Card == as.data.frame(table(Raw$Card))$Var1[i]]))
    myPlot <- ggplot(gnames, aes(label = Var1, size= Freq, color = Freq)) +
              geom_text_wordcloud() +
              scale_size_area(max_size = 20) +
              ggtitle(paste('Облако слов для карточки: ', as.data.frame(table(Raw$Card))$Var1[i])) +
              theme_minimal() + 
              theme(plot.title = element_text(hjust = 0.5, size=40, margin = unit(c(20,0,0,0), 'pt')))
  print(myPlot)
  dev.off()
}
#Конец 



