set.seed(1234)

grupo_1 <- cbind(rnorm(n = 200, mean = 0, sd = 8),
                 rnorm(n = 200, mean = 0, sd = 8))
grupo_2 <- cbind(rnorm(n = 300, mean = 30, sd = 8),
                 rnorm(n = 300, mean = 30, sd = 8))

datos <- rbind(grupo_1, grupo_2)
colnames(datos) <- c("x", "y")
head(datos)

plot(datos)

library(cluster)
library(factoextra)

clara_clusters <- clara(x = datos, k = 2, metric = "manhattan", stand = TRUE, samples = 50, pamLike = TRUE)
clara_clusters

fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

# Matriz de distancias euclídeas
mat_dist <- dist(x = datos, method = "euclidean")

# Dendrogramas con linkage complete y average
hc_euclidea_complete <- hclust(d = mat_dist, method = "complete")
hc_euclidea_average  <- hclust(d = mat_dist, method = "average")

cor(x = mat_dist, cophenetic(hc_euclidea_complete))
cor(x = mat_dist, cophenetic(hc_euclidea_average))

datos <- scale(datos)

hc_euclidea_completo <- hclust(d = dist(x = datos, method = "euclidean"), method = "complete")

fviz_dend(x = hc_euclidea_completo, k = 2, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=2")

fviz_dend(x = hc_euclidea_completo, k = 3, cex = 0.6) +
  geom_hline(yintercept = 3.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=3")

fviz_dend(x = hc_euclidea_completo, k = 4, cex = 0.6) +
  geom_hline(yintercept = 3.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=4")

fviz_cluster(object = list(data=datos, cluster=cutree(hc_euclidea_completo, k=2)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Hierarchical clustering + Proyección PCA",
       subtitle = "Distancia euclídea, Lincage complete, K=2") +
  theme_bw() +
  theme(legend.position = "bottom")

fviz_cluster(object = list(data=datos, cluster=cutree(hc_euclidea_completo, k=3)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Hierarchical clustering + Proyección PCA",
       subtitle = "Distancia euclídea, Lincage complete, K=3") +
  theme_bw() +
  theme(legend.position = "bottom")

fviz_cluster(object = list(data=datos, cluster=cutree(hc_euclidea_completo, k=4)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Hierarchical clustering + Proyección PCA",
       subtitle = "Distancia euclídea, Lincage complete, K=4") +
  theme_bw() +
  theme(legend.position = "bottom")

################################################################################################

library(tidyverse)
library(ggpubr)

set.seed(101)

# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2, dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)

# Se determina la media que va a tener cada grupo en cada una de las dos
# dimensiones. En total 2*4 medias. Este valor se utiliza para separar
# cada grupo de los demás.
media_grupos <- matrix(rnorm(n = 8, mean = 0, sd = 4), nrow = 4, ncol = 2, dimnames = list(NULL, c("media_x", "media_y")))
media_grupos <- as.data.frame(media_grupos)
media_grupos <- media_grupos %>% mutate(grupo = c("a","b","c","d"))

# Se genera un vector que asigne aleatoriamente cada observación a uno de
# los 4 grupos
datos <- datos %>% mutate(grupo = sample(x = c("a","b","c","d"), size = 100, replace = TRUE))

# Se incrementa el valor de cada observación con la media correspondiente al
# grupo asignado.
datos <- left_join(datos, media_grupos, by = "grupo")
datos <- datos %>% mutate(x = x + media_x, y = y + media_y)

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2.5) +
  theme_bw()
  
set.seed(101)
km_clusters <- kmeans(x = datos[, c("x", "y")], centers = 4, nstart = 50)
km_clusters

datos <- datos %>% mutate(cluster = km_clusters$cluster)
datos <- datos %>% mutate(cluster = as.factor(cluster), grupo   = as.factor(grupo))

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
  
table(km_clusters$cluster, datos[, "grupo"], dnn = list("cluster", "grupo real"))

# Resultados para K = 2
datos <- datos %>% select(x, y)

set.seed(101)

km_clusters_2 <- kmeans(x = datos, centers = 2, nstart = 50)
datos <- datos %>% mutate(cluster = km_clusters_2$cluster)
p1 <- ggplot(data = datos, aes(x = x, y = y, color = as.factor(cluster))) +
      geom_point(size = 3) +
      labs(title = "Kmenas con k=2") +
      theme_bw() +
      theme(legend.position = "none")


# Resultados para K = 6
datos <- datos %>% select(x, y)

set.seed(101)

km_clusters_6 <- kmeans(x = datos, centers = 6, nstart = 50)
datos <- datos %>% mutate(cluster = km_clusters_6$cluster)
p2 <- ggplot(data = datos, aes(x = x, y = y, color = as.factor(cluster))) +
      geom_point(size = 3) +
      labs(title = "Kmenas con k=6") +
      theme_bw() +
      theme(legend.position = "none")

ggarrange(p1, p2)

set.seed(101)
# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2, dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)

# Se determina la media que va a tener cada grupo en cada una de las dos
# dimensiones. En total 2*4 medias. Este valor se utiliza para separar
# cada grupo de los demás.
media_grupos <- matrix(rnorm(n = 8, mean = 0, sd = 4), nrow = 4, ncol = 2, dimnames = list(NULL, c("media_x", "media_y")))
media_grupos <- as.data.frame(media_grupos)
media_grupos <- media_grupos %>% mutate(grupo = c("a","b","c","d"))

# Se genera un vector que asigne aleatoriamente cada observación a uno de
# los 4 grupos
datos <- datos %>% mutate(grupo = sample(x = c("a","b","c","d"), size = 100, replace = TRUE))

# Se incrementa el valor de cada observación con la media correspondiente al
# grupo asignado.
datos <- left_join(datos, media_grupos, by = "grupo")
datos <- datos %>% mutate(x = x + media_x,
                          y = y + media_y)
datos <- datos %>% select(grupo, x, y)

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2.5) +
  theme_bw()

# Se calculan las distancias
matriz_distancias <- dist(x = datos[, c("x", "y")], method = "euclidean")

set.seed(567)
hc_euclidea_completo <- hclust(d = matriz_distancias, method = "complete")
hc_euclidea_single   <- hclust(d = matriz_distancias, method = "single")
hc_euclidea_average  <- hclust(d = matriz_distancias, method = "average")

par(mfrow = c(3,1))
plot(x = hc_euclidea_completo, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage complete")
plot(x = hc_euclidea_single, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage single")
plot(x = hc_euclidea_average, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage average")

############################################################################################3

data("USArrests")
head(USArrests)

str(USArrests)

datos <- scale(USArrests)

library(cluster)
library(factoextra)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15, diss = get_dist(datos, method = "euclidean"), nstart = 50)
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15, diss = dist(datos, method = "manhattan"))


calcular_totwithinss <- function(n_clusters, datos, iter.max=1000, nstart=50){
  # Esta función aplica el algoritmo kmeans y devuelve la suma total de
  # cuadrados internos.
  cluster_kmeans <- kmeans(centers = n_clusters, x = datos, iter.max = iter.max,
                           nstart = nstart)
  return(cluster_kmeans$tot.withinss)
}

# Mismo análisis pero sin recurrir a factoextra
# ==============================================================================
calcular_suma_dif_interna <- function(n_clusters, datos, distancia = "manhattan"){
  # Esta función aplica el algoritmo pam y devuelve la suma total de las
  # diferencias internas
  cluster_pam <- cluster::pam(x = datos, k = n_clusters, metric = distancia)
  # El objeto pam almacena la suma de las diferencias respecto a los medoides en
  # $objective["swap"]
  return(cluster_pam$objective["swap"])
}

# Se aplica esta función con para diferentes valores de k
total_withinss <- map_dbl(.x = 1:15,
                          .f = calcular_totwithinss,
                          datos = datos)
total_withinss

# Se aplica esta función con para diferentes valores de k
suma_dif_interna <- map_dbl(.x = 1:15,
                            .f = calcular_suma_dif_interna,
                            datos = datos)

data.frame(n_clusters = 1:15, suma_cuadrados_internos = total_withinss) %>%
  ggplot(aes(x = n_clusters, y = suma_cuadrados_internos)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:15) +
    labs(title = "Evolución de la suma total de cuadrados intra-cluster") +
    theme_bw()

data.frame(n_clusters = 1:15, suma_dif_interna = suma_dif_interna) %>%
  ggplot(aes(x = n_clusters, y = suma_dif_interna)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:15) +
    labs(title = "Evolución de la suma total de diferencias intra-cluster") +
    theme_bw()
    
set.seed(123)
km_clusters <- kmeans(x = datos, centers = 4, nstart = 50)

set.seed(123)
pam_clusters <- pam(x = datos, k = 4, metric = "manhattan")
pam_clusters

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

  
fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")
# Como en k-medoids no hay centroides, no se muestran en la representación ni
# tampoco las distancias desde este al resto de observaciones
  
# Como hay más de 2 variables, se están representando las 2 primeras componentes
# de un PCA. Se tienen que calcular el PCA y extraer las proyecciones almacenadas
# en el elemento x.
medoids <- prcomp(datos)$x

# Se seleccionan únicamente las proyecciones de las observaciones que son medoids
medoids <- medoids[rownames(pam_clusters$medoids), c("PC1", "PC2")]
medoids <- as.data.frame(medoids)

# Se emplean los mismos nombres que en el objeto ggplot
colnames(medoids) <- c("x", "y")

# Creación del gráfico
fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  # Se resaltan las observaciones que actúan como medoids
  geom_point(data = medoids, color = "firebrick", size = 2) +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")


set.seed(123)
km_clusters <- kmeans(x = datos, centers = 4, nstart = 50)
resultados <- data.frame(ciudad  = names(km_clusters$cluster),
                         cluster = as.factor(km_clusters$cluster)) %>%
              arrange(cluster)

library(igraph)
library(tidygraph)
library(ggraph)

datos_graph <- graph_from_data_frame(d = resultados, directed = TRUE)
datos_graph <- as_tbl_graph(datos_graph)

# Se añade información sobre a que cluster pertenece cada observacion
datos_graph <- datos_graph %>%
  activate(nodes) %>%
  left_join(resultados, by = c("name" = "ciudad"))

ggraph(graph = datos_graph) +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = cluster)) +
  geom_node_text(aes(label = name), repel = TRUE, alpha = 0.5, size = 3) +
  labs(title = "Resultados clustering K-means") +
  theme_graph()
