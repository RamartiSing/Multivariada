library(readxl)
library(dplyr)
library(factoextra)
library(cluster)
library(FactoMineR)
library(plotly)
library(gridExtra)
library(MASS, exclude = "select")
library(klaR)
library(clustMixType)
library(dbscan)
library(ggplot2)

# Cargue y depuración del conjunto de datos WDI
datos = read_xlsx("/Users/ccs/Documents/UNIVERSIDAD/Maestría en estadística/Semestre 1/Métodos multivariados/Datos/Trabajadores.xlsx") %>% 
  mutate(across(c(1,3,8,12), factor),
         Escolaridad = ordered(Escolaridad,
                               levels = c("Técnico","Pregrado","Posgrado")),
         Estrato = ordered(Estrato),
         Situación = ordered(Situación,
                             levels = c("Difícil","Regular","Buena")))

# Matriz de datos con variables numéricas estandarizadas
Z = datos %>%
  mutate(across(where(is.numeric), function(x) (x-mean(x))/sd(x)))

# Agrupamiento jerárquico
# Algoritmo aglomerativo

# Paso 1
# Cada trabajador corresponde a un grupo diferente
# Base con los 5 primeros trabajadores
Z5 = Z[1:5,] %>% 
  select(where(is.numeric)) %>%
  as.data.frame()
Z5

# Paso 2
# Matriz de distancias entre grupos
D =  get_dist(Z5, method = "euclidean") ; D

# Paso 3
# Formamos el grupo (2,4)
# Reemplazamos los trabajadores 2 y 4 por 
# su vector de promedios
z = (Z5["2",] + Z5["4",])/2
Z5 = rbind(Z5[!is.element(rownames(Z5),c("2","4")),], z)
rownames(Z5) = c(1,3,5,"2,4")
Z5

# Paso 4
# Matriz de distancias reducida
D = get_dist(Z5, method = "euclidean") ; D

# Paso 3
# Reemplazamos los grupos 5 y (2,4) por 
# su vector de promedios
z = (Z5["5",] + 2*Z5["2,4",])/3
Z5 = rbind(Z5[!is.element(rownames(Z5),c("5","2,4")),], z)
rownames(Z5) = c(1,3,"2,4,5")
Z5

# Paso 4
# Matriz de distancias reducida
D = get_dist(Z5, method = "euclidean") ; D

# Paso 3
# Reemplazamos los trabajadores 3 y (2,4,5) por 
# su vector de promedios
z = (Z5["3",] + 3*Z5["2,4,5",])/4
Z5 = rbind(Z5[!is.element(rownames(Z5),c("3","2,4,5")),], z)
rownames(Z5) = c(1,"2,4,5,3")
Z5

# Paso 4
# Matriz de distancias reducida
D = get_dist(Z5, method = "euclidean") ; D

# Variables numéricas
# Agrupamiento (método de Ward)
ag.jer = Z %>%
  select(where(is.numeric)) %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")

# Dendograma
plot(ag.jer, hang = -1, main = "", sub = "", 
     xlab = "Trabajadores", ylab = "Distancia")

# Asignación de los trabajadores a 2 grupos
ag.jer = Z %>%
  select(where(is.numeric)) %>%
  get_dist(method = "euclidean") %>%
  hcut(k = 2, isdiss = TRUE, hc_func = "hclust", hc_method = "ward.D2") 

# Asignación de los trabajadores a los grupos
ag.jer$cluster

# Dendograma con la partición en 2 grupos
fviz_dend(ag.jer, k=2, cex = 0.3, main = "",
          xlab = "Trabajadores", ylab = "Distancia")

# Representación de los grupos en un ACP
Z %>%
  select(where(is.numeric)) %>%
  fviz_cluster(ag.jer, data = .)

# Otra forma
acp = Z %>%
  select(where(is.numeric)) %>%
  mutate(Grupo = ag.jer$cluster) %>%
  PCA(ncp = 3, quali.sup = 7, graph = FALSE)

plot_ly(data.frame(acp$ind$coord), 
        x=~Dim.1, y=~Dim.2, z=~Dim.3,
        type="scatter3d", mode="markers",
        color=as.factor(ag.jer$cluster))

# Variables binarias
# Agrupamiento (método de Ward)
ag.jer = Z %>%
  select(Sexo, Ayuda) %>%
  mutate(Sexo = ifelse(Sexo=="F", 1, 0),
         Ayuda = ifelse(Ayuda=="Sí", 1, 0)) %>%
  get_dist(method = "binary") %>%
  hcut(k = 2, isdiss = TRUE, hc_func = "hclust", hc_method = "ward.D2")

# Dendograma (2 grupos)
fviz_dend(ag.jer, k=2, cex = 0.3, main = "",
          xlab = "Trabajadores", ylab = "Distancia")

# Representación mediante ACM
Z %>%
  select(Sexo, Ayuda) %>%
  mutate(Grupo = ag.jer$cluster) %>%
  MCA(quali.sup = 3, graph = FALSE) %>%
  fviz_mca_ind(habillage = 3, addEllipses = TRUE, repel = TRUE)

# Variables categóricas
# Agrupamiento (método de Ward)
ag.jer = Z %>%
  select(where(is.factor)) %>%
  daisy(metric = "gower") %>%
  hcut(k = 2, isdiss = TRUE, hc_func = "hclust", hc_method = "ward.D2")

# Dendograma (2 grupos)
fviz_dend(ag.jer, k=2, cex = 0.3, main = "",
          xlab = "Trabajadores", ylab = "Distancia")

# Representación mediante ACM
acm = Z %>%
  select(where(is.factor)) %>%
  mutate(Grupo = ag.jer$cluster) %>%
  MCA(ncp = 3, quali.sup = 8, graph = FALSE)

fviz_mca_ind(acm, habillage = 8, addEllipses = TRUE)

plot_ly(data.frame(acm$ind$coord), 
        x=~Dim.1, y=~Dim.2, z=~Dim.3,
        type="scatter3d", mode="markers",
        color=as.factor(ag.jer$cluster))

# Variables numéricas y categóricas
# Agrupamiento (método de Ward)
ag.jer = Z %>%
  daisy(metric = "gower") %>%
  hcut(k = 2, isdiss = TRUE, hc_func = "hclust", hc_method = "ward.D2") 

# Dendograma (2 grupos)
fviz_dend(ag.jer, k=2, cex = 0.3, main = "",
          xlab = "Trabajadores", ylab = "Distancia")

# Representación mediante AFDM
afdm = Z %>%
  mutate(Grupo = ag.jer$cluster) %>%
  FAMD(ncp = 3, sup.var = ncol(Z)+1, graph = FALSE)

fviz_famd_ind(afdm, habillage = ncol(Z)+1, addEllipses = TRUE, 
              col.quali.var = "transparent")

plot_ly(data.frame(afdm$ind$coord), 
        x=~Dim.1, y=~Dim.2, z=~Dim.3,
        type="scatter3d", mode="markers",
        color=as.factor(ag.jer$cluster))

# Agrupamiento no jerárquico
# Métodos basados en centroides
# Función para aplicar el método de Lloyd
Lloyd = function(X, k, max.it, sem){
  n = nrow(X)
  asig.cent = numeric(n)
  set.seed(sem)
  for(it in 0:max.it){
    if(it == 0){
      # centroide inicial
      cent = X[s<-sample(1:n, size = k),]
      centroides = cent
      asig.cent[s] = 1:k
      # asignación aleatoria al centroide
      asig.cent[-s] = sample(1:k, n-2, replace = TRUE)
      asignaciones = as.factor(asig.cent)
    } else {
      # actualización del centroide
      cent = aggregate(X, list(asig.cent), FUN=mean)[,-1]
      rownames(cent) = paste(it, 1:k, sep = "-")
      centroides = rbind(centroides, cent)
      # distancia de cada individuo al centroide
      d.cent = as.matrix(dist(rbind(cent,X)))[-(1:k),1:k] 
      # asignación al centroide más cercano
      asig.cent = apply(d.cent, 1, which.min) 
      asignaciones = data.frame(asignaciones, as.factor(asig.cent))
    }
  }
  colnames(asignaciones) = paste("it", 0:max.it, sep = "")
  list("centroides"=centroides, "asignaciones"=asignaciones)
}

# Elección de dos variables cuantitativas estandarizadas
Z2 = Z %>%
  select(Ingresos, Conocimiento) %>%
  as.data.frame()

# Aplicación del método de Lloyd
agr = Lloyd(Z2, k=2, max.it=5, sem=123)

# Centroides
agr$centroides

# Asignaciones
head(agr$asignaciones, 10) # primeras 10 filas

# Representación gráfica
Z2 = cbind(Z2,agr$asignaciones)

for(i in 1:ncol(agr$asignaciones)){
  assign(paste0("graf",i),
         ggplot(data=NULL) +
           geom_point(data=Z2, aes(x = Ingresos, y = Conocimiento),
                      colour = Z2[,(2+i)], alpha = .7) +
           geom_point(data=agr$centroides[(1:2)+2*(i-1),],
                      aes(x = Ingresos, y = Conocimiento), 
                      pch = 17, cex = 3, colour = c("black","red")))
}
grid.arrange(graf1, graf2, graf3, graf4, graf5, graf6,
             ncol=2, nrow = 3)

# Agrupamiento por k-medias con k = 2
ag.km = Z %>%
  select(where(is.numeric)) %>%
  as.data.frame() %>%
  kmeans(centers = 2, iter.max = 15, algorithm = "Hartigan-Wong")

# Asignación de los individuos a los grupos
ag.km$cluster

# Representación de los grupos mediante un ACP
Z %>%
  select(where(is.numeric)) %>%
  fviz_cluster(ag.km, data = .)

# Agrupamiento por k-modas con k = 2
ag.kmo = Z %>%
  select(where(is.factor)) %>%
  as.data.frame() %>%
  kmodes(modes = 2, iter.max = 15)

# Asignación de los individuos a los grupos
ag.kmo$cluster

# Representación de los grupos mediante un ACM
acm = Z %>%
  select(where(is.factor)) %>%
  mutate(Grupo = ag.kmo$cluster) %>%
  MCA(ncp = 3, quali.sup = 8, graph = FALSE) 

fviz_mca_ind(acm, habillage = 8, addEllipses = TRUE)

plot_ly(data.frame(acm$ind$coord), 
        x=~Dim.1, y=~Dim.2, z=~Dim.3,
        type="scatter3d", mode="markers",
        color=as.factor(ag.kmo$cluster))

# Agrupamiento por k-prototipos con k = 2
ag.kp = Z %>%
  as.data.frame() %>%
  kproto(k = 2, iter.max = 15, verbose = FALSE)

# Asignación de los individuos a los grupos
as.vector(ag.kp$cluster)

# Representación de los grupos mediante un AFDM
afdm = Z %>%
  mutate(Grupo = ag.kp$cluster) %>%
  FAMD(sup.var = ncol(Z)+1, graph = FALSE)

fviz_famd_ind(afdm, habillage = ncol(Z)+1, addEllipses = TRUE,
              col.quali.var = "transparent")

plot_ly(data.frame(afdm$ind$coord),
        x=~Dim.1, y=~Dim.2, z=~Dim.3,
        type="scatter3d", mode="markers",
        color=as.factor(ag.kp$cluster))

# Métodos basados en densidad
# Algoritmo DBSCAN

# Datos
Z2 = Z %>%
  select(Ingresos, Conocimiento) %>%
  as.data.frame()

# Definición de parámetros
MinPts = 3
eps = 0.5

# e-vécindad de cada punto
n = nrow(Z2)
D = as.matrix(dist(Z2))
diag(D) = Inf
e.vec = apply(D, 1, function(x) (1:n)[x <= eps])
e.vec[1:5] # primeros 5 puntos y sus e-vecinos

# Número de e-vecinos
Ne = sapply(e.vec, length, simplify = TRUE) 
names(Ne) = NULL ; Ne

# Identificación de puntos nucleo, borde y ruido
p.nucleo = (1:n)[Ne>=(MinPts-1)] ; p.nucleo
p.borde = setdiff(unlist(e.vec[p.nucleo]), p.nucleo) ; p.borde
p.ruido = setdiff(1:n, union(p.nucleo, p.borde)) ; p.ruido

# Gráfico de los tipos de puntos
Tipo = factor(levels = c("Nucleo","Borde","Ruido"))
Tipo[p.nucleo] = "Nucleo"
Tipo[p.borde] = "Borde"
Tipo[p.ruido] = "Ruido"

ggplot(Z2, aes(x = Ingresos, y = Conocimiento, col = Tipo)) +
  geom_point() +
  scale_color_brewer(palette = "Set1", direction = -1)

# Agrupamiento por DBSCAN
# Paso 3 del algoritmo
nuc.sel = numeric(0)
Grupo = numeric(n)
k = 0
while(length(nuc.sel) < sum(Ne>=(MinPts-1))){
  # selección aleatoria de un nucleo antes no seleccionado
  sel = sample(setdiff(p.nucleo, nuc.sel), 1)
  # puntos directamente alcanzables
  vec = union(intersect(unlist(e.vec[sel]), p.nucleo), sel)
  nuc.sel = union(nuc.sel, vec)
  # puntos alcanzables
  while(length(setdiff(intersect(unlist(e.vec[vec]), p.nucleo), vec))!=0){
    vec = intersect(unlist(e.vec[vec]), p.nucleo) ; vec
    nuc.sel = union(nuc.sel, vec) ; nuc.sel
  }
  # asignación de puntos nucleo a los grupos
  k = k+1
  Grupo[vec] = k
}
# asignación puntos borde a los grupos
D[, p.ruido] = Inf
D[, p.borde] = Inf
Grupo[p.borde] = Grupo[apply(D[p.borde, ], 1, which.min)]
niveles = sort(unique(Grupo))
Grupo = factor(Grupo, levels = c(niveles[-1],0),
               labels = c(1:(length(niveles)-1),"Ruido")) ; Grupo

# Gráfico de representación de los grupos
ggplot(Z2, aes(x=Ingresos, y=Conocimiento, col=Grupo)) +
  geom_point() +
  scale_color_brewer(palette="Set1", direction = -1)

# Superposición de los grupos sobre la estimación
# de las regiones de más alta densidad
ggplot(Z2, aes(x=Ingresos, y=Conocimiento)) +
  geom_hdr(probs = seq(0.025,0.975,0.025)) + 
  geom_point(aes(col = Grupo)) +
  geom_point(pch = 1, col = gray(.1)) +
  scale_color_brewer(palette="Set1", direction=-1) +
  theme(legend.position = "none")

D = as.matrix(dist(Z2)) ; diag(D) = Inf

# MinPts-1 puntos más cercanos a cada punto
dMinPts = apply(D, 1,
                function(x) x[rank(x, ties.method = "min") <= (MinPts)])
plot(sort(unlist(dMinPts), decreasing = FALSE),
     type = "l", xlab = "", ylab = "Distancias")
abline(h = 0.5, lty = 2)

# Matriz de distancias
D = Z %>% 
  select(where(is.numeric)) %>%
  get_dist(method =  "euclidean", stand = TRUE)

# Elección de parámetros
p = 6
MinPts = p+1
eps = 1.5
kNNdistplot(D, k = MinPts)
abline(h = eps, lty = 2)

# Agrupamiendo con DBSCAN
ag.dbs = dbscan(D, eps=eps, minPts=MinPts)

ag.dbs$cluster

# Representación de los grupos mediante un ACP
acp = Z %>%
  select(where(is.numeric)) %>%
  mutate(Grupo = ag.dbs$cluster) %>%
  PCA(ncp = 3, quali.sup = 7, graph = FALSE)

fviz_pca_ind(acp, habillage = 7, addEllipses = TRUE, 
             col.quali.var = "transparent")

plot_ly(data.frame(acp$ind$coord),
        x=~Dim.1, y=~Dim.2, z=~Dim.3,
        type="scatter3d", mode="markers",
        color=as.factor(ag.dbs$cluster))

# Matriz de distancias de Gower
D = Z %>% 
  daisy(metric =  "gower")

# Elección de parámetros
p = ncol(Z)
MinPts = p+1
eps = 0.2
kNNdistplot(D, k = MinPts)
abline(h = eps, lty = 2)

# Agrupamiendo con DBSCAN
ag.dbs = dbscan(D, eps=eps, minPts=MinPts)

ag.dbs$cluster

# Representación de los grupos mediante un AFDM
afdm = Z %>%
  mutate(Grupo = ag.dbs$cluster) %>%
  FAMD(ncp = 3, sup.var = ncol(Z)+1, graph = FALSE)

fviz_famd_ind(afdm, habillage = ncol(Z)+1, addEllipses = TRUE, 
              col.quali.var = "transparent")

plot_ly(data.frame(afdm$ind$coord),
        x=~Dim.1, y=~Dim.2, z=~Dim.3,
        type="scatter3d", mode="markers",
        color=as.factor(ag.dbs$cluster))
