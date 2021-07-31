ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

paquetes <- c("tidyverse","cluster","factoextra","NbClust","tidyr","dplyr")
ipak(paquetes)


# -------------------
#Importar datos y filtrar
# pokemon <- read.csv("pokemon.csv")
pok<-pokemon%>%
    filter(Total>550, HP>100)%>%
    select(Name:Speed)%>%
    distinct(Name, .keep_all = TRUE)

#exportar
#write.csv2(pok, file = "~/Matemáticas/ESTAPL/pokemon/pok.csv",row.names = FALSE)

#Medidas de tendencia central, cuartiles y medidas de dispersión


stdev<-pok%>%summarise(sd(Total), sd(HP), sd(Attack), sd(Defense), sd(Special.Attack), sd(Special.Defense), sd(Speed))
names(stdev)<-names(pok[,3:9])
varianza<-stdev^2

summary(pok[,3:9])
stdev
varianza

#Estandarización porque se desconoce si las variables están en la misma escala
pok.scale<-scale(pok[,4:9])
pokCluster<-scale(pok[,4:9])
pok.scale<-as.data.frame(pok.scale)

#Medidas de tendencia central y cuartiles con los datos estandarizados
summary(pok.scale)

#Gráficas de caja y bigotes
boxplot(pok.scale$Total, pok.scale$HP, pok.scale$Attack, pok.scale$Defense, pok.scale$Special.Attack,
        pok.scale$Special.Defense, pok.scale$Speed, names = names(pok.scale))


# -----------------------------------
# Cluster
pokCluster<-scale(pok[,4:9])
#calcular la matriz de distacias
m.distancia <- get_dist(pokCluster, method =  "euclidean")
#el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
# Total de suma de cuadrados.
# (apunta a 3 clusters, con la "rodilla o punto de corte)
fviz_nbclust(pokCluster, kmeans, method = "wss")
# Metodo de la silueta
fviz_nbclust(pokCluster, kmeans, method = "silhouette")
fviz_nbclust(pokCluster, kmeans, method = "gap_stat")

#Con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
pokCluster<-as.data.frame(pokCluster)
resnumclust <- NbClust(pokCluster, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

# La mayoria de metodos apuntan a 3 clusters
rownames(pokCluster) <- pok$Name

#calculamos los tres clústers
k3 <- kmeans(pokCluster, centers = 3, nstart = 25)
k3
str(k3)

fviz_cluster(k3, data = pokCluster)
fviz_cluster(k3, data = pokCluster, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"

# Realizar un endograma
res3 <- hcut(pokCluster, k = 3, stand = TRUE)
fviz_dend(res3, rect = TRUE, cex = 0.5,k_colors = c(1:3))

# Informacion medias por cluster no estandarizados 
pok[,3:9] %>%
    mutate(Cluster = k3$cluster) %>%
    group_by(Cluster) %>%
    summarise_all("mean")

# Informacion medias por cluster estandarizados 
pokCluster %>%
    mutate(Cluster = k3$cluster) %>%
    group_by(Cluster) %>%
    summarise_all("mean")

#Pasamos las asignaciones de cluster a mi df inicial para trabajar con ellos
# Sin Datos estandarizados
pok$clus<-as.factor(k3$cluster)
pok

# Con Datos estandarizados
pokCluster$clus<-as.factor(k3$cluster)
pokCluster

#  Vizualizacion con datos alargados y usando los Closter creados
pokCluster$clus<-factor(pokCluster$clus)
data_long <- gather(pokCluster, caracteristica, valor, HP:Speed, factor_key=TRUE)
data_long

ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) + 
    stat_summary(fun = mean, geom="pointrange", size = 1)+
    stat_summary(geom="line") +
    geom_point(aes(shape=clus))