library(data.table)
library(igraph)


DATADIR <- "../../data/"


casos <- fread(file.path(DATADIR, "casos-clean.csv"), encoding="UTF-8")
implicados <- fread(file.path(DATADIR, "implicados-clean.csv"), encoding="UTF-8")


SITUACIONES_SOSPECHA <- c("Imputado", "En libertad con fianza", "En libertad con cargos", "Detenido", "Condenado", "Encarcelado")
lista_implicados <- implicados[situacion_judicial_implicado %in% SITUACIONES_SOSPECHA, .(
  caso = caso, 
  nombre = nombre_implicado, 
  sitjudicial = situacion_judicial_implicado
)]



##########################
##   Grafo implicados   ##
##########################

## Dataset

links_impl <- merge(lista_implicados, lista_implicados, by="caso", suffixes=c("_1", "_2"), allow.cartesian=TRUE)
links_impl <- links_impl[nombre_1 != nombre_2, .N, by=.(nombre_1, nombre_2)]

links_impl[, pair := paste(pmin(nombre_1, nombre_2), pmax(nombre_1, nombre_2))]
links_impl <- links_impl[!duplicated(pair)]
links_impl[, pair := NULL]

write.csv(links_impl, file.path(DATADIR, "links_implicados.csv"), row.names=FALSE, quote=TRUE, fileEncoding="UTF-8")


## Grafo

g_impl <- graph_from_data_frame(links_impl[, -"N"], directed=FALSE, vertices=unique(lista_implicados$nombre))
E(g_impl)$weight <- links_impl$N
V(g_impl)$status <- lista_implicados$sitjudicial[match(V(g_impl)$name, lista_implicados$nombre)]

saveRDS(g_impl, file.path(DATADIR, "grafo_implicados.RData"))


## Layout grafo

layout_impl <- layout_nicely(g_impl)
plot(g_impl, layout=layout_impl, vertex.label=NA, vertex.size=2)

saveRDS(layout_impl, file.path(DATADIR, "layout_implicados.RData"))



#####################
##   Grafo casos   ##
#####################

## Dataset

links_casos <- merge(lista_implicados, lista_implicados, by="nombre", suffixes=c("_1", "_2"), allow.cartesian=TRUE)
links_casos <- links_casos[caso_1 != caso_2, .N, by=.(caso_1, caso_2)]

links_casos[, pair := paste(pmin(caso_1, caso_2), pmax(caso_1, caso_2))]
links_casos <- links_casos[!duplicated(pair)]
links_casos[, pair := NULL]

write.csv(links_casos, file.path(DATADIR, "links_casos.csv"), row.names=FALSE, quote=TRUE, fileEncoding="UTF-8")


## Grafo

g_casos <- graph_from_data_frame(links_casos[, -"N"], directed=FALSE, vertices=unique(lista_implicados$caso))
E(g_casos)$weight <- links_casos$N

saveRDS(g_casos, file.path(DATADIR, "grafo_casos.RData"))


## Layout grafo

layout_casos <- layout_nicely(g_casos)
plot(g_casos, layout=layout_casos, vertex.label=NA, vertex.size=2)

saveRDS(layout_casos, file.path(DATADIR, "layout_casos.RData"))

