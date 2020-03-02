library(data.table)
library(stringr)


DATADIR <- "../../data/"


casos <- fread(file.path(DATADIR, "casos.csv"), encoding="utf-8")
implicados <- fread(file.path(DATADIR, "implicados.csv"), encoding="utf-8")



#######################
##   Dataset casos   ##
#######################

write.csv(casos, file.path(DATADIR, "casos-clean.csv"), row.names=FALSE, quote=TRUE, na="", fileEncoding="utf-8")



############################
##   Dataset implicados   ##
############################

implicados[, nombre_implicado := gsub(" \\.$", "", nombre_implicado)]

implicados[grep("\\(Fallecid.\\)", nombre_implicado), `:=`(
  fallecido = TRUE,
  nombre_implicado = gsub("\\(Fallecid.\\)", "", nombre_implicado)
)]

implicados[cargo_implicado == "Partido Político", tipo := "Partido"]


## Limpieza nombres

lista_implicados <- unique(implicados[, .(name_raw = nombre_implicado, tipo)])
lista_implicados[, name := stringr::str_to_title(trimws(name_raw))]

# Apodos
lista_implicados[, name := gsub("\"\"", "'", name)]
lista_implicados[, name := gsub("\"", "'", name)]
lista_implicados[, name := gsub("\\(", "'", gsub("\\)", "'", name))]
lista_implicados[, nickname := stringr::str_extract(name, "'.*'")]
lista_implicados[, name := gsub(" +", " ", gsub("'.*'", "", name))]

# Empresas
lista_implicados[grepl(" S\\.?a\\.?( |$)", name), `:=`(tipo = "Empresa", name = gsub(",? S\\.?a\\.?", " S.A.", name))]
lista_implicados[grepl(" S\\.?a\\.?u\\.?( |$)", name), `:=`(tipo = "Empresa", name = gsub(",? S\\.?a\\.?u\\.?", " S.A.U.", name))]
lista_implicados[grepl(" S\\.?l\\.?( |$)", name), `:=`(tipo = "Empresa", name = gsub(",? S\\.?l\\.?", " S.L.", name))]
lista_implicados[grepl(" S\\.?l\\.?u\\.?( |$)", name), `:=`(tipo = "Empresa", name = gsub(",? S\\.?l\\.?u\\.?", " S.L.U.", name))]
lista_implicados[startsWith(name, "Ute "), `:=`(tipo = "Empresa", name = gsub("^Ute", "UTE", name))]

lista_implicados[name  == "Caixabank", tipo := "Empresa"]
lista_implicados[name  == "Islas Airways", tipo := "Empresa"]
lista_implicados[name  == "Seguros Zurich", tipo := "Empresa"]
lista_implicados[name  == "Partido Popular", tipo := "Empresa"]
lista_implicados[name  == "Promotora Puntalarga", tipo := "Empresa"]
lista_implicados[name  == "Promotora Victoria", tipo := "Empresa"]
lista_implicados[name  == "Bbva", `:=`(tipo = "Empresa", name = "BBVA")]
lista_implicados[name  == "Ciu", `:=`(tipo = "Partido", name = "CIU")]
lista_implicados[name  == "Psc", `:=`(tipo = "Partido", name = "PSC")]
lista_implicados[name  == "Ayuntamiento De Salamanca", tipo := "AA.PP."]


## Reemplazar nombres originales por limpios

ind_name_implicados <- match(implicados$nombre_implicado, lista_implicados$name_raw)
implicados[, nombre_implicado := lista_implicados$name[ind_name_implicados]]
implicados[, tipo := lista_implicados$tipo[ind_name_implicados]]

write.csv(implicados, file.path(DATADIR, "implicados-clean.csv"), row.names=FALSE, quote=TRUE, na="", fileEncoding="utf-8")

