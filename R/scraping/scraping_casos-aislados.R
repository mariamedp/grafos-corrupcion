library(httr)
library(rvest)


DATADIR <- "../../data/"

pagedir <- file.path(DATADIR, "webpages")
dir.create(pagedir, recursive=TRUE, showWarnings=FALSE)


MAPPING_CAMPOS_CASOS <- c(
  "partido" = "Partido/Org",
  "lugar" = "Lugar",
  "coste" = "Coste aprox.",
  "total_implicados" = "Total implicados",
  "fecha" = "Año de los hechos",
  "estado_judicial" = "Estado judicial"
)

MAPPING_CAMPOS_IMPLICADOS <- c(
  "cargo_implicado" = "Cargo",
  "sigue_cargo_implicado" = "¿Sigue en el cargo?",
  "situacion_judicial_implicado" = "Situación Judicial",
  "delito_implicado" = "Delito",
  "robo_implicado" = "Se llevó",
  "fecha_implicado" = "Fecha"
)


## Leer catálogo casos

casos_nombres <- read_html("https://www.casos-aislados.com/tramas.php") %>% html_nodes("div .bloc_title")

casos <- data.frame(
  caso = casos_nombres %>% html_text(),
  url = casos_nombres %>% html_node("a") %>% html_attr("href") %>% paste0("https://www.casos-aislados.com/", .),
  stringsAsFactors=FALSE
)

for (col in names(MAPPING_CAMPOS_CASOS)) {
  casos[[col]] <- as.character(NA)
}


## Leer páginas casos (detalles, implicados, etc)

implicados <- NULL
for (i in 1:nrow(casos))  {

  # Lee HTML página
  
  pagefname <- file.path(pagedir, paste0(gsub("/", "-", casos$caso[i]), ".html"))
  if (file.exists(pagefname)) {
    page_html <- read_html(pagefname)
  } else {
    cat(">>", casos$caso[i], "\n")
    Sys.sleep(3)
    page_resp <- GET(URLencode(casos$url[i]))
    page_html <- read_html(page_resp)
    write_html(page_html, pagefname)
  }

  
  # Detalles extra caso
  
  info_caso <- page_html %>% html_node("div .bloc_resumen") %>%
    html_node("ul") %>% html_children() %>%
    html_text() %>% trimws() %>%
    strsplit(": ?")

  info_caso <- setNames(sapply(info_caso, "[", 2), sapply(info_caso, "[", 1))

  for (col in names(MAPPING_CAMPOS_CASOS)) {
    casos[i, col] <- info_caso[MAPPING_CAMPOS_CASOS[col]]
  }
  
  known_fields_casos <- unname(MAPPING_CAMPOS_CASOS)
  if (any(! names(info_caso) %in% known_fields_casos)) {
    warning("Campos no guardados: ", paste(setdiff(names(info_caso), known_fields_casos), collapse=", "))
  }

  
  # Listado implicados y detalles
  
  implicados_nombres <- page_html %>% html_nodes("div .nombre_implicado") %>% 
    html_text() %>% trimws() %>% stringr:::str_to_title()
  if (length(implicados_nombres) == 0) { next }

  implicados_caso <- data.frame(
    caso = casos$caso[i],
    nombre_implicado = implicados_nombres,
    stringsAsFactors=FALSE
  )
  
  for (col in names(MAPPING_CAMPOS_IMPLICADOS)) {
    implicados_caso[[col]] <- as.character(NA)
  }
  

  lista_implicados <- page_html %>% html_nodes("div .detalles_implicado") %>% html_children()

  for (j in 1:nrow(implicados_caso)) {

    info_implicado <- lista_implicados[j] %>% html_children() %>% html_text() %>% trimws() %>% strsplit(": ?")
    info_implicado <- setNames(sapply(info_implicado, "[", 2), sapply(info_implicado, "[", 1))
    if (length(info_implicado) == 0) { next }

    for (col in names(MAPPING_CAMPOS_IMPLICADOS)) {
      implicados_caso[j, col] <- info_implicado[MAPPING_CAMPOS_IMPLICADOS[col]]
    }
    
    known_fields_implicados <- unname(MAPPING_CAMPOS_IMPLICADOS)
    if (any(! names(info_implicado) %in% known_fields_implicados)) {
      warning("Campos no guardados: ", paste(setdiff(names(info_implicado), known_fields_implicados), collapse=", "))
    }
    
  }

  implicados <- rbind(implicados, implicados_caso)
}


write.csv(casos, file.path(DATADIR, "casos.csv"), row.names=FALSE, quote=TRUE, na="", fileEncoding="utf-8")
write.csv(implicados, file.path(DATADIR, "implicados.csv"), row.names=FALSE, quote=TRUE, na="", fileEncoding="utf-8")
