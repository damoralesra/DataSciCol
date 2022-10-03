#' Esta función retorna una tabla de dos columnas: una con el nombre de los tópicos ingresados
#'  y la otra indica el total de registros en Scopus, asociados al tipo elegido, por ejemplo,
#'  títuloo (title) o en todos (all) los documentos.
#'
#' @param apiKey (string) API Key Scopus
#' @param typeQuery (string) tipo de consulta, ejemplos: "title" o "all"
#' @param topic (string) temas de interés para búsqueda en Scopus
#' @param docType (string) tipo de documento: Journal (j), Book (b),
#' Book Series (k), Conference Proceeding (k), Report (r) y Trade Publication (d). Por defecto
#' se agregó el nivel "All" para hacer referencia a todas.
#' @param year (string) retorna documentos publicados después de este año. 


totalRecord <- function(apiKey, typeQuery, topic, docType = "All", year) {
  options("elsevier_api_key" = apiKey)
  config_api = get_api_key()
  
  if (docType == "All") {
    num_record = topic %>%
      map_dbl(
        .x = .,
        .f = ~ scopus_search(
          query = glue("{typeQuery}({.x}) AND PUBYEAR > {year}"),
          max_count = 1,
          count = 1,
          verbose = FALSE
        ) %>%
          {
            .[["total_results"]]
          }
      )
  } else{
    num_record = topic %>%
      map_dbl(
        .x = .,
        .f = ~ scopus_search(
          query = glue("{typeQuery}({.x}) AND SRCTYPE({docType}) AND PUBYEAR > {year}"),
          max_count = 1,
          count = 1,
          verbose = FALSE
        ) %>%
          {
            .[["total_results"]]
          }
      )
  }
  
  table_record =
    tibble(topic = topic,
           total_record = num_record)
  
  
  
  return(table_record)
  
}