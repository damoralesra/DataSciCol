#'
#' @param topic (string) temas de interés para búsqueda en Scopus
#' @param resQuery (list) lista de resultados obtenidos con la función 02-infoPapersScopus

infoAffScopus <- function(resQuery, topic) {
  res = resQuery %>%
    map2_df(
      .x = .,
      .y = topic,
      .f = function(x = .x, y = .y) {
        out = x %>%
          {
            .[["entries"]]
          } %>%
          gen_entries_to_df() %>%
          {
            .[["affiliation"]]
          } %>%
          clean_names() %>%
          select(-fa) %>%
          mutate(topicSearch = y) %>%
          rename(country = affiliation_country) %>% 
          relocate(entry_number, topicSearch, country, everything())
        
        return(out)
      }
      
    )
  
  return(res)
  
}
