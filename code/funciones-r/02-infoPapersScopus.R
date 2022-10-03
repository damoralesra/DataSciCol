#'
#' @param apiKey (string) API Key Scopus
#' @param typeQuery (string) tipo de consulta, ejemplos: "title" o "all"
#' @param topic (string) temas de interés para búsqueda en Scopus
#' @param docType (string) tipo de documento: Journal (j), Book (b),
#' Book Series (k), Conference Proceeding (k), Report (r) y Trade Publication (d). Por defecto
#' se agregó el nivel "All" para hacer referencia a todas.
#' @param afterYear (string) retorna documentos publicados después de este año.
#' @param numPapers (integer) número de papers para extracción de información.
#' @param countRetrieval (integer) número de documentos a recuperar. Por defecto = 10
#' @param showProgress (logical) Opción para mostrar u ocultar el proceso de extracción.

infoPapersScopus <-
  function(apiKey,
           typeQuery,
           topic,
           docType = "All",
           afterYear = 1998,
           numPapers = 100,
           countRetrieval = 10,
           showProgress = FALSE) {
    options("elsevier_api_key" = apiKey)
    config_api = get_api_key()
    
    if (docType == "All") {
      result_query =
        topic %>%
        map(
          .x = .,
          .f = ~ scopus_search(
            query = glue("{typeQuery}({.x}) AND PUBYEAR > {afterYear}"),
            max_count = numPapers,
            count = countRetrieval,
            verbose = showProgress
          )
        )
      
      df_papers =  result_query %>%
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
                .[["df"]]
              } %>%
              mutate(topicSearch = y) %>%
              relocate(entry_number, everything()) %>%
              clean_names() %>%
              select(
                -c(
                  fa,
                  prism_url,
                  dc_identifier,
                  eid,
                  prism_volume,
                  prism_cover_display_date,
                  subtype,
                  article_number,
                  source_id,
                  openaccess_flag,
                  prism_issue_identifier,
                  prism_e_issn
                )
              ) %>%
              rename(
                paper_title = dc_title,
                paper_author = dc_creator,
                name_journal = prism_publication_name,
                issn = prism_issn,
                pub_date = prism_cover_date,
                doi = prism_doi,
                aggregation_type = prism_aggregation_type,
              ) %>%
              relocate(entry_number, issn, doi, everything())
            
            return(out)
          }
        )
      
      
    } else{
      result_query =
        topic %>%
        map(
          .x = .,
          .f = ~ scopus_search(
            query = glue(
              "{typeQuery}({.x}) AND SRCTYPE({docType}) AND PUBYEAR > {afterYear}"
            ),
            max_count = numPapers,
            count = countRetrieval,
            verbose = showProgress
          )
        )
      
      
      
      df_papers = result_query %>%
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
                .[["df"]]
              } %>%
              mutate(topicSearch = y) %>%
              relocate(entry_number, everything()) %>%
              clean_names() %>%
              select(
                -c(
                  fa,
                  prism_url,
                  dc_identifier,
                  eid,
                  prism_volume,
                  prism_cover_display_date,
                  subtype,
                  article_number,
                  source_id,
                  openaccess_flag,
                  prism_issue_identifier,
                  prism_e_issn
                )
              ) %>%
              rename(
                paper_title = dc_title,
                paper_author = dc_creator,
                name_journal = prism_publication_name,
                issn = prism_issn,
                pub_date = prism_cover_date,
                doi = prism_doi,
                aggregation_type = prism_aggregation_type,
              ) %>%
              relocate(entry_number, issn, doi, everything())
            
            return(out)
          }
        )
    }
    
    return(list(dataPapers = df_papers, queryScopus = result_query))
    
  }
