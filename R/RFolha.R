#' Scrap Noticia
#' 
#' Faz webscrap de uma notícia do site da folha a partir de seu url
#' 
#' @param url url da notícia do site da folha que você deseja obter os dados (caracter)
#' @export
scrap_noticia <- function(url){
  
  # noticia do site da folha
  
  noticia <- rvest::html(url, encoding = "UTF-8")
  
  # texto da notícia
  
  texto <- noticia %>% 
    rvest::html_nodes(".content") %>%
    rvest::html_text() %>% 
    subset(stringr::str_length(.) == max(stringr::str_length(.)))
  
  # data da notícia
  
  data <- noticia %>% 
    rvest::html_nodes("time") %>%
    rvest::html_text() %>% 
    subset(stringr::str_length(.) == max(stringr::str_length(.))) %>%
    as.Date(format = "%d/%m/%Y")
  
  # manchete da notícia
  
  manchete <- noticia %>% 
    rvest::html_nodes("h1") %>%
    rvest::html_text() %>% {
      .[3]
    }
  
  # caderno da notícia
  
  caderno <- noticia %>% 
    rvest::html_nodes("h1") %>%
    rvest::html_text() %>% {
      .[2]
    }
  
  b <- c(as.character(data), caderno, manchete, texto)
  names(b) <- c("data", "caderno", "manchete", "texto")
  return(b)  
}
#' Scrap Notícias
#' 
#' Faz webscrap de varias noticias do site da folha a partir de um vetor de urls
#' 
#' @param urls urls das notícias das quais você deseja obter os dados (vetor de caracteres)
#' @export
scrap_noticias <- function(urls){
  
  x <- plyr::adply(urls, .margins = 1, scrap_noticia)
  x[,1] <- urls
  names(x)[1] <- "url"
  return(x)
}
#' Busca Notícias
#' 
#' Busca as notícias do site da folha por datas específicas
#' 
#' @param datai data de início da busca
#' @param dataf data de fim da busca
#' 
#' @export
busca_noticias <- function(datai, dataf){
  
  sd <- as.Date(datai)
  ed  <- as.Date(dataf)
    
  busca <- paste0("http://search.folha.com.br/search?q=folha&site=online&sd=", lubridate::day(sd), "%2F", lubridate::month(sd), "%2F", lubridate::year(sd), "&ed=", lubridate::day(ed), "%2F", lubridate::month(ed), "%2F", lubridate::year(ed))
  
  n <- busca %>% html(encoding = "UTF-8") %>% 
    html_nodes(".search-title") %>% 
    html_text() %>% 
    str_replace_all(".*de ", "") %>%
    str_replace_all(")", "") %>%
    as.numeric()
  n.total <- n
  message(n)
  n <- n %/% 25
  paginas <- 0:(n)*25 + 1
  
  r <- NULL
  for(i in paginas){    
    b <- paste0(busca, "&sr=", i)
    b <-  b %>% html(encoding = "UTF-8") %>% 
      html_nodes("li, url") %>% 
      html_nodes("a") %>%
      html_attr("href") %>% 
      unique() %>%{
        .[19:43]
      }
    r <- c(r, b)
  }
  return(r[1:n.total])
}