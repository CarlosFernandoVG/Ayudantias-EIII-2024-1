library(purrr)
library(stringr)


#' Enmascara una frase completa con las letras del abecedario de manera aleatoria
#' 
#' @param phrase Frase a ser enmascarada.
#' @param sep Separador que contiene la frase entre las palabras.
#' @param seed Semilla a utilizar en la generación de muestras aleatorias del vector letters' 
#' @returns String de la misma longitúd que la frase original.
#' @examples
#' masking_words("Somos lo que hacemos de forma repetida. Por tanto, la excelencia no es un acto, sino un hábito", seed = 34)
#' masking_words("Todos-deseamos-llegar-a-viejos;-y-todos-negamos-que-hemos-llegado", sep = "-", seed = 3)
masking_words <- function(phrase, sep = " ", seed = 1){
  set.seed(seed)
  stringr::str_split(phrase, pattern = " ")[[1]] %>% as.list() %>% 
    purrr::map_chr(~paste0(sample(letters, size = stringr::str_length(.x)), collapse = "")) %>% 
    paste0(collapse = " ")
}