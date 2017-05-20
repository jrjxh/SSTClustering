#' A preprocess Function
#'
#' This function allows you to remove special characters and number.
#' @param x Any string.
#' @keywords remove special characters
#' @examples
#' del.special.characters()


del.special.characters = function(x) {
    clean = gsub("http:[a-zA-Z\\/\\.0-9]+|[a-zA-Z]+|[ 0-9０１-２３４５６７８９]", 
        "", x)
    clean
}
