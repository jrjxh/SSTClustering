#' A search Function
#'
#' This function allows you to match the seedwords to specific article.
#' @param x Any string.
#' @keywords removeNumbers
#' @examples
#' article.id()


article.id <- function(seedwords,originalTextFile = originalTextFile,listname=NULL){
	for(i in 1:length(originalTextFile)){
		if(grepl(seedwords, originalTextFile[i])){
			listname <- c(listname,i)
			}
		}
	return(listname)
}