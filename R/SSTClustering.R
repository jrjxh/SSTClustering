#' The main Function
#'
#' This function allows you to cluster articles.
#' default: it use 1000 comments articles about bank services. 
#' @param localfreq Frequency of keywords in selected articles.
#' @param ratio Frequency of keywords in selected articles/All article background frequency.
#' @param originalTextFileName Text filename used for parsing (one article in one line),You can provide your own text, remember with absolute paths, and one line of article.
#' @keywords sstcjob
#' @export 
#' @examples
#' sstcjob()
#' sstcjob(localfreq=0.005,ratio=2,originalTextFileName=system.file("extdata", "good_comments.txt", package = "SSTClustering"))
#' ###Not run
#' sstcjob(localfreq=0.005,ratio=2,originalTextFileName="d:\\a.txt")

sstcjob <- function(localfreq=0.005,ratio=2,originalTextFileName=system.file("extdata", "good_comments.txt", package = "SSTClustering")){
cat("***********Welcome to the financial text mining system.************\n\n")
numOfClus <- readline("Please enter the number of sample categories you want to form\n\n");
numOfClusters <- as.integer(numOfClus)
termSeed4EachCluster <- list()
for(i in 1:numOfClusters)
{	
	clusNum = i;
	cat("\n");
	#cat(paste("Please enter a key seed word for the ",clusNum," class (if you have more than one word, separated by spaces):\n",sep="")); 
	keyWords <-  readline(paste("Please enter a key seed word for the ",clusNum,"class (at least 1 keywords shoud be provided,if you have more than one word, separated by spaces):\n",sep=""));
	termSeed4EachCluster[[i]] <- keyWords
}


originalTextFile <- readLines(originalTextFileName)

d.vec2 <- lapply(originalTextFile,del.special.characters)

mixseg <- jiebaR::worker()

d.vec3 <- lapply(d.vec2,jiebaR::segment,mixseg)

d.vec <- list()
for( i in 1:length(d.vec3)){
 d.vec[[i]] <- d.vec3[[i]][stringr::str_length(d.vec3[[i]]) >1]
}


d.vec1 <- lapply(d.vec,stringr::str_trim)

plain <- as.data.frame(table(unlist(d.vec1)))[-1,]
plain$Var1 <- as.character(plain$Var1)
globalListOfTerms <- NULL
globalListOfTerms <- hash::hash(plain$Var1,plain$Freq)



thisLoopKeyWords <- list()
for(i in 1:length(termSeed4EachCluster)){
	thisLoopKeyWords[[i]] = unlist(stringr::str_split(termSeed4EachCluster[i]," "))
  }



  
continueLoop = TRUE; 
loopNum = 1;
termList4EachCluster <- list()

while(continueLoop) {

cat("***********loop times",loopNum,"***********","\n");
  cc <- list()
  dd <- list()
  ee <- list()
  ff <- list()
  for(i in 1:length(thisLoopKeyWords)){
  clusNum = i;
  cat("***********Class NO:",clusNum,"***********","\n");
  aa <- list()
  for(j in 1:length(thisLoopKeyWords[[i]])){
  aa <- thisLoopKeyWords[[i]][j]
  cc[[j]] <- ifelse(article.id(aa,originalTextFile = originalTextFile),article.id(aa,originalTextFile = originalTextFile),"")
  dd[[clusNum]] <- cc
  cat("Initial keywords:",unlist(aa),"\nThe text number appears:",article.id(aa,originalTextFile = originalTextFile),"\n[Number of text responding to initial keywords:",ifelse(!is.null(article.id(aa,originalTextFile = originalTextFile)),length(article.id(aa,originalTextFile = originalTextFile)),0),"]","\n");  
  ee[[clusNum]] <- unique(unlist(dd[[clusNum]]))
  }
  ff[[loopNum]] <- ee
  cat("This round of these initial keywords create new text quantity (delete duplicates) :",length(unique(unlist(dd[[i]]))),"\n")
  }
  cat("***********As of this round, the cumulative number of matched text for each category***********","\n");
			for(t in 1:length(dd)){
				cat("Class",t,":",length(unique(unlist(dd[t]))),"\n");
			}
            stp = TRUE;
			for(x in 1:length(ff[[loopNum]]))
			{	sizeNow = length(ff[[loopNum]][[x]]);
				sizeLast = ifelse(loopNum == 1,0,length(ff[[loopNum-1]][[x]]));
				if(sizeNow == sizeLast)
				{	
					thisStop = TRUE;
					stp = stp && thisStop;
				}
				else
				{	
					thisStop = FALSE;
					stp = stp && thisStop;
				}
			}
	cat("***********Is this loop terminated (no new text is generated)?",stp,"******","\n")
	newthisLoopKeyWords = list()
	if(stp){
		continueLoop = FALSE
		}else{
			  for(x in 1:length(ff[[loopNum]])){
			  listOfDocumentsInTerms  <- d.vec1[ff[[loopNum]][[x]]]
			  localplain <- as.data.frame(table(unlist(listOfDocumentsInTerms)))[-1,]
			  localplain$Var1 <- as.character(localplain$Var1)
			  localListOfTerms <- NULL
			  localListOfTerms <- hash::hash(localplain$Var1,localplain$Freq)			  						
			  newClusTermsInThisLoop = vector()
			  for(v in ls(localListOfTerms)){
			  localTermCount <- localListOfTerms[[v]]
			  localTermFrequency = 1.0 * localTermCount / sum(hash::values(localListOfTerms));
			  if(localTermFrequency < localfreq){
			  }else{
			  	  for(w in ls(globalListOfTerms)){
				  if(v == w){
			      globalTermCount <- globalListOfTerms[[w]]
			      globalTermFrequency = 1.0 * globalTermCount / sum(hash::values(globalListOfTerms));
				  theRatio = localTermFrequency/globalTermFrequency;
					if(theRatio < ratio) { 
					
						}
						else{		
							cat("Class",x,"[",v,": localTermFrequency:",localTermFrequency,"Ratio:",theRatio,"] ","\n");
							newClusTermsInThisLoop = c(newClusTermsInThisLoop,v);
										}
										
							}else{
										
								}
						}		  
				    } 
			  }
		    cat("Class",x,":{The number of new build keywords is:",length(newClusTermsInThisLoop),"}\n");
			newthisLoopKeyWords[[x]] = newClusTermsInThisLoop;
			}  
		    }
            stp1 = TRUE;

			for(y in 1:length(newthisLoopKeyWords))
			{	
				if(length(newthisLoopKeyWords[[y]]) == 0)
				{	
					thisStop1 = TRUE;
					stp1 = stp1 && thisStop1;
				}
				else{	
					thisStop1 = FALSE;
					stp1 = stp1 && thisStop1;
				}
			}
	        cat("***********Is this loop terminated (no new text is generated)?",stp1,"***********","\n")
			
			thisLoopKeyWordsdup <- list()
			thisLoopKeyWordsnodup <- list()
				if(stp1){
					continueLoop = FALSE;
			        }else{ 
				        for(z in 1:length(thisLoopKeyWords)){
						ll = thisLoopKeyWords[[z]]
                        nn = newthisLoopKeyWords[[z]]
						thisLoopKeyWordsdup[[z]] <- unique(nn[!nn%in%ll])
						thisLoopKeyWordsnodup[[z]] <- unique(c(ll,nn))

					   }
				   

			    cat("***********New keywords (excluding initial keywords)***********\n");
								
				gg <- list()
								
				for(i in 1:length(thisLoopKeyWordsdup)){
				clusNum = i;
				if(length(thisLoopKeyWordsdup[[i]]) == 0){
				cat("***********Class NO.",clusNum,"***********","New keywords (excluding initial keywords):","NONE","\n");
				gg[[i]] <- c(unlist(thisLoopKeyWords[i]))
				}else{
				cat("***********Class NO.",clusNum,"***********","New keywords (excluding initial keywords):","\n");
				for(u in 1:length(thisLoopKeyWordsdup[[i]])){
				    cat(u,thisLoopKeyWordsdup[[i]][u]," ","\n")
					}
				
				
				cat("Please enter keyword number you want:\n");
				cat("***********A. Select All Please enter A***********\n");
				cat("***********B.Select Nothing Please enter B***********\n");
				cat("***********C.If you select multiple keywords, Please use spaces***********\n");
				
				termSelection <-  readline("Please enter: ");
				 if(termSelection == "A"){
				   gg[[i]] <- thisLoopKeyWordsnodup[[i]]
				   }else if(termSelection == "B"){
					         gg[[i]] <- thisLoopKeyWords[[i]]
							}else{
							gg[[i]] <- c(unlist(thisLoopKeyWords[i]),thisLoopKeyWordsdup[[i]][as.integer(stringr::str_split(termSelection," ")[[1]])])
							}
							
			    }
					termList4EachCluster[[loopNum]] <- gg
			}
			
			
			   stp2 = TRUE;
		
		       if(loopNum == 1){
					thisStop2 = FALSE;
					stp2 = stp2 && thisStop2;
				
				}else if(length(setdiff(termList4EachCluster[[loopNum]],termList4EachCluster[[loopNum-1]]))== 0){
					thisStop2 = TRUE;
					stp2 = stp2 && thisStop2;				
				}

	        cat("**********Is this loop terminated (no new text is generated)?",stp2,"**********","\n")
			

			cat("**********Up to this round, the cumulative keyword situation of each category**********\n");
			finalTextfile <- vector()
				for(i in 1:length(termList4EachCluster[[loopNum]])){
				clusNum = i;
				cat("**********Class NO.",clusNum,"The total number of initial keywords and new keywords is: ",length(termList4EachCluster[[loopNum]][[i]]),".\n Each keyword's corresponding article ID is:","\n");
				for(j in 1:length(termList4EachCluster[[loopNum]][[i]])){
                aa <- termList4EachCluster[[loopNum]][[i]][j]
				finalTextfile <- c(finalTextfile,unlist(article.id(aa,originalTextFile = originalTextFile)));
                cat(aa,":",unlist(article.id(aa,originalTextFile = originalTextFile)),"\n")  
               }
			    cat("**********Class NO.",clusNum,"'s number is",length(unique(finalTextfile)),", Total article number is",length(originalTextFile),".\nThe corresponding article ID is :",sort(unique(finalTextfile)),"\n");
				}
			    thisLoopKeyWords <- termList4EachCluster[[loopNum]]
				if(stp2){
				continueLoop = FALSE; 
				}else{	
				loopNum = loopNum + 1;
				}
	}	
}			
}