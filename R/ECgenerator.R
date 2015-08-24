#' Error correction code generator
#' Generate error correction code based on the input polynomial.
#'
#'  @param GenPoly generated polynomial to calculate error correction code word
#'  @param DataPoly input data polynomial
#'  @param DCWordCount data code word count
#'  @param ECWordCount error code word count
#'
#'  @return Error code word polynomial
#'  @export

ECgenerator<-function(GenPoly,DataPoly,DCWordCount,ECWordCount){

  #antilog table
  logTable <- c();
  for(i in 0:255){
    exponent <- i
    ifelse(i==0,temp<-2^0,temp<-temp*2)
    if(temp>255){
      temp<- bitwXor(temp,285)
    }
    if(i==0){
      logTable <- c(0,1)
    }else{
      logTable <- rbind(logTable,c(exponent,temp))
    }
  }
  logTable <- as.data.frame(logTable)
  names(logTable) <- c('exponent','log')

  targetDataPoly <- c(DataPoly,rep(0,ECWordCount))
  for(j in 1:DCWordCount){
    if(targetDataPoly[1]>0){
    dataPoly_temp<-unlist(lapply(targetDataPoly,function(x) logTable[logTable$log==x,1]))
    poly_temp <- GenPoly+dataPoly_temp[1]
    poly_temp <- poly_temp%%255
    poly_temp <- unlist(lapply(poly_temp,function(x) logTable[logTable$exponent==x,2]))
    targetDataPoly[1:length(poly_temp)] <- bitwXor(targetDataPoly[1:length(poly_temp)], poly_temp)
    }
    targetDataPoly<-targetDataPoly[2:length(targetDataPoly)]
  }
  return(targetDataPoly)
}
