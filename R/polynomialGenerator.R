#' Function to generate polynomial
#'
#' @param ECcount error correction code word count
#'
#' @return polynomail to generate Error correction code
#'

polynomialGenerator <- function(ECcount){
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


  polysize <- ECcount
  poly <- c(0,0)
  for(p in 2:polysize){
    newpoly <- c(0,p-1)
    #tempPoly
    #oldpoly size = p+1
    #11, (12)(21) , (22)(31), (32)(41), (42)(51) (52)....[polysize==5]
    for(i in 1:(p+1)){
      if(i==1){
        # (1 1)
        #poly[1] newpoly[1]
        #first element always zero
        tempPoly <- c(0)
      }else if(i==(p+1)){
        #(i 2)
        temp <- logTable[logTable$exponent==((poly[i-1]+newpoly[2])%%255),2]
        if(temp>255){
          temp <- temp%%255
        }
        tempPoly <- c(tempPoly,temp)
      }else{
        #(i 2)(i+1 1)
        temp <- bitwXor(logTable[logTable$exponent==poly[i],2],logTable[logTable$exponent==((poly[i-1]+newpoly[2])%%255),2])
        if(temp>255){
          temp <- temp%%255
        }
        tempPoly <- c(tempPoly,temp)
      }
    }
    poly <- c(0,unlist(lapply(tempPoly,function(x) logTable[logTable$log==x,1])))
    poly <- poly[poly!=255]
  }
  return(poly)
}
