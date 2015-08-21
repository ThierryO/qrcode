#' Function to calculate version polynomial
#'
#' @param versionString Version in binary string
#' @param polyString polynomial in binary string, specified in the stardard
#'  to calculate version ECL.
#'
#' @return version polynomial.
#'

versionPolyGen<-function(versionString,polyString){

  versionString <- as.integer(unlist(strsplit(versionString,split = '')))
  oriVersionString <- versionString
  polyString <- as.integer(unlist(strsplit(polyString,split = '')))
  versionString <- c(versionString,rep(0,12))

  for(i in 1:6){
    if(versionString[1]==0){
      versionString <- versionString[2:length(versionString)]
    }else{
      polyTemp <- c(polyString,rep(0,length(versionString)- length(polyString)))
      versionString <- bitwXor(versionString,polyTemp)
      versionString <- versionString[2:length(versionString)]
    }
  }
  versionString <- c(oriVersionString,versionString)
  return(versionString)
}
