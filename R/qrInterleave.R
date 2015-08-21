#' Function to interleave the Data Code and Error Correction Core
#'
#' @param poly Error Correction code word polynomial
#' @param dataPoly Input data code word polynomial
#' @param qrInfo Dataframe that store all the required info to
#' generate qrcode. Via \code{qrVersionInfo}
#'
#' @return Interleaved polynomial readied to fill up the qrcode matrix
#'
#' @export

qrInterleave <- function(poly,dataPoly,qrInfo){
  groupCount <- c(qrInfo$Grp1,qrInfo$Grp2)
  groupDCCount <- c(qrInfo$DCinGrp1,qrInfo$DCinGrp2)

  counter <- 1
  #grp1Poly
  for(i in 1:groupCount[1]){
    targetDataPoly <- dataPoly[counter:(counter+qrInfo$DCinGrp1-1)]
    counter <- counter+qrInfo$DCinGrp1
    if(i==1){
      grp1ECPoly <- ECgenerator(poly,targetDataPoly,qrInfo$DCinGrp1,qrInfo$ECwordPerBlock)
      grp1DCPoly <- targetDataPoly
    } else{
      grp1ECPoly <- rbind(grp1ECPoly,ECgenerator(poly,targetDataPoly,qrInfo$DCinGrp1,qrInfo$ECwordPerBlock))
      grp1DCPoly <- rbind(grp1DCPoly,targetDataPoly)
    }
  }

  #grp2Poly
  if(groupCount[2]!=0){
    grp1ECPoly <- grp1ECPoly
    grp1DCPoly <- cbind(grp1DCPoly,-1)
    for(i in 1:groupCount[2]){
      targetDataPoly <- dataPoly[counter:(counter+qrInfo$DCinGrp2-1)]
      counter <- counter+qrInfo$DCinGrp2
      grp1ECPoly <- rbind(grp1ECPoly,ECgenerator(poly,targetDataPoly,qrInfo$DCinGrp2,qrInfo$ECwordPerBlock))
      grp1DCPoly <- rbind(grp1DCPoly,targetDataPoly)  }
  }

  #interleave
  targetECPoly<-grp1ECPoly[1:length(grp1ECPoly)]
  targetDCPoly <- grp1DCPoly[1:length(grp1DCPoly)]
  targetECPoly<- targetECPoly[targetECPoly>=0]
  targetDCPoly<- targetDCPoly[targetDCPoly>=0]

  ECBin <- paste0(R.utils::intToBin(targetECPoly),collapse = '')
  DCBin <- paste0(R.utils::intToBin(targetDCPoly),collapse = '')
  allBinary <- paste0(DCBin,ECBin,collapse = '')
  allBinary <- unlist(strsplit(allBinary,split = ''))



}
