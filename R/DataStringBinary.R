#' Function to convert input data string to binary polynomial
#'
#' @description  Convert input data string to binary polynomial
#'
#' @param dataString input data string.
#' @param qrInfo dataframe that store all the required info to generate qrcode.
#'
#'
#' @import R.utils
#' @export
#'
DataStringBinary<-function(dataString,qrInfo){

  if(qrInfo$mode == '0100'){
    # ##
    # #byte
     tempBin <- R.utils::intToBin(utf8ToInt(dataString))
     tempBin <- unlist(lapply(tempBin,function(x) stringr::str_pad(x,8,side='left',pad='0')))
     tempBin <- paste(tempBin,collapse = '')
  }else if(qrInfo$mode == '0010'){
    map <- c('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',' ','$','%','*','+','-','.','/',':')
    key <- seq(0,(length(map)-1),1)
    alpanumericTable <- data.frame(key,map)

    dataStringTemp <- unlist(strsplit(dataString,split = ''))
    dataStringValue <- sapply(dataStringTemp, function(x) alpanumericTable[map==x,1])
    index <- seq(1,nchar(dataString),2)

    if(nchar(dataString)%%2==0){
      tempBin <- sapply(index ,function(x) stringr::str_pad(R.utils::intToBin(dataStringValue[x]*45+dataStringValue[x+1]),11,side='left',pad='0'))
    }else{
      tempBin <- c(sapply(index[1:(length(index)-1)] ,function(x) stringr::str_pad(R.utils::intToBin(dataStringValue[x]*45+dataStringValue[x+1]),11,side='left',pad='0')),stringr::str_pad(R.utils::intToBin(dataStringValue[index[length(index)]]),6,side='left',pad='0'))
    }
    tempBin <- paste(tempBin,collapse = '')
  }

  CharCount <-0
  if(qrInfo$Version<=9){
    ifelse(qrInfo$mode=='0001', CharCount<-10, ifelse(qrInfo$mode=='0010',CharCount<-9,CharCount<-8))
  }else if(qrInfo$Version >=27){
    ifelse(qrInfo$mode=='0001', CharCount<-14, ifelse(qrInfo$mode=='0010',CharCount<-13,CharCount<-16))
  }else{
    ifelse(qrInfo$mode=='0001', CharCount<-12, ifelse(qrInfo$mode=='0010',CharCount<-11,CharCount<-16))
  }

  #mode + length
  tempBin <- paste0(qrInfo$mode,stringr::str_pad(R.utils::intToBin(nchar(dataString)),CharCount,side='left',pad='0'),tempBin,collapse = '')
  #pad 0
  if(qrInfo$Dcword*8-nchar(tempBin)>4){
    tempBin <- paste0(tempBin,paste(rep('0',4),collapse = ''))
  }else{
    tempBin <- paste0(tempBin,paste(rep('0',qrInfo$Dcword*8-nchar(tempBin)),collapse = ''))
  }
  padCount <- 8-nchar(tempBin)%%8
  tempBin <- paste0(tempBin,paste(rep('0',padCount),collapse = ''))

  padByte <- c('11101100','00010001')
  byteCount <- (qrInfo$Dcword*8-nchar(tempBin))/8
  if(byteCount>0){
    if(byteCount==1){
      bytearray <- suppressWarnings(cbind(1,padByte[1]))
      bytearray<-paste(bytearray[,2],collapse = '')
    }else{
      bytearray <- suppressWarnings(cbind(c(1:byteCount),padByte))
      bytearray<-paste(bytearray[,2],collapse = '')
    }
    tempBin <- paste0(tempBin,bytearray,collapse = '')
  }
  index <- seq(1,nchar(tempBin),8)
  dataPoly<-sapply(index,function(x) strtoi(substr(tempBin,x,x+7),base=2))
  return(dataPoly)
}
