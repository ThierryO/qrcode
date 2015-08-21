#'  Generate a QRcode in R
#'
#'  @description Main function to generate qrcode in R. This package can generate all variant of qrcode, version 1 to 40 and Error correct level of "L","M","Q" and " H". Not all reader in market can support all qrcode version, \code{qrcode_gen} has a software limit to version 10 which is tested working in most reader.
#'
#'
#'  @param dataString Input string for the QRCode
#'  @param ErrorCorrectionLevel Error Correction Level. The available options are "L","M","Q" and " H". Default value as "L"
#'  @param dataOutput Option to export data as matrix. Default value is FALSE.
#'  @param plotQRcode Option to plot QRcode. Default value is TRUE.
#'  @param wColor Color of the white module(white squre) in qrcode. Default value "white".
#'  @param bColor Color pf the black module(black squre) in qrcode. Default value "black".
#'  @param mask mask for qrcode to increase decodability. available value is 0-7.
#'  @param softLimitFlag flag to limit the qr code version to 10. Default value TRUE.
#'  @return A matrix that represent the QRcode. 1 as black module and 0 as white module.
#'
#'
#'  @examples
#'  qrcode_gen('www.r-project.org')
#'
#'  #User may change the color of the module
#'  qrcode_gen('www.r-project.org',bColor='Green3')
#'
#'
#'  @export

qrcode_gen <- function(dataString,ErrorCorrectionLevel='L',dataOutput = FALSE, plotQRcode=TRUE,wColor='White',bColor ='black',mask=1,softLimitFlag = TRUE){
  qrInfo <- qrVersionInfo(dataString,ECLevel = ErrorCorrectionLevel)
  if(qrInfo$Version >10 & softLimitFlag){
    warning('Input string size too big. Try lower Error Correction Level or shorter input string.')
  }else{

    #initialize a QRcode in a matrix
    data<- qrInitMatrix(qrInfo$Version)

    #convert data string into Binary in polynomial
    dataPoly <- DataStringBinary(dataString,qrInfo)

    #polynomial generator
    poly <- polynomialGenerator(qrInfo$ECwordPerBlock)

    #compile the final binary string.
    allBinary<-qrInterleave(poly,dataPoly,qrInfo)

    #fill up all binary into the qrcode template
    data <- qrFillUpMatrix(allBinary,data,qrInfo$Version)

    #apply mask
    dataMasked <- qrMask(data,qrInfo,mask)
    if(plotQRcode){
      heatmap(dataMasked[nrow(dataMasked):1,],Rowv = NA, Colv = NA,scale="none",col=c(wColor,bColor),labRow ='',labCol = '')
    }
    if(dataOutput){
      return(dataMasked)
    }
  }
}
