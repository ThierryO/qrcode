#' Apply mask to the qrcode matrix
#'
#' @param data qrcode matrix
#' @param qrInfo Dataframe that store all the required info to
#' generate qrcode. Via \code{qrVersionInfo}
#' @param mask mask for qrcode to increase decodability.
#' Available value is 0-7.
#'
#' @details
#'    Qrcode stardard specify 8 masks as listed below.
#'  \itemize{
#'    \item M0, (row + column) %% 2 == 0
#'    \item M1, (row) %% 2 == 0
#'    \item M2, (column) %% 3 == 0
#'    \item M3, (row + column) %% 3 == 0
#'    \item M4, ( row%/%2 + column%/%3 ) %% 2 == 0
#'    \item M5, ((row * column) %% 2) + ((row * column) %% 3) == 0
#'    \item M6, ( ((row * column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
#'    \item M7, ( ((row + column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
#'  }
#'@export

qrMask <- function(data,qrInfo,mask){

  size <- 21+ (qrInfo$Version-1)*4
  #mask
  #
  #M0, (row + column) %% 2 == 0
  #M1, (row) %% 2 == 0
  #M2, (column) %% 3 == 0
  #M3, (row + column) %% 3 == 0
  #M4, ( row%/%2 + column%/%3 ) %% 2 == 0
  #M5, ((row * column) %% 2) + ((row * column) %% 3) == 0
  #M6, ( ((row * column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
  #M7, ( ((row + column) %% 2) + ((row * column) %% 3) ) %% 2 == 0
  #
  testString <- c('maskresult<-(row + column) %% 2 == 0','maskresult<-(row) %% 2 == 0','maskresult<-(column) %% 3 == 0','maskresult<-(row + column) %% 3 == 0','maskresult<-(row%/%2 + column%/%3) %% 2 == 0','maskresult<-((row * column) %% 2) + ((row * column) %% 3) == 0','maskresult<-( ((row * column) %% 2) + ((row * column) %% 3) ) %% 2 == 0','maskresult<-(((row + column) %% 2) + ((row * column) %% 3) ) %% 2 == 0')
  dataMask<-data
  maskresult<-TRUE
  for(i in 1:size){
    #row
    for(j in 1:size){
      #column
      if(dataMask[i,j]<3){
        row <- i-1
        column <- j-1
        eval(parse(text=testString[mask+1]))
        if(maskresult==TRUE){
          #toggle if ==0
          if(dataMask[i,j]==1){
            dataMask[i,j]<-0
          }else{
            dataMask[i,j]<-1
          }
        }
      }
    }
  }
  dataMask[dataMask==1] <- 99
  dataMask[dataMask==0] <- 49



  if(qrInfo$ECL=='L'){
      formatString <- paste0('01', stringr::str_pad(R.utils::intToBin(mask),3,side='left',pad='0') ,collapse = '')
  }else if(qrInfo$ECL=='M'){
    formatString <- paste0('00', stringr::str_pad(R.utils::intToBin(mask),3,side='left',pad='0') ,collapse = '')
  }else if(qrInfo$ECL=='H'){
    formatString <- paste0('10', stringr::str_pad(R.utils::intToBin(mask),3,side='left',pad='0') ,collapse = '')
  }else{
    formatString <- paste0('11', stringr::str_pad(R.utils::intToBin(mask),3,side='left',pad='0') ,collapse = '')
  }
  polyString <- '10100110111'

  formatBin<-formatPolyGen(formatString,polyString)
  dataMask[c(1:6,8:9,(size-6):size),9] <- formatBin[seq(15,1,-1)]
  dataMask[9,c(1:6,8,(size-7):size)] <- formatBin[seq(1,15,1)]

  #version
  if(qrInfo$Version>=7){
    versionString <- '000111'#qrInfo$Version
    polyString <- '1111100100101'
    versionBin<-versionPolyGen(versionString,polyString)
    dataMask[c((size-8):(size-10)),c(6:1)] <- versionBin[c(18:1)]
    dataMask[c(1:6),c((size-10):(size-8))] <- matrix(versionBin,nrow = 6,ncol = 3,byrow = TRUE)
  }

  dataMask[dataMask==1] <- 98
  dataMask[dataMask==0] <- 48
  dataMask[dataMask<60] <-0
  dataMask[dataMask>60] <-1
  return(dataMask)
}
