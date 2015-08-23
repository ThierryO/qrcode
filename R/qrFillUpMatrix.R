#' Function to fill up the data bits
#'
#' \code{qrFillUpMatrix} fill up the predefined QRcode raster.
#' @param allBinary all data in binary in character format.
#' @param data raster data created by \code{\link{qrFillUpMatrix}}
#' @param version version of the QRcode.
#'
#' @return raster data filled up with data bits
#' @export


qrFillUpMatrix<-function(allBinary,data,version){
  counter<-1
  direction <- 1
  byteCount <- length(allBinary)/8
  size <- 21+ (version-1)*4

  pointer <- c(size,size)

  for(j in 1:byteCount){
    #data[pointer[1],pointer[2]] <- j#1
    data[pointer[1],pointer[2]] <- as.integer(allBinary[counter]);
    counter <- counter+1
    for(i in 1:8){
      if(direction==1){
        #UP
        if((pointer[2]%%2==1 & pointer[2]>7)|(pointer[2]%%2==0 & pointer[2]<7)){
          #odd number, right-hand-side
          if(data[pointer[1],pointer[2]-1]==0){
            pointer <- c(pointer[1],pointer[2]-1)
          }else if(data[pointer[1]-1,pointer[2]]==0){
            pointer <- c(pointer[1]-1,pointer[2])
          }else if(data[pointer[1],pointer[2]-1]==100){
            if(data[pointer[1]-1,pointer[2]]==95 | data[pointer[1]-1,pointer[2]]==55){
              pointer <- c(pointer[1]-2,pointer[2])
            }

          }else{
            print("Error-UP-ODD")
          }
        }else{
          #even number, left-hand-side
          if(pointer[1]!=1){
            if(data[pointer[1]-1,pointer[2]+1]==0){
              pointer <- c(pointer[1]-1,pointer[2]+1)
            }else if(data[pointer[1]-1,pointer[2]]==0){
              pointer <- c(pointer[1]-1,pointer[2])
            }else if(data[pointer[1]-1,pointer[2]+1]==20){
              if(data[pointer[1],pointer[2]-1]==55 | data[pointer[1],pointer[2]-1]==95){
                pointer <- c(pointer[1],pointer[2]-2)
              }else{
                pointer <- c(pointer[1],pointer[2]-1)
              }

              direction <- 2
            }else if(data[pointer[1]-1,pointer[2]+1]==100){
              if(data[pointer[1]-1,pointer[2]]==55){
                pointer <- c(pointer[1]-2,pointer[2])
              }else{
                pointer <- c(pointer[1]-6,pointer[2]+1)
              }

            }else if(data[pointer[1]-1,pointer[2]+1]==95 | data[pointer[1]-1,pointer[2]+1]==55){
              #timing code
              if(data[pointer[1]-2,pointer[2]+1]==40){
                pointer <- c(1,pointer[2]-2)
                direction <- 2
              }else{
                pointer <- c(pointer[1]-2,pointer[2]+1)
              }

            }else{
              print("Error-UP-EVEN")
            }
          }else{
            pointer <- c(pointer[1],pointer[2]-1)
            direction <- 2
          }
        }

      }else{
        #DOWN
        if((pointer[2]%%2==1 & pointer[2]>7)|(pointer[2]%%2==0 & pointer[2]<7)){
          #odd number, right-hand-side
          if(data[pointer[1],pointer[2]-1]==0){
            pointer <- c(pointer[1],pointer[2]-1)
          }else if(data[pointer[1]+1,pointer[2]]==0){
            pointer <- c(pointer[1]+1,pointer[2])
          }else if(data[pointer[1],pointer[2]-1]==100){
            if(data[pointer[1]+1,pointer[2]]==95 | data[pointer[1]+1,pointer[2]]==55 ){
              pointer <- c(pointer[1]+2,pointer[2])
            }else{
              pointer <- c(pointer[1]+1,pointer[2])
            }

          }else{
            print("Error-DOWN-ODD")
          }
        }else{
          #even number, left-hand-side
          if(pointer[1]!=size){
            if(data[pointer[1]+1,pointer[2]+1]==0){
              pointer <- c(pointer[1]+1,pointer[2]+1)
            }else if(data[pointer[1]+1,pointer[2]]==0){
              pointer <- c(pointer[1]+1,pointer[2])
            }else if(data[pointer[1]+1,pointer[2]+1]==100){
              if(data[pointer[1]+1,pointer[2]]==55){
                pointer <- c(pointer[1]+2,pointer[2])
              }else{
                pointer <- c(pointer[1]+6,pointer[2]+1)
              }

            }else if(data[pointer[1]+1,pointer[2]+1]==50 | data[pointer[1]+1,pointer[2]+1]==40){
              pointer <- c(pointer[1],pointer[2]-1)
              direction <- 1
            }else if(data[pointer[1]+1,pointer[2]+1]==95 | data[pointer[1]+1,pointer[2]+1]==55){
                pointer <- c(pointer[1]+2,pointer[2]+1)

            }else{
              print("Error-DOWN-EVEN")
            }
          }else{
            if(data[pointer[1],pointer[2]-1]==20){
              #left botom eye
              pointer <- c(pointer[1]-8,pointer[2]-1)
            }else{
              pointer <- c(pointer[1],pointer[2]-1)
            }

            direction <- 1
          }

        }
      }
      if(i<8){
        #data[pointer[1],pointer[2]] <- j#i+1
        data[pointer[1],pointer[2]] <- as.integer(allBinary[counter]);
        counter <- counter+1
      }
    }
  }
  return(data)
}
