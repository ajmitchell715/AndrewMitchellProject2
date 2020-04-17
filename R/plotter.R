#' Shiny Plotting Function
#'
#' @param plotType the type of plot to be made
#' @param mile whether or not to include the MILE variable
#' @param length whether or not to include the LENGTH variable
#' @param weight whether or not to include the WEIGHT variable
#' @param ddt whether or not to include the DDT variable
#' @param FCM whether or not to include the FCM River
#' @param LCM whether or not to include the LCM River
#' @param SCM whether or not to include the SCM River
#' @param TRM whether or not to include the TRM River
#' @param catfish whether or not to include the CCATFISH Species
#' @param buffalo whether or not to include the SMBUFFALO Species
#' @param bass whether or not to includ ethe LMBASS Species
#' @param pcaType whether to use the covariance or correlation matrix for PCA
#' @param col variable to use to differentiate points in scatterplot
#'
#' @return a plot of data
#' @export
#'
#' @examples plotter("scatterplot",length=FALSE,buffalo=FALSE)
plotter=function(plotType="screeplot",mile=TRUE,length=TRUE,weight=TRUE,ddt=TRUE,
                 FCM=TRUE,LCM=TRUE,SCM=TRUE,TRM=TRUE,
                 catfish=TRUE,buffalo=TRUE,bass=TRUE,
                 pcaType="covariance",col="RIVER"){

  # Exit if not enough data is being used to conduct analysis.

  # For inadequate PCA variables
  if (mile+length+weight+ddt<2){
    stop("At least quantiative variables must be input.")
  }

  # Simplify descriptions of plots if needed.
  if (plotType=="scatterplot (1st two variables)"){
    plotType="scatterplot"
  }
  if (plotType=="boxplot (1st variable)"){
    plotType="boxplot"
  }
  if (plotType=="connected dot plot (1st two variables)"){
    plotType="connected"
  }

  # Determine what will be needed from the constructor.

  if (pcaType=="covariance"){
    cor=FALSE
  }
  else {
    cor=TRUE
  }

  # Create the inf object.
  infObj=const(plotType,cor,mile,length,weight,ddt,FCM,LCM,SCM,TRM,catfish,
               buffalo,bass,swap)

  # Plotting function for inf objects
  plot.inf=function(obj,type="scatterplot",col="RIVER",
                    prop=FALSE,cumProp=FALSE,eig=FALSE,screeVar=FALSE){

    if (plotType=="scatterplot"){
      dat=obj$dat

      lab=c(length=2)
      if (colnames(dat)[1]=="MILE"){
        lab[1]="Mile"
      }
      if (colnames(dat)[2]=="MILE"){
        lab[2]="Mile"
      }
      if (colnames(dat)[1]=="LENGTH"){
        lab[1]="Length"
      }
      if (colnames(dat)[2]=="LENGTH"){
        lab[2]="Length"
      }
      if (colnames(dat)[1]=="WEIGHT"){
        lab[1]="Weight"
      }
      if (colnames(dat)[2]=="WEIGHT"){
        lab[2]="Weight"
      }
      if (colnames(dat)[1]=="DDT"){
        lab[1]="DDT Level"
      }
      if (colnames(dat)[2]=="DDT"){
        lab[2]="DDT Level"
      }

      library(ggplot2)

      # Create a ggplot object.
      cn=colnames(dat)
      if (col=="RIVER"){
        if (lab[1]=="Mile"){
          if (lab[2]=="Length"){
            g=ggplot(dat,aes(x=MILE,y=LENGTH,col=RIVER))
          }
        }
        if (lab[1]=="Mile"){
          if (lab[2]=="Weight"){
            g=ggplot(dat,aes(x=MILE,y=WEIGHT,col=RIVER))
          }
        }
        if (lab[1]=="Mile"){
          if (lab[2]=="DDT Level"){
            g=ggplot(dat,aes(x=MILE,y=DDT,col=RIVER))
          }
        }
        if (lab[1]=="Length"){
          if (lab[2]=="Mile"){
            g=ggplot(dat,aes(x=LENGTH,y=MILE,col=RIVER))
          }
        }
        if (lab[1]=="Length"){
          if (lab[2]=="Weight"){
            g=ggplot(dat,aes(x=LENGTH,y=WEIGHT,col=RIVER))
          }
        }
        if (lab[1]=="Length"){
          if (lab[2]=="DDT Level"){
            g=ggplot(dat,aes(x=LENGTH,y=DDT,col=RIVER))
          }
        }
        if (lab[1]=="Weight"){
          if (lab[2]=="Mile"){
            g=ggplot(dat,aes(x=WEIGHT,y=MILE,col=RIVER))
          }
        }
        if (lab[1]=="Weight"){
          if (lab[2]=="Length"){
            g=ggplot(dat,aes(x=WEIGHT,y=LENGTH,col=RIVER))
          }
        }
        if (lab[1]=="Weight"){
          if (lab[2]=="DDT Level"){
            g=ggplot(dat,aes(x=WEIGHT,y=DDT,col=RIVER))
          }
        }
        if (lab[1]=="DDT Level"){
          if (lab[2]=="Mile"){
            g=ggplot(dat,aes(x=DDT,y=MILE,col=RIVER))
          }
        }
        if (lab[1]=="DDT Level"){
          if (lab[2]=="Length"){
            g=ggplot(dat,aes(x=DDT,y=LENGTH,col=RIVER))
          }
        }
        if (lab[1]=="DDT Level"){
          if (lab[2]=="Weight"){
            g=ggplot(dat,aes(x=DDT,y=WEIGHT,col=RIVER))
          }
        }
      }

      if (col=="SPECIES"){
        if (lab[1]=="Mile"){
          if (lab[2]=="Length"){
            g=ggplot(dat,aes(x=MILE,y=LENGTH,col=SPECIES))
          }
        }
        if (lab[1]=="Mile"){
          if (lab[2]=="Weight"){
            g=ggplot(dat,aes(x=MILE,y=WEIGHT,col=SPECIES))
          }
        }
        if (lab[1]=="Mile"){
          if (lab[2]=="DDT Level"){
            g=ggplot(dat,aes(x=MILE,y=DDT,col=SPECIES))
          }
        }
        if (lab[1]=="Length"){
          if (lab[2]=="Mile"){
            g=ggplot(dat,aes(x=LENGTH,y=MILE,col=SPECIES))
          }
        }
        if (lab[1]=="Length"){
          if (lab[2]=="Weight"){
            g=ggplot(dat,aes(x=LENGTH,y=WEIGHT,col=SPECIES))
          }
        }
        if (lab[1]=="Length"){
          if (lab[2]=="DDT Level"){
            g=ggplot(dat,aes(x=LENGTH,y=DDT,col=SPECIES))
          }
        }
        if (lab[1]=="Weight"){
          if (lab[2]=="Mile"){
            g=ggplot(dat,aes(x=WEIGHT,y=MILE,col=SPECIES))
          }
        }
        if (lab[1]=="Weight"){
          if (lab[2]=="Length"){
            g=ggplot(dat,aes(x=WEIGHT,y=LENGTH,col=SPECIES))
          }
        }
        if (lab[1]=="Weight"){
          if (lab[2]=="DDT Level"){
            g=ggplot(dat,aes(x=WEIGHT,y=DDT,col=SPECIES))
          }
        }
        if (lab[1]=="DDT Level"){
          if (lab[2]=="Mile"){
            g=ggplot(dat,aes(x=DDT,y=MILE,col=SPECIES))
          }
        }
        if (lab[1]=="DDT Level"){
          if (lab[2]=="Length"){
            g=ggplot(dat,aes(x=DDT,y=LENGTH,col=SPECIES))
          }
        }
        if (lab[1]=="DDT Level"){
          if (lab[2]=="Weight"){
            g=ggplot(dat,aes(x=DDT,y=WEIGHT,col=SPECIES))
          }
        }
      }

      g=g+coord_equal()


      if (plotType=="scatterplot"){
        sg=g+geom_point()+ggtitle("Scatterplot of DDT Data")
      }

    }

    if (plotType=="screeplot"){
      pc=obj$pc
      len=length(pc$sdev)+1
      screeplot(pc,xlim=c(0,len),ylim=c(0,max(pc$sdev)^2+1000),
                main="Principal Components of DDT Data")
    }

    if (plotType=="biplot"){
      pc=obj$pc
      biplot(pc,main="Biplot of DDT Data")
    }

    if (plotType=="boxplot"){
      if (mile==TRUE){
        b=dat$MILE
        lab="Mile"
      }
      else if (length==TRUE){
        b=dat$LENGTH
        lab="Length"
      }
      else if (weight==TRUE){
        b=dat$WEIGHT
        lab="Weight"
      }
      else if (ddt==TRUE){
        b=dat$DDT
        lab="DDT Concentration"
      }

      boxplot(b,main=substitute(paste("Boxplot of ",lab)))

    }

    if (plotType=="scatterplot"){
      sg
    }

  }

  plot.inf(infObj,type=plotType,col=col,prop=prop,
           cumProp=cumProp,eig=eig,screeVar=screeVar)

}
