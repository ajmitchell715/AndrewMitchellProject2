#' Constructor Function
#'
#' @param plotType the type of plot to be made
#' @param cor whether or not to use the correlation matrix for PCA
#' @param mile whether or not to include the mile variable
#' @param length whether or not to include the length variable
#' @param weight whether or not to include the weight variable
#' @param ddt whether or not to include the DDT variable
#' @param FCM whether or not to include the FCM River
#' @param LCM whether or not to include the LCM River
#' @param SCM whether or not to include the SCM River
#' @param TRM whether or not to include the TRM River
#' @param catfish whether or not to include the CCATFISH species
#' @param buffalo whether or not to include the SMBUFFALO species
#' @param bass whether or not to include the LMBASS species
#'
#' @return
#' @export
#'
#' @examples
const=function(plotType="scatterplot",cor=FALSE,mile=TRUE,length=TRUE,weight=TRUE,ddt=TRUE,
                FCM=TRUE,LCM=TRUE,SCM=TRUE,TRM=TRUE,
                catfish=TRUE,buffalo=TRUE,bass=TRUE,swap=FALSE){



  # Create the DDT data set.
  x=Project2AndrewMitchell::ddt

  # Remove subsets of the data if need be.

  # Rivers not to be included

  if (FCM==FALSE){
    x=subset(x,RIVER!="FCM")
  }

  if (LCM==FALSE){
    x=subset(x,RIVER!="LCM")
  }

  if (SCM==FALSE){
    x=subset(x,RIVER!="SCM")
  }

  if (TRM==FALSE){
    x=subset(x,RIVER!="TRM")
  }

  # Species to not be used

  if (catfish==FALSE){
    x=subset(x,SPECIES!="CCATFISH")
  }

  if (buffalo==FALSE){
    x=subset(x,SPECIES!="SMBUFFALO")
  }

  if (bass==FALSE){
    x=subset(x,SPECIES!="LMBASS")
  }

  # Remove quantitative variables if need be.
  if (ddt==FALSE){
    x=x[,-4]
  }
  if (weight==FALSE){
    x=x[,-3]
  }
  if (length==FALSE){
    x=x[,-2]
  }
  if (mile==FALSE){
    x=x[,-1]
  }

  # Conduct PCA on the data.
  xPC=x
  # Remove the quantitative variables from the data.
  rem=c()
  for (i in 1:ncol(xPC)){
    c=ncol(xPC)+1-i
    if (class(xPC[1,c])=="factor"){
      rem[i]=c
    }
  }
  if (length(rem)>=1){
    for (i in 1:length(rem)){
      xPC=xPC[,-rem[i]]
    }
  }

  # Conduct PCA.
  pc=princomp(xPC,cor=cor)

  # Find the eigenvalues and eigenvectors for the chosen matrix.

  # Covariance matrix
  if (cor==FALSE){
    ei=eigen(cov(xPC))
  }
  # Correlation matrix
  else {
    ei=eigen(cor(xPC))
  }
  # Store the values and vectors.
  eiVal=ei$values
  eiVec=ei$vectors

  obj=list(dat=x,pc=pc,eiVal=eiVal,eiVec=eiVec)

  class(obj)="inf"

  obj

}

