#' A parametric g formula prediction function
#'
#' This function allows you simulate the follow-up of a complex
#' longitudinal dataset under different intervention regimes.
#' @param ii A numeric sequence (e.g., 1:5000) indicating monte
#' carlo resample size.
#' @param mc_data The original re-sampled baseline data.
#' @param randomization A numeric randomization. "NULL" indicates
#' natural course, "1" indicates set randomization to treatment,
#' "0" indicates set randomization to placebo. Defaults to "NULL".
#' @param exposure A numeric exposure. "NULL" indicates natural course,
#' "1" indicates set to exposed, "0" indicates set to unexposed.
#' @param length A numeric length of follow-up to be simulated.
#' @param censoring A numeric censoring. "NULL" indicates no
#' censoring, and "natural" indicates censoring as in empirical
#' data. Defaults to "NULL".
#' @keywords g-formula, causal inference
#' @export
#' @examples
#' pgf()
pgf<-function(ii,mc_data,length,randomization=NULL,exposure=NULL,censoring=NULL){
  # select first observation from baseline data
  d<-mc_data[ii,]
  # length of follow-up
  lngth<-length
  # first time-point is sampled empirical data
  # initiate variables to be simulated
  Vp<-Rp<-Xp<-Bp<-Np<-Zp<-Cp<-Sp<-Dp<-Yp<-mm<-numeric()
  # time & id
  mm[1]<-j<-1;id<-d$id;
  # baseline & randomization indicator
  Vp<-d[,names(d) %in% c("id","V1","V2","V3","V4","V5","V6","V7","V8")]
  if(is.null(randomization)){
    Rp<-d$R
  } else{
    Rp<-randomization
  }
  # time-varying exposure
  if(is.null(exposure)){
    Xp[1]<-d$X
  } else{
    Xp[1]<-exposure
  }
  # time-varying covariates
  Bp[1]<-d$B;Np[1]<-d$N;Zp[1]<-d$Z;
  # outcomes
  if(is.null(censoring)){
    Cp[1]<-0
  } else if(censoring=="natural"){
    Cp[1]<-d$C
  }
  if(Cp[1]==0){
    # by design, efuwp only possible after ~4 months
    Sp[1]<-0
    # fetal loss possible in month 1, therefore predict
    dDp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=0,B=Bp[j],Bl=0,N=Np[j],Nl=0,j)
    Dp[1]<-ifelse(Zp[1]==1,
                  pFunc(fitD,dDp),
                  0)
    # by design, live birth only possible after ~6 months
    Yp[1]<-0
  } else {
    Sp[1]<-Dp[1]<-Yp[1]<-0
  }
  # second month onwards
  for(j in 2:lngth){
    # if no terminal events, then simulate
    if(Cp[j-1]==0&Sp[j-1]==0&Dp[j-1]==0&Yp[j-1]==0) {
      # exposure
      if(is.null(exposure)){
        dXp<-data.frame(Vp,R=Rp,Xl=Xp[j-1],Bl=Bp[j-1],Nl=Np[j-1],j)
        Xp[j]<-pFunc(fitX,dXp)
      } else{
        Xp[j]<-exposure
      }
      # time-varying confounder 1
      dBp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],Bl=Bp[j-1],Nl=Np[j-1],j)
      Bp[j]<-pFunc(fitB,dBp)
      # time-varying confounder 2
      dNp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],Nl=Np[j-1],j)
      Np[j]<-pFunc(fitN,dNp)
      # time-varying confounder 3
      dZp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],N=Np[j],Nl=Np[j-1],j)
      Zp[j]<-ifelse(Zp[j-1]==0,
                    pFunc(fitZ,dZp),
                    1)
      # outcome 1
      dCp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],N=Np[j],Nl=Np[j-1],Z=Zp[j],j)
      if(is.null(censoring)){
        Cp[j]<-0
      } else if(censoring=="natural"){
        Cp[j]<-pFunc(fitC,dCp)
      }
      # outcome 2
      dSp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Xp[j-1],N=Np[j],Nl=Np[j-1],j)
      if(j<=11){
        Sp[j]<-ifelse(Zp[j]==0&Cp[j]==0&j>4,
                      pFunc(fitS,dSp),
                      0)
      } else{
        Sp[j]<-ifelse(Zp[j]==0&Cp[j]==0,1,0)
      }
      # outcome 3
      dDp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],N=Np[j],Nl=Np[j-1],j)
      Dp[j]<-ifelse(Cp[j]==0&Sp[j]==0&Zp[j]==1&j>1&j<9,
                    pFunc(fitD,dDp),
                    0)
      # outcome 4
      dYp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],N=Np[j],Nl=Np[j-1],j)
      Yp[j]<-ifelse(Cp[j]==0&Dp[j]==0&Sp[j]==0&Zp[j]==1&j>7,
                    pFunc(fitY,dYp),
                    0)
    } else {
      break
    }
    # time-point
    mm[j]<-j
  }
  gdat<-as.data.frame(cbind(id,mm,Vp,Rp,Bp,Np,Xp,Zp,Sp,Cp,Dp,Yp,row.names=NULL))
}
