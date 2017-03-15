#' A parametric g formula prediction function
#'
#' This function allows you simulate the follow-up of a complex
#' longitudinal dataset under different intervention regimes.
#' @param ii A numeric sequence (e.g., 1:5000) indicating monte carlo resample size.
#' @param mc_data The original re-sampled baseline data.
#' @param randomization A numeric randomization. Null indicates natural course.
#' @param exposure A numeric exposure. Null indicates natural course.
#' @param length A numeric length of follow-up to be simulated.
#' @param censoring A numeric censoring . Null indicates no censoring.
#' @keywords g-formula
#' @export
#' @examples
#' pgf()
pgf<-function(ii,mc_data,length,randomization=NULL,exposure=NULL,censoring=NULL){
  # select first observation from baseline data
  Vp<-mc_data[ii,]
  # assign randomization indicator
  Rp<-Vp$R
  # length of follow-up
  lngth<-length
  # initiate variables to be simulated
  Xp<-Bp<-Np<-Zp<-Cp<-Sp<-Dp<-Yp<-mm<-numeric()
  Xp[1]<-Vp$X;Bp[1]<-Vp$B;Np[1]<-Vp$N;Zp[1]<-Vp$Z;
  Cp[1]<-Vp$C;Sp[1]<-Vp$S;Dp[1]<-Vp$D;Yp[1]<-Vp$Y;
  mm[1]<-j<-1;Vp<-Vp[,names(Vp) %in% c("id","V1","V2","V3","V4","V5","V6","V7","V8")]
  id<-Vp$id;Vp$id<-NULL
  # first time-point is sampled empirical data. for second time-point onwards, simulate
  for(j in 2:lngth){
    # if no terminal events, then simulate
    if(Cp[j-1]==0&Sp[j-1]==0&Dp[j-1]==0&Yp[j-1]==0) {
      # exposure
      dXp<-data.frame(Vp,R=Rp,Xl=Xp[j-1],Bl=Bp[j-1],Nl=Np[j-1],j)
      Xp[j]<-pFunc(fitX,dXp)
      # time-varying confounder 1
      dBp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],Bl=Bp[j-1],Nl=Np[j-1],j)
      Bp[j]<-pFunc(fitB,dBp)
      # time-varying confounder 2
      dNp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Xp[j-1],Nl=Xp[j-1],j)
      Np[j]<-pFunc(fitN,dNp)
      # time-varying confounder 3
      dZp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],N=Np[j],Nl=Np[j-1],j)
      Zp[j]<-ifelse(Zp[j-1]==0,
                    pFunc(fitZ,dZp),
                    1)
      # outcome 1
      dCp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],N=Np[j],Nl=Xp[j-1],Z=Zp[j],j)
      Cp[j]<-pFunc(fitC,dCp)
      # outcome 2
      dSp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Xp[j-1],N=Np[j],Nl=Xp[j-1],j)
      if(j<=9){
        Sp[j]<-ifelse(Zp[j]==0&Cp[j]==0&j>4,
                      pFunc(fitS,dSp),
                      0)
      } else{
        Sp[j]<-ifelse(Zp[j]==0&Cp[j]==0,1,0)
      }
      # outcome 3
      dDp<-data.frame(Vp,R=Rp,X=Xp[j],Xl=Xp[j-1],B=Bp[j],Bl=Bp[j-1],N=Np[j],Nl=Np[j-1],j)
      Dp[j]<-ifelse(Cp[j]==0&Sp[j]==0&Zp[j]==1&j>1&j<11,
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
  gdat<-as.data.frame(cbind(id,mm,Vp,Rp,Bp,Np,Xp,Zp,Sp,Cp,Dp,Yp))
}
