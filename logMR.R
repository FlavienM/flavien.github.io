logMR <- function(I,delta,sigma_e,alpha,mu_H,sigma_H=1,Hbar=0,T=100, N=100000){
  
  # Generates log mortality rates given parameters  
  #  syntax: X =  I,delta,sigma_e,alpha,mu_H,sigma_H,Hbar
  # N: number of individuals for simulation, 
  
  # Initialization
  H <-matrix(0,N,T);
  H[,1] <- mu_H+sigma_H*rnorm(N,1);
  
  deathyear <- T*matrix(1,N,1); #will record death year
  alive <- matrix(1,N,T); #record survivors
  Nt<-N*matrix(1,T,1); #count surviving population
  MR<-matrix(1,1,T);
  
  # Iteration
  justdied<-which(H[,1]<Hbar); #find those who just died
  mortality<-length(justdied); #count the deaths
  Nt<-N-mortality; 
  deathyear[justdied]<-1; #record infant mortality rate
  alive[justdied,1]<-0;
  MR[1,1]<-mortality/N; # here maybe 
  
  t<-2;
  go_on<-1;
  while (t<T & go_on==1){
    Eps<-sigma_e*rnorm(N,1); #draw shock
    H[,t]<-H[,t-1]+(I-delta*(t^alpha)+Eps);
    alive[,t]<-(alive[,t-1]==1 & H[,t]>Hbar);
    justdied<-which(alive[,t-1]!=alive[,t]);
    mortality<-length(justdied);
    Nt<-Nt-mortality;
    deathyear[justdied]<-t; #record death year
    
    if(sum(alive[,t])>0){
      MR[1,t]=mortality/sum(alive[,t])
    }
    else{
      go_on=0;
    }
    t<-t+1;
  }
  
  lMR<-t(log(MR))
  
  # Control 0's and Infinites
  
  log_0<-which(lMR>0)
  lMR[log_0]<-0
  
  #log_NaN<-which(is.infinite(lMR))
  #lMR[log_NaN]<- -7;
  
  return(lMR)
  
  
  #plot(lMR)
}

Y <- logMR(I=0.13,delta=0.0015,sigma_e=0.42,alpha=1.32,mu_H=0.9)
plot(Y)


    
    Halive=H.*alive;
    #CHECKS AND FIGURES
    # mortality                     OK
    #{
    plot(log(MR));
    title('log mortality, 1m indivdiuals');
    #}

end
