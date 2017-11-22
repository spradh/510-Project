x = na.omit(read.csv('cluster.csv', header = FALSE)[,-1])

#standardize data
dataset <- scale(dataset)

km<-function(x,K){
  #running the k-means algorith
  kmeans=kmeans(x,K)
  
  #Mean of each cluster
  mu=t(kmeans$centers)
  
  
  cluster_data<-vector()
  for(k in 1:K){
    a<-paste("cluster",k,sep="");
    cluster<-x[kmeans$cluster==k,]
    assign(a,cluster)
    cluster_data<-c(cluster_data, a)
  }
  
  sigma<-list(K)
  for(k in 1:K){
    sigma[[k]]<- cov(as.matrix(get(cluster_data[k])))
  }
  
  result<-list(mu=mu,sigma=sigma)
  return (result)
}

clusters<-vector()


for(K in 1:20){
  a<-paste("K=",K,sep="")
  assign(a,km(x=x,K=K))
  clusters<-c(clusters,a)
}

get(clusters[2])$mu
get(clusters[2])$sigma[[3]]


em<-function(x ,K,pi_k,mu,covar,maxits=100,tol=1e-20){
  library(mvtnorm)
  N=nrow(x )                       #total number of observations
  it=0                            #initializing iteration parameter
  converged=FALSE                 #convergence boolean
  loglike=0                       #loglikelihood parameter
  gamma=matrix (0,ncol=K,nrow=N)  #initializing responsibility matrix 
  cat(paste("Running EM for K =", K,"\n"))
  while(!converged && it<maxits){
    mu_old=mu
    loglike_old=loglike
    pi_k_old=pi_k
    
    #Iteration
    it=it+1
    if(it==1 | it%%5==0){cat(paste("\t Iterations of EM:", it,"\n"))}
    
    
    #Ex ecuting E-step
    for(k in 1:K){
      gamma[,k]=pi_k[k] * dmvnorm(x , mu[,k], sigma = covar[[k]], log=F)
    }
    
    gamma=gamma/rowSums(gamma)
    gamma
    
    #Ex ecuteing M-step
    
    #Cluster Sizes
    N_k=colSums(gamma)
    
    #Cluster Probabilities
    pi_k=N_k/N
    pi_k
    
    for(k in 1:K){
      #updating Means
      mu[,k]=(t(x ) %*% gamma[,k]) / N_k[k]
      #Updating covar
      covar[[k]]=cov.wt(x , wt = gamma[,k],center=mu[,k])$cov
      
    }
    
    #Calculating Log-Likelihood
    loglike=0
    for(i in 1:N){
      inter=0
      for(k in 1:K){
        inter=inter+pi_k[k]*dmvnorm(x [i,],mu[,k],sigma=covar[[k]],log=F)
      }
      loglike=loglike+log(inter)
    }
    
    converged=abs(sum(pi_k_old - pi_k))<tol
  }
  cat(paste("\t \t Done with running EM for K = ", K,"!\n\n\n"))
  results<-list(mu=mu,covar=covar,gamma=gamma,pi_k=pi_k,clusters=K,ll=loglike)
  return (results)
}

em_list<-vector()
for(K in 1:20){
  a<-paste("K=",K,sep="")
  result<-em(x ,K,pi_k=rep(1,K)/K,mu=get(clusters[K])$mu,covar=get(clusters[K])$sigma,maxits=100,tol=1e-20)
  assign(a,result)
  em_list<-c(em_list,a)
}
