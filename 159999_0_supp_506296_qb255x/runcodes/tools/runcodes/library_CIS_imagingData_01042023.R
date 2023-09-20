################################################################
################################################################


mLDA.Kclass <-
function(X, y, X.new, Z=NULL, Z.new=NULL, COV=NULL, COR=NULL, K=3, strong.X.list=NULL, tau=20, alpha=0.5, nu=100, d=2, nb=10){

    ### strong.X.list is of length K*(K-1)/2. The strong.X.set for pair (k1, k2) [k2<k1] is its ((k1-2)*(k1-1)/2+k2)'th element. 

    p <- dim(X)[2]
    n <- dim(X)[1]

    if(length(y)!=n){
        stop("X and Y contain different number of samples!!")
     }
    if(dim(X.new)[2]!=p){
        stop("The test data do not contain the same number of features as the training data!!")
     }
    
    if(is.null(COV)){
	COV <- cov(X)
    }
  
    if(is.null(COR)){
	COR <- cor(X)
    }


    marker.name.vec <- colnames(X)
    if(is.null(marker.name.vec)){
	marker.name.vec <- c(1:p) #paste("V", c(1:p), sep="")
	colnames(X) <- marker.name.vec
    }
    colnames(COV) <- marker.name.vec
    rownames(COV) <- marker.name.vec
    colnames(COR) <- marker.name.vec
    rownames(COR) <- marker.name.vec
  

	
    ISmatrix <- matrix(0, p, (K*(K-1)/2))
    rownames(ISmatrix) <- marker.name.vec

    MImatrix.name <- matrix(0, tau, (K*(K-1)/2))
    MImatrix.cord <- matrix(0, tau, (K*(K-1)/2))

    conn_Array <- array(0, dim=c(tau, p, (K*(K-1)/2)))
    dimnames(conn_Array)[[2]] <- marker.name.vec

    network.nodes_list <- NULL
    network.Omega_list <- NULL 
    
    local.network_LIST <- NULL
    
    nu.vec.tmp <- NULL

    for(k1 in 2:K){
   	 for(k in 1:(k1-1)){
   	 
   	    if(is.null(strong.X.list)){
   	    	strong.X.set <- NULL
   	    }else{
   	    	strong.X.set <- strong.X.list[[(k1-2)*(k1-1)/2+k]]
   	    }
   	    
   	    tmp <- mLDA.pair(X, y, X.new, Z=Z, Z.new=Z.new, COV=COV, COR=COR, pair=c(k,k1), strong.X.set=strong.X.set, tau=tau, alpha=alpha, nu=nu, d=d, nb=nb)     
 		ISmatrix[as.numeric(names(tmp$iffcond)), ((k1-1)*(k1-2)/2+k)] <- tmp$iffcond
		MImatrix.cord[,((k1-1)*(k1-2)/2+k)] <- tmp$MIset
		MImatrix.name[,((k1-1)*(k1-2)/2+k)] <- tmp$MIset ###names(tmp$MIset) 01/02/2023
		conn_Array[,,((k1-1)*(k1-2)/2+k)] <- tmp$connMatrix
		network.nodes_list[[((k1-1)*(k1-2)/2+k)]] <- tmp$network.nodes
		network.Omega_list[[((k1-1)*(k1-2)/2+k)]] <- tmp$network.Omega
		
		local.network_LIST[[((k1-1)*(k1-2)/2+k)]] <- tmp$local.network_list
		
	        nu.vec.tmp <- c(nu.vec.tmp, ISmatrix[which(ISmatrix[,1]!=0), ((k1-1)*(k1-2)/2+k)])
	  }
    }


    nu.value <- sort(abs(nu.vec.tmp), decreasing=TRUE)[nu]

    ISmatrix[which(abs(ISmatrix)<nu.value, arr.ind=TRUE)] <- 0
    Sset <- which(apply(ISmatrix, 1, function(x) sum(abs(x)))!=0)
    ISmatrix.o <- ISmatrix[Sset, ]

    #### this is not really useful 08212020
#    Sset.tmp <- apply(ISmatrix, 1, function(x) sum(abs(x)))[Sset]
#    names(Sset.tmp) <- Sset
#    Sset.order <- sort(Sset.tmp, decreasing=TRUE)
    ########################################

    mean.cis.mat <- NULL

    for(k in 1:K){
       id <- which(y==k)
       X.k <- X[id, Sset]
       mean.cis.vec <- apply(X.k, 2, mean)
       mean.cis.mat <- cbind(mean.cis.mat, mean.cis.vec)
    }

    COV.cis.mat <- COV[Sset, Sset]
    Omega.cis.mat <-  my.inv(COV.cis.mat)

    Fisher.matrix <- NULL

    for(i in 1:dim(X.new)[1]){
       X.new.cis.i <- X.new[i, Sset]
       Fisher.vec <- NULL
       for(k in 1:K){
           Fisher.ik <- (X.new.cis.i-mean.cis.mat[,k]/2)%*%Omega.cis.mat%*%mean.cis.mat[,k]
           Fisher.vec <- c(Fisher.vec, Fisher.ik)
       }
       Fisher.matrix <- rbind(Fisher.matrix, Fisher.vec)
    }

    row.names(Fisher.matrix) <- c(1:(dim(X.new)[1]))

    predClass <- apply(Fisher.matrix, 1, which.max)


    result <- list(network.nodes_list=network.nodes_list, network.Omega_list=network.Omega_list, local.network_LIST=local.network_LIST, ISmatrix=ISmatrix.o, screenset=Sset, Fisher.matrix=Fisher.matrix, PredClass=predClass, MImatrix.cord=MImatrix.cord, MImatrix.name=MImatrix.name, conn_Array=conn_Array)

    result


}

##### ISmatrix lists all "iffcond"s for only selected features in network.nodes_list (in an descent order).








mLDA.pair <-
function(X, y, X.new, Z=NULL, Z.new=NULL, COV=NULL, COR=NULL, pair=c(1,2), strong.X.set=NULL, tau=20, alpha=0.5, nu=100, d=2, nb=10){
  
    p <- dim(X)[2]
    n <- dim(X)[1]

    if(length(y)!=n){
        stop("X and Y contain different number of samples!!")
     }
    if(dim(X.new)[2]!=p){
        stop("The test data do not contain the same number of features as the training data!!")
     }

    id1<- which(y==pair[1])
    if(length(id1)==0){
		stop(paste("There is no y entries labeled as ", pair[1], "!", sep=""))
	}

    id2<- which(y==pair[2])
    if(length(id2)==0){
		stop(paste("There is no y entries labeled as ", pair[2], "!", sep=""))
	}

	y12 <- c(rep(0, length(id1)), rep(1, length(id2)))
	X12 <- X[c(id1, id2),]
    
    if(is.null(COV)){
		COV <- cov(X)
    }
  
    if(is.null(COR)){
		COR <- cor(X)
    }


    marker.name.vec <- colnames(X)
    if(is.null(marker.name.vec)){
	marker.name.vec <- c(1:p) #paste("V", c(1:p), sep="")
	colnames(X) <- marker.name.vec
    }
    colnames(COV) <- marker.name.vec
    rownames(COV) <- marker.name.vec
    colnames(COR) <- marker.name.vec
    rownames(COR) <- marker.name.vec
#    colnames(X12) <- marker.name.vec

	COR[which(abs(COR)<alpha, arr.ind=TRUE)] <- 0

	meanDiff <- (apply(as.matrix(X12[which(y12==0),]), 2, mean) - apply(as.matrix(X12[which(y12==1),]), 2, mean))/apply(as.matrix(X12), 2, sd)
		#names(meanDiff) <- marker.name.vec
	meanDiffRank <- order(-abs(meanDiff))

	if(is.null(strong.X.set)){
		meanDiff_firstTau <- meanDiffRank[1:tau]
		names(meanDiff_firstTau) <- marker.name.vec[meanDiff_firstTau]
	}else{
		if(length(strong.X.set)>=tau){
			meanDiff_firstTau <- strong.X.set[1:tau]  #### strong.X.set needs to be ordered from the most to the least strong X predictor. And it is the orders of the features in the X feature set with names. 
		}else{
			meanDiff_firstTau <- strong.X.set
			sprintf("Warning: Tau > length(strong.X.set). All features in strong.X.set will be included.")
		}
	}

	meanAvg <- (apply(as.matrix(X12[which(y12==0),]), 2, mean) + apply(as.matrix(X12[which(y12==1),]), 2, mean))/2

	Omega <- matrix(0, p, p)
	colnames(Omega) <- marker.name.vec
	rownames(Omega) <- marker.name.vec

	vis=rep(0, p)
	id <- meanDiff_firstTau[1]
	scr <- meanDiff_firstTau
	conn_matrix	<- matrix(0, tau, p)
	rownames(conn_matrix) <- meanDiff_firstTau
	colnames(conn_matrix) <- marker.name.vec
	
	local.network_list <- list() 

	while(!is.na(id)){
		#tmp <- con_node(id, COR, visited=vis, m=d)
		tmp <- con_node_dn(id, COR, visited=vis, depth.max=d, neighbor.max=nb)
		con_set <- tmp$connect
		names(con_set) <- marker.name.vec[con_set]
		vis <- tmp$visited
		Omega[con_set, con_set] <- my.inv(COV[con_set, con_set])
		
		local.network_list <- append(local.network_list, list(con_set))

		tau_id <- which(meanDiff_firstTau==id)
		conn_matrix[tau_id, con_set] <- 1 

		id <- scr[is.na(match(scr, con_set))][1]
		scr <- scr[is.na(match(scr, con_set))]
	}

	screenID <- which(apply(Omega, 1, sum)!=0)
	Omega.s <- Omega[screenID, screenID]

	if(!is.null(Z)){
#		Z <- apply(Z, 2, function(x) (x-mean(x))/sd(x))
		ZmeanDiff <- apply(as.matrix(Z[which(y12==0),]), 2, mean) - apply(as.matrix(Z[which(y12==1),]), 2, mean)
		ZmeanAvg <- (apply(as.matrix(Z[which(y12==0),]), 2, mean) + apply(as.matrix(Z[which(y12==1),]), 2, mean))/2
		ZOmega <- qr.solve(cov(Z))
		pZ <- dim(Z)[2]
	}else{
		ZmeanDiff <- NULL
		ZmeanAvg <- NULL
		ZOmega <- NULL
		pZ <- 0
	}

	meanDiff.s <- meanDiff[screenID]
	iffcond.s <- as.vector(Omega.s%*%meanDiff.s)
	names(iffcond.s) <- screenID
	iffcond.s_order_id <- order(abs(iffcond.s), decreasing=TRUE)
	iffcond.s_order <- iffcond.s[iffcond.s_order_id]

    if(length(iffcond.s_order)>nu){
		screenID2 <- as.numeric(names(iffcond.s_order)[1:nu])
	}else{
 		screenID2 <- as.numeric(names(iffcond.s_order))
    }

	meanAvg.s0 <- meanAvg[screenID2] 
	Omega.s20 <- Omega[screenID2, screenID2] 
	meanDiff.s20 <- meanDiff[screenID2]
	X.new.s0 <- X.new[,screenID2]

	if(!is.null(Z)){
		meanAvg.s <- c(ZmeanDiff, meanAvg.s0)
    	pXs2 <- length(screenID2)
		Omega.s2 <- matrix(0, (pZ+pXs2), (pZ+pXs2)) 
		Omega.s2[1:pZ, 1:pZ] <- ZOmega
		Omega.s2[(pZ+1):(pZ+pXs2), (pZ+1):(pZ+pXs2)] <- Omega[screenID2, screenID2] 
#	Omega.s2 <- Omega[screenID2, screenID2] 
		meanDiff.s2 <- c(ZmeanDiff, meanDiff[screenID2])
		X.new.s <- cbind(Z.new, X.new[,screenID2])
	}else{
		meanAvg.s <- meanAvg.s0
		Omega.s2 <- Omega[screenID2, screenID2] 
		meanDiff.s2 <- meanDiff[screenID2]
		X.new.s <- X.new[,screenID2]
	}

	if(length(meanAvg.s)==1){
		meanAvg.s <- as.vector(meanAvg.s)
		X.new.s <- as.matrix(X.new.s)
		meanAvg.s.m <- matrix(rep(meanAvg.s, dim(X.new.s)[1]), dim(X.new.s)[1], length(meanAvg.s), byrow=TRUE)
	}else{
		meanAvg.s.m <- matrix(rep(meanAvg.s, dim(X.new.s)[1]), dim(X.new.s)[1], length(meanAvg.s), byrow=TRUE)
	}

#	ZmeanAvg.m0 <- matrix(rep(ZmeanAvg, dim(Z)[1]), dim(Z)[1], dim(Z)[2], byrow=TRUE)
#	meanAvg.s.m <- cbind(ZmeanAvg.m0, meanAvg.s.m0)


	FisherDR <- (X.new.s-meanAvg.s.m)%*%Omega.s2%*%meanDiff.s2
 	PredClass <- (FisherDR>=0)

	result <- list(network.nodes=screenID, network.Omega=Omega.s20, local.network_list=local.network_list, iffcond=iffcond.s_order, screenset=screenID2, FisherDR=FisherDR, PredClass=PredClass, MIset = meanDiff_firstTau, connMatrix = conn_matrix)

    result
}


##### iffcond lists iffcond for the first nu selected features in network.nodes (in an descent order). screenset lists only the first nu selected features.






my.inv <-
function(X, eps=1e-12){
	eig.X <- eigen(X, symmetric=TRUE)
	P <- eig.X[[2]]
	lambda <- eig.X[[1]]
	ind <- lambda > eps
	lambda[ind] <- 1/lambda[ind]
	lambda[!ind] <- 0
	ans <- P%*%diag(lambda, nrow=length(lambda))%*%t(P)
	return(ans)
}






####################
####################
### 08/06/2021
###




#==============================  final version of neighbor+layer selection
con_node_dn <-
  function(vid, Sig, visited=rep(0, dim(Sig)[1]), depth=0, depth.max=2, neighbor.max=10, connect=NULL, path=NULL, depth.path=NULL){
    
    if(dim(Sig)[1] != dim(Sig)[2]){ stop("Sig is not a square matrix") }
    if(vid>dim(Sig)[1] | vid<=0){ stop("Wrong vertex index")}

    neighbor <- 0
    
    if(neighbor<=neighbor.max & depth<=depth.max){    
    	connect <- c(connect, vid)
    	path <- c(path, vid)
    	visited[vid] <- 1
     }
    
    
    flag <- 0
    
    tmp <- which(visited==0)  
    ranks <- order(-abs(Sig[vid,tmp]))
    rest.ranks <- tmp[ranks]
    
    depth0 <- depth
    neighbor0 <- neighbor

    
    for(i in rest.ranks){
          depth1 <- depth0
             
      if(Sig[vid,i]!=0 & visited[i]==0 & neighbor<neighbor.max & depth1<depth.max){
      
      
      
        	neighbor <- neighbor +1

         
                depth1 <- depth1+1
         
        	tmp <- con_node_dn(i, Sig, visited=visited, depth=depth1, depth.max=depth.max, neighbor.max=neighbor.max, connect=connect, path=path, depth.path=depth.path)
        
        	visited <- tmp$visited
        	depth <- tmp$depth
         	connect <- tmp$connect
        	path <- tmp$path
        	
        	depth.path <- tmp$depth.path
        	depth.path <- c(depth1, depth.path)
        	
        	flag <-1

     }  # end of  if(Sig[vid,i]!=0 & visited[i]==0 & neighbor<=neighbor.max)
    } # end of for
    

    
   if(flag==0){stop}
    
    return(list(connect=connect, visited=visited, depth=depth, path=path, depth.path=depth.path))
  }








#########################################################################
#########################################################################
##### Cross-validation



mLDA.cv <-
function(X, Y, Z=NULL, fold=5, seed=1, strong.X.list=NULL, tau=50, alpha=0.9, nu=500, d=2, nb=5)
{

### X, Z are matrices, Y is a vector
  n=nrow(X)
#  p=ncol(X)
#  q=ncol(Y)
  K= length(table(Y))
    
 ############################### set seed and generate cross validation seperations
 set.seed(seed)

# index.cv<-NULL
# ran.order=sample(1:n, n, replace=F)
# f.n=floor(n/fold)
# for (f in 1:(fold-1))
#    {  
#     index.cv[[f]]<-ran.order[(f-1)*f.n+1:f.n] 
#    } 
# index.cv[[fold]]<-ran.order[((fold-1)*f.n+1):n]
# 
 #### Need to be rewritten
 
 index.cv<- vector(mode='list', length=fold)
 f.n=floor(n/fold)
 
 for(k in 1:K){
 
    index_set_k <- which(Y==(k-1))
    ran.order_set_k =sample(index_set_k, length(index_set_k), replace=F)
    
    if( length(index_set_k) < fold){
    	err <- sprintf(paste("There are less than ", fold, "samples in class ", k, sep=""))
    }
    f.nk <- floor(length(index_set_k)/fold) 
    
    for (f in 1:(fold-1)){  
     index.cv[[f]]<-c(index.cv[[f]], ran.order_set_k[(f-1)*f.nk+1:f.nk])
    } 
    index.cv[[fold]]<-c(index.cv[[fold]], ran.order_set_k[((fold-1)*f.nk+1):length(index_set_k)])

 
 }
 
 
 ################################ begin cross validation
 err.cv <- NULL
 err.tot <- 0
 
 Y.test.arr <- NULL
 Y.pred.arr <- NULL

 for(f in 1:fold)
  {  print(paste("fold=",f))   
     index.cur.cv<-index.cv[[f]]   
     X.m<-X[-(index.cur.cv),]   
     Y.m<-Y[-(index.cur.cv)]   
     X.t<-X[index.cur.cv,]   
     Y.t<-Y[index.cur.cv] 
     if(!is.null(Z)){ 
     	Z.m<-Z[-(index.cur.cv),] 
     	Z.t<-Z[index.cur.cv,]
     }else{
     	Z.m <- NULL
     	Z.t <- NULL
     }
     
     Y.test.arr[[f]] <- Y.t
     
     XName <- colnames(X.m)

     X.m_sd <- apply(X.m, 2, sd)
     X.t_sd <- apply(X.t, 2, sd)
     rm_Xid <- union(which(X.m_sd==0),  which(X.t_sd==0)) 
    
     
     if(length(rm_Xid)==0){
     	X.m_new <- X.m
     	X.t_new <- X.t
     	
     	X.m_new <- apply(X.m_new, 2, function(x) (x-mean(x))/sd(x))
	X.t_new <- apply(X.t_new, 2, function(x) (x-mean(x))/sd(x))
     }else{
	X.m_new <- X.m[,-rm_Xid]
	X.t_new <- X.t[,-rm_Xid]
	XName.new <- XName[-rm_Xid]


	X.m_new <- apply(X.m_new, 2, function(x) (x-mean(x))/sd(x))
	X.t_new <- apply(X.t_new, 2, function(x) (x-mean(x))/sd(x))
	colnames(X.m_new) <- XName.new
	colnames(X.t_new) <- XName.new
    }
    
    if(!is.null(Z)){ 
    	Z.m_sd <- apply(Z.m, 2, sd)
    	Z.t_sd <- apply(Z.t, 2, sd)
    	rm_Zid <- union(which(Z.m_sd==0),  which(Z.t_sd==0)) 
    
    	if(length(rm_Zid)==0){
     		Z.m_new <- Z.m
     		Z.t_new <- Z.t
   	 }else{
		Z.m_new <- Z.m[,-rm_Zid]
		Z.t_new <- Z.t[,-rm_Zid]
   	 }
   }else{
   	Z.m_new <- NULL
   	Z.t_new <- NULL
   }


	if(K==2){
	  pair <- c(max(Y), min(Y))
   	    if(is.null(strong.X.list)){
   	    	strong.X.set <- NULL
   	    }else{
   	    	strong.X.set <- strong.X.list[[1]]
   	    }	  
	  tmp  <- mLDA.pair(X.m_new, Y.m, X.t_new, Z=Z.m_new, Z.new=Z.t_new, pair=pair, strong.X.set=strong.X.set, tau=tau, alpha=alpha, nu=nu, d=d, nb=nb)	
	}else if(K>2){
          tmp  <- mLDA.Kclass(X.m_new, Y.m, X.t_new, Z=Z.m_new, Z.new=Z.t_new, K=K, strong.X.list=strong.X.list, tau=tau, alpha=alpha, nu=nu, d=d, nb=nb)
        }
           
       pred_Y <- as.numeric(tmp$PredClass)
       err.cv.c <- length(which(pred_Y!=Y.t)) 
       err.cv<- c(err.cv, err.cv.c)
       err.tot <- err.tot + err.cv.c
       
       Y.pred.arr[[f]] <- pred_Y
    
   }###end fold loop
   
   result=list(err.tot=err.tot, err.cv=err.cv, Y.pred.arr=Y.pred.arr, Y.test.arr=Y.test.arr) 
   
   return(result)
}




#########################################
##########################################
###################################
#### permutation test

mLDA.permutation.test <- 
function(X, Y, Z=NULL, K=3, select.set, Iff, Omega.X, permN=100000)
{
 
 	ISresampleMat_list <- vector(mode='list', length=K*(K-1)/2)
 	
	X.s <- X[, select.set]
#	Omega.X <- my.inv(cov(X.s))
	
	if(!is.null(Z)){
	    Omega.Z <- my.inv(cov(Z))
	}
 	
 	
	for(k1 in 2:K){
   	    for(k in 1:(k1-1)){
#   	 	    if(!is.null(Z)){
#   	 	    	mean.diff.Z <- apply(Z[which(Y==k),], 2, mean) - apply(Z[which(Y==k1),], 2, mean)
#   	 	    	mean.diff.X <- apply(X.s[which(Y==k),], 2, mean) - apply(X.s[which(Y==k1),], 2, mean)
#   	 	    	ISresampleMat_list[[(k1-1)*(k1-2)/2+k]] <- cbind(ISresampleMat_list[[(k1-1)*(k1-2)/2+k]], Omega.Z%*%mean.diff.Z+ Omega.X%*%mean.diff.X)
#   	 	    }else{
#   	 	    	mean.diff.X <- apply(X.s[which(Y==k),], 2, mean) - apply(X.s[which(Y==k1),], 2, mean)
#   	 	    	ISresampleMat_list[[(k1-1)*(k1-2)/2+k]] <- cbind(ISresampleMat_list[[(k1-1)*(k1-2)/2+k]], Omega.X%*%mean.diff.X)
#   	 	    }

  	 	    	ISresampleMat_list[[(k1-1)*(k1-2)/2+k]] <- cbind(ISresampleMat_list[[(k1-1)*(k1-2)/2+k]], Iff)
   	   }
   	}	
 	
 	
 	for(permut_id in 1:permN){
	    set.seed(permut_id)
		
	    id_perm <- sample(c(1:length(Y)), length(Y), replace=FALSE)
#	    for(k in 1:K){
#		id_perm[which(Y==k)] <- sample(which(Y==k), length(which(Y==k)), replace=TRUE)
#	    }

	   Y.p <- Y[id_perm]
##### test whether need to permuted Omega matrices?	    
#	    X.p <- X[id_perm, select.set]
#	    Omega.X <- my.inv(cov(X.p))
	
#	    if(!is.null(Z)){
#	    	Z.p <- Z[id_perm, ]
#	    	Omega.Z <- my.inv(cov(Z.p))
#	    }
	    
	    for(k1 in 2:K){
   	 	for(k in 1:(k1-1)){
   	 	    if(!is.null(Z)){
   	 	    	mean.diff.Z <- apply(Z[which(Y.p==k),], 2, mean) - apply(Z[which(Y.p==k1),], 2, mean)
   	 	    	mean.diff.X <- apply(X.s[which(Y.p==k),], 2, mean) - apply(X.s[which(Y.p==k1),], 2, mean)
   	 	    	ISresampleMat_list[[(k1-1)*(k1-2)/2+k]] <- cbind(ISresampleMat_list[[(k1-1)*(k1-2)/2+k]], Omega.Z%*%mean.diff.Z+ Omega.X%*%mean.diff.X)
   	 	    }else{
   	 	    	mean.diff.X <- apply(X.s[which(Y.p==k),], 2, mean) - apply(X.s[which(Y.p==k1),], 2, mean)
   	 	    	ISresampleMat_list[[(k1-1)*(k1-2)/2+k]] <- cbind(ISresampleMat_list[[(k1-1)*(k1-2)/2+k]], Omega.X%*%mean.diff.X)
   	 	    }
   	       }
   	    }	

	}


	pM <- NULL
	pM.colname <- NULL 
	
	for(k1 in 2:K){
   	   for(k in 1:(k1-1)){
   	   	pM <- cbind(pM, apply(abs(ISresampleMat_list[[(k1-1)*(k1-2)/2+k]]), 1, function(x) length(which(x>x[1])))/(permN+1))  
   	   	pM.colname <- c(pM.colname, paste(k,"<->", k1, sep="")) 	   	
   	   }
   	}	
        
        colnames(pM) <- pM.colname
        
        result <- list(pM=pM, ISresampleMat_list =ISresampleMat_list)
   
	return(result)
}




