aleatorio <- function(n="1000",m="1000",dis="runif"){
    if(dis=="rgamma"){
        sumas <- lapply(rep(m,n),dis,0.5)
        hist(sapply(sumas,mean))
    }else{
        sumas <- lapply(rep(m,n),dis)
        hist(sapply(sumas,mean))
    }
}

