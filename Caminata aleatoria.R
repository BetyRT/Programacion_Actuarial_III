
z<-100
vec <- vector("numeric",0)
aleatoria<- function(x){
    for(i in 1:x){
        length(vec) <- length(vec) + 1
        vec[i] <- z
        moneda <- rbinom(1,1,0.5)
        if(moneda == 1){
            z <- z + 1
            vec[i] <- z  
        } else {
            z <- z - 1
            vec[i] <- z
        }
        
    }
    print(vec)
    plot(vec,type="l", lwd=5 )
    
}
aleatoria(100)