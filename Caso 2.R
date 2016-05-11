#FUNCION 1 
mejor <- function(estado="TX",resultado="infarto"){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
      options(warn = -1)
      if(resultado == "infarto"){
            dat <- split(outcome[c(2,7,11)],outcome[c(2,7,11)]$State)
          }else if(resultado=="falla"){  
                dat <- split(outcome[c(2,7,17)],outcome[c(2,7,17)]$State)
              }else if(resultado=="neumonía"){
                    dat <- split(outcome[c(2,7,23)],outcome[c(2,7,23)]$State)
                  }
      Hosp <- dat[estado][[estado]][,1]
      Mor <- as.numeric(dat[estado][[estado]][,3])
      x <- na.omit(data.frame(Hosp,Mor))
      index <- with(x,order(Mor,Hosp))
      y <- x[index,]
      head(y,1)
}  


#FUNCION 2

rankhospital <- function(estado="TX",resultado="infarto",num="mejor"){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
    options(warn = -1)
      if(resultado == "infarto"){
            d <- split(outcome[c(2,7,11)],outcome[c(2,7,11)]$State)
          }else if(resultado=="falla"){  
                d <- split(outcome[c(2,7,17)],outcome[c(2,7,17)]$State)
              }else if(resultado=="neumonía"){
                    d <- split(outcome[c(2,7,23)],outcome[c(2,7,23)]$State)
                  }else{ 
                        print(NA)
                      }
    Hosp <- d[estado][[estado]][,1]
    Rate <- as.numeric(d[estado][[estado]][,3])
    x <- na.omit(data.frame(Hosp,Rate))
    index <- with(x,order(Rate,Hosp))
    y <- x[index,]
    w <- nrow(x)
    datos <- data.frame(y,rank=1:w)
      if(num=="mejor"){
            head(datos,1)
          }else if(num=="peor"){
                tail(head(datos,w),1)  
              }else if(0<num & num<=w){
                    tail(head(datos,num),1)
                  }else if(w<num){
                        print("Error")
                      }
}


#FUNCTION 3
rankingcompleto <- function(resultado="infarto",num="mejor"){
      outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
      options(warn = -1)
      if(resultado == "infarto"){
            da <- split(outcome[c(2,7,11)],outcome[c(2,7,11)]$State)
          }else if(resultado=="falla"){  
                da <- split(outcome[c(2,7,17)],outcome[c(2,7,17)]$State)
              }else if(resultado=="neumonía"){
                    da <- split(outcome[c(2,7,23)],outcome[c(2,7,23)]$State)
                  }
      Hosp <- c()
      for(j in 1:54){
            H <- da[j][[1]][,1]
            Rate <- as.numeric(da[j][[1]][,3])
            x <- na.omit(data.frame(H,Rate))
            index <- with(x,order(Rate,H))
            x <- x[index,][1]
            y <- nrow(x)
            if(num=="mejor"){
                  Hosp <- c(Hosp,as.character(x[1,1]))
                }else if(num=="peor"){
                      Hosp <- c(Hosp,as.character(x[y,1]))
                    }else if(0<num & num<=y ){
                          Hosp <- c(Hosp,as.character(x[num,1]))
                        }else if (y<num){
                              Hosp <- c(Hosp,as.character(x[y+1,1]))
                            } 
          }
      Estado <- sort(unique(outcome$State))
      data.frame(Hosp,Estado)
    }

    head(rankingcompleto("infarto",20),10)
tail(rankingcompleto("neumonía","peor"),3)
tail(rankingcompleto("falla","mejor"),10)