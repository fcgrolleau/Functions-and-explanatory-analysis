### Feature scaling function

### Function working at the column  level
scaling<-function(x){
        if(is.numeric(x)==FALSE){
                z<-x
                x<-NA
        }
        y<-x[which(is.na(x)==FALSE)]
        sc<-(y-mean(y))/sd(y)
        if(all(is.na(x)==TRUE)){
                sc<-z
        }
        print(sc)
}

### Function working at the level of all colums
featurescaling<-function(data){
        data.frame(lapply(data, scaling))
}

### Example
str(scaling(diamonds[,5]))
str(featurescaling(diamonds)) ## of note, it leaves the non numeric column untouched
