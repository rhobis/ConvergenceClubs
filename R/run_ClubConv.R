require(lmtest)
require(sandwich)
# setwd("~/Desktop/Prima parte tesi dottorato/Club convergenza prove/ClubConvergenza ok")
# source('~/Desktop/Prima parte tesi dottorato/Club convergenza prove/ClubConvergenza ok/ClubConv.R')

setwd("~/R/Funzioni/ClubConvergenza/")
source('~/R/Funzioni/ClubConvergenza/findClubs.R')
source('~/R/Funzioni/ClubConvergenza/mergeClub.R')
# source('~/R/Funzioni/ClubConvergenza/mergeDivergent.R')


# X <- read.table("GDP_PC_PPP2.csv", sep=",",header=T)
X <- read.table("GDPfiltered.txt",header=T)
head(X)

#load("clubConv.RData")

# X[,9:23] <- log(X[,9:23])
X[,2:16] <- log(X[,2:16])

soglia <- 0
# yearVar <- 9:23
# lastT <- 23
yearVar <- 2:16
lastT <- 16
# IDvar <- 8
IDvar <- 1

clubbis <- findClubs(X,IDvar,yearVar,lastT,cstar=0)
names(clubbis$clubs)
names(clubbis$clubs[[1]])

mclubs <- mergeClub(clubbis,X,IDvar,yearVar,lastT)
mclubs
names(mclubs)
names(mclubs[[1]])

mergeClub(clubbis,X,IDvar,yearVar,lastT,method='vLT')
mergeClub(clubbis,X,IDvar,yearVar,lastT,method='vLT',divergent=T)

mergeClub(clubbis2,X,IDvar,yearVar,lastT2)
mergeClub(clubbis2,X,IDvar,yearVar,lastT2,method='vLT')



#################
betas <- vector()
for(ind in 1:length(clubbis)){
    betas <- c(betas,clubbis[[ind]][[1]]$beta)
}
betas

pvals <- vector()
for(ind in 1:length(clubbis)){
    pvals <- c(pvals,clubbis[[ind]][[1]]$pvalue)
}
pvals

##################
### Merge

# 
# for(k in dim(dati)[1]){
#     computeH(X,which(X[,IDvar] %in% as.character(clubs[[k]][[1]])))
# }


i <- 0
j <- 1

# while(j < length(clubbis)){
#     i <- i + 1 
#     j <- j + 1
#     
#     subs <- rbind(X[clubbis[[i]]$indice_dataset,],X[clubbis[[j]]$indice_dataset,])
#     H <- computeH(subs,id=1:dim(subs)[1])
#     tvalue <- estimateMod(H,yearVar)$tvalue
#     if(tvalue > -1.65){
#         clubbis[[j]]$indice_dataset <- which(X[,IDvar] %in% as.character(subs[,IDvar]))
#         clubbis[[j]]$mergiato <- TRUE
#     }
############  
#     if(j == length(clubbis)){
    #         return()
    #     }else break
    # }else if(j == length(clubbis)){
    #     return(FALSE)
    # } 
}

# for(ind in 1:length(clubbis)){
    # 
# }
# subs <- X[clubbis[[1]]$indice_dataset]
# mergiato <- coreG(subs,lastT,type="all")

i <- 14
j <- 15


subs1 <- rbind(X[clubbis[[1]]$indice_dataset,]) ##primo gruppo

H1 <- computeH(subs1,id=1:dim(subs1)[1]) ## H del primo gruppo
  
tvalue1 <- estimateMod(H1,yearVar)$tvalue  #####valori primo gruppo
beta1 <- estimateMod(H1,yearVar)$beta
pvalue1 <- estimateMod(H1,yearVar)$pvalue
tvalue1
beta1
pvalue1
######


#####trova gli h piccoli del primo gruppo
######
id <- 1:dim(subs1)[1]
h <- matrix(0,dim(subs1)[1],15) 
i <- 0
for(x in id){#for sulle regioni
  i <- i + 1
  j <- 0
  for(t in yearVar){#for sul tempo
    j <- j+1
    h[i,j] = subs1[x,t]/mean(subs1[id,t])
  }
}

v <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")
h1<- t(h)
rownames(h1) <- v
dat1 <- matrix(h1,ncol=25) # make data
matplot(dat1, type = c("l"),pch=1,col = 1:25, ylab="Relative Transition Paths") #plot

#######
                                                #secondo gruppo
subs2 <- rbind(X[clubbis[[2]]$indice_dataset,],   
              X[clubbis[[3]]$indice_dataset,]) 

         
H2 <- computeH(subs2,id=1:dim(subs2)[1])        # H del secondo gruppo

tvalue2 <- estimateMod(H2,yearVar)$tvalue  #####valori primo gruppo
beta2 <- estimateMod(H2,yearVar)$beta
pvalue2 <- estimateMod(H2,yearVar)$pvalue
tvalue2
beta2
pvalue2

#####trova gli h piccoli del secondo gruppo
######
id <- 1:dim(subs2)[1]
h <- matrix(0,dim(subs2)[1],15) 
i <- 0
for(x in id){#for sulle regioni
  i <- i + 1
  j <- 0
  for(t in yearVar){#for sul tempo
    j <- j+1
    h[i,j] = subs2[x,t]/mean(subs2[id,t])
  }
}

v <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")
h2<- t(h)
rownames(h2) <- v
dat2 <- matrix(h2,ncol=dim(h2)[2]) # make data
matplot(dat2, type = c("l"),pch=1,col = 1:25, ylab="Relative Transition Paths",
        axes=FALSE) #plot
axis(1,at=seq(1,15),labels=seq(2000,2014))
axis(2)
box()

######
subs3 <- rbind(X[clubbis[[4]]$indice_dataset,],    #terzo gruppo
              X[clubbis[[5]]$indice_dataset,],
              X[clubbis[[6]]$indice_dataset,],
              X[clubbis[[7]]$indice_dataset,],
              X[clubbis[[8]]$indice_dataset,],
              X[clubbis[[9]]$indice_dataset,],
              X[clubbis[[10]]$indice_dataset,]
              )

H3 <- computeH(subs3,id=1:dim(subs3)[1])          # H del terzo gruppo

tvalue3 <- estimateMod(H3,yearVar)$tvalue
beta3<- estimateMod(H3,yearVar)$beta
pvalue3 <- estimateMod(H3,yearVar)$pvalue
tvalue3
beta3
pvalue3

#####trova gli h piccoli del secondo gruppo
######
id <- 1:dim(subs3)[1]
h <- matrix(0,dim(subs3)[1],15) 
i <- 0
for(x in id){#for sulle regioni
  i <- i + 1
  j <- 0
  for(t in yearVar){#for sul tempo
    j <- j+1
    h[i,j] = subs3[x,t]/mean(subs3[id,t])
  }
}

v <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")
h3<- t(h)
rownames(h3) <- v
dat3 <- matrix(h3,ncol=dim(h3)[2]) # make data
matplot(dat3, type = c("l"),pch=1,col = 1:25, ylab="Relative Transition Paths") #plot

######

subs4 <- rbind(X[clubbis[[11]]$indice_dataset,],
              X[clubbis[[12]]$indice_dataset,]
              )
          

H4 <- computeH(subs4,id=1:dim(subs4)[1])

# H <- computeH(subs,id=1:dim(subs)[1])
# tvalue <- estimateMod(H,yearVar)$tvalue
# tvalue


beta1
pvalue1

beta2
pvalue2

beta3
pvalue3

beta4
pvalue4




plot(H1)
plot(H2)
plot(H3)
plot(H4)

M1 <- matrix(0, nrow=15, ncol=4)
M <- rbind(H1, H2, H3, H4)
for(j in 1 : 15) {
M1[j,] <- M[,j]  
                }
M
M1
M1 <- M1[4:15,]

M


dat <- matrix(M1,ncol=4) # make data
matplot(dat, type = c("b"),pch=1,col = 1:4) #plot
legend("topleft", legend = 1:4, col=1:4, pch=1) # optional legend

