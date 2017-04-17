########################################################
###
### item based collaborative filtering
###
#########################################################


#####################################################
##### before start
dataPathHdfs <- "/user/binbin.chen/MovieLens/data"
step0hdfs <- file.path(dataPathHdfs, "dataCF")
step1hdfs <- file.path(dataPathHdfs,"step1")
step2hdfs <- file.path(dataPathHdfs,"step2")
step3hdfs <- file.path(dataPathHdfs,"step3")
step4hdfs <- file.path(dataPathHdfs,"step4")
step5hdfs <- file.path(dataPathHdfs,"step5")
step6hdfs <- file.path(dataPathHdfs,"step6")
### remove the files in the hdfs
hdfs.del(step0hdfs)
hdfs.del(step1hdfs)
hdfs.del(step2hdfs)
hdfs.del(step3hdfs)
hdfs.del(step4hdfs)
hdfs.del(step5hdfs)
hdfs.del(step6hdfs)
#####################################################


### load the rmr2 and rhdfs package
library(rmr2)
library(rhdfs)
hdfs.init()


### read/generate the data
#dataCF <- generateDataFun()
dataCF <- read.table("/home/binbin.chen/rHadoop/data/small.txt",header=F,sep=",")
names(dataCF)<-c("user","item","pref")

#### use the hadoop backend
rmr.options(backend = 'hadoop')
## The data path in hdfs
dataPathHdfs <- "/user/binbin.chen/MovieLens/data"
#### put the data into hdfs
step0hdfs <- file.path(dataPathHdfs, "dataCF")
##hdfs.del(step0hdfs)
to.dfs(dataCF,output=step0hdfs)


######################
### The following item-CF is implemented in .. steps:
###   1. merge the transcation for each user.
###   2. calculate the product the each (item1, item2, inProd, module.x)
###   3. calculate the module of item2, such that (item.x, item.y, inProd, module.x, module.y)
###   4. calculate the similarity of each combination of (item.x, item.y), such that (item.x, item.y, sim)
###   5. merge the similarity matrix with the transaction data by the item.x, such that (item.x.l, item.y.l, sim.l, user.r, ite.r, pref.r)
###   6. Calculate the preference for each (item,user), such that (item,user,pref)
######################


### mapreduce job 1: calculate the matrix multiplying
step1hdfs <- file.path(dataPathHdfs,"step1")
mapreduce(input=step0hdfs, output=step1hdfs,
          map= function(k,v) keyval(v$user,v),
          reduce=function(k,v){
            m = merge(v,v,by="user")
            keyval(m$item.x,m)
          })
step1RAM <- from.dfs(step1hdfs)


### mapreduce job2: pref.x*pref.y and create the module of item.x
step2hdfs <- file.path(dataPathHdfs,"step2")
mapreduce(input=step1hdfs, output=step2hdfs,
          map=function(k,v) keyval(k,v), 
          reduce=function(k,v) {
            val<-ddply(v,c("item.x","item.y"), function(x) sum(x$pref.x*x$pref.y))
            val$module.x <- sqrt(as.numeric(val[val$item.x==val$item.y,3]))
            names(val)[3] <- "inProd"
            keyval(val$item.y,val)})
step2RAM <- from.dfs(step2hdfs)


### mapreduce job 3: 
step3hdfs <- file.path(dataPathHdfs,"step3")
step3hdfs <- mapreduce(input=step2hdfs, output=step3hdfs, 
  map=function(k,v) keyval(k,v), 
  reduce=function(k,v){
    val <- v
    val$module.y <- sqrt(as.numeric(val[val$item.x==val$item.y,3]))
    keyval(k,val)
    })
step3RAM <- from.dfs(step3hdfs)




### mapreduce job 4: 
step4hdfs <- file.path(dataPathHdfs,"step4")
mapreduce(input=step3hdfs, output=step4hdfs, 
  map=function(k,v){
    val <- v
    val$sim <- val$inProd/val$module.x/val$module.y
    keyval(val$item.y,val[,c("item.x","item.y","sim")])
    })
step4RAM <- from.dfs(step4hdfs)

### mapreduce job 5: 
step5hdfs <- file.path(dataPathHdfs,"step5")
equijoin(left.input=step4hdfs, right.input=step0hdfs,
         output=step5hdfs,
         map.left=function(k,v) keyval(k,v), 
         map.right=function(k,v) keyval(v$item,v), 
         outer=c("left"))
step5RAM <- from.dfs(step5hdfs)


### mapreduce job 6: 
step6hdfs <- file.path(dataPathHdfs,"step6")
mapreduce(input=step5hdfs, output=step6hdfs,
          map=function(k,v) keyval(v$user.r,v), 
          reduce=function(k,v){
            val=ddply(v,c('item.x.l','user.r'), function(x) sum(x$sim.l*x$pref.r))
            names(val) <- c("item","user","pref")
            keyval(val$user,val)
          })
step6RAM <- from.dfs(step6hdfs)








#########################
### Do it locally
#########################
### reshape the data into rating matrix
dataCF2 <- as.matrix(simple_triplet_matrix(i = dataCF$user, j = dataCF$item-100, v = dataCF$pref))
rownames(dataCF2) <- 1:5
colnames(dataCF2) <- 101:107

### Calculate the similarity matrix
tmp1 <- t(dataCF2)%*%dataCF2 
tmp2 <- diag(tmp1)
tmp2 <- tmp2%*%t(tmp2)
simMat <- round(tmp1/sqrt(tmp2),digits = 2)

### calculate the preference
prefMat <- simMat %*% t(dataCF2)
