#计算欧式距离
eud <-function(x,y){
    sqrt(sum((x-y)^2))
}#eud

#在矩阵m中查找与p距离小于等于eps距离的所有点的索引
region<- function(m,p,eps){
r<-c()
i<-1
while(i <= nrow(m)){
if(eud(m[i,],p) <= eps)
r<-c(r,i)
i<-i+1
}
return(r)
}#region

#DBSCAN聚类
dbscan <- function(m,minPts,eps){
  #检查参数
  if(class(m) != "matrix")
    stop("argument 1 require matrix")
  if(class(minPts) != "numeric" || length(minPts) != 1)
    stop("argument 2 require scale numeric")
  if(class(eps) != "numeric" || length(eps) != 1)
    stop("argument 3 require scale numeric")

  ms<-matrix(nrow=nrow(m),ncol=3) #标记矩阵
  colnames(ms) <- c("label","access","cluster") #label::=(noise,core,border),access::=(TRUE,FALSE),cluster::=(i)
  ms[,"label"] <- "noise" #默认标记为噪声
  ms[,"access"] <- FALSE #访问标记
  ms[,"cluster"] <- "0" #默认为簇0,即噪声
  clusterCount<-1 #从第一个簇开始

  #遍历每个点
  repeat{
    unaccess <- which(ms[,"access"] == FALSE) #查找未访问的点
    if(length(unaccess) <= 0 ) 
      break

    firstp <- unaccess[1] #第一个未访问的点
    ms[firstp,"access"]<- TRUE #标记为已访问
    reg<-region(m,m[firstp,],eps) #计算在eps范围内的点，包括自身
    if(length(reg) >= minPts){ # 核心点
      ms[firstp,"label"] <- "core"
      ms[firstp,"cluster"] <-clusterCount #属于第clusterCount个簇

      for(i in reg){ #扩展密度可达区域
        if(ms[i,"access"] == FALSE) {#未访问
	  ms[i,"access"] <- TRUE
          newreg<-region(m,m[i,],eps)
          if(length(newreg) >= minPts) {#核心点,间接密度可达
	    reg<-union(reg,newreg) #合并连通区域  
	  }#if
	}
	if(ms[i,"cluster"] == 0) #之前为噪声
	  ms[i,"cluster"] <- clusterCount
      }#for
      clusterCount<-clusterCount +1 #下一个簇
    } #可能为噪声或者边界点，不用处理
  }#repeat
data.frame(m,ms[,c("label","cluster")])
}#descan

#删除m中t指定的列
delcol <- function(m,t)
  m[,names(m)!= t]

#对绘制m
showp<- function(m,minPts,eps=mean(m)){
  sq<-seq(0,2*pi,length=1000) #用于画圆
  colorSet<-colors()
  colorIndex<-1
  sm<- delcol(delcol(m,"cluster"),"label")
  f<-unique(m[,"cluster"]) #将簇转化为因子
  plot(c(max(abs(sm)),-max(abs(sm))),c(max(abs(sm)),-max(abs(sm))),type="n",xlab="x",ylab="y",main="DBSACN") #绘制框架
  grid(col="black")
  for(i in f){
    colorIndex<-colorIndex +30
    curp<-which(m[,"cluster"] == i)
    corep<-intersect(which(m[,"label"]== "core") ,curp)#提取核心点
    points(sm[curp,],col=colorSet[colorIndex],pch=i ) #绘制所有点
         
    for(k in corep){
      points(cos(sq)*eps+sm[k,1],sin(sq)*eps+sm[k,2],pch=".",col=colorSet[colorIndex])
    }
  }

}#showp
  
#
pdbscan <-function(m,minPts,eps)
  showp(dbscan(m,minPts=minPts,eps=eps),minPts,eps)