get_cluster_counts<-function(cluster.counts.long.filepath,cluster.annotations.filepath=NULL){
  batch<-cluster<-visit<-cell.type<-NULL
  ##read in as a data.table; set factors
  cluster.counts <- data.table::fread(cluster.counts.long.filepath,stringsAsFactors = T)
  ##factor batch and cluster
  cluster.counts[,batch := factor(sprintf("%03d",batch))]
  cluster.counts[,cluster := factor(cluster,levels=sort(unique(cluster)))]
  ##relevel visit
  cluster.counts[,visit := factor(visit,levels=c(paste0("V",c(4,6,7)),"Adult"),labels = c("Birth","6-months","12-months","Adult"))]
  cluster.counts[,visit.alias := factor(visit,levels=c(paste0("V",c(4,6,7)),"Adult"),labels = c("Birth","6-months","12-months","Adult"))]
  ##
  cell.types<-split(cluster.counts,by='cell.type')
  cell.types<-lapply(cell.types,function(i){
    lhs<-names(which(!sapply(i[,!'cluster'],is.numeric)))
    value.vars<-names(which(sapply(i[,!'cluster'],is.numeric)))
    value.vars.list<-sapply(value.vars,function(v){
      data.table::dcast.data.table(i,stats::as.formula(paste(paste0(lhs,collapse="+"),"~","cluster")),value.var = v)
    },simplify = F)
  })
  ##add annotations
  if(!is.null(cluster.annotations.filepath)){
    cluster.annotations<-data.table::setDT(readxl::read_xlsx(cluster.annotations.filepath))
    cluster.annotations[,cluster := factor(cluster,levels=sort(unique(cluster)))]
    cluster.annotations[,cell.type := factor(cell.type,levels=as.character(levels(cluster.counts$cell.type)))]
    ##merge
    cluster.counts<-data.table::merge.data.table(cluster.counts,cluster.annotations,by=c('cell.type','cluster'),all=T)
  }
  ##
  return(
    list(
      long=cluster.counts,
      wide=cell.types
    )
  )
}
