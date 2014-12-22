library(gmodels)
library(rJava)
library(tm)
library(ggplot2)
library(Rcpp)
library(RColorBrewer)
library(wordcloud)
#install.packages("Rwordseg", repos = "http://R-Forge.R-project.org")
library(Rwordseg)
library(igraph)
library(scales)
library(plyr)

file=paste("qq.txt",sep="")
file.data<-scan(file,what='',sep='\n',encoding="UTF-8")
#file.data[1:12]
data<-data.frame(user.name=c(),time=c(),text=c())
time<-c()
user.name<-c()
text<-c()
for(i in 9:length(file.data))
{
  reg.time<-regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]+:[0-9]+:[0-9]+",file.data[i]) #日期时间正则表达式
  if(reg.time==1) #遍历有效的行数据
  {
    data<-rbind(data,data.frame(time=time,user.name=user.name,text=text))
    text<-c()
    begin<-reg.time
    end<-reg.time+attr(reg.time,"match.length")-1
    time<-substr(file.data[i],begin,end)#读取时间信息
    begin<-as.numeric(reg.time+attr(reg.time,"match.length")+1)
    end<-nchar(file.data[i])
    user.name<-substr(file.data[i],begin,end)
  }
  else
  {
    text<-paste(text,file.data[i])
  }
}
data$text<-as.character(data$text)
data$user.name<-as.character(data$user.name)

#发言时间
split.datetime<-as.data.frame(matrix(unlist(strsplit(as.character(data$time),' ')),ncol=2,byrow=T))
split.time<-as.data.frame(matrix(unlist(strsplit(as.character(split.datetime$V2),':')),ncol=3,byrow=T))
temp <- CrossTable(split.time$V1)
df <-as.data.frame(t(as.data.frame(rbind(dimnames(temp$t)[[2]],temp$t)))) #利用dimnames获得不同时间发言条数
df$V1 <- as.numeric(as.character(df$V1))
df$V2 <- as.numeric(as.character(df$V2))
df <- df[order(df$V1),]
V1 <- c(0:23)
target <- data.frame(V1)
result <- merge(x = target, y = df, by = "V1", all.x=TRUE) #左联
result$V2[is.na(result$V2)] <- 0
result$V1 <- paste(result$V1,":00")
result$V1 <- strptime(result$V1,format="%H")
ggplot(result,aes(V1,V2))+geom_line()+theme_bw()+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +theme(axis.line = element_line(color = 'black'))+scale_y_continuous(breaks=seq(0,450,50))+labs(x='时间 Time', y='消息数量（条）Number of News')+scale_x_datetime(breaks = "3 hour",labels = date_format("%H:%M"))+coord_equal(148)
#+xlim(0,23)

#生成频数表
result2 <- result
result2$V1 <- substr(result2$V1,11,16)
result2 <- rename(result2,c("V1"="Time","V2"="Freq"))

#常用话题词云
ovid<-Corpus(VectorSource(rwordseg.n))
inspect(ovid)
ovid<-tm_map(ovid,FUN=removeWords,c("图片","表情","圖片")) #删除qq系统词
inspect(ovid)
dtm<-DocumentTermMatrix(ovid)
#findFreqTerms(dtm, 5)
#findAssocs(dtm,"流行病",0.5)
tdm<-TermDocumentMatrix(ovid)
#qq.matrix<-as.matrix(dtm)
#qq.freq<-apply(qq.matrix,2,sum)
#qq.freq.top<-rev(sort(qq.freq))[1:30]
#plot(qq.freq)
#text(c(1:length(qq.freq)),qq.freq,names(qq.freq))
tdm_matrix <- as.matrix(tdm)
v <- sort(rowSums(tdm_matrix),decreasing=T)
data <- data.frame(word=names(v),freq=v)
pal2 <- brewer.pal(8,"Dark2")
#png("wordcloud_packages.png", width=1280,height=800)  #词云
#wc<-wordcloud(data$word,data$freq,min.freq=2,max.words=Inf, random.order=FALSE, rot.per=.45, colors=pal2)
wc<-wordcloud(data$word,data$freq,min.freq=2,max.words=Inf, random.order=FALSE, rot.per=.45, colors=pal2)

from<-c()
to<-c()
for(i in 1:length(user.text$user.name))
{
  from<-c(from,rep(user.text$user.name[i],length(rwordseg.n[[i]])))
  to<-c(to,rwordseg.n[[i]])
}
from[which(from=="")]<-"数据集内务用户名"
init.igraph<-function(data,dir=F){
  labels<-union(unique(data[,1]),unique(data[,2]))  
  ids<-1:length(labels)
  names(ids)<-labels
  g<-graph.empty(directed=dir)#建立空igraph对象
  g<-add.vertices(g,length(labels))#添加点
  V(g)$label=labels   
  from<-as.character(data[,1]);to<-as.character(data[,2])
  edges<-matrix(c(ids[from],ids[to]),nc=2)
  g<-add.edges(g,t(edges))#添加线
  g
}
g.dir<-init.igraph(data.frame(from=from,to=to),T)
#degree=50

#重点词项网络图
std.degree.words=50 #点的入度大于50，则为重点词条
words.index<-(degree(g.dir,mode="in")>=std.degree.words)
words<-degree(g.dir,mode="in")[words.index]
names(words)<-V(g.dir)[words.index]$label
label=NA
label[words.index]<-names(words)
V(g.dir)$size=1
max.d<-max(words)
min.d<-min(words)
V(g.dir)[words.index]$size=2*(words-min.d)/(max.d-min.d)+2
V(g.dir)$color="white"
V(g.dir)[words.index]$color="red"
svg("words.svg",width=40,height=40)
#png("words.png", width=1280,height=800)
plot(g.dir,layout=layout.fruchterman.reingold,vertex.label=label,vertex.label.cex=V(g.dir)$size/3,vertex.color=V(g.dir)$color,vertex.label.family="GB1")
dev.off()

#重点用户网络图
std.degree.user=150 #点的出度大于150，则为重点词条
user.index<-(degree(g.dir,mode="out")>=std.degree.user)
user<-degree(g.dir,mode="out")[user.index]
names(user)<-V(g.dir)[user.index]$label
label=NA
label[user.index]<-names(user)
V(g.dir)$size=1 #一般词汇和用户的大小为1
max.d<-max(user)
min.d<-min(user)
V(g.dir)[user.index]$size=2*(user-min.d)/(max.d-min.d)+2 #关键点词汇的大小为2-4
V(g.dir)$color="white"
V(g.dir)[user.index]$color="green"
svg("user.svg",width=40,height=40)
plot(g.dir,layout=layout.fruchterman.reingold,vertex.label=label,vertex.label.cex=V(g.dir)$size/3,vertex.color=V(g.dir)$color,vertex.label.family="GB1")
dev.off()

#用户-词项网络关系图
indexword<-(degree(g.dir,mode="in")>=50)
indexuser<-(degree(g.dir,mode="out")>=150)
words<-degree(g.dir,mode='in')[indexword]
user<-degree(g.dir,mode="out")[indexuser]
wordname<-V(g.dir)[indexword]$label
username<-V(g.dir)[indexuser]$label
label=NA
label[indexword]<-wordname
label[indexuser]<-username
V(g.dir)$size=1
V(g.dir)[user.index]$color="green"
V(g.dir)[words.index]$color="red"
svg("userwords.svg",width=40,height=40)
plot(g.dir,layout=layout.fruchterman.reingold,vertex.size=1,vertex.label=label,vertex.label.family="GB1")
dev.off()
