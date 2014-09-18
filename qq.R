file=paste("/home/oracle/","qq.txt",sep="")

file.data<-scan(file,what='',sep='\n',encoding="UTF-8")

#file.data[1:12]

data<-data.frame(user.name=c(),time=c(),text=c())

time<-c()

user.name<-c()

text<-c()

for(i in 7:length(file.data))
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

summary(split.time$V1)

#合并每个用户的所有留言

user.name<-unique(data$user.name)

text<-c()

text.num<-c()

for(i in 1:length(user.name))
{
  text.i<-data$text[which(data$user.name==user.name[i])] #用户i的所有留言向量
  
  text.i.num<-length(text.i) #用户i发几次留言
  
  for(j in 1:text.i.num)
  {
    text[i]<-paste(text[i],text.i[j],sep=" ") #把所有的留言向量合并成一个元素
  }
  
  text.num[i]<-text.i.num
}

user.text<-data.frame(user.name=user.name,text=text,text.num=text.num)

user.text$user.name<-as.character(user.text$user.name)

user.text$text<-as.character(user.text$text)

library(rJava)

#install.packages("Rwordseg", repos = "http://R-Forge.R-project.org")

library(Rwordseg)

rwordseg<-segmentCN(user.text$text,nature=T)

rwordseg.n<-rwordseg

for(i in 1:length(rwordseg))
{
  rwordseg.n[[i]]<-rwordseg[[i]] [which(names(rwordseg[[i]])=="n")]  #？
}

#常用话题词云

library(tm)

ovid<-Corpus(VectorSource(rwordseg.n))

inspect(ovid)

ovid<-tm_map(ovid,FUN=removeWords,c("图片","表情","圖片")) #删除qq系统词

inspect(ovid)

dtm<-DocumentTermMatrix(ovid)

#findFreqTerms(dtm, 5)

#findAssocs(dtm,"卡介苗",0.7)

tdm<-TermDocumentMatrix(ovid)

#qq.matrix<-as.matrix(dtm)

#qq.freq<-apply(qq.matrix,2,sum)

#qq.freq.top<-rev(sort(qq.freq))[1:30]

#plot(qq.freq)

#text(c(1:length(qq.freq)),qq.freq,names(qq.freq))

tdm_matrix <- as.matrix(tdm)

v <- sort(rowSums(tdm_matrix),decreasing=T)

data <- data.frame(word=names(v),freq=v)

library(Rcpp)

library(RColorBrewer)

library(wordcloud)

pal2 <- brewer.pal(8,"Dark2")

#png("wordcloud_packages.png", width=1280,height=800)  #词云

#wc<-wordcloud(data$word,data$freq,min.freq=30,max.words=Inf, random.order=FALSE, rot.per=.45, colors=pal2)

wc<-wordcloud(data$word,data$freq)

#用户-词项网络关系图

from<-c()

to<-c()

for(i in 1:length(user.text$user.name))
{
  from<-c(from,rep(user.text$user.name[i],length(rwordseg.n[[i]])))
  to<-c(to,rwordseg.n[[i]])
}

from[which(from=="")]<-"数据集内务用户名"

#install.packages("igraph")

library(igraph)

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

degree=200

indexword<-(degree(g.dir,mode="in")>=degree)

indexuser<-(degree(g.dir,mode="out")>=degree)

words<-degree(g.dir,mode='in')[indexword]

user<-degree(g.dir,mode="out")[indexuser]

wordname<-V(g.dir)[indexword]$label

username<-V(g.dir)[indexuser]$label

label=NA

label[indexword]<-wordname

label[indexuser]<-username

svg("userwords.svg",width=40,height=40)

plot(g.dir,layout=layout.fruchterman.reingold,vertex.size=1,vertex.label=label,vertex.label.family="GB1")

dev.off()

#重点词项网络图

std.degree.words=500

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

std.degree.user=6

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