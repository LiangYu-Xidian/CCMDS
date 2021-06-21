source('getMatrix.R')
source('detect.R')
library(janitor)
library(pROC)
library(rpart)

kirp = cbind(1,getMatrix("kirp.maf")[,151:246])
lihc = cbind(2,getMatrix("lihc.maf")[,151:246])
luad = cbind(3,getMatrix("luad.maf")[,151:246])
prad = cbind(4,getMatrix("prad.maf")[,151:246])
lusc = cbind(5,getMatrix("lusc.maf")[,151:246])
ucec = cbind(6,getMatrix("ucec.maf")[,151:246])
thca = cbind(7,getMatrix("thca.maf")[,151:246])
read = cbind(8,getMatrix("read.maf")[,151:246])
hnsc = cbind(9,getMatrix("hnsc.maf")[,151:246])
blca = cbind(10,getMatrix("blca.maf")[,151:246])
laml = cbind(11,getMatrix("laml.maf")[,151:246])
paad = cbind(12,getMatrix("paad.maf")[,151:246])

# kirp = cbind(1,getMatrix("kirp.maf"))
# lihc = cbind(2,getMatrix("lihc.maf"))
# luad = cbind(3,getMatrix("luad.maf"))
# prad = cbind(4,getMatrix("prad.maf"))
# lusc = cbind(5,getMatrix("lusc.maf"))
# ucec = cbind(6,getMatrix("ucec.maf"))
# thca = cbind(7,getMatrix("thca.maf"))
# read = cbind(8,getMatrix("read.maf"))
# hnsc = cbind(9,getMatrix("hnsc.maf"))
# blca = cbind(10,getMatrix("blca.maf"))
# laml = cbind(11,getMatrix("laml.maf"))
# paad = cbind(12,getMatrix("paad.maf"))

cancer = rbind(lihc,kirp,luad,prad,lusc,ucec,thca,read,hnsc,blca,laml,paad)


train_sub = sample(nrow(cancer),8/10*nrow(cancer))
train_data = data.frame(cancer[train_sub,])
train_data = clean_names(train_data)
test_data = data.frame(cancer[-train_sub,])
test_data = clean_names(test_data)


train_data[,1] <- factor(train_data[,1])
test<-rpart(v1~.,data=train_data)

pre_ran <- predict(test,newdata=test_data)


ans = cbind(pre_ran,test_data[,1])
cnt = 0
for(i in 1 : nrow(ans)){
  temp = 0
  ind = 0
  for(j in 1:12){
    if(ans[i,j] > temp){
      temp = pre_ran[i,j]
      ind = j
    }
  }
  if(ind == ans[i,13])
    cnt = cnt + 1
}
acc = cnt/nrow(ans)
