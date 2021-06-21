timeBegin = proc.time()
dna1_b = read.csv(file = "out.fasta", header = FALSE, sep = '\t', stringsAsFactors = FALSE, comment.char = "#")
dna = c()
count = 0
for (i in 1 : length(dna1_b[[1]])){
  if(i %% 814 == 1){
    count = count + 1
    dna[count] = ""
    next
  }
  dna[count] = paste0(dna[count] , dna1_b[[1]][i])
}
base = c("a","t","c","g")
tag = c()
for(i in 1 : length(base)){
  for(j in 1 : length(base)){
    for(k in 1 : length(base)){

          temp = paste0(base[i] , base[j] , base[k])
          tag = append(tag , temp)

    }
  }
}
tag = unique(tag)
num = matrix(0 ,nrow = length(dna) ,ncol = length(tag) ,dimnames = list(1:length(dna) , tag))
for (i in 1 : length(dna)){
  print(i)
  temp = strsplit(dna[i],"")[[1]]
  for(j in 1 : (length(temp) - 2)){
    if(j %% 3 == 1){
      if(!temp[j] %in% base){
        temp[j] = sample(base,1)
      }
      cha = temp[j]
      for(k in 1 : 2){
        
        if(!temp[j+k] %in% base){
          temp[j+k] = sample(base,1)
        }
        cha = paste0(cha , temp[j + k])
      }
      
      
      num[i, cha] = num[i, cha] +1
    }
    
  }
  
}
# kmer = unique(kmers)


timeEnd = proc.time()
timeRun = timeEnd - timeBegin
print(paste0('：',timeRun[3][[1]],'秒'))

e = 0
dis = c()
max= 0
min = 12321323323
for (i in 1 : length(dna)){
  temp = dist(rbind(num[1,] , num[i,]),"euclidean")
  if(i <= 525){
    if(temp >= 400)
      e  = e + 1
    if(temp > max)
      max = temp
  }
  else{
    if(temp < 400)
      e  = e + 1
    if(temp < min)
      min = temp
  }
  dis = c(dis , temp)
}
timeCacu = proc.time()
timeRun2 = timeCacu - timeEnd
print(paste0('：',timeRun2[3][[1]],'秒'))