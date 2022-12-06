SO <- read.csv('C:/Users/Dmitry/Desktop/НИР/Data/CSV/SO - DiscrStat.csv',sep = ';',encoding = 'UTF-8')
POD <- read.csv('C:/Users/Dmitry/Desktop/НИР/Data/CSV/PO - domestic -stat.csv',sep = ';',encoding = 'UTF-8')
POF <- read.csv('C:/Users/Dmitry/Desktop/НИР/Data/CSV/PO - foreign.csv',sep = ';',encoding = 'UTF-8')
ind <- read.csv('C:/Users/Dmitry/Desktop/НИР/Data/CSV/NACEREV2.csv',sep = ',')

##initial corrections
rearrange <- function(soind){
  n <- length(soind[,2])
  for( i in 1:(n-1)){
    index <- which(soind[,2] == max(soind[,2][c(i:n)]))
    if(soind[i,2] != soind[index,2]){
      t <- soind[i,]
      soind[i,] <- soind[index,]
      soind[index,] <- t
    }
  }
  return(soind[,1])
}

aggrbyind <- function(SO){
  k <- data.frame(unique(SO$industry))
  k <- aggregate(SO$ISH...Прямой..[SO$ISH...Прямой.. >= 10],
                 list(industry = SO$industry), FUN = sum)
  return(k)
}

indfun <- function(SO, ind){
  n <- length(which(is.na(as.numeric(ind$Code))))
  vec <- which(is.na(as.numeric(ind$Code)))
  dis <- ind$Description[vec]
  for(i in 1:(n-1)){
    SO$industry[as.numeric(SO$NACE.Rev..2.Основной.код..4.цифры.) >= as.numeric(ind$Code[vec[i] + 1]) & 
                  as.numeric(SO$NACE.Rev..2.Основной.код..4.цифры.) <= as.numeric(ind$Code[vec[i + 1] - 1])] <- dis[i]
  }
  SO$industry[as.numeric(SO$NACE.Rev..2.Основной.код..4.цифры.) >= as.numeric(ind$Code[vec[n] + 1])] <- dis[n]
  return(SO)
}

agrframe <- function(d){
  k <- data.frame(c(2020:2012))
  for(i in 0:(ncol(d)/9-1)) {
    k <- cbind(k,apply(d[which(as.logical(apply(!is.na(d[,c((i * 9 + 1):((i + 1 ) * 9))]),1,prod))),
                         c((i * 9 + 1):((i + 1 ) * 9))], 2, mean)) 
  }
  names(k) <- c("год",gsub(pattern = "2020",replacement = "" ,names(d)[seq(1,(ncol(d)-1),9)]))
  return(k)
}

SO[,c(1,6:286)] <- apply(SO[,c(1,6:286)],2,
                         function(x){
                           x <- gsub(',','.',x)
                           as.numeric(x)
                         })
POD[,c(1,6:286)] <- apply(POD[,c(1,6:286)],2,
                          function(x){
                            x <- gsub(',','.',x)
                            as.numeric(x)
                          })
POF[,c(1,6:286)] <- apply(POF[,c(1,6:286)],2,
                          function(x){
                            x <- gsub(',','.',x)
                            as.numeric(x)
                          })

nace <- function(x) ifelse(x %/% 1000 == 0,
                           paste0('0',as.character(x %/% 100)),
                           as.character(x %/% 100))

SO$NACE.Rev..2.Основной.код..4.цифры. <- nace(SO$NACE.Rev..2.Основной.код..4.цифры.)
POD$NACE.Rev..2.Основной.код..4.цифры. <- nace(POD$NACE.Rev..2.Основной.код..4.цифры.)
POF$NACE.Rev..2.Основной.код..4.цифры. <- nace(POF$NACE.Rev..2.Основной.код..4.цифры.)

so <- agrframe(SO[,c(8:286)])
pod <- agrframe(POD[,c(8:286)])
pof <- agrframe(POF[,c(8:286)])


#discrstat
library(ggplot2)
vis <- function(so,pod,pof,prt){
  ggplot() + 
    scale_color_manual(values = c('cadetblue4','tomato3','olivedrab'),
                       labels = c('State','Domestic', 'Foreign'),
                       name = "Type of owner") + 
    labs(title = paste0("Parameter: ",names(so)[prt]),
         x = 'Year',
         y = 'Value') +
    geom_line(mapping = aes(x = so[,1],y = so[,prt], color = 'cadetblue4'),size = 1) + 
    geom_line(mapping = aes(x = pod[,1],y = pod[,prt],color = 'tomato3'),size = 1) + 
    geom_line(mapping = aes(x = pof[,1],y = pof[,prt], color = 'olivedrab'),size = 1) +
    geom_point(mapping = aes(x = so[,1],y = so[,prt],color = 'cadetblue4'),size = 2) + 
    geom_point(mapping = aes(x = pod[,1],y = pod[,prt],color = 'tomato3'),size = 2) + 
    geom_point(mapping = aes(x = pof[,1],y = pof[,prt],color = 'olivedrab'),size = 2) +
    geom_text(aes(x = so[,1],y = so[,prt], label = round(so[,prt],2), color = 'cadetblue4'),
              vjust = -1) + 
    geom_text(aes(x = pod[,1],y = pod[,prt], label = round(pod[,prt],2), color = 'tomato3'),
              vjust = 1.5) + 
    geom_text(aes(x = pof[,1],y = pof[,prt], label = round(pof[,prt],2), color = 'olivedrab'),
              vjust = 1.5)
}



ggplot() + 
  scale_color_manual(values = c('cadetblue4','tomato3','olivedrab'),
                     labels = c('State','Domestic', 'Foreign'),
                     name = "Type of owner") + 
  labs(title = paste0("Parameter: ",names(so)[16]),
       x = 'Year',
       y = 'Value') +
  geom_line(mapping = aes(x = so[,1],y = so[,16], color = 'cadetblue4'),size = 1) + 
  geom_line(mapping = aes(x = pod[,1],y = pod[,16],color = 'tomato3'),size = 1) + 
  geom_line(mapping = aes(x = pof[,1],y = pof[,16], color = 'olivedrab'),size = 1) +
  geom_point(mapping = aes(x = so[,1],y = so[,16],color = 'cadetblue4'),size = 2) + 
  geom_point(mapping = aes(x = pod[,1],y = pod[,16],color = 'tomato3'),size = 2) + 
  geom_point(mapping = aes(x = pof[,1],y = pof[,16],color = 'olivedrab'),size = 2) +
  geom_text(aes(x = so[,1],y = so[,16], label = round(so[,16],2), color = 'cadetblue4'),
            vjust = -1) + 
  geom_text(aes(x = pod[,1],y = pod[,16], label = round(pod[,16],2), color = 'tomato3'),
            vjust = 1.5) + 
  geom_text(aes(x = pof[,1],y = pof[,16], label = round(pof[,16],2), color = 'olivedrab'),
            vjust = 1.5)

library(ggplot2)
i <- 2
g <- 0
i <- i + 1 
ggplot() + 
  scale_color_manual(values = c('cadetblue4','tomato3','olivedrab'),
                     labels = c('State','Domestic', 'Foreign'),
                     name = "Type of owner") + 
  labs(title = paste0("Parameter: ",names(so)[i]),
       x = 'Year',
       y = 'Value') +
  geom_line(mapping = aes(x = so[,1],y = so[,i], color = 'cadetblue4'),size = 1) + 
  geom_line(mapping = aes(x = pod[,1],y = pod[,i],color = 'tomato3'),size = 1) + 
  geom_line(mapping = aes(x = pof[,1],y = pof[,i], color = 'olivedrab'),size = 1) +
  geom_point(mapping = aes(x = so[,1],y = so[,i],color = 'cadetblue4'),size = 2) + 
  geom_point(mapping = aes(x = pod[,1],y = pod[,i],color = 'tomato3'),size = 2) + 
  geom_point(mapping = aes(x = pof[,1],y = pof[,i],color = 'olivedrab'),size = 2) +
  geom_text(aes(x = so[,1],y = so[,i], label = round(so[,i],g), color = 'cadetblue4'),
            vjust = -1) + 
  geom_text(aes(x = pod[,1],y = pod[,i], label = round(pod[,i],g), color = 'tomato3'),
            vjust = 1.5) + 
  geom_text(aes(x = pof[,1],y = pof[,i], label = round(pof[,i],g), color = 'olivedrab'),
            vjust = 1.5)


##industries 
SO$industry <- cbind(rep(NA,nrow(SO)))
POD$industry <- cbind(rep(NA,nrow(POD)))
POF$industry <- cbind(rep(NA,nrow(POF)))

SO <- indfun(SO,ind)
POD <- indfun(POD,ind)
POF <- indfun(POF,ind)

so <- aggregate(by = list(industry = SO$industry, 
                          ISH = SO$ISH...Прямой..),data = SO,subset = SO$ISH...Прямой.. >= 10)

## industrial divison


k <-aggrbyind(SO)
soind <- aggregate(SO[,c(8:287)],
                   list(industry = SO$industry), function(x) mean(x[!is.na(x)]), drop = T)
soind$order <- c(1:17)

#Histograms

SO <- rbind(SO,rbind(POD, POF))
rearrange(soind[,c(1,35)])

library(ggplot2)
i <- 2
i <- i + 1 
ggplot(mapping = aes(y = soind$industry[!is.na(soind[,i])])) + 
  geom_linerange(aes(xmin = 0, xmax = soind[,i][!is.na(soind[,i])]),
                 colour = "cadetblue4", size = length(soind[,i][!is.na(soind[,i])])) + 
  labs ( title =  names(soind)[i],
         y = "Industry",
         x = "Value") + 
  geom_vline(xintercept = 0, size = 1)
install.packages('forcats')
install.packages('dplyr')

library(dplyr)
library(forcats)

soind[,c(1,i)] %>% 
  ggplot(aes(reorder(industry,
                     soind[,i]), soind[])) +
  geom_col() +
  labs ( title =  names(soind)[i],
         y = "Industry",
         x = "Value")
  
