PROJHOME <- "/home/rstudio/R/clustering"
csvdir <- "~/daniel/data"

setwd(csvdir)
rawschoolfunding <- read.csv("FY16 Data for Tableau1.csv")
setwd(PROJHOME)

library(cluster)
#########clean#########
remove.accounting.symbols <- function(str1){
  char <- gsub("%", "", str1)
  char <- gsub("[$]", "", char)
  char <- gsub("-", "0", char)
  char <- gsub(",", "", char)
  num <- as.numeric(char)
  return(num)
}

remove.accounting.from.df.vars <- function(df, li.vars){
  for(var in li.vars){
    df[[var]] <- remove.accounting.symbols(df[[var]])
  }
  return(df)
}

largeschools <- c("20K490", "31R440", "28Q505", "26Q495")



colnames(rawschoolfunding) <- c(colnames(rawschoolfunding)[1:3], "FSF", "numstudents", "pctIEP", 
                                "salary", "titleone", "AIDP", "prioritydollars", colnames(rawschoolfunding)[11:16]) 
rawschoolfunding <- rawschoolfunding[-c(71:73),]

rawschoolfunding <- remove.accounting.from.df.vars(rawschoolfunding, c(4:7, 9:16))
rawschoolfunding$Portfolio <- factor(rawschoolfunding$Portfolio)
rawschoolfunding$titleone <- factor(rawschoolfunding$titleone)
rawschoolfunding$is_title_one <- logical(length(rawschoolfunding$titleone))
rawschoolfunding[rawschoolfunding$titleone == "Non Title I",]$is_title_one <- FALSE
rawschoolfunding[rawschoolfunding$titleone == "Title I",]$is_title_one <- TRUE
rawschoolfunding$titleone <- NULL
#########create#########
fn.create.is.zero <- function(df, li.vars){
  colnms <- colnames(df)[li.vars]
  for(colm in colnms){
    newcolm <- paste0("is_", colm, "_zero")
    df[[newcolm]] <- df[[colm]]==0
  }
  return(df)
}
rawschoolfunding <- fn.create.is.zero(rawschoolfunding, c(8:15))
rawschoolfunding$isSIGall_zero <- rawschoolfunding$is_SIG...Campus_zero & rawschoolfunding$is_SIG_zero
rawschoolfunding$isTLall_zero <- rawschoolfunding$is_TL.Safe.School_zero & rawschoolfunding$is_TL.Transitional.Relief_zero

#########impute#########
rawschoolfunding[is.na(rawschoolfunding$salary),]$salary <- median(rawschoolfunding$salary)

#########remove schools from clustering#########
rawschoolfunding$is_largeSchool <- ifelse(rawschoolfunding$dbn %in% largeschools, T, F )
rawschoolfunding$is_CTE_or_T <- ifelse(rawschoolfunding$Portfolio %in% c("CTE", "T"), T, F) 
rawschoolfunding$is_sixtwelve <- ifelse(rawschoolfunding$Portfolio %in% c("6-12"), T, F) 

schoolfunding <- rawschoolfunding[rawschoolfunding$is_largeSchool ==F & rawschoolfunding$is_CTE_or_T ==F & rawschoolfunding$is_sixtwelve ==F, ]

#########gower#########
g.dist = daisy(schoolfunding[3:length(schoolfunding)], metric="gower" ) 

source("dendrogram.R")
#########plot#########
windows(height=3.5, width=12)
layout(matrix(1:3, nrow=3))
#layout(matrix(1:3, nrow=1))
RIGHTMARGIN <- 30
par(mar = c(3,3,3,RIGHTMARGIN))
plot(dend.s, horiz =  TRUE)
par(mar = c(3,3,3,RIGHTMARGIN))
plot(dend.m, horiz =  TRUE)
par(mar = c(3,3,3,RIGHTMARGIN))
plot(dend.c, horiz =  TRUE)










par(mar = c(3,3,3,RIGHTMARGIN))
plot(dend, 
     main = "schools", 
     horiz =  TRUE,  nodePar = list(cex = .007))
