install.packages("stringdist")
library(stringdist)

################# What is the qrams ##########################
x <- "I will not buy this record, it is scratched"
y <- "My hovercraft is full of eels"
z <- c("this", "is", "a", "dead","parrot")
qgrams(A = x, B = y, C = z,q=2)

x <- "peter piper picked a peck of pickled peppers"
qgrams(x, q=2) 
qgrams(x, q=2, useNames=FALSE) 
qgrams(x, q=2, useBytes=TRUE)
qgrams(x, q=2, useBytes=TRUE, useNames=TRUE)

################# #####################

Dist_Check <- function(a, b){
    a <- tolower(a)
    b <- tolower(b)
    M <- data.frame(
    m = c("osa", "lv", "dl", "lcs", "qgram", "qgram", "qgram",
          "cosine", "cosine", "cosine", "jaccard", "jaccard", "jaccard",
          "jw", "jw", "jw"), 
    q = c(0,0,0,0,1,2,3,1,2,3,1,2,3,0,0,0), 
    p = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.1,0.2)
  )
  output <- as.vector(apply(M,1,function(m) stringdistmatrix(a, b, method = m["m"], q = m["q"], p = m["p"])))
  return(data.frame(method = M["m"], qgram = M["q"], penalty = M["p"], original = rep(a, 16), comparison = rep(b, 16), distance_prob = output))
}

Dist_Check("Highway", "Hwy")
Dist_Check("Street", "ST")
Dist_Check("Drive", "Dr")
Dist_Check("ROAD", "Rd")
Dist_Check("Avenue", "AVE")
Dist_Check("Highway", "Hihgwya")
Dist_Check("Highway", "Highway")

Test_address <- as.character(unique(OR_Roster$Line1Address))

# Remove the punctuation/remove digit/change it to lower case
Cleaned_address <- as.character(unique(apply(as.matrix(Test_address),1, function(x) tolower(gsub("[[:digit:]]|[[:punct:]]","",x, ignore.case = TRUE)))))

JW_dist <- stringdistmatrix(Cleaned_address, Cleaned_address, method = "cosine", q = 1)

Quality <- function(x, th){
  hist(JW_dist[x,])
  return(data.frame(original = Cleaned_address[x], comparison = Cleaned_address[which(JW_dist[x,]<= th)]))
}


a <- Cleaned_address[which(JW_dist[25,]<= 0.05)]
b <- Cleaned_address[25]
Dist_Check(b, a[3])
