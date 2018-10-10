# Knn-imputering av numeriska och kategoriska värden

library(VIM)
library(magrittr)
df <- matrix(rnorm(1000), ncol = 10) %>% as.data.frame(., stringsAsFactors =F)

df[df>1] <- NA
djur <- rep(c("katt", "hund", "giraff", "frenchie", ""), 20)
df <- cbind(djur,df) 
df$djur[df$djur==""] <- NA
df2 <- kNN(df, imp_var = F, impNA = T)
#kateoriska variabler som ska imputeras måste vara som fctor och ha NA i sig.

df2[1:10,1:2]
