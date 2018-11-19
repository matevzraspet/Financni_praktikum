library(readr)

umrl2 <- read_delim("umrl2.csv",  ";",
                    
                    locale = locale(encoding = "Windows-1250", decimal_mark = "."))

colnames(umrl2) <- c("starost","1","2")  ### Preimenoval moški-1, ženske- 2
umrl2 <- umrl2[-1,] 
umrljivost <- melt(umrl2, id.vars ="starost", variable.name = "spol", value.name = "stevilo") %>% mutate(starost = parse_number(starost))
View(umrljivost)



library(rgl)
persp3d(umrljivost[,1], umrljivost[,2], umrljivost[,3], col="skyblue")

