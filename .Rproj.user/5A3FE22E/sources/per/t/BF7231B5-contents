

temp <- data.table(ID = RESPONSIBILITY_TABLE$ID)
z <- 1
for (i in 1:2) {
  merge( x = data.table(i = 1:20, ID = 1:20),
         y = temp, by = "ID") 
}





for (i in 8:length(RESPONSIBILITY_TABLE)-2) {
  as.data.table(model.matrix(
    ~get(names(RESPONSIBILITY_TABLE)[i]) -1,data = RESPONSIBILITY_TABLE)
    )[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)] %>% 
    merge(by= "ID", y= temp)
}

x <- 8
y <- "Plan_Academic_"

model.matrix( ~ get(names(RESPONSIBILITY_TABLE)[x]) -1,
              data = RESPONSIBILITY_TABLE)



?model.matrix()

?as.col_spec()


RESPONSIBILITY_TABLE[,(names(RESPONSIBILITY_TABLE)[x]), with=F]

as.name(names(RESPONSIBILITY_TABLE)[x])

paste(names(RESPONSIBILITY_TABLE)[x])
