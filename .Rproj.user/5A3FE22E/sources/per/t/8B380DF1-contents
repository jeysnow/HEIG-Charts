
#importing----
HEIG_DATA <- read.csv("HEIG_Data.csv", header = TRUE, sep = ";", strip.white = TRUE)


#factoring----
HEIG_DATA$Maintainer_Type <-
  factor(HEIG_DATA$Maintainer_Type, levels = 
           c("Limited", "Corporate", "Association", "Special", "Foundation")
           )

HEIG_DATA$HEI_Type <-
  factor(HEIG_DATA$HEI_Type, levels = 
           c("University", "Uni_Center", "Faculty")
           )

HEIG_DATA$Rector_Appointment <- 
  factor(HEIG_DATA$Rector_Appointment, levels = 
           c("Universitocracy",  "Academicracy",  "Designatory-Colaborative",  "Designatory-Directive"))





HEIG_DATA[18:29]<- lapply(HEIG_DATA[18:29],
       factor, levels = c("Executive", "Board", "Both", "None")
              )


HEIG_DATA[6:17] <- sapply(HEIG_DATA[6:17], "as.logical")

HEIG_DATA$Maintainer_Profit <- sapply(HEIG_DATA$Maintainer_Type, FUN = function(x){
  if(x == "Limited" || x == "Corporate"){
    return("For-Profit")
    }
  else return("Nonprofit")
  }) %>% factor()

setDT(HEIG_DATA)

HEIG_DATA[,ID:= 1:length(HEIG_DATA$HEI_Name)] %>% 
  setkey(cols=ID)


#check for errors in factoring
if(length( which(is.na(HEIG_DATA)))>0){
  print(which(is.na(HEIG_DATA), arr.ind=TRUE))
  stop("The above cells in HEIG_DATA have NA values")
}

summary(HEIG_DATA)


#REP_Table----

#Subsetting the representation variables, as percentages of each category
REP_TABLE <- HEIG_DATA[,.(
  Student = mean(Repres_Student),
  Alumni = mean(Repres_Alumni),
  Academic = mean(Repres_Academic),
  Executive = mean(Repres_Exec),
  Staff = mean(Repres_Staff),
  Industry = mean(Repres_Industry),
  Union = mean(Repres_Union),
  Owner = mean(Repres_Owner),
  Government = mean(Repres_Government),
  Community = mean(Repres_Community)
),by= .(Maintainer_Type)] %>% 
  #Renaming the grouped category to be able to bind the next round
  setnames("Maintainer_Type","Institutional_Category")


#rbind adds the rows below the first table, granted they have the same col names
REP_TABLE <- rbind(REP_TABLE,
      HEIG_DATA[,.(
        Student = mean(Repres_Student),
        Alumni = mean(Repres_Alumni),
        Academic = mean(Repres_Academic),
        Executive = mean(Repres_Exec),
        Staff = mean(Repres_Staff),
        Industry = mean(Repres_Industry),
        Union = mean(Repres_Union),
        Owner = mean(Repres_Owner),
        Government = mean(Repres_Government),
        Community = mean(Repres_Community)
      ),by= .(HEI_Type)] %>% 
        #Renaming the grouped category to be able to bind the next round
        setnames("HEI_Type","Institutional_Category"))

REP_TABLE <- rbind(REP_TABLE,
      HEIG_DATA[,.(
        Student = mean(Repres_Student),
        Alumni = mean(Repres_Alumni),
        Academic = mean(Repres_Academic),
        Executive = mean(Repres_Exec),
        Staff = mean(Repres_Staff),
        Industry = mean(Repres_Industry),
        Union = mean(Repres_Union),
        Owner = mean(Repres_Owner),
        Government = mean(Repres_Government),
        Community = mean(Repres_Community)
      ),by= .(Maintainer_Profit)] %>% 
        #Renaming the grouped category to be able to bind the next round
        setnames("Maintainer_Profit","Institutional_Category"))

REP_TABLE <- rbind(REP_TABLE,
                   HEIG_DATA[,.(
                     Student = mean(Repres_Student),
                     Alumni = mean(Repres_Alumni),
                     Academic = mean(Repres_Academic),
                     Executive = mean(Repres_Exec),
                     Staff = mean(Repres_Staff),
                     Industry = mean(Repres_Industry),
                     Union = mean(Repres_Union),
                     Owner = mean(Repres_Owner),
                     Government = mean(Repres_Government),
                     Community = mean(Repres_Community)
                   ),by= .(Certif_Communitarian
                   )][, #excluding the logical variable for grouping
                      Certif_Communitarian:=NULL][1, #creating the category for binding, with the correct value
                                                  Institutional_Category := "Certif_Communitarian"][1,])


REP_TABLE <- rbind(REP_TABLE,
                   HEIG_DATA[,.(
                     Student = mean(Repres_Student),
                     Alumni = mean(Repres_Alumni),
                     Academic = mean(Repres_Academic),
                     Executive = mean(Repres_Exec),
                     Staff = mean(Repres_Staff),
                     Industry = mean(Repres_Industry),
                     Union = mean(Repres_Union),
                     Owner = mean(Repres_Owner),
                     Government = mean(Repres_Government),
                     Community = mean(Repres_Community)
                   ),by= .(Certif_Relig
                   )][, #excluding the logical variable for grouping
                      Certif_Relig:=NULL][1, #creating the category for binding, with the correct value
                                              Institutional_Category := "Certif_Relig"][1,])

#transforms table into long format for ease of charting
REP_TABLE <- melt(REP_TABLE, 
     id.vars = "Institutional_Category",
     measure.vars = c(2:11),
     variable.name = "Stakeholder",
     value.name = "Repres_Freq"
       )

# Responsibilities----

#data.table function is necessary to create a copy, not just a reference.
RESPONSIBILITY_TABLE <- data.table(HEIG_DATA)


#Get rid of the unwanted columns
RESPONSIBILITY_TABLE[,8:17 := NULL][,ID:= 1:length(RESPONSIBILITY_TABLE$HEI_Name)][,]


#adjust the names for the model matrix operation
RESPONSIBILITY_TABLE[,8:19] %>%
  names() %>%
  sub(pattern = "(.*)", replacement = "\\1_") %>% 
  setnames(x=RESPONSIBILITY_TABLE,old = 8:19)

## separating the factors into columns----

RESPONSIBILITY_TABLE <- as.data.table(model.matrix(~Plan_Academic_ -1,
                                   data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)] %>% 
  merge(by= "ID", y= as.data.table(model.matrix(
    ~Plan_Financial_ -1,data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)])  %>% 
  merge(by= "ID", y= 
          as.data.table(model.matrix(
            ~Plan_Org_ -1,data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Plan_Staff_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Decide_Academic_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Decide_Financial_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Decide_Org_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Decide_Staff_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Supervise_Academic_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Supervise_Financial_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Supervise_Org_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  merge(by= "ID", y=
          as.data.table(model.matrix(~Supervise_Staff_ -1,
                       data = RESPONSIBILITY_TABLE))[,ID:= 1:length(RESPONSIBILITY_TABLE$ID)]) %>% 
  #Finnally, merge it all back to responsibility_table 
  merge(by="ID", x= RESPONSIBILITY_TABLE)

#excluding old columns, note that ID became col #1, thus the numbers shift by 1
RESPONSIBILITY_TABLE[,9:20:=NULL] 


## Converting the numbers----

#Converting the numbers to means by institutional category
RESPONSIBILITY_TABLE <- RESPONSIBILITY_TABLE[,lapply(.SD,mean), .SDcols = 10:length(RESPONSIBILITY_TABLE),
                     by = Maintainer_Type] %>% 
  setnames("Maintainer_Type","Institutional_Category") %>% 
  rbind(
    RESPONSIBILITY_TABLE[,lapply(.SD,mean), .SDcols = 10:length(RESPONSIBILITY_TABLE),
                         by = HEI_Type] %>% 
      setnames("HEI_Type","Institutional_Category")
  ) %>% 
  rbind(
    RESPONSIBILITY_TABLE[,lapply(.SD,mean), .SDcols = 10:length(RESPONSIBILITY_TABLE),
                         by = Maintainer_Profit] %>% 
      setnames("Maintainer_Profit","Institutional_Category")
  ) %>% 
  rbind(
    RESPONSIBILITY_TABLE[,lapply(.SD,mean), .SDcols = 10:length(RESPONSIBILITY_TABLE),
                         by = Certif_Communitarian][,Certif_Communitarian:=NULL][,Institutional_Category := "Certif_Communitarian"][1,]) %>% 
      rbind(
    RESPONSIBILITY_TABLE[,lapply(.SD,mean), .SDcols = 10:length(RESPONSIBILITY_TABLE),
                         by = Certif_Relig] [,Certif_Relig:=NULL][,Institutional_Category := "Certif_Relig"][1,])


# Selecting governance step combined with governance theme allows me to isolate 
#the responsible party,l and then rename the levels based on the order of appearance of the columns

RESPONSIBILITY_TABLE <- melt(RESPONSIBILITY_TABLE,
     id.vars = "Institutional_Category",
     measure.vars = patterns(names(HEIG_DATA[,18:29])),
     variable.name = "Responsible_party",
     value.name = names(HEIG_DATA[,18:29]),
     variable.factor = TRUE)


levels(RESPONSIBILITY_TABLE$Responsible_party) <- c("Executive", "Board", "Both", "None")  

#selecting the patterns allow the grouping of governance themes.
RESPONSIBILITY_TABLE <- melt(RESPONSIBILITY_TABLE,
     id.vars = c("Institutional_Category","Responsible_party"),
     measure.vars = patterns("Plan", "Decide", "Supervise"),
     variable.name = "Governance_Theme",
     value.name = c("Plan", "Decide", "Supervise"),
     variable.factor = TRUE)

levels(RESPONSIBILITY_TABLE$Governance_Theme) <- c("Academic", "Financial", "Organizational", "Staff")

# The levels here worked automatically because there was only one column name for each measure.vars
RESPONSIBILITY_TABLE <- melt(RESPONSIBILITY_TABLE,
     id.vars = c("Institutional_Category","Responsible_party", "Governance_Theme"),
     measure.vars = c("Plan", "Decide", "Supervise"),
     variable.name = "Governance_Role",
     value.name = "Bylaw_Freq",
     variable.factor = TRUE)