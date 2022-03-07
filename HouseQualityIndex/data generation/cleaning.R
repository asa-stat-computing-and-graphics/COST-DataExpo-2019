library(readr)

data <- read_csv("data.csv")

data <- data[data$Type_of_Schedule>10,]

data$Year_Built=2000-(data$Year_Built-1)*10
data$year_exist=ifelse(data$Year-data$Year_Built>0,data$Year-data$Year_Built,0)/10

data$Rent[data$Rent>9000] <- NA

data$Household_Income <- abs(data$Household_Income)
data$Household_Income[data$Household_Income==999998|
                        data$Household_Income==999999|
                        data$Household_Income==9999999] <- NA

data$Length_of_Lease[data$Length_of_Lease<6] <- data$Length_of_Lease[data$Length_of_Lease<6]/2
data$Length_of_Lease[data$Length_of_Lease==6] <- 0
data$Length_of_Lease[data$Length_of_Lease>6] <- 1

data$Sex[data$Sex>1]=0  #female to 0

data$new_apt <- as.numeric(data$First_Occupants==1)

data$Married <- as.numeric(data$Household_Composition<=4)

data$Year_Moved[(data$Year_Moved<=17)&(data$Year>2000)]=data$Year_Moved[(data$Year_Moved<=17)&(data$Year>2000)]+2000
data$Year_Moved[(data$Year_Moved<=17)&(data$Year<2000)]=data$Year_Moved[(data$Year_Moved<=17)&(data$Year<2000)]+1900
data$Year_Moved[(data$Year_Moved<100)&(data$Year_Moved>17)]=data$Year_Moved[(data$Year_Moved<100)&(data$Year_Moved>17)]+1900

data$Recent_Place_Lived <- data$Recent_Place_Lived-2
#data$Same_Building <- as.numeric(data$Recent_Place_Lived<=0)
#data$Same_Borough <- as.numeric((data$Recent_Place_Lived==data$Borough)|data$Same_Building)
#data$Same_City <- as.numeric(data$Recent_Place_Lived<7)
#data$Same_Country <- as.numeric(data$Recent_Place_Lived<8)

### Birth
data$Householder_Birth[(data$Householder_Birth==25)&(data$Year==1999)]=14
data$Householder_Birth[(data$Householder_Birth==26)&(data$Year==1999)]=19

data$Householder_Birth[(data$Householder_Birth==25)&(data$Year==2002)]=14
data$Householder_Birth[(data$Householder_Birth==26)&(data$Year==2002)]=19

data$Householder_Birth[(data$Householder_Birth==7)&(data$Year>=2005)]=8
data$Householder_Birth[(data$Householder_Birth<=13)&(data$Year>=2005)]=data$Householder_Birth[(data$Householder_Birth<=13)&(data$Year>=2005)]+1
data$Householder_Birth[(data$Householder_Birth>=15)&(data$Year>=2005)]=data$Householder_Birth[(data$Householder_Birth>=15)&(data$Year>=2005)]-1

data$Householder_Birth[(data$Householder_Birth>=97)] <- 24

#Father birth
data$Father_Birth[(data$Father_Birth==25)&(data$Year==1999)]=14
data$Father_Birth[(data$Father_Birth==26)&(data$Year==1999)]=19

data$Father_Birth[(data$Father_Birth==25)&(data$Year==2002)]=14
data$Father_Birth[(data$Father_Birth==26)&(data$Year==2002)]=19

data$Father_Birth[(data$Father_Birth==7)&(data$Year>=2005)]=8
data$Father_Birth[(data$Father_Birth<=13)&(data$Year>=2005)]=data$Father_Birth[(data$Father_Birth<=13)&(data$Year>=2005)]+1
data$Father_Birth[(data$Father_Birth>=15)&(data$Year>=2005)]=data$Father_Birth[(data$Father_Birth>=15)&(data$Year>=2005)]-1

data$Father_Birth[(data$Father_Birth>=97)] <- 24

# Mother Birth
data$Mother_Birth[(data$Mother_Birth==25)&(data$Year==1999)]=14
data$Mother_Birth[(data$Mother_Birth==26)&(data$Year==1999)]=19

data$Mother_Birth[(data$Mother_Birth==25)&(data$Year==2002)]=14
data$Mother_Birth[(data$Mother_Birth==26)&(data$Year==2002)]=19

data$Mother_Birth[(data$Mother_Birth==7)&(data$Year>=2005)]=8
data$Mother_Birth[(data$Mother_Birth<=13)&(data$Year>=2005)]=data$Mother_Birth[(data$Mother_Birth<=13)&(data$Year>=2005)]+1
data$Mother_Birth[(data$Mother_Birth>=15)&(data$Year>=2005)]=data$Mother_Birth[(data$Mother_Birth>=15)&(data$Year>=2005)]-1

data$Mother_Birth[(data$Mother_Birth>=97)] <- 24

###nyc
data$is_im2 <- as.numeric((((data$Mother_Birth>10)|(data$Father_Birth>10))&(data$Householder_Birth<=10))|(((data$Mother_Birth<=10)|(data$Father_Birth<=10))&(data$Householder_Birth>10)))
data$is_im1 <- as.numeric((data$Mother_Birth>10)&(data$Father_Birth>10)&(data$Householder_Birth>10))

## no longer usefull
data$Household_Composition <- NULL
data$First_Occupants <- NULL
data$Recent_Place_Lived <- NULL

data$Householder_Birth <- NULL
data$Mother_Birth <- NULL
data$Father_Birth <- NULL

# focus on renter only
data$Value <- NULL
data$Mortgage_Status <- NULL
data$Mortgage <- NULL

# delete a few not interesting var
data$Reason_for_Moving <- NULL
data$Year_Moved <- NULL


#
for (i in 12:39){
  data[data[,i]>1,i] <- 0
}
# None of the problem 17 22 26 34  good for 38,39,40

data[data[,40]<=2,40] <- 1
data[(data[,40]==3)|(data[,40]==4),40] <- 2
data[data[,40]==5,40] <- 3
data[data[,40]==6,40] <- 4
data[data[,40]==7,40] <- 6
data[data[,40]==8,40] <- 10
data[data[,40]==9,40] <- 15
data[data[,40]==10,40] <- 30
data[data[,40]==11,40] <- 80
data[data[,40]==12,40] <- 100

data[data[,41]>1,41] <- 0

data[data[,42]==2,42] <- 3
data[data[,42]==3,42] <- 4
data[data[,42]==4,42] <- 5
data[data[,42]==5,42] <- 8
data[data[,42]==6,42] <- 15
data[data[,42]==7,42] <- 25

for (i in 43:45){
  data[data[,i]>1,i] <- 0
}

#bedroom start from 0
data[,47] <- data[,47]-1

#
data[,48] <- 1-data[,48]
data[data[,48]<0,48]=0

## Exclusive just provide too few information
data$`Exclusive use of plumbing facilities` <- NULL
data$`Exclusive use of kitchen facilities` <- NULL

#
data[data[,49]>1,49] <- 0

data[,50] <- 1-data[,50]
data[data[,50]<0,50]=0

data[data[,51]>1,51] <- 0

# may change to 3 column 
data[data[,52]>4,52] <- 4

#remove useless column
data$`Heating equipment breakdown` <- NULL
data[,53] <- data[,53]-1
data[data[,53]>6,53] <- 0

data[data[,54]>1,54] <- 0

data[data[,55]>1,55] <- 0

data[data[,56]>4,56] <- 4

data[data[,57]>1,57] <- 0

data[data[,58]>1,58] <- 0

data[,59] <- 1-data[,59]
data[data[,59]<0,59]=0

data[,60] <- data[,60]-1
data[data[,60]>1,60] <- 0

data[data[,61]>1,61] <- 0

data[data[,62]>4,62] <- 3


### Summary and Imputation
data$Year_Built <- NULL
data <- data[!is.na(data$Rent),]
data$Household_Income[is.na(data$Household_Income)] <- data$Rent[is.na(data$Household_Income)]*50
data$Type_of_Schedule <- NULL


data <- data[c(1:3,8,9,63:65,4:7,10:62)]
#13-65 are conditions
#data$`Condition of Stairways (Exterior and Interior): No exterior steps or stairways` <- NULL


write.csv(data,file="data2.csv",row.names = F)
