
# Downloaded data are called and organized in a .csv file


ICD7 <- read.table("MortIcd7",sep=",",header=TRUE)   
# 5020,Australia;4010,Austria;4020,Belgium;2090,Canada;4050,Denmark;4070,Finland;4080,France;4140,Greece;4150,Hungary;4160,Iceland;4170,Ireland;4180,Italy;3160,Luxembourg;4210,Netherlands;4220,Norway;4230,Poland;4240,Portugal;5150,New Zealand;4280,Spain;4290,Sweden;4300,Switzerland;4308,United Kingdom;2450,United States of America

mycountry <- c(5020,4010,4020,2090,4050,4070,4080,4140,4150,4160,4170,4180,3160,4210,4220,4230,4240,5150,4280,4290,4300,4308,2450)

ICD7 <- ICD7[which(ICD7$Country %in% mycountry),]

colnames(ICD7)[10:35] <- c("All","Age0","Age1","Age2","Age3","Age4","Age5-9","Age10-14","Age15-19","Age20-24","Age25-29","Age30-34","Age35-39","Age40-44","Age45-49","Age50-54","Age55-59","Age60-64","Age65-69","Age70-74","Age75-79","Age80-84","Age85-89","Age90-94","Age95+","AgeUNK")

InfectList <- c("A001", "A002", "A003", "A004", "A005", "A006", "A007", "A008", "A009", "A010", "A011", "A012","A013", "A014", "A015", "A016", "A017", "A018", "A019", "A020", "A021", "A022", "A023", "A024", "A025", "A026", "A027", "A028","A029", "A030", "A031", "A032", "A033", "A034", "A035", "A036", "A037", "A038", "A039", "A040", "A041", "A042", "A043", "A104","A132","B001", "B002", "B003", "B004", "B005","B006", "B007", "B008", "B009", "B010", "B011", "B012", "B013", "B014", "B015", "B016", "B017","B043")
NeopList <- c("A044","A045", "A046", "A047", "A048", "A049","A051", "A052", "A053", "A054", "A055", "A056", "A057", "A058", "A059", "A060","B018","B019")
LungList <- c("A050")
EndocList <- c("A061","A061", "A062", "A063", "A064","B020")
CircList <- c("A070","A079", "A080", "A081", "A082", "A083", "A084", "A085", "A086","B022","B024", "B025", "B026", "B027", "B028", "B029")
RespList <- c("A087", "A088", "A089", "A090", "A091", "A092", "A093", "A094", "A095", "A096","A097","B030", "B031", "B032")
DigestList <- c("A098", "A099", "A100", "A101", "A102", "A103", "A104", "A105", "A106", "A107","B033", "B034", "B035", "B036", "B037")
ExternList <- c("A138", "A139", "A140","A141", "A142", "A143", "A144", "A145", "A146", "A147", "A148", "A149", "A150","B047", "B048", "B049", "B050")
OthList <- c("A137","B045")

ICD7$CauseM <- NA
ICD7$CauseM <- ifelse(ICD7$Cause%in%InfectList, 1,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%NeopList, 2,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%LungList, 3,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%EndocList, 4,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%CircList, 5,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%RespList, 6,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%DigestList, 7,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%ExternList, 8,ICD7$CauseM)
ICD7$CauseM <- ifelse(ICD7$Cause%in%OthList, 9,ICD7$CauseM)
#ICD7$CauseM <- ifelse(is.na(ICD7$CauseM), 9,ICD7$CauseM)

AllDeaths <- ICD7[ICD7$Cause%in% c("A000","B000"),c(1,4,7,10:35)]
SpecDeaths <- ICD7[!is.na(ICD7$CauseM),]
AggS.Deaths <- aggregate(SpecDeaths[,c(10:35)], by=SpecDeaths[,c("Country","Year","Sex","CauseM")], sum) 



AllDeaths2 <- reshape(AllDeaths,varying = c("All","Age0","Age1","Age2","Age3","Age4","Age5-9","Age10-14", "Age15-19","Age20-24", "Age25-29", "Age30-34", "Age35-39", "Age40-44", "Age45-49", "Age50-54", "Age55-59","Age60-64", "Age65-69", "Age70-74", "Age75-79", "Age80-84", "Age85-89", "Age90-94", "Age95+","AgeUNK"),timevar="Age", times=c("All","0","1","2","3","4","5-9","10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+","UNK"),direction="long",v.names = "Counts")
AllDeaths2$CauseM <- 0

AggS.Deaths2 <- reshape(AggS.Deaths,varying = c("All","Age0","Age1","Age2","Age3","Age4","Age5-9","Age10-14", "Age15-19","Age20-24", "Age25-29", "Age30-34", "Age35-39", "Age40-44", "Age45-49", "Age50-54", "Age55-59","Age60-64", "Age65-69", "Age70-74", "Age75-79", "Age80-84", "Age85-89", "Age90-94", "Age95+","AgeUNK"),timevar="Age", times=c("All","0","1","2","3","4","5-9","10-14", "15-19","20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59","60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+","UNK"),direction="long",v.names = "Counts")


final <- rbind(AggS.Deaths2,AllDeaths2[,c(1,2,3,7,4,5,6)])

complete <- NULL
for (j in 1:length(table(final$Country))){
    c <- as.numeric(names(table(final$Country)))[j]
    for(i in 1:length(table(final$Year[final$Country==c]))){
        y <- as.numeric(names(table(final$Year[final$Country==c])))[i]
        All0 <- final[final$Country==c&final$Year==y&final$CauseM==0,]
        final2 <- final[final$CauseM>0&final$Country==c&final$Year==y,]
        Agg2 <- aggregate(final2[,6], by=final2[,c("Age","Sex")],sum)
        rest <- merge(Agg2,All0,by=c("Age","Sex"))
        rest$Counts2 <- rest$Counts-rest$x
        rest$CauseM <- 10
        rest <- rest[,c(4,5,2,6,1,9,8)]
        colnames(rest)[6] <- "Counts"
        final3 <- rbind(final2,rest)
        total <- All0[,-c(4,7)]
        colnames(total)[5] <- "AllCauses"
        comp <- merge(final3[,-7],total, by=c("Country","Year","Sex","Age"))
        complete <- rbind(complete,comp)
    }
}

write.csv(complete,file="ICD7_comp.csv",sep=",")        

