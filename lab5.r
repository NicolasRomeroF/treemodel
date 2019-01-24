require('C50')
library('caret')
library('dplyr')


path = "F:/Google Drive/USACH/Nivel 8/Analisis de datos/lab3/hepatitis.data"
#path = "~/Documentos/AnalisisDatosLab4/hepatitis.data"
hepatitis <- read.table(path,sep=",", na.strings = c("?"))

names <- c("CLASS","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
           "ANOREXIA","LIVER_BIG","LIVER_FIRM","SPLEEN_PALPABLE","SPIDERS",
           "ASCITES","VARICES","BILIRUBIN","ALK_PHOSPHATE","SGOT","ALBUMIN",
           "PROTIME","HISTOLOGY")
namesNoCLass <- c("AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
                  "ANOREXIA","LIVER_BIG","LIVER_FIRM","SPLEEN_PALPABLE","SPIDERS",
                  "ASCITES","VARICES","BILIRUBIN","ALK_PHOSPHATE","SGOT","ALBUMIN",
                  "PROTIME","HISTOLOGY")

colnames(hepatitis) <- names

hepatitis.without.na <- na.omit(hepatitis)
data = hepatitis.without.na

data[data["AGE"] <= 30,"AGE"] <- 0
data[data["AGE"] > 30,"AGE"] <- 1

data[data["BILIRUBIN"] < 0.1,"BILIRUBIN"] <- 0
data[data["BILIRUBIN"] > 1.2,"BILIRUBIN"] <- 0
data[data["BILIRUBIN"] != 0,"BILIRUBIN"] <- 1

data[data["ALK_PHOSPHATE"] < 44,"ALK_PHOSPHATE"] <- 0
data[data["ALK_PHOSPHATE"] > 147,"ALK_PHOSPHATE"] <- 0
data[data["ALK_PHOSPHATE"] != 0,"ALK_PHOSPHATE"] <- 1

data[data["SGOT"] < 5,"SGOT"] <- 0
data[data["SGOT"] > 40,"SGOT"] <- 0
data[data["SGOT"] != 0,"SGOT"] <- 1

data[data["ALBUMIN"] < 3.4,"ALBUMIN"] <- 0
data[data["ALBUMIN"] > 5.4,"ALBUMIN"] <- 0
data[data["ALBUMIN"] != 0,"ALBUMIN"] <- 1

meanPT = mean(data$PROTIME)

data[data["PROTIME"] <= meanPT,"PROTIME"] <- 0
data[data["PROTIME"] > meanPT,"PROTIME"] <- 1

data[names] <- lapply(data[names], factor)


#data = select(hepatitis.without.na,-CLASS)
print("factor")
set.seed(2019)

trainIndex=createDataPartition(data$CLASS, p=0.7)$Resample1
train=data[trainIndex, ]
test=data[-trainIndex, ]

print(table(data$CLASS))
print(table(train$CLASS))

print("fin tabla")

#train.noclass = select(train,-CLASS)


treeModel <- C5.0(x=train[,-1],y=train$CLASS)

print(treeModel)
print(summary(treeModel))
plot(treeModel)

testPred <- predict(treeModel, newdata = test)
print(testPred)

testTable=table(test$CLASS, testPred)
testAcc=(testTable[1,1]+testTable[2,2])/sum(testTable)

message("Contingency Table for Test Data")
print(testTable)

# 
# ruleModel <- C5.0(CLASS ~ ., data = train, rules = TRUE)
# ruleModel
# print(summary(ruleModel))
