

data <- rbind(read.table("~/UCI HAR Dataset/train/X_train.txt"),read.table("~/UCI HAR Dataset/test/x_test.txt"))
encabezados <- read.table("~/UCI HAR Dataset/features.txt")
activities <- rbind(read.table("~/UCI HAR Dataset/train/y_train.txt"),read.table("~/UCI HAR Dataset/test/y_test.txt"))[,1]
labels <- as.character(read.table("~/UCI HAR Dataset/activity_labels.txt")[,2])
participantes <- rbind(read.table("~/UCI HAR Dataset/train/subject_train.txt"),read.table("~/UCI HAR Dataset/test/subject_test.txt"))[,1]

colnames(data) <- encabezados$V2
medidas <- grepl('-(mean|std)\\(',encabezados$V2)
data <- data[medidas]
activities <- labels[activities]

colnames(data) <- gsub("mean", "Mean", colnames(data))
colnames(data) <- gsub("std", "Std", colnames(data))
colnames(data) <- gsub("t", "Time", colnames(data))
colnames(data) <- gsub("f", "Frequency", colnames(data))
colnames(data) <- gsub("\\(\\)", "", colnames(data))
colnames(data) <- gsub("-", "", colnames(data))
colnames(data) <- gsub("BodyBody", "Body", colnames(data))
colnames(data) <- gsub("^", "MeanOf", colnames(data))

data <- cbind(Participante = participantes,Actividad = activities,data)
library(dplyr)
prom_data_set <- data %>% group_by(Participante,Actividad) %>% summarise_each(funs(mean))
write.table(prom_data_set,row.names = F,file = "Base de datos.txt")