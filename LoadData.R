data <- read.csv("D:/RStudio/RepData_PeerAssessment1/activity.csv")
data[[2]] <- as.Date(data[[2]], format = "%m/%d/%Y") 