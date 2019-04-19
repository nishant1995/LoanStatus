###################################################################################################################
# Function to check if packages are installed 
###################################################################################################################

begin_time = Sys.time()

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages = c("lubridate","caret","glmnet")
check.packages(packages)

###################################################################################################################
# Read in data
###################################################################################################################

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$loan_status <- ifelse(train$loan_status == "Fully Paid", 0, 1)

test = data.frame(test, loan_status = rep(0,nrow(test)))
data = rbind(train, test)

###################################################################################################################
# Treatment of Missing Values
###################################################################################################################

levels(data$emp_length) <- c(levels(data$emp_length), "N")
data$emp_length[is.na(data$emp_length)] = "N"
data$revol_util[is.na(data$revol_util)] = 53
data$mort_acc[is.na(data$mort_acc)] = 2
data$dti[is.na(data$dti)] = 18
data$pub_rec_bankruptcies[is.na(data$pub_rec_bankruptcies)] = 0

###################################################################################################################
# Recoding some variables
###################################################################################################################

cr_line_dates = parse_date_time(data$earliest_cr_line, orders=c("by"))
base_date = rep("2007-01-01", nrow(data))
base_date = ymd(base_date)
cr_line_dates = ymd(cr_line_dates)
cr_line_dates = interval(base_date, cr_line_dates) %/% months(1)
data$earliest_cr_line = cr_line_dates

data['fico_score'] = 0.5*data$fico_range_high + 0.5*data$fico_range_low


###################################################################################################################
# Log - Transformations
###################################################################################################################

data$annual_inc = log(1 + data$annual_inc)
data$revol_bal = log(1 + data$revol_bal)
data$revol_util = log(1 + data$revol_util)
data$pub_rec_bankruptcies = log(1 + data$pub_rec_bankruptcies)
data$mort_acc = log(1 + data$mort_acc)
data$open_acc = log(1 + data$open_acc)
data$fico_score = log(1 + data$fico_score)





###################################################################################################################
# Function to remove unecessary or dominating categorical columns
###################################################################################################################

remove_columns = function(data){
  data$emp_title = NULL
  data$title = NULL
  data$zip_code = NULL
  data$fico_range_high = NULL
  data$fico_range_low = NULL
  data$grade = NULL
  return(data)
}

data = remove_columns(data)

###################################################################################################################
# One-hot encoding
###################################################################################################################

dmy = dummyVars("~.", data = data)
data = data.frame(predict(dmy, newdata=data))

###################################################################################################################
# Getting the splits back
###################################################################################################################

I.D = test$id
train = data[1:nrow(train),]
test = data[nrow(train)+1:nrow(test), names(data) != "loan_status"]

train$id = NULL
test$id = NULL

###################################################################################################################
# Logistic Regression Model
###################################################################################################################

model = glm(loan_status ~ ., data = train, family = binomial)
pred = predict(model, test, type = "response")

output = data.frame(I.D, pred)
colnames(output) = c('id','prob')
write.csv(output,'mysubmission1.txt',row.names = FALSE)

end_time = Sys.time()

Run_Time = end_time - begin_time 


