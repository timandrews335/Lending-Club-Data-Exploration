#setup
library(scales)
library(ggplot2)
library(sqldf)

#Download and process data
url <- 'https://resources.lendingclub.com/LoanStats3c.csv.zip'
download.file(url,destfile="LendingClubLoans2014.csv.zip")
unzip('LendingClubLoans2014.csv.zip', overwrite = TRUE)

#working file (get rid of the first row in the csv)
filename <- 'LoanStats3c.csv'
loans <- read.csv(filename, skip = 1, header=TRUE)

#set up the issue date and the earliest credit line to Date types
loans$IssueDate <- as.Date(paste(paste('01', substr(loans$issue_d, 1, 3), sep=''), substr(loans$issue_d, 5, 9), sep=''),   '%d%B%Y')
loans$FirstCreditDate <- as.Date(paste(paste('01', substr(loans$earliest_cr_line, 1, 3), sep=''), substr(loans$earliest_cr_line, 5, 9), sep=''),   '%d%B%Y')
loans$CreditLength <- as.numeric((loans$IssueDate - loans$FirstCreditDate) / 365.0)

#Set the proper order for the loan_status factor
loans$loan_status <- factor(loans$loan_status, levels = c('Charged Off', 'Default', 'Late (31-120 days)', 'Late (16-30 days)', 'In Grace Period', 'Current', 'Fully Paid'))

#Get a variable to denote if the loan is charged off or not
loans$IsChargedOff <- ifelse(loans$loan_status == 'Charged Off', 1, 0)

#the int_rate variable is a factor with a percentage sign, so we need to derivce a numeric
loans$Interest_Rate <- (as.numeric(gsub("%", "", loans$int_rate)) / 100)


#Summarize the data by purpose.  Using sqldf as we are aggregating a ratio
grouped <- sqldf("select purpose, COUNT(*) AS Loans, SUM(IsChargedOff) / COUNT(*) as ChargedOffPct, AVG(loan_amnt) AS AverageLoan, AVG(CreditLength) AS AverageCreditLength FROM loans WHERE purpose <> '' GROUP BY purpose") 


#Create a labeled scatterplot
myGraph <- ggplot(data = grouped, aes(x=ChargedOffPct, y=AverageLoan, color=purpose))
myGraph <- myGraph + geom_point(alpha = 1, size=6, shape=15)
#set the background to white
myGraph <- myGraph + theme(panel.background = element_rect(fill = 'white', colour = 'gray'))
myGraph <- myGraph + geom_text(aes(label=purpose), vjust=2, hjust=0, color='black', angle=0, size=4)
#we want to reverse the x axis so that "good" is to the right, and "bad" is to the left
myGraph <- myGraph + scale_x_reverse()
#title, etc
myGraph <- myGraph + labs(title = "Lending Club 2014 Loans by Purpose")
myGraph <- myGraph + xlab("Percent that were charged off") + ylab("Average amount of loan")
myGraph <- myGraph + scale_y_continuous(labels = dollar) + scale_x_continuous(labels = percent)


#write an image
png(filename="lendingClub2014LoansByPurpose.png", width=800, height=600)
plot(myGraph)
dev.off()


