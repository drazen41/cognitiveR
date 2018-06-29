
# Strings -----------------------------------------------------------------

putanja = "~/R/cognitiveR/cognitiveR/R101/The_Artist.txt"
putanja1 = "~/R/cognitiveR/cognitiveR/R101/"
download.file("https://ibm.box.com/shared/static/l8v8g8e6uzk7yj2j1qc8ypezbhzukphy.txt", destfile=putanja)

my_data <- readLines(putanja)
my_data

length(my_data)

file.size(putanja)

my_data1 <- scan(putanja, "")
my_data1

length(my_data1)

nchar(my_data[1])
toupper(my_data[3])
tolower(my_data[3])
chartr(" ", "-", my_data[1])

character_list <- strsplit(my_data[1], " ")
word_list <- unlist(character_list)
word_list

sorted_list <- sort(word_list)
sorted_list

paste(sorted_list, collapse = " ")

sub_string <- substr(my_data[1], start = 4, stop = 50)
sub_string
trimws(sub_string)

#install.packages("stringr")
library(stringr)
str_sub(my_data[1], -8, -1)

m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
m

file1 = paste(putanja1,"my_text_file_txt",sep="")
write(m, file = file1, ncolumns = 3, sep = " ")

file2 = paste(putanja1,"my_text_file2_txt.txt",sep="")
write(my_data[1], file = file2, ncolumns = 1, sep = " ")


getwd()


# head(C02)

var1 <- "var1"
var2 <- "var2"
var3 <- "var3"

rdata = paste(putanja1,"variables.RData",sep="")
save(list = c("var1", "var2", "var3"), file = rdata, safe = T)


# Date --------------------------------------------------------------------

# use as.Date( ) to convert strings to dates 
mydates <- as.Date(c("2007-06-22", "2004-02-13"))
# number of days between 6/22/07 and 2/13/04 
days <- mydates[1] - mydates[2]
mydates
days
# print today's date
today <- Sys.Date()
formatiran = format(today, format="%d %m %Y")
formatiran
# https://www.stat.berkeley.edu/~s133/dates.html


# RegEx -------------------------------------------------------------------


email_df <- read.csv("https://ibm.box.com/shared/static/cbim8daa5vjf5rf4rlz11330lvqbu7rk.csv")
email_df

grep("@.+",  c("test@testing.com" , "not an email", "test2@testing.com"))
grep("@.+",  c("test@testing.com", "not an email", "test2@testing.com"), value=TRUE)
gsub("@.+", "@newdomain.com", c("test@testing.com", "not an email", "test2@testing.com"))

matches <- regexpr("@.*", c("test@testing.com", "not an email", "test2@testing.com"))
regmatches(c("test@testing.com", "not an email", "test2@testing.com"), matches)

matches <- regexpr("@.*\\.", email_df[,'Email'])
email_df[,'Domain'] = regmatches(email_df[,'Email'], matches)
email_df
table(email_df[,'Domain'])


# quiz --------------------------------------------------------------------

hw <- c("Hello", "World")
hw
paste(hw, collapse = " ")





















