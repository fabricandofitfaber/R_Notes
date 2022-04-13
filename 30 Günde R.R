# Chapter 1: Getting started with R Language ----

## Section 1.3: Getting Help ----

#For help on the help function of R
help()
#For help on the paste function
help(paste) #OR
help("paste") #OR
?paste #OR
?"paste"

data() ## verisetlerini g?sterir.

data(package = .packages(all.available = TRUE)) ## Tüm verisetlerini gösterir.


data("PlantGrowth") ## Loads the example dataset

str(PlantGrowth) ## shows information about the dataset

anova(lm(weight ~ group, data = PlantGrowth))


boxplot(weight ~ group, data = PlantGrowth, ylab = "Dry weight")


# Chapter 2: Variables ----

list( '.11' ="a")


foo <- 42
fooEquals = 43


# The following command assigns a value to the variable named x and prints the value simultaneously:
(x <- 5)

# actually two function calls: first one to `<-`; second one to the `()`-function
is.function(`(`)
# Often used in R help page examples for its side-effect of printing.


# Example objects
a <- 1
b <- 2
c <- c(2,3,4)
d <- c(10,10,10)
e <- c(1,2,3,4)
f <- 1:6
W <- cbind(1:4,5:8,9:12)
Z <- rbind(rep(0,3),1:3,rep(10,3),c(4,7,1))


# Some vector operations
a+b # scalar + scalar

c+d # vector + vector

a*b # scalar * scalar

c*d # vector * vector (componentwise!)

c+a # vector + scalar

c^2 #

exp(c)


# Some vector operation Warnings!
c+e # warning but.. no errors, since recycling is assumed to be desired.

# Some Matrix operations Warning!
Z+W # matrix + matrix #(componentwise)
Z*W # matrix* matrix#(Standard product is always componentwise)

W + a # matrix+ scalar is still componentwise
W + c # matrix + vector... : no warnings and R does the operation in a column-wise manner


foo <- 'foo'
.foo <- 'bar'

ls()

ls(all.names = TRUE)

# Chapter 3: Arithmetic Operators ----

# Chapter 4: Matrices ----

## Section 4.1: Creating matrices ----

matrix(data = 1:6, nrow = 2, ncol = 3)

matrix(data = 1:6, nrow = 2, ncol = 3, byrow = TRUE)

matrix(data = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), nrow = 3, ncol = 2)

matrix(data = c("a", "b", "c", "d", "e", "f"), nrow = 3, ncol = 2)  

mat1 <- matrix(data = 1:6, nrow = 2, ncol = 3, byrow = TRUE)
rownames(mat1)

colnames(mat1)

rownames(mat1) <- c("Row 1", "Row 2")
colnames(mat1) <- c("Col 1", "Col 2", "Col 3")
mat1

class(mat1)

is.matrix(mat1)

as.vector(mat1)

# Chapter 5: Formula ----

## Section 5.1: The basics of formula ----

my_formula1 <- formula(mpg ~ wt)
class(my_formula1)

mod1 <- lm(my_formula1, data = mtcars)
coef(mod1)

form <- mpg ~ wt
class(form)

form_mt <- formula(mpg ~ wt, env = mtcars)

coef(lm(mpg ~ 0 + wt, data = mtcars))
coef(lm(mpg ~ wt -1, data = mtcars))

coef(lm(mpg ~ wt:vs, data = mtcars))

coef(lm(mpg ~ wt*vs, data = mtcars))

coef(lm(mpg ~ wt*vs*hp, data = mtcars))

coef(lm(mpg ~ wt*vs*hp - wt:vs:hp, data = mtcars))

coef(lm(mpg ~ (wt + vs + hp) ^ 2, data = mtcars))

coef(lm(mpg ~ ., data = mtcars))

# Chapter 6: Reading and writing strings ----

## Section 6.1: Printing and displaying strings ----

print("Hello World")

cat("Hello World\n")

cat(c("hello", "world", "\n"))

cat("Hello World")

print("Hello World")

s <- "Hello World"
s

"Hello World"

print(c("Hello World", "Here I am."))

c("Hello World", "Here I am!", "This next string is really long.")

print(1)

print(TRUE)

message("hello world")

suppressMessages(message("hello world"))

## Section 6.2: Capture output of operating system command ----

# Bu k?s?mdaki kodlar ?al??mad?
system("top -a -b -n 1", intern = TRUE) 
system2("top", "-a -b -n 1", stdout = TRUE)

devtools::system_output("top", "-a -b -n 1")

fread("top -a -b -n 1", check.names = TRUE)

## Section 6.3: Reading from or writing to a file connection ----

# Establish a file connection to read with file() command ("r" is for read mode):

conn <- file("/path/example.data", "r") #when file is in local system

conn1 <- file("stdin", "r") #when just standard input/output for files are available

# As this will establish just file connection, 
# one can read the data from these file connections as follows:

line <- readLines(conn, n=1, warn=FALSE)

conn2 <- file("/path/result.data", "w") #when file is in local system
conn3 <- file("stdout", "w") #when just standard input/output for files are available

writeLines("text",conn2, sep = "\n")

# Chapter 7: String manipulation with stringi package ----

## Section 7.1: Count pattern inside string ----

# install.packages("stringi")
# library(stringi)

# With fixed pattern
stri_count_fixed("babab", "b")

stri_count_fixed("babab", "ba")

stri_count_fixed("babab", "bab")

# Natively:
length(gregexpr("b","babab")[[1]])

length(gregexpr("ba","babab")[[1]])

length(gregexpr("bab","babab")[[1]])

# function is vectorized over string and pattern:
stri_count_fixed("babab", c("b","ba"))

stri_count_fixed(c("babab","bbb","bca","abc"), c("b","ba"))

# A base R solution:
sapply(c("b","ba"),function(x)length(gregexpr(x,"babab")[[1]]))

# With regex
# First example - find a and any character after
# Second example - find a and any digit after
stri_count_regex("a1 b2 a3 b4 aa", "a.")

stri_count_regex("a1 b2 a3 b4 aa", "a\\d")

## Section 7.2: Duplicating strings ----

stri_dup("abc",3)

paste0(rep("abc",3),collapse = "")

## Section 7.3: Paste vectors ----

stri_paste(LETTERS,"-", 1:13)

stri_paste(rep("ali",12), sep = "--", rep("osman", 12))

paste(LETTERS,1:13,sep="-")

## Section 7.4: Splitting text by some fixed pattern ----

# Split vector of texts using one pattern:
stri_split_fixed(c("To be or not to be.", "This is very short sentence.")," ")

# Split one text using many patterns:
stri_split_fixed("Apples, oranges and pineaplles.",c(" ", ",", "s"))




# Chapter 8: Classes ----

## Section 8.1: Inspect classes ----

class(iris)

str(iris)



class(iris$Species)

## Section 8.2: Vectors and lists ----

x <- 1826
class(x) <- "Date"
x

x <- as.Date("1970-01-01")
class(x)

is(x,"Date")

is(x,"integer")

is(x,"numeric")

mode(x)

mylist <- list( A = c(5,6,7,8), B = letters[1:10], CC = list( 5, "Z") )

# Lists have two very important uses:
# Since functions can only return a single value, 
# it is common to return complicated results in a list:
f <- function(x) list(xplus = x + 10, xsq = x^2)
f(7)

# Lists are also the underlying fundamental class for data frames. 
# Under the hood, a data frame is a list of
# vectors all having the same length:
L <- list(x = 1:2, y = c("A","B"))
DF <- data.frame(L)
DF

is.list(DF)

## Section 8.3: Vectors ----

c(1, 2, 3)

c(TRUE, TRUE, FALSE)

c("a", "b", "c")

x <- c(1, 2, 5)
y <- c(3, 4, 6)
z <- c(x, y)
z







# Chapter 9: Lists ----

## Section 9.1: Introduction to lists ----

l1 <- list(c(1, 2, 3), c("a", "b", "c"))
l1

names(l1)

names(l1) <- c("vector1", "vector2")

l2 <- list(vec = c(1, 3, 5, 7, 9),
           mat = matrix(data = c(1, 2, 3), nrow = 3))
l2

names(l2)

## Section 9.2: Quick Introduction to Lists ----

exampleList1 <- list('a', 'b')
exampleList2 <- list(1, 2)
exampleList3 <- list('a', 1, 2)


str(exampleList1)
str(exampleList2)
str(exampleList3)

# Returns List
exampleList3[1]
exampleList3[1:2]

# Returns Character
exampleList3[[1]]

exampleList4 <- list(
  num = 1:3,
  numeric = 0.5,
  char = c('a', 'b')
)

exampleList4[['char']]

exampleList4[["num"]]

exampleList4$num

exampleList5 <- exampleList4[2:3]
exampleList4$num

exampleList5$num

exampleList5[['num']]

## Numeric vector
exampleVector1 <- c(12, 13, 14)
## Character vector
exampleVector2 <- c("a", "b", "c", "d", "e", "f")
## Matrix
exampleMatrix1 <- matrix(rnorm(4), ncol = 2, nrow = 2)
## List
exampleList3 <- list('a', 1, 2)
exampleList6 <- list(
  num = exampleVector1,
  char = exampleVector2,
  mat = exampleMatrix1,
  list = exampleList3
)
exampleList6

## Section 9.3: Serialization: using lists to pass information ----

df <- tibble::tribble( ~n0, ~name, ~height, ~team, ~fun_index, ~title, ~age, ~desc, ~Y,
                       1, "Andrea", 195, "Lazio", 97, 6, 33, "eccellente", 1,
                       2, "Paja", 165, "Fiorentina", 87, 6, 31, "deciso", 1,
                       3, "Roro", 190, "Lazio", 65, 6, 28, "strano", 0,
                       4, "Gioele", 70, "Lazio", 100, 0, 2, "simpatico", 1,
                       5, "Cacio", 170, "Juventus", 81, 3, 33, "duro", 0,
                       6, "Edola", 171, "Lazio", 72, 5, 32, "svampito", 1,
                       7, "Salami", 175, "Inter", 75, 3, 30, "doppiopasso", 1,
                       8, "Braugo", 180, "Inter", 79, 5, 32, "gjn", 0,
                       9, "Benna", 158, "Juventus", 80, 6, 28, "esaurito", 0,
                       10, "Riggio", 182, "Lazio", 92, 5, 31, "certezza", 1,
                       11, "Giordano", 185, "Roma", 79, 5, 29, "buono", 1
)


# df <- tibble::tribble(
#             ~no,      ~name, ~height,        ~team, ~fun_index, ~title, ~age,         ~desc, ~Y,
#              1L,   "Andrea",    195L,      "Lazio",        97L,     6L,  33L,  "eccellente", 1L,
#              2L,     "Paja",    165L, "Fiorentina",        87L,     6L,  31L,      "deciso", 1L,
#              3L,     "Roro",    190L,      "Lazio",        65L,     6L,  28L,      "strano", 0L,
#              4L,   "Gioele",     70L,      "Lazio",       100L,     0L,   2L,   "simpatico", 1L,
#              5L,    "Cacio",    170L,   "Juventus",        81L,     3L,  33L,        "duro", 0L,
#              6L,    "Edola",    171L,      "Lazio",        72L,     5L,  32L,    "svampito", 1L,
#              7L,   "Salami",    175L,      "Inter",        75L,     3L,  30L, "doppiopasso", 1L,
#              8L,   "Braugo",    180L,      "Inter",        79L,     5L,  32L,         "gjn", 0L,
#              9L,    "Benna",    158L,   "Juventus",        80L,     6L,  28L,    "esaurito", 0L,
#             10L,   "Riggio",    182L,      "Lazio",        92L,     5L,  31L,    "certezza", 1L,
#             11L, "Giordano",    185L,       "Roma",        79L,     5L,  29L,       "buono", 1L
#             )

number <- "42"

paste(df$name[4],"is a",df$team[4], "supporter." )


paste(df$name[1], "is a", df$height[1], "tall")

paste("The answer to THE question is", number )



l <- list(df,number)
dataframe_container <- data.frame(out2 = as.integer(serialize(l, connection=NULL)))

#----- unserialize ----------------------------------------+
unser_obj <- unserialize(as.raw(dataframe_container$out2))
#----- taking back the elements----------------------------+
df_mod <- unser_obj[1][[1]]
number_mod <- unser_obj[2][[1]]



paste(df_mod$name[4],"is a",df_mod$team[4], "supporter." )

paste("The answer to THE question is", number_mod )






# Chapter 10: Hashmaps ----

## Section 10.1: Environments as hash maps ----

H <- new.env(hash = TRUE)
H <- new.env()

object.size(new.env())

object.size(new.env(size = 10e4))

H <- new.env()
H[["key"]] <- rnorm(1)
key2 <- "xyz"
H[[key2]] <- data.frame(x = 1:3, y = letters[1:3])
H$another_key <- matrix(rbinom(9, 1, 0.5) > 0, nrow = 3)

H["error"] <- 42

H[["key3"]] <- "original value"
H[["key3"]] <- "new value"
H[["key3"]]

# Key Lookup
# Likewise, elements may be accessed with [[ or $, but not with [:
H[["key"]]

H[[key2]] ## assuming key2 <- "xyz"

H$another_key

H[1]                                                                

# Inspecting the Hash Map
# Being just an ordinary environment, the hash map can be inspected 
# by typical means:

names(H)

ls(H)

str(H)

ls.str(H)

# Elements can be removed using rm:
rm(list = c("key", "key3"), envir = H)
ls.str(H)

# Flexibility

H2 <- new.env()
H2[["a"]] <- LETTERS
H2[["b"]] <- as.list(x = 1:5, y = matrix(rnorm(10), 2))
H2[["c"]] <- head(mtcars, 3)
H2[["d"]] <- Sys.Date()
H2[["e"]] <- Sys.time()
H2[["f"]] <- (function() {
  H3 <- new.env()
  for (i in seq_along(names(H2))) {
    H3[[names(H2)[i]]] <- H2[[names(H2)[i]]]
  }
  H3
})()
ls.str(H2)

ls.str(H2$f)

# Limitations

names(H2)

H2[[c("a", "b")]]

Keys <- c("a", "b")
H2[[Keys]]

E1 <- new.env()
invisible({
  vapply(letters, function(x) {
    E1[[x]] <- rnorm(1)
    logical(0)
  }, FUN.VALUE = logical(0))
})
all.equal(sort(names(E1)), letters)

Keys <- letters
E2 <- list2env(
  setNames(
    as.list(rnorm(26)),
    nm = Keys),
  envir = NULL,
  hash = TRUE
)
all.equal(sort(names(E2)), letters)

## Section 10.2: package:hash ----

install.packages("hash")
library(hash)

# Generic unique string generator
unique_strings <- function(n){
  string_i <- 1
  string_len <- 1
  ans <- character(n)
  chars <- c(letters,LETTERS)
  new_strings <- function(len,pfx){
    for(i in 1:length(chars)){
      if (len == 1){
        ans[string_i] <<- paste(pfx,chars[i],sep='')
        string_i <<- string_i + 1
      } else {
        new_strings(len-1,pfx=paste(pfx,chars[i],sep=''))
      }
      if (string_i > n) return ()
    }
  }
  while(string_i <= n){
    new_strings(string_len,'')
    string_len <- string_len + 1
  }
  sample(ans)
}
# Generate timings using an enviornment
timingsEnv <- plyr::adply(2^(10:15),.mar=1,.fun=function(i){
  strings <- unique_strings(i)
  ht1 <- new.env(hash=TRUE)
  lapply(strings, function(s){ ht1[[s]] <<- 0L})
  data.frame(
    size=c(i,i),
    seconds=c(
      system.time(for (j in 1:i) ht1[[strings[j]]]==0L)[3]),
    type = c('1_hashedEnv')
  )
})
timingsHash <- plyr::adply(2^(10:15),.mar=1,.fun=function(i){
  strings <- unique_strings(i)
  ht <- hash::hash()
  lapply(strings, function(s) ht[[s]] <<- 0L)
  data.frame(
    size=c(i,i),
    seconds=c(
      system.time(for (j in 1:i) ht[[strings[j]]]==0L)[3]),
    type = c('3_stringHash')
  )
})

## Section 10.3: package:listenv ----

timingsListEnv <- plyr::adply(2^(10:15),.mar=1,.fun=function(i){
  strings <- unique_strings(i)
  le <- listenv::listenv()
  lapply(strings, function(s) le[[s]] <<- 0L)
  data.frame(
    size=c(i,i),
    seconds=c(
      system.time(for (k in 1:i) le[[k]]==0L)[3]),
    type = c('2_numericListEnv')
  )
})

# Chapter 11: Creating vectors ----
## Section 11.1: Vectors from build in constants: Sequences of letters & month names ----

# R has a number of build in constants. The following constants are available:
# LETTERS: the 26 upper-case letters of the Roman alphabet
# letters: the 26 lower-case letters of the Roman alphabet
# month.abb: the three-letter abbreviations for the English month names
# month.name: the English names for the months of the year
# pi: the ratio of the circumference of a circle to its diameter

# 1) Sequences of letters:
letters

LETTERS[7:9]

letters[c(1,5,3,2,4)]

# Sequences of month abbreviations or month names:
# month.abb
month.name[1:4]

month.abb[c(3,6,9,12)]

vector2 <- rep(month.name[1:4], 12)

## Section 11.2: Creating named vectors ----

xc <- c('a' = 5, 'b' = 6, 'c' = 7, 'd' = 8)
xc

xl <- list('a' = 5, 'b' = 6, 'c' = 7, 'd' = 8)
xl

# With the setNames function, two vectors of the same length can be 
# used to create a named vector:
x <- 5:8
y <- letters[1:4]
xy <- setNames(x, y)

# You may also use the names function to get the same result:
xy <- 5:8
names(xy) <- letters[1:4]

xy["c"]

mydf <- data.frame(let = c('c','a','b','d'))

dataf <- data.frame("de?i?ken1" = c(1,2,3,4,5,6),
                    "de?i?ken2" = c("ali", "veli", "ahmet", "mehmet", "rabia","cem"),
                    "de?i?ken3" = rep(c(12,14),3))

mydf$num <- xy[match(mydf$let, names(xy))]

## Section 11.3: Sequence of numbers ----

x <- 1:5

10:4

1.25:5

-4:4

## Section 11.4: seq() ----

seq(5)

seq(2, 5) # or seq(from=2, to=5)

seq(2, 5, 0.5) # or seq(from=2, to=5, by=0.5)

seq(2,5, length.out = 10)

x = 1:8
seq(2,5,along.with = x)

seq_along(x)

# counting numbers 1 through 10
seq_len(10)

# indices of existing vector (or list) with seq_along
letters[1:10]

seq_along(letters[1:10])

sequence(4)

sequence(c(3, 2))

sequence(c(3, 2, 5))

## Section 11.5: Vectors ----

vector('integer',2) # creates a vector of integers of size 2.
vector('character',2) # creates a vector of characters of size 2.
vector('logical',2) # creates a vector of logicals of size 2.

integer(2) # is the same as vector('integer',2) and 
# creates an integer vector with two elements
character(2) # is the same as vector('integer',2) and 
# creates an character vector with two elements
logical(2) # is the same as vector('logical',2) and 
# creates an logical vector with two elements

c(1, 2) # creates a integer vector of two elements: 1 and 2.
c('a', 'b') # creates a character vector of two elements: a and b.
c(T,F) # creates a logical vector of two elements: TRUE and FALSE.

c(1,1.1,'a',T) # all types (integer, numeric, character and logical) 
# are converted to the 'lowest' type which is character.

vec_int <- c(1,2,3)
vec_char <- c('a','b','c')
vec_int[2] # accessing the second element will return 2
vec_char[2] # accessing the second element will return 'b'

vec_int[2] <- 5 # change the second value from 2 to 5
vec_int # returns [1] 1 5 3

vec_int <- 1:10
vec_int # returns [1] 1 2 3 4 5 6 7 8 9 10

vec_char <- c('a','b','c','d','e')
vec_char[2:4] # returns [1] "b" "c" "d"
vec_char[c(1,3,5)] # returns [1] "a" "c" "e"

## Section 11.6: Expanding a vector with the rep() function ----

# repeat counting numbers, 1 through 5 twice
rep(1:5, 2)

# repeat vector with incomplete recycling
rep(1:5, 2, length.out=7)

# same except repeat each integer next to each other
rep(1:5, each=2)

# automated length repetition
rep(1:5, 1:5)

# hand-fed repetition length vector
rep(1:5, c(1,1,1,2,2))

# repeat counting numbers, 1 through 5 twice
rep.int(1:5, 2)

# repeat vector with incomplete recycling
rep_len(1:5, length.out=7)

# Chapter 12: Date and Time ----

## Section 12.1: Current Date and Time ----

Sys.Date() # Returns date as a Date object

Sys.time() # Returns date & time at current locale as a POSIXct object

as.numeric(Sys.time()) # Seconds from UNIX Epoch (1970-01-01 00:00:00 UTC)

Sys.timezone() # Time zone at current location

# Use OlsonNames() to view the time zone names in Olson/IANA 
# database on the current system:
str(OlsonNames())

## Section 12.2: Go to the End of the Month ----

library(lubridate)

eom <- function(x, p=as.POSIXlt(x)) as.Date(modifyList(p, list(mon=p$mon + 1, mday=0)))

x <- seq(as.POSIXct("2000-12-10"),as.POSIXct("2001-05-10"),by="months")
data.frame(before=x, after = eom(x))


eom('2000-01-01')

## Section 12.3: Go to First Day of the Month ----

date <- as.Date("2017-01-20")

as.POSIXlt(cut(date, "month"))

## Section 12.4: Move a date a number of months consistently by months ----

# Let's say we want to move a given date a numof months. 
# We can define the following function, that uses the mondate package:

# install.packages("mondate")
# library(mondate)

moveNumOfMonths <- function(date, num) {
  as.Date(mondate(date) + num)
}

# Back one month:
moveNumOfMonths("2017-10-30",-1)

# Back two months:
moveNumOfMonths("2017-10-30",-2)

# Chapter 13: The Date class ----

## Section 13.1: Formatting Dates ----

d = as.Date("2016-07-21") # Current Date Time Stamp
format(d,"%a") # Abbreviated Weekday

format(d,"%A") # Full Weekday

format(d,"%b") # Abbreviated Month

format(d,"%B") # Full Month

format(d,"%m") # 00-12 Month Format

format(d,"%d") # 00-31 Day Format

format(d,"%e") # 0-31 Day Format

format(d,"%y") # 00-99 Year

format(d,"%Y") # Year with Century

## Section 13.2: Parsing Strings into Date Objects ----

as.Date('2016-08-01') # in ISO format, so does not require formatting string

as.Date('05/23/16', format = '%m/%d/%y')

as.Date('March 23rd, 2016', '%B %drd, %Y') # add separators and literals to format

as.Date(' 2016-08-01 foo') # leading whitespace and all trailing characters are ignored

as.Date(c('2016-01-01', '2016-01-02'))

## Section 13.3: Dates ----

x <- as.Date("2016-8-23")
x

class(x)

as.Date("23-8-2016", format="%d-%m-%Y") # To read in an European-style date

#
# It tries to interprets the string as YYYY-m-d
#
as.Date("9-6-1962")

as.Date("9/6/1962")
#again interprets as "%Y-%m-%d"

# It has no problem in understanding, if the date is in form YYYY-m-d or YYYY/m/d
#
as.Date("1962-6-9")
 # no problem
as.Date("1962/6/9")
  
# Format Code Meaning
# %d day
# %m month
# %y year in 2-digits
# %Y year in 4-digits
# %b abbreviated month in 3 chars
# %B full name of the month

as.Date("9-6-1962",format="%d-%m-%Y")

as.Date("9-6-1962", "%d-%m-%Y")

as.Date("6Kas1962","%d%b%Y")

as.Date("6 Kas, 1962","%d %b, %Y")

as.Date("Ekim 12, 2016", "%B %d, %Y")

as.Date("12 Ekim, 2016", "%d %B, %Y")


# Chapter 14: Date-time classes (POSIXct and POSIXlt) ----

## Section 14.1: Formatting and printing date-time objects ----

# test date-time object
options(digits.secs = 3)
d = as.POSIXct("2016-08-30 14:18:30.58", tz = "UTC")
format(d,"%S") # 00-61 Second as integer

format(d,"%OS") # 00-60.99? Second as fractional

format(d,"%M") # 00-59 Minute

format(d,"%H") # 00-23 Hours

format(d,"%I") # 01-12 Hours

format(d,"%p") # AM/PM Indicator

format(d,"%z") # Signed offset

format(d,"%Z") # Time Zone Abbreviation

# See ?strptime for details on the format strings here, as well as other formats

## Section 14.2: Date-time arithmetic ----

## adding/subtracting times - 60 seconds
as.POSIXct("2016-01-01") + 60

## adding 3 hours, 14 minutes, 15 seconds
as.POSIXct("2016-01-01") + ( (3 * 60 * 60) + (14 * 60) + 15)

## Günde 5 saat R çalışması ile toplam 10 bin saat çalışabilmek için,
## 2 bin gün geçmesi gerekiyor. 2000 gün sonra hangi tarihte olacağız.

as.POSIXct(Sys.time()) + 2000*24*60*60

as.POSIXct("2016-01-01") +
  as.difftime(3, units="hours") +
  as.difftime(14, units="mins") +
  as.difftime(15, units="secs")

# using POSIXct objects
difftime(
  as.POSIXct("2016-01-01 12:00:00"),
  as.POSIXct("2016-01-01 11:59:59"),
  unit = "secs")
# Time difference of 1 secs

## Doğum tarihimden bugüne ne kadar (gün) geçti?

difftime(
  as.POSIXct(Sys.time()),
  as.POSIXct("1986-10-27 14:00"),
  unit = "days"
)

# To generate sequences of date-times use seq.POSIXt() or simply seq

## Section 14.3: Parsing strings into date-time objects ----

as.POSIXct("11:38", # time string
           format = "%H:%M") # formatting string

strptime("11:38", # identical, but makes a POSIXlt object
         format = "%H:%M")

as.POSIXct("11 AM",
           format = "%I %p")

# Note that date and timezone are imputed.
as.POSIXct("11:38:22", # time string without timezone
           format = "%H:%M:%S",
           tz = "America/New_York") # set time zone

as.POSIXct("2016-07-21 00:00:00",
           format = "%F %T") # shortcut tokens for "%Y-%m-%d" and "%H:%M:%S"


