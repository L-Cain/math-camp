#' ---
#' title: "1. Objects and Logic"
#' author: "Luke Cain"
#' output: pdf_document
#' ---

#Purpose: Explain the different types of objects, their interactions, and boolean logic


#----Setup----
#Comments start with a #. Provide a comment any time the code is not completely obvious. 
  ##comment:code < 1:10

#Clear environment  
rm(list = ls())
# 
# Comment out
# all 
# this
# text with 
# 'ctrl (or cmd)+shift+c'
# then undo with 'ctrl (or cmd) z'

#line numbers are to the left. Go to line 86

#I forgot how to spell, corect all misspellings of 'apple' 'aple'

#delineate sections like this, and can be collapsed

#----Example section----
#Hide this with the arrow


#----Numbers and calculation----
#Basic calculations (note they're going to the terminal)
3+8
3*8
3/8
3^8
8%%3

#Built-in functions
log(3)
exp(8)
sin(-1.5*pi)
abs(-38)

#Looking up functions
?log()


#----Strings----
'this is a string'
"so, too, is 'this'"

#This throws an error
#3+"8"

#Some functions require strings
tolower('TEST')

#Going back-and-forth
as.numeric('308')
as.character(308)


#----Defining objects----
#a+b

a<-3
  ##'a' is now in environment
b<-8
(c<-a+b)

#Naming conventions
This_name_is_too_long = TRUE
##these are non-descript and probably bad
plot = '...'
plot2 = '...'
data_final_2 = '...'

#If you use a number, string, etc. more than once, you should make it a variable!
#----Logics----

TRUE
  ##great, return to line 23.
FALSE
NA
NULL
TRUE & FALSE
T | F



3 == 3
b == 8
a!=b

a>b
b>b
b>=b

is.numeric(b)
is.numeric('b')


#order can matter
8%%3 == 1 & a == a | b>a
8%%3 == 1 & (a ==a | b>a)


#Exercise 1: test whether a variable, "c" is more than 30 and divisble by 12, or less than 800 and a perfect square
(c>30 & c%%12 == 0) | (c<800 & c^.5 %% 1 == 0)

#booleans are also binary
TRUE + TRUE
T*F

#NAs mess things up.
c<-NA
c>a
T+c


#----Vectors----
letter_vec<-c('x','y','aple','z')
int_vec<- seq(0,30,3)

mixed_vec<-c(a,'b',log(3),letter_vec,int_vec)
typeof(mixed_vec)


#Index
letter_vec[3]
int_vec[4]*int_vec[9]
letter_vec[3] = 'banana'

length(int_vec)

#Vectors of logic
letter_vec == 'y'

letter_vec[3] == 'aple'

int_vec > b


#Subsetting
letter_vec_subset<-letter_vec[2:3]
int_vec_subset<-int_vec[int_vec>b*2]
  

#Exercise 2: Make every third entry in int_vec copy the entry before it. Make it robust to changes in the length of int_vec
int_vec2<-int_vec
int_vec2[seq(3,length(int_vec),3)]<-int_vec[seq(2,length(int_vec)-1,3)]


#Accessing "everything but" uses negative values
letter_vec[-3]

#Appending vectors
letter_vec[5]<-'a'
c(letter_vec,'b')

#Vector operations
int_vec2+int_vec

#Vectors that are too short will be "recycled"
int_vec_subset+int_vec[-10]


#Exercise 3: what is the intersect of the following vectors? What is the union?
int_vec
evens<-seq(0,30,2)

c(int_vec,evens)
intersect<-unique(int_vec[int_vec %in% evens])

#----Matrices----
#Generating
examp_mat <- matrix(1:9, 
                    nrow = 3, 
                    ncol = 3)

(diag_mat<-diag(3)*2)


#Index: row, column
examp_mat[1,3]


#Operations
diag_mat*examp_mat
diag_mat%*%examp_mat

diag_mat+examp_mat


#Logic
diag_mat==0

#Exercise 4: Create a 3x8 matrix that alternates between the first 3 positive integers and the first 3 letters
letters<-c('a','b','c')
numbers<-c(1,2,3)

letter_number<-c(letters,numbers)
matrix(data = c(letter_number,letter_number,letter_number,letter_number),
       nrow = 3,
       ncol= 8)

#----Data frames----
# Create a data frame
students <- data.frame(
  name = c("Alice", "Bob", "Charlie", "Diana"),
  age = c(20, 22, 19, 21),
  major = c("Biology", "Physics", "Chemistry", "Biology"),
  gpa = c(3.8, 3.6, 3.9, 3.7),
  graduated = c(FALSE, FALSE, FALSE, TRUE)
)

#Open and investigate, or...

# Dimensions (rows, columns)
dim(students)
nrow(students) 
ncol(students)  

# Column names
names(students)
colnames(students)

# First and last few rows
head(students)      # first 6 rows by default
head(students, 3)   # first 3 rows
tail(students)


#Specific Columns are vectors
students$name
students[, "name"]

#Index like a matrix
students[4,'gpa']

#Exercise 5: Extract Charlie's age by 
  #a) indexing numerically
students[3,2]
  #b) extracting his entry from the age vector
students$age[3]
  #c) indexing to the row with "Charlie" in it, and the 'age' column
students[students$name == 'Charlie','age']
  #d) same as c), but extract all of Charlie's information except whether he's graduated
students[students$name == 'Charlie',2:4]



#We can add new columns
students$fruit<-c('aple','pear',NA,'banana')

#and modify specific cells
students[3,'fruit']<-'orange'

#Exercise 6: Boost every bio major's GPA by 10%
students[students$major=='Biology',"gpa"]<-students[students$major=='Biology',"gpa"]*c(1.1,1.1)


#Fruits are irrelevant, let's remove them
students[,-6]


#----Lists----
#lists can contain anything...much like sets!
example_list<-list(students = students, 
                   fruit = 'aple', 
                   numbers= int_vec)

#indexing is a little more complicated. 
example_list[[2]]
example_list[[3]][4]
example_list[[1]][3,2]

#named elements
example_list$students$major

#Lists can contain anything...even messes.
big_list<-list(example_list,list(students,example_list,list('aple',example_list)))


#Lists are a lot like sets
giraffes <- list(names = c('Molly','Isaac','Bobby'),
                kingdom = 'Mammal',
                size = 'Large',
                legs = 4,
                sexes = c('Female','Male','Male'),
                ages = rep('adult',3),
                herbivorous = T)
snakes<- list(names = c('Anaconda','Worm Snake','Diamondback'),
              kingdom = 'Reptile',
              size = c('Large','small','medium'),
              legs = 0,
              sexes = c('Feale','Male','Female'),
              ages = c(rep('adult',2),'juvenile'),
              herbivorous = T)
seals <- list(names = c('Flipper','Dipper','Zipper'),
              kingdom = 'Mammal',
              size = c('Medium','Large','Large'),
              legs = 0,
              sexes = c('Male','Male','Female'),
              ages = c('juvenile','adult','adult'),
              herbivorous = F)
frogs <- list(names = c('Bud','Weis','Er'),
              kingdom = 'Amphibian',
              size = c('Small','Small','Small'),
              legs = 4,
              sexes = c('Female','Male','Female'),
              ages = c('adult','adult','adult'),
              herbivorous = F)
kangaroos <- list(names = c('Ausie','Zipper','Roo'),
                  kingdom = 'Amphibian',
                  size = c('Large','Large','Medium'),
                  legs = 2,
                  sexes = c('Female','Male','Female'),
                  ages = c('adult','adult','Juvenile'),
                  herbivorous = T)

zoo<-list(snakes,giraffes,seals,frogs,kangaroos)

#Exercise 7: 
#Return all the bipeds
#Return all carnivorous mammals
#Return non-large, adult, non-mammals
#Return everything but the kangaroos

#There are other object types, including functions, plots, and many more...
