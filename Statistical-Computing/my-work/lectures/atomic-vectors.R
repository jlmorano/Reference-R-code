
# Vectors
#     Atomic Vectors  (This Video)
#         Matrices
#         Arrays
#     Lists           (Next Video)
#         Data Frames


# Atomic Vectors - What we normally consider as a vector in R
a <- 1:20
a

# "atomic" because each element of "a" is indivisible
# has the additional property that all elements are of the same type
typeof(a)
# ?typeof

# logical
b <- c(TRUE, FALSE, TRUE, FALSE)
typeof(b)
#"logical"

# integer
a
d<- c(1, 2, 3)  #this is a double-precision
typeof(d)
#to force an integer
d<- c(1L, 2L, 3L)

# double
e <- c(5, 8, 3)
typeof(e)
e <- c(5.3, pi, exp(1))
e
typeof(e)

# complex
i
1i
g <- 0 + 1i
typeof(g)
typeof(fft(rnorm(10)))


# character
f <- c("hello", "there")
typeof(f)


# NA, NULL, NaN, -Inf, Inf
#     NA is used to indicate missing values
#     NULL is a generic length-0 object, serving several purposes
a <- c(3, NA, 4.5)
a
a <- c(3, NULL, 4.5)
b <- c(3.7, sqrt(-1), 7.2)
d <- c(3.7, Inf, 7.2)

is.na(a)
is.na(b)
is.na(d)

# try these functions on the other objects to see what they return


# if you try to combine types in a vector, R automatically coerces to "higher" type
# logical < integer < double < complex < character





# NOTES:
# automatic coercion is a common source of silent bugs
# R does not distinguish between scalar and length-1 vector
# look at how "c" function is used above
# can use '' or "" for strings but most people use ""

# coercion can be useful - some functions do it automatically 

# R has functions for manual coercion

# "as" function less often used,
# but could be handy if you have to loop over types stored in variable

# vectors have certain properties

# length

# names

# names can be set with "names" function or specified when vector is created

# our first example of a replacement function for setting an attribute of an object
# ?names

# also works for length    

# remove names with unname or as.vector

# Exercise: What do as.vector and is.vector do?


# DIGRESSION: <- vs =
# try to use <- when assigning a value to a variable
# vs
# they do the same thing, but note that = is used for arguments to "c" function

# you actually can use <- in arguments, but this sets a value
# This is confusing and not advisable. To keep things straight,
# use  <-  for assignment outside functions,
# use  =   for setting arguments in functions


# dim attribute for matrices

# a matrix is simply a vector with a "dim" attribute
# note how values get ordered - this is called "column major" ordering

# can also define matrices directly with "matrix" function
# ?matrix


# "class" function returns c("matrix","array")


# an array is a d-dimensional object for d >= 2

# we'll study the purpose of the class of an object later
# for now, you can view it as a way to get info about an object

# recycling: Many R objects use the concept of recycling to fill out objects

# Exercises:
#     special vectors: factors, dates, date-times

# generic attributes - metadata for R objects

# get and set them with attr function
