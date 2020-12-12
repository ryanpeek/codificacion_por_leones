# playing with lists

vector1 <- c(1, 2, 3) # vectors

vector2 <- c("raccoon", "squirrel", "dog") # vector

list1 <- list(vector1, vector2)

big_list <- list(list1, vector1, vector2)

# to access pieces use [[]]
big_list[[1]][[2]][1] # returns single item from vector in list


# to get away from the list what we can use is unlist
vector3 <- unlist(big_list[[1]])

# unlist the whole list
unlist(big_list)
