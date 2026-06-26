# test

iris2 <- 
  iris %>% 
  mutate(pasted = map(Sepal.Length, ~ paste(sample(c("a", "b", "c"), 
                                   .x, replace = T), collapse = "")))

stor <- c()

for(i in 1:9){
  stor <- c(stor, i)
}

for(i in iris){
  print()
}