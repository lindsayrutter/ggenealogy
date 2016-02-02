my_ellipsis_function <-function(...) {
  #input_list <- get_list_from_ellipsis(...)
  #input_list <- as.list(substitute(list(...)))[-1L]
  input_list <- list(...)
  output_list <- lapply(X=input_list, mean)
  return(output_list)
}

my_ellipsis_function(a=1:10,b=11:20,c=21:30)
