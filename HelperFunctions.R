# ===================================================================
# 1. Functions 
# 1.1 General helpers

library(stringr)

# Generalised input grabber
GetInput <- function (prompt="Come come, elucidate your thoughts: ", default="") 
{
  if (interactive()) {
    # Interactive R
    readline(prompt=prompt) -> x
  } else {
    # Rscript
    cat(prompt)
    readLines("stdin", n=1) -> x
  }
  
  if (x == "")
    return(default)
  else
    return(x)
}

# Display as a fixed width field, truncating or padding as required.
# Requires library stringr
str_field <- function (string, width=1, side="right") {
  Field = data.frame()
  print(string)
  
  for (row in string) {
    string[row] -> s
    print(string[row])
    if (is.na(s)) "<NA>" -> s
    
    if(nchar(s) < width) 
      nchar(s) -> l
    else
      width -> l
    
    rbind(Field, str_pad(substr(s,1,l), width=width, side=side))
  }
  return(str_pad(substr(s,1,l), width=width, side=side))
}

# Wrap(String, Width) -> String (vector)


