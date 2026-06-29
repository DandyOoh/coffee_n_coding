#' retrieve data from a specific location in directory
#' @param file where the file is stored
#' @returns a dataframe of hospital admission activity

get_data <- function(file){
  
  data <- read.csv(file)
  
  return(data)
}