#' @title Loading text file
#' @param Example.txt This is an example text file
#' @return An object called textfile is created in the global environment.
#' @examples loadtext()
#' @export
#'
#'

loadtext <- function(file_name) {
  # textfile <- read.delim("Example.txt", header=FALSE, sep="") #Loading example text file
  textfile <- read.delim(file_name, header=FALSE, sep="") #Row above changed, file_name added as an argument in fuction to enable any file to be inputted
  textfile <- tolower(textfile) #Changing all characters to lowercase
  textfile <- gsub("\\.","",textfile) #Deleting .
  textfile <- gsub("\\,","",textfile) #Deleting ,
  textfile <<- textfile #Storing in global environment ##Since textfile is being returned seems like you don't need to also store it in the global in this step
  return(textfile)
}
