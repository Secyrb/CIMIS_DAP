CIMIS_API_KEY <- function(Rcimis_key)
{
  Rcimis_key <- readline(prompt = "Please enter your private CIMIS API key.")
  Rcimis_key <- as.character(Rcimis_key)
  print("Thank you, your is private and will not be displayed during any code output.")
}