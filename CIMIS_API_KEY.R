CIMIS_API_KEY <- function(Rcimis_key)
{
  Rcimis_key <- readline(prompt = "Please enter your private CIMIS API key.")
  Rcimis_key <- as.character(Rcimis_key)
  print("Thank you, your is private and will not be displayed during any code output.")
}


CIMIS_API_OPT <- function()
{
  options("Rcimis_key" = "94611544-6c5c-40eb-ae87-83c12b6208d3")
}