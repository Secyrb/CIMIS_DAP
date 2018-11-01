RCIMIS_setArgs = function(start,end,targets,unitOfMeasure,prioritizeSCS,includeQC)
{
  start <- readline(prompt = "Please enter your start date YYYY-MM-DD: ")
  start <- as.Date.character(start)
  
  end <- readline(prompt = "Please enter your end date YYYY-MM-DD: ")
  end <- as.Date.character(end)
  
  targets <- readline(prompt = "Please enter CIMIS station #: ")
  targets <- as.numeric(targets)
  
  unitOfMeasure <- readline(prompt = "Please enter desired unit of measure 'M' for metric units, 'E' for empirical: ")
  unitOfMeasure <- as.character(unitOfMeasure)

  
  prioritizeSCS <- readline(prompt = "Should results from Spatial CIMIS (interpolated) be prioritized? (TRUE|FALSE): ")
  prioritizeSCS <- as.logical(prioritizeSCS)
  
  includeQC  <- readline(prompt = "Should columns with quality control flags be included in results? (TRUE|FALSE): ")
  includeQC  <- as.logical(includeQC)
  
  
  print(start)
  print(end)
  print(targets)
  print(unitOfMeasure)
  print(prioritizeSCS)
  print(includeQC)
}