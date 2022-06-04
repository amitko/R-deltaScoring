dS.PCR <- function(parameters,Dscore,o = dS.options()) {
  
  res = matrix(nrow = length(Dscore),ncol = nrow(parameters));
  
  for (k in 1:length(Dscore)) {
    if ( o$model == 1){
      res[k,] = 1 / ( 1 + ((1-Dscore[k])*parameters[,1]) / ((1 - parameters[,1]) * Dscore[k]));
    }
    if ( o$model == 2){
      res[k,] = 1 / ( 1 + ((1-Dscore[k])*parameters[,1]) / ((1 - parameters[,1]) * Dscore[k])^parameters[,2]);
    }
    if ( o$model == 3){
      res[k,] = parameters[,3] + (1-parameters[,3])*(1 / ( 1 + ((1-Dscore[k])*parameters[,1]) / ((1 - parameters[,1]) * Dscore[k])^parameters[,2]));
    }
  }
  return(res);
}