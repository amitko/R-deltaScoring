dS.logitDeltaFit <- function(itemData,Dscore,o = dS.options() ) {
  
  oldt = ds.observedLogitDelta(itemData,Dscore,o);
  # 
  # if (o$model == 1) {}
  #   nParams = 1;
  #   Model_lb = 0.01;
  #   Model_ub = 0.99;
  #   start_point = 0.5;
  # }
  # else if (o.$model == 2) {
  #   nParams = 2;
  #   Model_lb = [0.01, 0.01];
  #   Model_ub = [0.99, 5];
  #   start_point = [0.5, 1];
  # }  
  # elseif o.model == 3
  # nParams = 3;
  # Model_lb = [0.01 0.01 0];
  # Model_ub = [0.99 5 0.5];
  # start_point = [0.5 1 0.1];
  # else
  #   error('Unsuported model!!!');
  # end
  
params = data.frame(matrix(ncol = o$model, nrow = ncol(oldt)))  
params_se = data.frame(matrix(ncol = o$model, nrow = ncol(oldt)))  
params_p = data.frame(matrix(ncol = o$model, nrow = ncol(oldt)))  

for (k in 1:ncol(oldt)) {
  tt = oldt[,k];
  tt[which(tt == 0)] = 0.001;
  dd = data.frame(o$dScale,tt);
  print(k)
  colnames(dd) <- c('x','y');
  print(dd)
  m<-nls(o$Models[2],data = dd,start = list("b" = 0.5, "s" = 1), lower = list("b" = 0.01, "s" = 0.3), upper = list("b" = 0.99, "s" = 5));
  
  print(summary(m));
  
  params[k,] = summary(m)$coefficients[,1];
  params_se[k,] = summary(m)$coefficients[,2];
  params_p[k,] = summary(m)$coefficients[,4];
}
  


}