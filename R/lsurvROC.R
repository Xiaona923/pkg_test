#' lsurvROC
#'
#' A list of two example data frames used for demonstrating package functionality.
#' It returns model results
#' then plots the time-varying coefficients with 95% confidence interval.
#'
#' @param dat.long a data frame with long data format. It should contain subject id (id), longitudinal marker (Xt), measurement time (vtime)
#' @param dat.short a data frame. It should contain unique subject id (id), observed event time (Y), event indicator (delta, 1 = observed event, 0 = censored), covariates
#' @param cutoff.type.basis basis function to estimate marker threshold ("FP" = fractional polynomial, "bs" = "B splines", "linear" = linear basis)
#' @param sens.type.basis basis function to estimate sensitivity levels ("FP" = fractional polynomial, "bs" = "B splines", "linear" = linear basis)
#' @param covariate1 a vector of name of covariates to estimate marker thresholds at level tau(s)
#' @param covariate2 a vector of name of covariates to estimate sensitivity levels
#' @param tau the quantile(s) of marker threshold to be estimated
#' @param time.window a numeric value. This value will be used to specify the future time point to perform the evaluation, meaning vtime + time.window 
#' @param nResap a numeric value to perform perturbation resampling. The default value = 200. Larger value can generate more accurate ASE value
#' @param show_plots if TRUE then the time-varying coefficients plot will be displayed
#' @param nknot if basis function is B splines, need to specify the number of knot
#' @param tol a constant used for removing unconverged results
#' @examples
#'   data(example_data)
#'   head(example_data$data.long)
#'   head(example_data$data.short)
#'   
#'   res <- lsurvROC(dat.long = example_data$data.long, 
#'     dat.short = example_data$data.short,
#'     cutoff.type.basis = "FP",
#'     sens.type.basis = "FP", 
#'     covariate1 = c("Z", "Zcont"), 
#'     covariate2 = c("Z", "Zcont"), 
#'     tau = c(0.7, 0.8, 0.9), 
#'     time.window = 1, 
#'     nResap = 50, 
#'     show_plots = TRUE
#'     )
#' @import ggplot2
#' @import dplyr
#' @importFrom quantreg rq
#' @import survival
#' @import splines
#' @export
lsurvROC <- function(dat.long, dat.short,
                               cutoff.type.basis, sens.type.basis, 
                               covariate1 = 1, covariate2 = 1, 
                               tau, time.window, nResap = 200, 
                               show_plots = TRUE, nknot = NULL, tol = 1e3){
 
  model_results <- lsurvROC_main(dat.long, 
                            dat.short, 
                            time.window,
                            cutoff.type.basis,
                            sens.type.basis, 
                            covariate1, 
                            covariate2, 
                            tau, 
                            nResap,
                            nknot)

  vtime = sort(unique(dat.long$vtime))
  
  #extract coefficients
  cutoff_coef = coefficients(model_results$model.results$cutoff.model$model)
  sens_coef = as.data.frame(lapply(model_results$model.results$sensitivity.model,
                                   function(x) coefficients(x$model)))
  #check convergence and extreme values
  sens_converge = sapply(model_results$model.results$sensitivity.model, 
                         function(x){y = x$model; y$converged & !any(y$coefficients >= tol)})

  if (any(!sens_converge)) {
    warning(
      paste0(sum(!sens_converge),
             " sensitivity model(s) did not converge; delete tau = ",
             paste0(tau[which(!sens_converge)],collapse = ", "))
    )
  }
  
  cutoff_resap = lapply(model_results$resap.results, 
                        function(x){
                          cutoff_resap = coefficients(x$cutoff.model$model)
                          })
  sens_resap = lapply(model_results$resap.results, 
                      function(x){sens_resap = as.data.frame(
                        lapply(x$sensitivity.model, function(x) coefficients(x$model))
                                                             )})
  sens_resap_converge <- lapply(model_results$resap.results, 
                                function(x){sens_conv = as.data.frame(
                                  lapply(x$sensitivity.model, function(y){y$converged})
                                  )})
  
  #retain results only if the model has converged
  
  sens_resap_clean <- mapply(function(x, y){
                            extreme_val <- apply(x, 2, function(q) any(abs(q) > tol)) 
                            x[ , !y | extreme_val] <- NA
                            Z = x[, sens_converge]
                              }, 
                       sens_resap, sens_resap_converge, SIMPLIFY = FALSE)

  
 
  #plot cutoff model time-dependent coef
  ncovari1 <- length(covariate1)
  cutoff_plot <- function(){
    tau_values <- unique(range(tau))
    plot_beta_main(coefficients = cutoff_coef,
              beta.Resap = cutoff_resap,
              basis = cutoff.type.basis,
              covariates = covariate1,
              visit.time = vtime, 
              titles = unlist(sapply(0:ncovari1, function(i) bquote(beta[.(paste(i)) ~ ","~ tau ~ "= ["~.(paste(tau_values, collapse = ", ")) ~ "]"] ~ "(s)"))),
              reverse = 0, 
              nknot, 
              tol)}
  
  #plot sens model time-dependent coef
  ncovari2 <- length(covariate2)
  sens_plot <- function(){ 
    tau_values <- unique(range(tau[sens_converge]))
    plot_beta_main(coefficients = sens_coef[sens_converge], 
              beta.Resap = sens_resap_clean, 
              basis = sens.type.basis,
              covariates = covariate2,
              visit.time = vtime, 
              titles = unlist(sapply(0:ncovari2, function(i) bquote(gamma[.(paste(i)) ~ ","~ tau ~ "= ["~.(paste(tau_values, collapse = ", ")) ~ "]"] ~ "(s)"))),
              reverse = 0, 
              nknot, tol)}
  

  if (isTRUE(show_plots)) {
    cutoff_plot()
    sens_plot()
  }
  
  invisible((list(cutoff_plots = cutoff_plot,
                  sens_plots = sens_plot)))
  
  output <- list(models = model_results,
                 plots = list(cutoff = cutoff_plot, 
                              sensitivity = sens_plot))
  
  return(output)
  
}

#' @keywords package function 
