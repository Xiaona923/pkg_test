#' plot_lsurvROC is used to fit flexible regression models to estimate the longitudinal marker threshold at controlled specificity levels and the corresponding sensitivity levels.
#'
#' It returns model results
#' then plots the time-varying coefficients with 95% confidence interval.
#'
#' @param model a model result from lsurvROC( )
#' @param my.newdat a data.frame used for calculating the conditional ROC curve
#' @param tau a vector of tau values used in the model
#' @param basis basis function used in the model
#' @param tol a constant used for removing unconverged results
#' @param add if TURE, add the ROC curve on an existing plot
#' @param col color of the curve
#' @param lty line type
#' @param main plot title
#' @examples
#'   model.results <- lsurvROC(dat.long = example_data$data.long, 
#'     dat.short = example_data$data.short,
#'     cutoff.type.basis = "FP",
#'     sens.type.basis = "FP", 
#'     covariate1 = c("Z", "Zcont"), 
#'     covariate2 = c("Z", "Zcont"), 
#'     tau = seq(0.1, 0.9, 0.05), 
#'     time.window = 1, 
#'     nResap = 50, 
#'     show_plots = FALSE
#'     )
#'  
#'  plot_lsurvROC(model = model.results$models, 
#'    my.newdat = data.frame(vtime = 0.5, Z = 1, Zcont = 0.25), 
#'     tau = seq(0.1, 0.9, 0.05),
#'     basis = "FP", 
#'     tol = 1e3, 
#'     add = FALSE,
#'     col = "black", 
#'     lty = 1)
#' @import ggplot2
#' @import dplyr
#' @importFrom quantreg rq
#' @import survival
#' @import splines
#' @importFrom graphics abline axis curve lines par title
#' @importFrom stats approxfun as.formula binomial coefficients formula glm model.matrix na.omit
#'            predict quantile rexp sd stepfun
#' @export
plot_lsurvROC <- function(model, my.newdat, tau, basis, tol = 1e3, add = FALSE,
                                col = "black", lty = 1, main = NULL){

  model_results <- model
  
  #get the main ROC curve
  my.ROC <- get_ROC(model = model_results$model.results$sensitivity.model,
                  basis, 
                  my.newdat,
                  tau, 
                  tol)
  rownames(my.ROC$ROC) <- NULL
  
  #get perturbed ROC curves
  ROC.resap <- lapply(model_results$resap.results, 
                      function(x){get_ROC(x$sensitivity.model, basis, my.newdat, tau)})
  
  AUC.sd = sd(unlist(lapply(ROC.resap, "[", "AUC")))
  
  plot_ROC(my.ROC$ROC, my.add = add, my.col = col, my.lty = lty, my.main = main)
  
  return(list(ROC.results = my.ROC, 
              AUC.sd = AUC.sd,
              ROC.resap = ROC.resap))
}
