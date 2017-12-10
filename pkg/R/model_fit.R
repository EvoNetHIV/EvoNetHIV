#' @title Title
#'
#' @description Description
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here


#' @export
model_fit <- function(model, outpath, name) {
 
  # Calculate least-squares statistic for prevalence
  sa.year <- c(seq(1,13,1), 15, 18, 22)
  sa.prev <- c(.002, .005, .010, .019, .031, .048, .067, .088, .108, .126, .141,
               .153, .156, .162, .169, .188)
  
  model.year <- sa.year * 365/model$param[[1]]$popsumm_freq
  model.prev.sims <- t(sapply(model$popsumm, function(x) {
    rbind(x$prevalence[model.year])
  }))
  model.prev <- colMeans(model.prev.sims)
  
  q.ls <- sum((sa.prev - model.prev) ^ 2)
  
  save(q.ls, file = file.path(outpath, paste(name, "_LSstat.RData", sep = "")))
  
  # Plot of prevalence
  # 95% CI is not available for year 2002, so is excluded from plot
  lb <- c(0.002, 0.005, 0.009, 0.017, 0.029, 0.044, 0.063, 0.084, 0.103, 0.121, 0.136, 0.147, 0.149, 0.155, 0.175)
  ub <- c(0.003, 0.006, 0.011, 0.020, 0.033, 0.050, 0.071, 0.092, 0.113, 0.131, 0.146, 0.158, 0.177, 0.184, 0.203)
  
  sa.year.ci <- c(seq(1,12,1), 15, 18, 22)
  
  prev.band <- list(c(sa.year.ci, rev(sa.year.ci)),
                    c(c(lb, rev(ub))))
  
  pdf(file = file.path(outpath, paste(name, ".pdf", sep = "")), width = 10)
  plot(1:23, 1:23, col = "white", xlab = "Year", ylab = "Prevalence", main = "Prevalence",
       ylim = c(0.0, max(c(ub, model.prev))), xaxt = "n")
  
  x.axis <- c(seq(1990,2002,1), 2005, 2008, 2012)
  axis(1, at = sa.year, labels = x.axis)
  
  polygon(prev.band[[1]], prev.band[[2]], border = NA, col = adjustcolor("green3", alpha.f = 0.4))
  lines(x = sa.year, y = sa.prev, col = "green3", lwd = 2)
  
  for(i in 1:nrow(model.prev.sims)) {
    lines(x = sa.year, y = model.prev.sims[i, ], col = "dimgray", lty = 2, lwd = 1)
  }
  
  lines(x = sa.year, y = model.prev, col = "black", lty = 2, lwd = 2)
  
  legend("topleft", legend = c("Observed prevalence and 95% CI", "Model output mean prevalence",
                               "Model output individual simulations"),
         col = c(adjustcolor("green3", alpha.f = 0.4), "black", "dimgray"),
         pch = c(15, NA, NA), lty = c(1, 2, 2), lwd = c(2, 2, 1), pt.cex = 2, bty = "n")
  
  
  legend("topleft", legend = c("", "", ""), col = "green3", pch = c(rep(NA, 3)), pt.cex = 2, lty = c(1, NA, NA), bty = "n",
         lwd = 2)
  
  dev.off()

}