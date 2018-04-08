linReg <- function(x, y, z) {
  require(ggplot2)
  mod <- lm(y ~ x, data = z)
  sumMod <- summary(mod)
  
  writeLines("")
  
  cat(paste0("Adjusted R-squared correlation coefficient = ", round(sumMod$adj.r.squared, 3)))
  
  writeLines("")
  
  cat(paste0("Equation: ", "y = ", round(mod$coefficients[2], 2), "x", " + ", 
               round(mod$coefficients[1], 2)))
  
  writeLines("")
  
  pVal = 1 - pf(sumMod$fstatistic[1], sumMod$fstatistic[2], sumMod$fstatistic[3]) 
  
  cat(paste0("p-value = ", round(pVal, 5)))
  capOb <- paste0("R-squared = ", round(sumMod$adj.r.squared, 3), "; ", "p-value = ", round(pVal, 5))
  
  writeLines("")
  
  if (pVal < 0.05) cat("The correlation is considered to be statistically significant")
  else cat("The correlation is not considered statistically significant")
  
  writeLines("")
  writeLines("")
  
  shapTest <- shapiro.test(sumMod$residuals)
  
  if (shapTest$p.value > 0.05) cat("")
  else cat("WARNING - The distribution of the residuals is significantly different from normal,
           the assumptions for linear regression may not be valid")
  
  writeLines("")
  writeLines("")
  
  ggplot(z, aes(x, y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
    labs(title = "Linear Regression Plot", caption = capOb)
}
