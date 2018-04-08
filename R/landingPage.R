splitTest <- function(landA, conA, landB, conB) {

  require(ggplot2)
  require(tidyr)

  pageA <- c(conA, landA - conA)
  pageB <- c(conB, landB - conB)

  contFrame <- data.frame(pageA, pageB)

  test <- chisq.test(contFrame)

  pVal <- round(test$p.value, 5)

  writeLines("")
  writeLines("")

  cat(paste0("Version A conversion rate = ", round((conA / landA) * 100), 1), "%")
  writeLines("")
  cat(paste0("Version B conversion rate = ", round((conB / landB) * 100), 1), "%")

  writeLines("")
  writeLines("")

  cat(paste0("p-value = ", pVal))

  writeLines("")
  writeLines("")

  if (pVal < 0.05) cat("The difference is considered to be statistically significant")
  else cat("The difference is not considered statistically significant")

  writeLines("")
  writeLines("")

  VersionA <- c(rbeta(1000, conA, (landA - conA)))
  VersionB <- c(rbeta(1000, conB, (landB - conB)))

  rateFrame <- data.frame(VersionA, VersionB)

  rateFrameLong <- gather(rateFrame, Version)

  ggplot(rateFrameLong, aes(value, fill = Version)) + geom_density(alpha = 0.7) +
    labs(title = "Monte-Carlo Estimated Conversion Rate", x = "Conversion Rate",
           y = "Probability Density", caption = "MC simulation based on 1000 runs")

}
