data <- c(600, 625, 620, 630, 720, 750, 755, 800, 790)

MA <- function(v, n) {
  N = length(v)
  result = rep(0, N)  
  for (i in 1:N) {
    if (i < n) {
      result[i] = mean(v[1:i]) 
    } else {
      result[i] = mean(v[(i - n + 1):i])  
    }
  }
  return(result)
}

A2 <- MA(data, 2)

# Create a plot
plot(data, type="l", col="black", xlab="Time", ylab="Yield", main="Yield per Year")
lines(A2, col="red")
legend("bottomright", legend=c("Time Series Plot", "2-term moving average"), col=c("black", "red"), lty=1, cex=1)