point_line_distance <- function(b, m, x, y)
  (y + (m*x + b))/sqrt(m^2 + 1) 

find_group_coefficients <- function(data){
  coef <- t(sapply(levels(data$participants), 
                   function(grp) coefficients(lm(IQ ~ Alcohol_Consumption, data=data[data$participants==grp,]))))
  coef[!is.na(coef[,1]) & ! is.na(coef[,2]),]
}
striped_scatterplot <- function(formula, grouped_data){
  # blue on top and red on bottom, to match the Wikipedia figure
  colors <- rev(rainbow(length(levels(grouped_data$participants)), end=2/3))
  plot(formula, grouped_data, bg=colors[grouped_data$participants], pch=21, asp=1)
  grp_coef <- find_group_coefficients(grouped_data)
  # if some coefficents get dropped, colors won't match exactly
  for (r in 1:nrow(grp_coef)) 
    abline(grp_coef[r,1], grp_coef[r,2], col=colors[r], lwd=2)
}

