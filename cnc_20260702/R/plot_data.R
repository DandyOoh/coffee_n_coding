#' create a plot of our data
#' @param data the target object with relevant data
#' @returns a lovely plot of our data


plot_data <- function(data){
  
  plot <- data |>
    ggplot() +
    geom_col(aes(x=as.character(group), y=avg_los), fill = "skyblue") +
    labs(title = "Really great insights on length of stay",
         subtitle = "Woohoo, let's go!",
         x = "Group",
         y = "Value")
  
  return(plot)
}