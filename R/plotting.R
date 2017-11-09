
#' @export
theme_tidy <- function(){
  tidyquant::theme_tq() %+replace%
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          plot.title = element_text(vjust = 1.5,size = 24),
          legend.title = element_text(size = 12),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          strip.text = element_text(size = 12))
}


#' Save a ggplot2 object as a .png
#' @description Save ggplot2 object as a .png. If a folder called Plots exists
#' in your current working directory then the plot will be saved to this location.
#' @param plot the plot to save
#' @param title name to give file
#' @param pwidth width of the plot in pixels
#' @param pheight heigh of the plot in pixels
#' @export
save_plot_png <- function(plot, title,
                          pwidth = 790,
                          pheight = 553){

  filename <- ifelse(dir.exists("./Plots"),
                     paste("./Plots/", title, ".png", sep = ""),
                     paste(title,".png", sep = ""))

  png(filename, pwidth, pheight, bg="white")

  print(plot)
  dev.off()

}
