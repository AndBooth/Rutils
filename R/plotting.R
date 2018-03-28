
#' @export
theme_tidy <- function(){
  ggplot2::theme_minimal() %+replace%
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
#' @param subfolder a subdirectory of existing 'Plots' directory where current plot
#' should be saved
#' @param pwidth width of the plot in pixels
#' @param pheight heigh of the plot in pixels
#' @export
save_plot_png <- function(plot, title,
                          subfolder = NULL,
                          pwidth = 790,
                          pheight = 553){

  if(dir.exists("./Plots")) {
    if(!is.null(subfolder)) {
      filename <- paste("./Plots/", subfolder, "/", title, ".png", sep = "")
    } else {
      filename <- paste("./Plots/", title, ".png", sep = "")
    }
  } else {
    filename <- paste(title,".png", sep = "")
  }

  png(filename, pwidth, pheight, bg="white")

  print(plot)
  dev.off()

}
