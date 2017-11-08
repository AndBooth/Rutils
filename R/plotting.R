
#' @export
my_theme <- function(){
  theme_bw() %+replace%
    theme(
      axis.title = element_text(size = 22),
          axis.text = element_text(size = 18),
          plot.title = element_text(vjust = 1.5,size = 24),
          legend.title = element_blank(),
          legend.text = element_text(size=18),
          legend.key = element_blank(),
          strip.text = element_text(size = 22)
      )
}



#' @export
save_plot_png <- function(plot, title,
                          pwidth = 790,
                          pheight = 553){

  if(!dir.exists("./Plots")) {
    filename <- paste(title,".png")
    png(filename, pwidth, pheight, bg="white")
  } else {
    filename <- paste("./Plots/", title, ".png")
    png(filename, pwidth, pheight, bg="white")
  }
  print(plot)
  dev.off()

}
