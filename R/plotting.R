
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
                          width = 790,
                          height = 553){

  fileName <- paste(title,".png")
  png(fileName, width = width, height = height, bg="white")
  print(plot)
  dev.off()

}
