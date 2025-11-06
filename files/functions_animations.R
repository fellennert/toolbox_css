library(magick)

world_animation <- function(worlds) {
    img_list <- list()
  
  for (i in seq_along(worlds)) {
    img <- image_graph(width = 800, height = 800, res = 72)
    world_print(worlds[[i]],i)
    dev.off()
    img_list[[i]] <- img
  }
  img_joined <- image_join(img_list)
  animation <- image_animate(img_joined, fps = 2)
  print(animation)
}
