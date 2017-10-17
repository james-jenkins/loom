library(png)
library(tidyverse)
library(cowplot)

set.seed(1234)

ref    <- readPNG("comp.png")
pix_x  <- dim(ref)[1]
pix_y  <- dim(ref)[2]
pixels <- length(ref)

stringToPath <- function(chromosome, 
                         pins   = 90, 
                         radius = 1,
                         center.x = 0,
                         center.y = 0)
{
  factor = 2*pi/pins
  data.frame(chromosome = as.integer(chromosome)) %>% 
    mutate(x = center.x + radius*cos(factor*chromosome),
           y = center.y + radius*sin(factor*chromosome))
}

constructLoom <- function(chromosome, 
                          trial.name = "trial.png")
{
  path <- stringToPath(chromosome,
                       pins     = 359,
                       center.x = pix_x/2, 
                       center.y = pix_y/2, 
                       radius   = pix_x/2)
  p <- ggplot(path, aes(x = x, y = y)) +
    theme_nothing() +
    geom_path(size = 0.4) +
    # geom_point(size = 1) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = NULL, y = NULL) 
  png(width = pix_x, height = pix_y, res = 300, trial.name)
  print(p)
  dev.off()
}

evaluate <- function(chromosome,
                     trial.name = "trial.png") 
{
  constructLoom(chromosome, trial.name)
  trial <- readPNG(trial.name)
  abs(sum(trial - ref))/pixels
}

monitor <- function(gen)
{
  chromosome <- gen$population[which.min(gen$evaluation),]
  constructLoom(chromosome, trial.name = paste0("history/best_",gen$iter,".png"))
  plot(gen$best)
  gc()
}

constructLoom(sample(1:45,size = 1000,TRUE))

library(genalg)
gen <- rbga(rep(0,500), rep(359,500), verbose = TRUE,
            evalFunc = evaluate,iters = 2000, popSize = 150,
            monitorFunc = monitor, mutationChance = 0.003)

save(gen, file = "generation1.rds")
