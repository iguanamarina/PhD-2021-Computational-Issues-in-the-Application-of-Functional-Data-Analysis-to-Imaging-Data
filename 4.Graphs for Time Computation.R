####################################################################
# SCRIPT FOR GRAPHICS OF COMPUTING TIMES OF SCCs
####################################################################

install.packages("plotly")
install.packages("gapminder")
library(ggplot2)
library(plotly)
library(gapminder)

data_triangulation <- read.csv2("triangulation_times.csv", header=T, sep=";")
data_triangulation <- data_triangulation[,2:3]
View(data_triangulation)

grids <- function(axis = c("xy", "x", "y"), color = "grey92", size = NULL, linetype = NULL)
{
  axis <- match.arg(axis)
  grid.major <- element_line(color = color, size = size,
                             linetype = linetype)
  grid.minor <- element_line(color = color, size = 0.25,
                             linetype = linetype)

  switch(axis,
         xy = theme(panel.grid.major = grid.major, panel.grid.minor = grid.minor),
         x = theme(panel.grid.major.x = grid.major, panel.grid.minor.x = grid.minor),
         y = theme(panel.grid.major.y = grid.major, panel.grid.minor.y = grid.minor)
         )
}

###
# For saving:
tiff("test.tiff", units="in", width=8, height=5, res=300)
ggplot(data_triangulation, aes(n, t)) +
       geom_point() + geom_smooth() +
       theme_classic() + grids(linetype = "dashed") +
       xlab("Fineness Degree (n)") + ylab("Computing Time (s)") +
       ggtitle("Computing times for Delaunay Triangulations") + 
       theme(plot.title=element_text(hjust=1, vjust=0.5, face='bold'))

dev.off()

# For interactive plot:
triang <- ggplot(data_triangulation, aes(n, t)) +
       geom_point() + geom_smooth() +
       theme_classic() + grids(linetype = "dashed") +
       xlab("Fineness Degree (n)") + ylab("Computing Time (s)") +
       ggtitle("Computing times for Delaunay Triangulations") + 
       theme(plot.title=element_text(hjust=1, vjust=0.5, face='bold'))
ggplotly(triang)

###

# ONE-SAMPLE TIMES:

one_sample_times_N8 <- cbind(matrix(rep("N8", length.out=nrow(one_sample_times_N8))), one_sample_times_N8)
one_sample_times_N15 <- cbind(matrix(rep("N15", length.out=nrow(one_sample_times_N15))), one_sample_times_N15)
one_sample_times_N25 <- cbind(matrix(rep("N25", length.out=nrow(one_sample_times_N25))), one_sample_times_N25)

colnames(one_sample_times_N8)[1] <- "N"
colnames(one_sample_times_N15)[1] <- "N"
colnames(one_sample_times_N25)[1] <- "N"
      
data <- rbind(one_sample_times_N8, one_sample_times_N15, one_sample_times_N25)

one_sample <- ggplot(data = data, aes(x = n,  y = t, color = N)) +
  geom_point() +
  geom_smooth() +
  theme_classic() + grids(linetype = "dashed") +
  xlab("Nº Simulated Cases (n)") + ylab("Computing Time (s)") +
  ggtitle("Computing times for One-Sample SCC Estimation") + 
  theme(plot.title=element_text(hjust=1, vjust=0.5, face='bold')) +
  theme(legend.position=c(.9,.9))

one_sample
ggplotly(one_sample)
  
## this is just for saving
tiff("one_sample.tiff", units="in", width=8, height=5, res=300)
ggplot(data = data, aes(x = n,  y = t, color = N)) +
  geom_point() +
  geom_smooth() +
  theme_classic() + grids(linetype = "dashed") +
  xlab("Nº Simulated Cases (n)") + ylab("Computing Time (s)") +
  ggtitle("Computing times for One-Sample SCC Estimation") + 
  theme(plot.title=element_text(hjust=1, vjust=0.5, face='bold')) +
  theme(legend.position=c(.9,.9))

dev.off()

###

# TWO-SAMPLE TIMES:

load("my_data.RData") 

two_sample_times_N8 <- cbind(matrix(rep("N8", length.out=nrow(two_sample_times_N8))), two_sample_times_N8)
two_sample_times_N15 <- cbind(matrix(rep("N15", length.out=nrow(two_sample_times_N15))), two_sample_times_N15)
two_sample_times_N25 <- cbind(matrix(rep("N25", length.out=nrow(two_sample_times_N25))), two_sample_times_N25)

colnames(two_sample_times_N8)[1] <- "N"
colnames(two_sample_times_N15)[1] <- "N"
colnames(two_sample_times_N25)[1] <- "N"
      
data <- rbind(two_sample_times_N8, two_sample_times_N15, two_sample_times_N25)

two_sample <- ggplot(data = data, aes(x = n,  y = t, color = N)) +
  geom_point() +
  geom_smooth() +
  theme_classic() + grids(linetype = "dashed") + ylim(200,600) + xlim(50,500)+
  xlab("Nº Simulated Cases (n)") + ylab("Computing Time (s)") +
  ggtitle("Computing times for Two-Sample SCC Estimation") + 
  theme(plot.title=element_text(hjust=1, vjust=0.5, face='bold')) +
  theme(legend.position=c(.9,.9))

two_sample
ggplotly(two_sample)
  
## this is just for saving
tiff("two_sample.tiff", units="in", width=8, height=5, res=300)
ggplot(data = data, aes(x = n,  y = t, color = N)) +
  geom_point() +
  geom_smooth() +
  theme_classic() + grids(linetype = "dashed") + ylim(200,600) + xlim(50,500)+
  xlab("Nº Simulated Cases (n)") + ylab("Computing Time (s)") +
  ggtitle("Computing times for Two-Sample SCC Estimation") + 
  theme(plot.title=element_text(hjust=1, vjust=0.5, face='bold')) +
  theme(legend.position=c(.9,.9))

dev.off()



