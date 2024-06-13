
setwd("/Users/cselva1/Desktop/BF_export/Analysis/paper/sem/")
library(semPlot)
library(lavaan)


# do not run this!





#Model1 <- 'Resilient_Coping =~ Resilience + Coping'
mod1 <- '
Resilient_Coping =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 +I11 + I12 + I13 + I14 + I15 + I16 
F1 =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 
F2 =~ I9 + I10 +I11 + I12 + I13 + I14 + I15 + I16 
'

dat <- simulateData(mod1)
fit <- cfa(mod1, dat, orthogonal=TRUE)

# Define labels for the items using LaTeX formatting with subscript for "i" and "j"
labels <- c(expression(paste("Item ", italic({i}))),
            " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
            expression(paste("Item ", italic({j}))),
            "Resilient Coping", "Resilience", "    Coping    ")


m1 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 0
               ,nCharEdges =0
               ,fixedStyle = 1
               ,unCol = "#353839",
               sizeLat = 9, # Increase label sizes for better visibility
               optimizeLatRes = TRUE,
               rotation = 1, # Change to 2, 3, or 4 to adjust orientation
               sizeMan = 4, # Adjust box size
               sizeMan2 = 4,
               asize = 2, # Arrow size
               node.width = 1, # Adjust node width for clearer visualization
               edge.width = 0.8 # Increase edge width for emphasis
               ,sizeLat2 =5
               ,edge.label.cex = 10
               ,label.cex = 2
               #, sizeInt2 =30
)

plot(m1)









m1 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 20
               ,fixedStyle = 1
               ,unCol = "#353839"
               ,sizeLat = 9 #adjust lable sizes
               ,optimizeLatRes = TRUE
               , rotation = 1 # change to 2,3,4 to flip
               , sizeMan = 4 #4x4 box
               , sizeMan2= 4
               ,asize=2    #arrow size
               ,node.width = .8 
               ,edge.width = 0.5   
               #,width=70, height=20, filetype="png",filename="sem1"
)
#style="lisrel" adds some length to labels

m1$graphAttributes$Edges$edgeConnectPoints[1:16,2] <- 100 * pi
m1$graphAttributes$Edges$edgeConnectPoints[17:32,2] <- 5 * pi

m1$plotOptions$filetype <- "png"
m1$plotOptions$filename <- "sem1"
m1$plotOptions$width <- 70
m1$plotOptions$height <- 20
plot(m1)




#Model2 <- 'Resilient_Coping =~ Avoidance + Cognitive_Reconstruction + Problem_Solving + Distraction + Support_Seeking'
mod2 <- '
Resilient_Coping =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 +I11 + I12 + I13 + I14 + I15 + I16 
F1 =~ I1 + I2 + I3 
F2 =~ I4 + I5 + I6 
F3 =~ I7 + I8 + I9
F4 =~ I10 + I11 + I12
F5 =~ I13 + I14 + I15 + I16
'

dat <- simulateData(mod2)
fit <- cfa(mod2, dat, orthogonal=TRUE)


# Define labels for the items using LaTeX formatting with subscript for "i" and "j"
labels <- c(expression(paste("Item ", italic({i}))),
            " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
            expression(paste("Item ", italic({j}))),
            "Resilient Coping", "Avoidance", "Cognitive\nreconstruction", "Problem solving" , "Distraction", "Support seeking")
m2 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 20
               ,fixedStyle = 1
               ,unCol = "#353839"
               ,sizeLat = 9 #adjust lable sizes
               ,optimizeLatRes = TRUE
               , rotation = 1 # change to 2,3,4 to flip
               , sizeMan = 4 #4x4 box
               , sizeMan2= 4
               ,asize=2    #arrow size
               ,node.width = .8 
               ,edge.width = 0.5   
               #,width=70, height=20, filetype="png",filename="sem2"
)
#style="lisrel" adds some length to labels

m2$graphAttributes$Edges$edgeConnectPoints[1:16,2] <- 100 * pi
m2$graphAttributes$Edges$edgeConnectPoints[17:32,2] <- 5 * pi

m2$plotOptions$filetype <- "png"
m2$plotOptions$filename <- "sem2"
m2$plotOptions$width <- 70
m2$plotOptions$height <- 20
plot(m2)









Model3 <- 'Resilient_Coping =~ Self_Perception + Future_Planning + Social_Competence + Family_Cohesion + Social_Resources + Coping'
mod3 <- '
Resilient_Coping =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 +I11 + I12 + I13 + I14 + I15 + I16
F1 =~ I1 + I2 + I3 
F2 =~ I4 + I5 + I6 
F3 =~ I7 + I8 + I9  
F4 =~ I10 + I11 + I12
F5 =~ I13 + I14   
F6 =~ I15 + I16 
'

dat <- simulateData(mod3)
fit <- cfa(mod3, dat, orthogonal=TRUE)


# Define labels for the items using LaTeX formatting with subscript for "i" and "j"
labels <- c(expression(paste("Item ", italic({i}))),
            " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
            expression(paste("Item ", italic({j}))),
            "Resilient Coping", "Self perception", "Future planning", "Social competence" ,"Family cohesion", "Social resources", "    Coping    ")
m3 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 20
               ,fixedStyle = 1
               ,unCol = "#353839"
               ,sizeLat = 9 #adjust lable sizes
               ,optimizeLatRes = TRUE
               , rotation = 1 # change to 2,3,4 to flip
               , sizeMan = 4 #4x4 box
               , sizeMan2= 4
               ,asize=2    #arrow size
               ,node.width = .8 
               ,edge.width = 0.5   
               #,width=70, height=20, filetype="png",filename="sem3"
)
#style="lisrel" adds some length to labels

m3$graphAttributes$Edges$edgeConnectPoints[1:16,2] <- 100 * pi
m3$graphAttributes$Edges$edgeConnectPoints[17:32,2] <- 5 * pi

m3$plotOptions$filetype <- "png"
m3$plotOptions$filename <- "sem3"
m3$plotOptions$width <- 70
m3$plotOptions$height <- 20
plot(m3)










#Model4 <- 'Resilient_Coping =~ Avoidance + Active_coping  + Distraction + Family_Cohesion + Self_Perception + Future_Planning + Social_Competence + Social_Resources'
mod4 <- '
Resilient_Coping =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 +I11 + I12 + I13 + I14 + I15 + I16 
F1 =~ I1 + I2 
F2 =~ I3 + I4 
F3 =~ I5 + I6 
F4 =~ I7 + I8 
F5 =~ I9 + I10
F6 =~ I11 + I12
F7 =~ I13 + I14
F8 =~ I15 + I16
'


dat <- simulateData(mod4)
fit <- cfa(mod4, dat, orthogonal=TRUE)


# Define labels for the items using LaTeX formatting with subscript for "i" and "j"
labels <- c(expression(paste("Item ", italic({i}))),
            " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ",
            expression(paste("Item ", italic({j}))),
            "Resilient\nCoping", "Avoidance", "Active\ncoping" ,  "Distraction", "Self\nperception", "Family\ncohesion" , "Future\nplanning", "Social\ncompetence", "Social\nresources")




m4 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 0
               ,nCharEdges =0
               ,fixedStyle = 1
               ,unCol = "#353839",
               sizeLat = 9, # Increase label sizes for better visibility
               optimizeLatRes = TRUE,
               rotation = 1, # Change to 2, 3, or 4 to adjust orientation
               sizeMan = 4, # Adjust box size
               sizeMan2 = 4,
               asize = 2, # Arrow size
               node.width = 1, # Adjust node width for clearer visualization
               edge.width = 0.8 # Increase edge width for emphasis
               ,sizeLat2 =5
               ,edge.label.cex = 2
               ,label.cex = 2
               #, sizeInt2 =30
               ,levels = c(1,2,3)
)


m4 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 20
               ,fixedStyle = 1
               ,unCol = "#353839"
               ,sizeLat = 9 #adjust lable sizes
               ,optimizeLatRes = TRUE
               , rotation = 1 # change to 2,3,4 to flip
               , sizeMan = 4 #4x4 box
               , sizeMan2= 4
               ,asize=2    #arrow size
               ,node.width = .8 
               ,edge.width = 0.5   
               #,width=70, height=20, filetype="png",filename="sem4"
)
#style="lisrel" adds some length to labels

m4$graphAttributes$Edges$edgeConnectPoints[1:16,2] <- 100 * pi
m4$graphAttributes$Edges$edgeConnectPoints[17:32,2] <- 5 * pi

m4$plotOptions$filetype <- "png"
m4$plotOptions$filename <- "sem4"
m4$plotOptions$width <- 70
m4$plotOptions$height <- 20
plot(m4)








'
library(magick)

# Define the image files
image_files <- c("sem1.png", "sem2.png", "sem3.png", "sem4.png")

# Define the dimensions for cropping
left <- 0
right <- 0
top <- 350
bottom <- 350

# Crop the images
for (file in image_files) {
  # Read the image
  img <- image_read(file)
  
  # Get the dimensions of the image
  width <- image_info(img)$width
  height <- image_info(img)$height
  
  # Calculate the new dimensions after cropping
  new_width <- width - left - right
  new_height <- height - top - bottom
  
  # Crop the image
  img_cropped <- image_crop(img, geometry = paste0(new_width, "x", new_height, "+", left, "+", top))
  
  # Save the cropped image
  image_write(img_cropped, path = file)
  
  # Stop after processing the four images
  if (file == image_files[4]) {
    break
  }
}

'





library(cowplot)

# Define file paths for the images
image_paths <- c("sem1.png", "sem2.png", "sem3.png", "sem4.png")

# Read images and create plot objects
plots <- lapply(image_paths, function(path) {
  img <- cowplot::ggdraw() +
    cowplot::draw_image(path)
})

# Combine plots vertically with labels
combined_plot <- cowplot::plot_grid(plotlist = plots, labels = c("a", "b", "c", "d"), ncol = 1, label_size = 8)


# Set the desired dimensions for the plot
png("combined_plot.png", width = 1000, height = 1200, units = "px", res = 300)

# Display the combined plot
print(combined_plot)

# Turn off the graphics device
dev.off()














#Model4 <- 'Resilient_Coping =~ Avoidance + Active_coping  + Distraction + Family_Cohesion + Self_Perception + Future_Planning + Social_Competence + Social_Resources'
mod4 <- '
Resilient_Coping =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 +I11 + I12 + I13 + I14 + I15 + I16 + I17 + I18 + I19 + I20
F1 =~ I1 + I2 + I3 + I4 
F2 =~ I5 + I6 
F3 =~ I7 + I8 + I9 + I10
F4 =~ I11 
F5 =~ I12
F6 =~ I13 + I14 + I15 + I16 + I17
F7 =~ I18 + I19
F8 =~ I20
'



dat <- simulateData(mod4)
fit <- cfa(mod4, dat, orthogonal=TRUE)


# Define labels for the items using LaTeX formatting with subscript for "i" and "j"
labels <- c("Behavioral disengagement (-)", "Rejection (-)", "Self-blame (-)", "Wishful thinking (-)", 
            "Active coping", "Dealing with anxiety",
            "Distraction", "Cognitive restructuring", "Mental effort", "Physical effort",
            "Family cohesion",
            "Future planning",
            "Self-perception", "Optimism", "Meaning, purpose, and growth", "Positive thinking", "Acceptance",
            "Humor", "Instrumental support",
            "Social support"
            #factors
            ,"Resilient\nCoping", "Avoidance", "Active\ncoping" ,  "Distraction", "Family\ncohesion" , "Future\nplanning","Self\nperception", "Social\ncompetence", "Social\nresources")




m4 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 0
               ,nCharEdges =0
               ,fixedStyle = 1
               ,unCol = "#353839",
               optimizeLatRes = TRUE,
               rotation = 4, # Change to 2, 3, or 4 to adjust orientation
               sizeMan = 25, # Adjust box size
               sizeMan2 = 2,
               asize = 2, # Arrow size
               node.width = 1, # Adjust node width for clearer visualization
               node.height =1,
               edge.width = 0.8 # Increase edge width for emphasis
               ,sizeLat = 18 # Increase label sizes for better visibility
               ,sizeLat2 =11
               ,edge.label.cex = 3
               ,label.cex = 2
               ,mar = c(2,5,2,3) # adjust margins
               ,  reorder = FALSE
               #, sizeInt2 =30
               #,levels = c(1,2,3)
               #,  nodeNames = sapply(labels, function(x)       # adds line of text next to plot
                # paste(strwrap(x, 30), collapse = '\n      '))
               ,label.prop = 1.3 #### THIS Scales the labels!!
               ,label.norm = "OOOOOOOOOOOOOOOOOOOOO" # O letters that have the same size!!
               ,label.scale = TRUE
               ,label.font = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)
               ,width=70, height=20
               ,DoNotPlot = TRUE
)

m4$graphAttributes$Edges$edgeConnectPoints

m4$graphAttributes$Edges$edgeConnectPoints[1:20,2] <- 8
m4$graphAttributes$Edges$edgeConnectPoints[21:40,2] <- 8
m4$Arguments$Edges$DoNotPlot <- FALSE


plot(m4)





m4 <- semPaths(fit,"model", bifactor = "Resilient_Coping", nodeLabels = labels, layout = "tree2"
               , residuals = FALSE, exoCov = FALSE,whatLabels = "names"
               #, label.cex = 3
               , nCharNodes = 20
               ,fixedStyle = 1
               ,unCol = "#353839"
               ,sizeLat = 9 #adjust lable sizes
               ,optimizeLatRes = TRUE
               , rotation = 1 # change to 2,3,4 to flip
               , sizeMan = 4 #4x4 box
               , sizeMan2= 4
               ,asize=2    #arrow size
               ,node.width = .8 
               ,edge.width = 0.5   
               #,width=70, height=20, filetype="png",filename="sem4"
)


#style="lisrel" adds some length to labels

m4$graphAttributes$Edges$edgeConnectPoints[1:16,2] <- 100 * pi
m4$graphAttributes$Edges$edgeConnectPoints[17:32,2] <- 5 * pi

m4$plotOptions$filetype <- "png"
m4$plotOptions$filename <- "sem4"
m4$plotOptions$width <- 70
m4$plotOptions$height <- 20
plot(m4)






