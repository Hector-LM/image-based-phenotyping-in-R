
library(EBImage)
library(Momocs)
library(ggplot2)
library(gridExtra)
library(dplyr)

## directory with all photos
## where are the photos?
dir <- '/Users/DELL/Documents/fotos_hojas/hojas_bloque_B/'

## set directory
setwd(dir)

## find all photos in the directory (files with the pattern .jpg)
allpictures<-list.files(dir,'.jpg')

## We create an empty list to store all the data
data<-list()


## loop
for (i in 1:length(allpictures)){
  
  ## photo to analyze in this loop
  pic<-allpictures[i]
  
  ## read image 
  A<-readImage(pic)
  
  # Tip: use the display() function (i.e. display(A)) from the EBImage package to display the images in R. This function is useful
  # when you are defining the thresholds you will use when displaying your image but you shouldn't use it when
  # you want to analyze all your photos because the analysis could take a long time
  
  
  # reduce the size of the images (we do this to make the analysis run faster. Reducing the size of the images or not will depend on the quality of your images, mine are 400 pix/inch)
  
  # NOTE: the images have "3 dimensions". If you write dim(A) you will see 3 values, the first is the width (pixels), the second is the length and the third corresponds to the number of color channels. The images have 3 channels: red, green and blue.
  
  dims<-round(dim(A)[1:2]*.3) # Calculate a reduction of the image measurements in pixels (width x length), in this case to 1/3 of the original size
  A<-EBImage::resize(A,dims[1],dims[2]) # reduce the size of our image to the measurements that correspond to 1/3 of the original size (dims object)
  
  # Let's explore different color channels of the image to see which one provides the best threshold 
  # for differentiating the objects (fruits, leaves or whatever we are measuring and the reference) 
  # from the background
  
  
  ## explore a channel (use the display() function to see the results of the color channels)
  B <- channel(A, mode = 'blue')
  #display(B)
  
  
  ## exploramos otro canal
  B2 <- channel(A, mode = 'red')
  
  # As you can see, none of the channels provide a perfect filter, so by joining them we could obtain a better result
  
  R <- (B < .9) # you can play with these filters until you see that all objects can be identified in the image
  R2 <- (B2 <.9)
  R3<-R+R2 # joining both images
  
  ## we need this list to get the data for the red, green and blue channels
    D <- list(r = A[, , 1], g = A[, , 2], b = A[, , 3])
  
    
  ######### segmentation (extract all objects from an image)
    
    # here we use some thresholds to identify the objects in the image
  ope0<-round(nrow(R3)/100) # define threshold dimensions for the threshold function
  C <- EBImage::thresh(R3,ope0,ope0, 0.05)
  C <- EBImage::opening(R3, EBImage::makeBrush(3, shape = "disc"))
  binaryImage <- EBImage::fillHull(C) # make sure there are no "gaps" inside our binary image
  
  
  # NOTE: In case there are other objects in your photos apart from your fruits (pieces of leaves, or 
  # whatever) they will probably be detected by the program. You don't have to worry about that now, 
  # you can easily remove them after the for loop has completed by filtering the objects by 
  # their area or length, I'll explain that below.
  
  ## recognize each object in the image
  labeledImage <- EBImage::bwlabel(binaryImage)
  
  ## pixel count per object in image
    tt<-table(labeledImage)
  
  
  
    ## color measurements (we get values of each color channel for each object in the image, with some basic statistics)
      intensityMeasurements <- lapply(D, computeFeatures.basic, 
                                  x = labeledImage, properties = F, basic.quantiles = 0.05)
  
 ## put color data in a dataframe, with correct colnames
  int<-do.call(cbind,intensityMeasurements)
  colnames(int)<-c('r.mean','r.sd','r.mod','r.q005','g.mean','g.sd','g.mod','g.q005','b.mean','b.sd','b.mod','b.q005')
  int<-as.data.frame(int)
  
  
  ## NOW WE USE THE MOMOCS PACKAGE FOR SHAPE ANALYSIS
  
  ## extract contours of each recognized object in the image
  bondCont<-ocontour(labeledImage)
  
  
  superX<-Out(bondCont)
  
  # We will store the shape and size data of the objects in our image in the "data" list that we created before the for loop
  data[[i]]<-list(color=int,
                  len=coo_length(superX),
                  wid=coo_width(superX),
                  area=coo_area(superX),
                  per=coo_perim(superX),
                  circ=coo_circularity(superX),
                  id=pic)
  
  cat(paste0('Picture ', pic,' done!\n')) # this will tell us which image has already been processed
  
  
}



# extract color data from the data list
x<-lapply(data, function(x){(x[[1]])})

data_color<-do.call(rbind,x)

## len
x<-lapply(data, function(x){(x[[2]])})
data_len<-do.call(c,x)

## wid
x<-lapply(data, function(x){(x[[3]])})
data_wid<-do.call(c,x)

## len
x<-lapply(data, function(x){(x[[4]])})
data_area<-do.call(c,x)


# create ids for each object in the image
nom<-numeric()
for (i in 1:length(data)){
  nom<-c(nom,rep(data[[i]]$id,length(data[[i]]$len)))
}

# create a data frame with all the data and ids of the photo
tmp<-data.frame(id=nom,len=data_len,wid=data_wid,area=data_area,r=data_color$r.mean,g=data_color$g.mean,b=data_color$b.mean)



uni_fotos<-unique(tmp$id)

# here we will add a "label" to each image object (fruit or reference)
tt<-numeric()
for (i in 1:length(uni_fotos)){
  x<-tmp[which(tmp$id==uni_fotos[i]),]
  tt<-c(tt,c(rep('fruto',nrow(x)-1),'ref'))  
}
tmp$type<-tt
tmp2<-tmp


## in this for loop we will use the dimensions of the "ref" object to obtain the size of the fruits in cm (so far they are in pixels)
uni_fotos<-unique(tmp2$id)
normdata<-numeric()
for (i in 1:length(uni_fotos)){
  x<-tmp2[which(tmp2$id==uni_fotos[i]),]
  
  refvalue<-x[which(x$type=='ref'),]
  fruitvalue<-x[which(x$type=='fruto'),]
  
  
  # remember that the reference of these photos (blue rectangle) measures 2 long x 1 cm wide
  len<-(fruitvalue$len*2)/refvalue$len
  wid<-(fruitvalue$wid)/refvalue$wid
  area<-(fruitvalue$area*2)/refvalue$area
  
  
  
  
  normdata <- rbind(normdata,cbind(uni_fotos[i],len,wid,area,fruitvalue[,5:7]))
  
  
}
# You can also compute a length/width ratio as a form index type
normdata$lw <- (normdata$len/normdata$wid)


colnames(normdata)[1]<-'id'

# because in some photos there are some small objects (garbage) that were also measured
# we can remove those objects by simply filtering the data by area or whatever features you prefer
normdata[normdata$area > 1, ]


# NOW YOU HAVE YOUR PHENOTYPIC DATA READY. 

