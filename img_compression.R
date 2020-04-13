

 
require(jpeg)
require(RCurl)
 

img <- readJPEG(getURLContent("https://raw.githubusercontent.com/PravGitHub/Image_compression_R/master/Penguins.jpg", binary=TRUE))
dm <- dim(img)
r <- img[,,1]
g <- img[,,2]
b <- img[,,3]

df <- data.frame(x=rep(1:dm[2], each=dm[1]), y=rep(dm[1]:1, dm[2]), r=as.vector(r),g=as.vector(g),b=as.vector(b))

#----K Means----------------------------

for (i in seq.int(3, 25, 5)) {
  
km <- kmeans(df[,c("r","g","b")], centers = i)
 
cc <- km$centers[km$cluster,]
clustercolor <- rgb(cc)
 
l <- array(cc,dim=c(768,1024,3))
 
writeJPEG(l,paste("Penguins_compressed_K=",i,".jpg"))
}

#--------------------------------------

#-------PCA----------------------------

pca <- list(prcomp(r, center = FALSE), prcomp(g, center = FALSE), prcomp(b, center = FALSE))

for( i in seq.int(3, 25, 5)){
  
pca.img <- sapply(pca, function(j) {
  compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
}, simplify = 'array')
writeJPEG(pca.img, paste('penguins_compressed_components=', i, '.jpg', sep = ''))

}
#--------------------------------------

