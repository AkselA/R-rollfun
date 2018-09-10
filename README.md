# image

Change the colorspace of images.

[png](http://www.rforge.net/png/)/[jpeg](http://www.rforge.net/jpeg/) recommended for file I/O

```R

library(devtools)
install_github("AkselA/R-image")
library(image)

### get and convert included dataset
data(oslo.nat)
img4 <- as.image(oslo.nat)


### edit in HSV
img <- img_colorspace(img4, c_in="RGB", c_out="HSV")
img4 <- img
summary(img4)

img_display(img4)

# gently increase value (brightness)
img4[,,3] <- tanh(img4[,,3] * 1.5)
img4[,,3] <- img4[,,3] / max(img4[,,3])
summary(img4)

dev.new()
img_display(img4)


### edit in CIELAB
# intensify green/magenta
img5 <- img_colorspace(img, c_in="HSV", c_out="LAB")
img5[,,2] <- img5[,,2] * 2
summary(img5)

dev.new()
img_display(img5)

# intensify blue/yellow
img5 <- img_colorspace(img, c_in="HSV", c_out="LAB")
img5[,,3] <- img5[,,3] * 2
summary(img5)

dev.new()
img_display(img5)
```