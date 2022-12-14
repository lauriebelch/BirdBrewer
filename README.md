# BirdBrewer

Colour palettes based on BIRDS :parrot::flamingo::dodo::swan::eagle::penguin::chicken::turkey::rooster::dove::duck::owl::peacock:

Code based entirely on the fantastic [wesanderson](https://github.com/karthik/wesanderson) and [MetBrewer](https://github.com/BlakeRMills/MetBrewer) packages

Colours selected with [Adobe Color](https://color.adobe.com/create/image) using 'Extract Theme' & 'Colorful' theme

Images downloaded from [unsplash](https://unsplash.com/)

All palettes are colorblind-safe according to [chroma.js](https://gka.github.io/palettes/#/9|s|00429d,96ffea,ffffe0|ffffe0,ff005e,93003a|1|1)

# How to install
install.packages("devtools")\
devtools::install_github("lauriebelch/BirdBrewer")

# How to use 
library(BirdBrewer)
bird_palette("WoodPigeon")\
pal <- bird_palette("WoodPigeon", 21, type = "continuous")

# Available palettes
bird_palette("BlueTit")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/BlueTit.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/BlueTit.jpeg">
</p>
bird_palette("Kingfisher")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/KingFisher.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Kingfisher.jpeg">
</p>
bird_palette("ChafFinch")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Chaffinch.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/ChafFinch.jpeg">
</p>
bird_palette("GoldFinch")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/GoldFinch.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/GoldFinch.jpeg">
</p>
bird_palette("GreatTit")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/GreatTit.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/GreatTit.jpeg">
</p>
bird_palette("HerringGull")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/HerringGull.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/HerringGull.jpeg">
</p>
bird_palette("Kestrel")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Kestrel.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Kestrel.jpeg">
</p>
bird_palette("Mallard")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Mallard.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Mallard.jpeg">
</p>
bird_palette("Mandarin")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Mandarin.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Mandarin.jpeg">
</p>
bird_palette("Parakeet")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Parakeet.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Parakeet.jpeg">
</p>
bird_palette("Pheasant")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Pheasant.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Pheasant.jpeg">
</p>
bird_palette("Robin")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Robin.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Robin.jpeg">
</p>
bird_palette("Shoveller")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Shoveller.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Shoveller.jpeg">
</p>
bird_palette("Sparrowhawk")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Sparrowhawk.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/Sparrowhawk.jpeg">
</p>
bird_palette("WoodPigeon")
<p align="center">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/WoodPigeon.jpg">
  <img width="500" height="330" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/WoodPigeon.jpeg">
</p>

# Example plot
Using the Kingfisher palette
<p align="center">
  <img width="600" height="600" src="https://github.com/lauriebelch/BirdBrewer/blob/master/notebook/images/GGPLOT.jpeg">
</p>

