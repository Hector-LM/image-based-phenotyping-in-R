# image-based-phenotyping-in-R
This is a high-throughput image-based phenotyping R script. With this code you can accurately measure a variety of plant organs such as leaves, fruits, seeds, tubers, etc. As a result you will obtain several phenotypic traits related to size, shape and color. 

This phenotypic pipeline contains an R script and some sample images

- The R script: The functionality of this code is based on the Momocs and EBImage packages. This script consists of several for loops that will process the images consecutively and generate a csv file with the desired phenotypic data.

- The sample images: The sample images are of chili leaves with a reference of known dimensions (1x2 cm). The reference must be of a contrasting color to the background and the organs being measured. Similarly, the background must be in contrast with the fruits and the reference to facilitate the identification of the objects. You must be careful that when generating your images you do not capture other objects that are not the organs of interest and the reference.

This phenotyping pipeline was used in the reference cited below where you can find more details about the methodology (please don't forget to cite this paper)


Lopez-Moreno, H., Basurto-Garduño, A. C., Torres-Meraz, M. A., Diaz-Valenzuela, E., Arellano-Arciniega, S., Zalapa, J., ... & Diaz-Garcia, L. (2023). Genetic analysis and QTL mapping of domestication-related traits in chili pepper (Capsicum annuum L.). Frontiers in Genetics, 14, 1101401.
