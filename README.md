This project showcases the application of functional programming concepts in Scala for performing various image processing operations and generating mathematical patterns.

Functional programming constructs in Scala are leveraged to manipulate images stored in the PPM format. Images, represented as lists of characters, are transformed through a series of operations without side effects or mutable variables.

Images are concatenated both vertically and horizontally, considering that they share the same length or height respectively. In addition to image concatenation, the capability to perform image rotation in multiples of 90 degrees counter-clockwise is also demonstrated.

A significant part of the project deals with the implementation of the Sobel operator, a widely used algorithm in edge detection in image processing. The Sobel operator works by convoluting the image with a specific kernel, effectively identifying edges based on sudden intensity changes in the image.

A novel aspect of this project is the generation of colored patterns using the concept of Pascal's triangle coupled with modulo operation. Each value in the Pascal's triangle is represented by a particular color leading to the creation of an interesting pattern.

This project serves as a testament to the effectiveness of functional programming in complex problem-solving, specifically in the realm of image processing and pattern generation. The functional approach ensures that the original data remains unchanged while transformations are effectively performed.
