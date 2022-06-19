# Dataset and Usecase
The initial Kvasir dataset consists of 8000 images, annotated and
verified by medical doctors (experienced endoscopists), including
8 classes showing anatomical landmarks, phatological findings or
endoscopic procedures in the GI tract, i.e., 1000 images for each
class. The anatomical landmarks are
Z-line, pylorus and cecum, while the pathological finding includes
esophagitis, polyps and ulcerative colitis. In addition, 
two set of images related to removal of polyps, the "dyed and lifted
polyp" and the "dyed resection margins" are also present. The dataset consist of the
images with different resolution from 720x576 up to 1920x1072
pixels and organized in a way where they are sorted in separate
folders named accordingly to the content. Some of the included
classes of images have a green picture in picture illustrating the
position and configuration of the endoscope inside the bowel.
# Implemetation using Neural Networks
Programming was done using python and Tensorflow packages. GPU was used for model training.
The dataset was split into train, test and validation in the ratio 70:15:15. Data Augmentation was conducted using ImageDataGenerator.
Base CNN model with 2 convolution layer and 1 dense layer with 60 epochs gave a validation accuracy of 75%.
Later an advance model was trained with 7 convolution layers and 3 dense layers. Batch normalization and dropout was employed.
Padding was given as 'same' and the activation function used was 'relu'.Finally softmax function was used to obtain hte result.
Model compilation was done with the optimizer 'adam'. Model fit was conducted with 100 epochs, call back and early stopping with best model parameters were used.
Finally the advanced model gave a validation accuracy of 83.3% and a test accuracy of 88.2%.
# New Data Augmentation Techniques
Later Mixup, cutmix and cuout data augmentation techniques were used using keras and tensorflow
# 6GF Random Forest
A 6GF random forest was implemented with hyper parameter tuning to match  the results with the Kvasir paper.
