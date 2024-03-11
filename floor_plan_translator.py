import numpy as np
import PIL
from PIL import Image
import matplotlib.pyplot as plt
import cv2

# get image path
image_path = "C:\\Users\\caden\\OneDrive\\Desktop\\STAT_447\\STAT447_GH\\fp_num_1_processed.png"
plan_path = "C:\\Users\\caden\\OneDrive\\Desktop\\STAT_447\\STAT447_GH\\fp_num_1.png"
# load floor plan 
pre_processed_image = cv2.imread(plan_path, cv2.IMREAD_GRAYSCALE)
# load image
original_image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
#  resize width and height if needed
width, height = 1080, 350  
# no resizing currently
image_resized = original_image # cv2.resize(image, (width, height)) 
# use thresholding to simplify the image
useless, image_thresh = cv2.threshold(original_image, 127, 255, cv2.THRESH_BINARY)

# define the grid size (m x n)
m, n = 15, 45  

# get the size of each cell in the grid
cell_height = height // m
cell_width = width // n

# draw grid lines
for i in range(0, width, cell_width):
    cv2.line(image_thresh, (i, 0), (i, height), color=(0), thickness=1)
for j in range(0, height, cell_height):
    cv2.line(image_thresh, (0, j), (width, j), color=(0), thickness=1)

# plot the images
fig, ax = plt.subplots(1, 3, figsize=(24, 8))  

# pre-processed (original) floor plan
ax[0].imshow(pre_processed_image, cmap='gray')
ax[0].title.set_text('Pre-Processed Floor Plan')
ax[0].axis('off')  

# original floor plan (with label edits)
ax[1].imshow(original_image, cmap='gray')
ax[1].title.set_text('Processed Floor Plan')
ax[1].axis('off')  

# with the grid overlay
ax[2].imshow(image_thresh, cmap='gray')
ax[2].title.set_text('Floor Plan with Grid Overlay')
ax[2].axis('off')  

plt.show()


