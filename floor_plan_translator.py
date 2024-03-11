import numpy as np
import PIL
from PIL import Image
import matplotlib.pyplot as plt
import cv2

# get image path
image_path = "C:\\Users\\caden\\OneDrive\\Desktop\\STAT_447\\STAT447_GH\\images\\fp_num_1_processed.png"
plan_path = "C:\\Users\\caden\\OneDrive\\Desktop\\STAT_447\\STAT447_GH\\images\\fp_num_1.png"
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
m, n = 22, 45  

# get the size of each cell in the grid
cell_height = height // m
cell_width = width // n

matrix = np.zeros((m, n), dtype=int)
# Analyze each cell in the grid
for i in range(m):
    for j in range(n):
        # Extract the cell from the thresholded image
        cell = image_thresh[i*cell_height:(i+1)*cell_height, j*cell_width:(j+1)*cell_width]
        # Determine if the cell contains any black or grey (value < 255)
        if np.any(cell < 255):
            matrix[i, j] = 1

# Create a copy of the original resized image for drawing
annotated_image = cv2.cvtColor(image_resized, cv2.COLOR_GRAY2BGR)

# Draw the grid and annotate
for i in range(m):
    for j in range(n):
        top_left = (j*cell_width, i*cell_height)
        bottom_right = ((j+1)*cell_width, (i+1)*cell_height)
        cv2.rectangle(annotated_image, top_left, bottom_right, (0, 0, 0), 1)
        if matrix[i, j] == 1:
            col_printed = (255, 0, 0)
        else: col_printed = (0, 0, 255)
        text_position = (top_left[0] + 5, top_left[1] + 15)
        cv2.putText(annotated_image, str(matrix[i, j]), 
                    text_position, cv2.FONT_HERSHEY_SIMPLEX, 0.4, col_printed, 1)


# draw grid lines
for i in range(0, width, cell_width):
    cv2.line(image_thresh, (i, 0), (i, height), color=(0), thickness=1)
for j in range(0, height, cell_height):
    cv2.line(image_thresh, (0, j), (width, j), color=(0), thickness=1)


# plot the images
fig, ax = plt.subplots(2, 2, figsize=(30, 12))  

# pre-processed (original) floor plan
ax[0,0].imshow(pre_processed_image, cmap='gray')
ax[0,0].title.set_text('Pre-Processed Floor Plan')
ax[0,0].axis('off')  

# original floor plan (with label edits)
ax[0,1].imshow(original_image, cmap='gray')
ax[0,1].title.set_text('Processed Floor Plan')
ax[0,1].axis('off')  

# with the grid overlay
ax[1,0].imshow(image_thresh, cmap='gray')
ax[1,0].title.set_text('Floor Plan with Grid Overlay')
ax[1,0].axis('off')  

# with annotation
ax[1, 1].imshow(annotated_image)
ax[1, 1].set_title('Floor Plan Matrix with Annotations')
ax[1, 1].axis('off')

plt.subplots_adjust(wspace=0.05, hspace=0.005)
plt.show()

# save the matrix 
destination = "C:\\Users\\caden\\OneDrive\\Desktop\\STAT_447\\STAT447_GH\\data\\floor_plan_matrix1.csv"

np.savetxt(destination, matrix, delimiter=',', fmt='%d')