import numpy as np
import cv2
import matplotlib.pyplot as plt
import tkinter as tk
from tkinter import filedialog

root = tk.Tk()
root.withdraw()

identity = np.array([[0,0,0],[0,1,0],[0,0,0]])
sharpen = np.array([[0,-1,0],[-1,5,-1],[0,-1,0]])
blur = np.array([[1,1,1],[1,1,1],[1,1,1]])/9
emboss = np.array([[-2,-1,0],[-1,1,1],[0,1,2]])
edge_detection = np.array([[-1,-1,-1],[-1,8,-1],[-1,-1,-1]])
left_sobel = np.array([[1,0,-1],[2,0,-2],[1,0,-1]])
top_sobel = np.array([[1,2,1],[0,0,0],[-1,-2,-1]])
blur2 = np.array([[0.0625,0.125,0.0625],[0.125,0.25,0.125],[0.0625,0.125,0.0625]])
custom1 = np.array([[1305,4444,333],[2,3,7],[45,8,123456789]])
custom2 = np.array([[1,2,3],[4,5,6],[7,8,9]])
custom3 = np.array([[1,0,-1],[0,5,0],[-1,0,1]])
custom4 = np.array([[-1,0,-1],[0,5,0],[-1,0,1]])
custom5 = np.array([[2,0,-2],[0,2,0],[-2,0,2]])
custom6 = np.array([[-3,0,3],[-3,1,3],[-3,0,3]])
custom7 = np.array([[1,-1,0],[3,-1,-1],[1,-1,0]])

file_name = filedialog.askopenfilename()

img = cv2.imread(file_name, cv2.IMREAD_COLOR)
img = cv2.resize(img,(int(min(img.shape[0],img.shape[1],1000)),int(min(img.shape[0],img.shape[1],1000))))
arr = np.array(img)
kernel = sharpen
kernelSize = 3
print(kernelSize)

for i in range(kernelSize):
    for j in range(kernelSize):
        print(kernel[i,j])

print(arr.shape[0])

for k in range(3):
    for i in range(img.shape[0]):
        for j in range(img.shape[0]):
            print(arr[i,j,k])
        
