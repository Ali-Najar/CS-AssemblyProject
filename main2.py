import numpy as np
import cv2
import matplotlib.pyplot as plt

kernelSize = 3
size = int(input()) - kernelSize + 1
array = np.array([[[0.0 for j in range(size)] for i in range(size)] for k in range(3)],dtype=np.float32)
array = np.reshape(array,(size,size,3))
for k in range(3):
    for i in range(size):
        array[i,:,k] = np.array(list(float(num) for num in input().split()))

cv2.imwrite('outputs/output.jpg',array)
cv2.imshow("after",cv2.imread("outputs/output.jpg", cv2.IMREAD_COLOR))
cv2.waitKey(0) 