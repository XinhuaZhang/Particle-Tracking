import numpy as np
import os
import os.path as op
import sys
import tifffile as tf

def getExtension(fileName):
    return op.splitext(fileName)[1]

if len(sys.argv) <= 2:
    print('Not enough arguments (', str(len(sys.argv)), ').')
    exit(0)


# There is something wrong with the original tif files.
# We need this pythons script to covert the files
# that can be used by the codes at
# https://github.com/XinhuaZhang/Particle-Tracking

folderPath = sys.argv[1]
outputFolder = sys.argv[2]

if not op.isdir(outputFolder):
    os.mkdir(outputFolder)

count = 0
for fileName in os.listdir(folderPath):
    if getExtension(fileName) == '.tif':
        img = tf.imread(op.join(folderPath, fileName))
        print(np.amin(img), np.amax(img))
        print(np.shape(img))
        print(img.dtype)
        tf.imwrite(op.join(outputFolder, fileName), img)
        count += 1


print(count)



