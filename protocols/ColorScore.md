
# Color Score Protocol

Based on: https://github.com/Putnam-Lab/Lab_Management/blob/master/Lab_Resources/Physiology_Protocols/ImageJ-Analysis-Protocols.md

## Download RAW tiff images (only jpegs are on github) and rename files

1. Find TIFF Images here: https://drive.google.com/drive/folders/1VJvZi2nuNFpBn-t-O4A9bNAgSROkeTPa?usp=drive_link (Request access from Zoe)
2. If they are not yet named correctly, rename JPEG images to correct name as listed on ColorScore.csv
   1. [Pcom file](https://github.com/zdellaert/TimeSeries/blob/main/1-Pcom/data/Images/ColorScore.csv)
   2. [Mcap file](https://github.com/zdellaert/TimeSeries/blob/main/2-Mcap/data/Images/ColorScore.csv)
   3. [Pacu file](https://github.com/zdellaert/TimeSeries/blob/main/3-Pacu/data/Images/ColorScore.csv)

## ImageJ Download

Download the ImageJ application from the National Institute of Health's webpage:
https://imagej.nih.gov/ij/download.html.  

## <a name="Color_Score"></a> **Color Score**

Goal: To quantify bleaching from images taken of coral fragments next to a red, blue, green color standard ruler. Protocol written to analyze images taken in the field for the 2018-2019 Holobiont Integration project.

**Image Example**  
Limit the amount of shadow and atypical light reflection by placing a piece of white paper behind the coral fragments. Place the red, blue, and green standard ruler behind the coral fragments. The water needs to be clean to avoid any particles blocking the coral standard ruler and/or coral fragments.

**Quantifying Bleaching Score**
1. Open the ImageJ application. The application only appears as a tool bar until an image is opened.  
2. Open "Bleaching ImageJ" google spreadsheet.    
3. In ImageJ: "File" > "Open" > Choose the desired photo.

![imageJ](https://github.com/emmastrand/EmmaStrand_Notebook/blob/8cbb3a598e769d25816f0f8b0f19d4ca741e1115/images/ImageJ_1.png)

4. Open the histogram analysis option: "Analyze" > Histogram.
![image](https://github.com/emmastrand/EmmaStrand_Notebook/blob/8cbb3a598e769d25816f0f8b0f19d4ca741e1115/images/ImageJ_2.png)

5. Click "Live" and "RGB". Once in live, selecting different regions of the photo, the histogram will change intensity levels.  

![image](https://github.com/emmastrand/EmmaStrand_Notebook/blob/8cbb3a598e769d25816f0f8b0f19d4ca741e1115/images/ImageJ_3.png)

6. Select each of the three color standards and record the mean value given by the histogram.  
  1. Hover the mouse over a corner of the blue tape, right click to drag and select as much of the blue square as possible.  
  2. On the histogram window, click through the "RGB" options: "Intensity (unweighted), Intensity (weighted), "R+G+B", "Red", "Green", and stop at "Blue". Record the Mean value in "Bleaching ImageJ" spreadsheet under "Blue.Standard".
  3. Repeat steps previous steps for both the red and green tape squares. Stop at the "Red" histogram when analyzing the red square, stop at the "Green" histogram when analyzing the green square, and record the mean values under the respective Color.Standard column.  

![example](https://github.com/emmastrand/EmmaStrand_Notebook/blob/master/images/ImageJ_5.png?raw=true)

7. Click the fourth drawing tool option on the main toolbar, freehand selections. Outline the **LIVE coral tissue only**. Do not select any portions of the coral fragment that have a shadow.
8. On the histogram window, click through the "RGB" options to record the mean red, blue, and green color concentration.

![ex](https://github.com/emmastrand/EmmaStrand_Notebook/blob/master/images/ImageJ_6.png?raw=true)
