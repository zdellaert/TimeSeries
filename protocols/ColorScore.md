
# Color Score Protocol

Based on: https://github.com/Putnam-Lab/Lab_Management/blob/master/Lab_Resources/Physiology_Protocols/ImageJ-Analysis-Protocols.md

## Find Data Files

   1. [Pcom file](https://github.com/zdellaert/TimeSeries/blob/main/1-Pcom/data/Images/ColorScore.csv)
   2. [Mcap file](https://github.com/zdellaert/TimeSeries/blob/main/2-Mcap/data/Images/ColorScore.csv)
   3. [Pacu file](https://github.com/zdellaert/TimeSeries/blob/main/3-Pacu/data/Images/ColorScore.csv)

## ImageJ or Photoshop Download

Download the ImageJ application from the National Institute of Health's webpage:
https://imagej.nih.gov/ij/download.html. 

Download the Photoshop Application using your organization Adobe Access

## <a name="Color_Score"></a> **Color Score**

Goal: To quantify mean coral tissue color from images taken of coral fragments next to a red, blue, green color standard ruler. Protocol written to analyze images taken in the field for the 2018-2019 Holobiont Integration project.

**Image Example**  
Limit the amount of shadow and atypical light reflection by placing a piece of white paper behind the coral fragments. Place the red, blue, and green standard ruler behind the coral fragments. The water needs to be clean to avoid any particles blocking the coral standard ruler, Coral color standard chart as well as the coral fragements.

**Quantifying Color Score**
1. Open the Photoshop application. The main window appears where you can navigate different options.   
2. Open csv file in excel. As you make edits, save the file and push to github regularly.    
3. In Photoshop: "File" > "Open" > Choose the desired photos you wish you analyze.

![Main Window Image](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/main_window.jpg?raw=TRUE)

4. Open the histogram analysis option: "Window" > Histogram.

**Note: Select the Expanded View from the  ☰ option on the right top corner of the window. The expanded view is the only way to view and obtain color data for the respective image. The compact view doesn't let you see the data.**

![Select Histogram Option link](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/Select_histogram.png?raw=TRUE)

![Select Histogram  expanded view](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/Histogram_expand_view.png?raw=TRUE)

6. In the Channel section of the expanded view, select the respective color from the you want to obtain the mean data from as shown in the image below.

7. Select each of the three color standards and record the mean value given by the histogram.
   
![Standard Color Card](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/std_colorcard.jpeg?raw=TRUE)

  1. Select the standard color i.e. either red, blue or green from the color card as shown above.

To select the area, either it be coral fragment or the standard color area in the card you can use the following tools. 

 ![Selectiont tools in Photoshop](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/different_tools.jpg?raw=TRUE)
 
  2. On the histogram window, Channel > click through the different colors options: "Red", "Green", and "Blue". Record the Mean value in spreadsheet under "Red.Standard".

![Select Red Standard](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/select_stdred.jpg?raw=TRUE)

  3. Repeat steps previous steps for both the Green and Blue squares. Stop at the "Red" histogram when analyzing the red square, stop at the "Green" histogram when analyzing the green square, and record the mean values under the respective Color.Standard column.  

7. Outline the **LIVE coral tissue only** using the above selection tools shown. Do not select any portions of the coral fragment that have a shadow or glare.
   
![Incorrect selection](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/incorrect_selection.png?raw=TRUE)

In case of any glare on the coral tissue like shown below, deselect the area using the tools and manually selecting the area by also holding "Alt" key.

![Correct selection](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/selecting_reflecting_area.png?raw=TRUE)


8. On the histogram window, click through the "RGB" options to record the mean red, blue, and green color concentration.

![Color Data](https://github.com/ppednekar25/Pednekar_Putnam_Lab_Notebook/blob/d99cad122c4dd59f7e5886b726d2f70c0425bfef/images/color_value_histogram.png?raw=TRUE)
