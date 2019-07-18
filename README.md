Analysis of Logistics Performance Index, International Tourism and Trade & Impact of Logistics Performance on Net Trade

1.	Introduction:

For the last one century, there was a huge rise in the global trade. All countries started to depend on other countries for goods and merchandises and they made a free trade agreement(FTA) between them. The purpose of these agreements were to lower the difficulties in trade between countries. My focus is primarily on impact of Logistics Performance Index & it’s sub indicators on Net trade and how these trade agreements will help poor nations who are struggling with poor Logistics and Net trade. I also analysed how Logistics Performance Index drives the trade of the nation.
The case study is comprising of two tasks 

1.	Visualization of data related to Logistics Performance Index, International Tourism and Net Trade by using dashboards, line charts, bar chart and Simple Linear Regression Charts(Overall LPI vs LPI supply chain components).
2.	Quantitative Analysis between Overall LPI, LPI supply chain components vs Net Trade.
 
For visualization I have used Shiny[2] R to develop web based interactive dashboards and for Statistical Data Analysis I have used SAS Enterprise Guide 7.1[4]. Most of the visualization part includes Line charts(timeseries charts), Relative Line Charts, Bar Charts, Simple Linear Regression charts of LPI indicators and Non-linear Regression charts of LPI vs Net Trade.
2.	Background Research
As the case study is extensively comprising two tasks, I performed back ground research for each task separately. 

2.1	Background research for Visualization: 

As part of visualization process I performed basic research on different kinds of dashboard technologies and techniques to develop dashboards. There are so many technologies available like tableau, power BI, Shiny R, Plotly(Python) etc to design and develop dashboards. I have selected Shiny R for developing dashboards for this task for various reasons. As Shiny is a power full tool to develop interactive web based dash board, and as I am very much interested to write a code rather than using a tool, I preferred Shiny R over tableau. One more advantage of Shiny R over tableau is, it’s open source which can be developed without any licence and It can be deployed anywhere in cloud(AWS, Azure, R Cloud, Shinyapp.io etc) and can be accessed from anywhere using browser. 
(Note: I deployed this application in Shinyapp cloud with url: https://ravikanth.shinyapps.io/lpitradetourism/ )

Next step is to identify different visualization techniques used to represent data. There are many visualization techniques which can be used, but I have used Line Charts, Bar Charts and tables to display data. As data is mainly time oriented, I used line charts to compare indicators over a period of time. I used bar graphs to compare different indicators between different countries and I used simple linear regression charts to determine how Overall LPI is related to LPI supply chain components. 

2.2	Background research for Quantitative analysis(LPI vs Net Trade):

For this case study, I have gone through a quantitative analysis of Logistics Performance Index and Net Trade using SAS Enterprise Guide 7.1. The Logistics Performance Index is measured for every two years with several scores form a scale between 1-5 with 1 is low and 5 is high. The Overall score is based on other six supply chain indicators such as Customs, Infrastructure, International Shipments, Logistics Competence, Tracking & Tracking and Timeliness. The Total Imports and Total Exports together makes Net Trade. The aim of this case study is to find relation between how Logistics Performance Index drives the Net Trade and how trade agreement has benefited these countries to achieve higher LPI score. I have also analysed how bottom countries of African continent suffering with poor LPI and Net trade. As these African nations, not have any trade agreement they are suffering with poor customs, infrastructure etc.

