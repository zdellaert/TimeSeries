# How to download data log from Apex

1. You need to be on the same wifi network as the Apex (for us, URI-secure will work)
1. Get IP address for Apex 
   1. Dashboard > Network Setup > IP Address
   2. example 10.10.XXX.YYY
2. In Chrome, go to the following url, filled in with your correct Apex IP and the date and # of days 
   1. http://10.10.XXX.YYY/cgi-bin/datalog.xml?sdate=2506240000&days=1
   2. This example:
      1. 2506240000 = 06/24/2025, 00:00 hrs (midnight)
      2. 1 day (increase to increase amount of days you want to download data for)
3. Save the .xml file from this webpage
   1. File > Save Page As > data/LoggerData/Apex_log.xml