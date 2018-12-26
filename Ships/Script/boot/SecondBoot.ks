@LAZYGLOBAL OFF. //Turns off auto global call of parameters and prevents verbose errors where calling recursive functions

WAIT UNTIL SHIP:UNPACKED.
WAIT 1. //ensures all game physiscs have loaded
Print "Running Second Bootscript".

// open up the KOS terminal

CORE:PART:GETMODULE("kOSProcessor"):DOEVENT("Open Terminal").
SET TERMINAL:HEIGHT TO 45.
SET TERMINAL:WIDTH TO 45.
SET TERMINAL:BRIGHTNESS TO 0.8.
SET TERMINAL:CHARHEIGHT TO 10.

//Get file
Print "Library connection status:" + ADDONS:RT:HASKSCCONNECTION(SHIP).
Local filename is CORE:Part:Tag.
Print filename.
Local filePath is "0:/Second_Stages/"+filename+".ks".
Print filepath.
COPYPATH (filePath, "1:/"+ filename +".ks"). // moved the file onto local volume
RUNPATH ("1:/"+ filename +".ks").//runs file