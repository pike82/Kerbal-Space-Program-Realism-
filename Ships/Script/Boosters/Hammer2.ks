// Get Booster Values
Print core:tag.
local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Booster Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_azi is wndw:addhlayout().
	local azi_label is box_azi:addlabel("Heading").
	local azivalue is box_azi:ADDTEXTFIELD("90").
	set azivalue:style:width to 100.
	set azivalue:style:height to 18.

local box_pitch is wndw:addhlayout().
	local pitch_label is box_pitch:addlabel("Start Pitch").
	local pitchvalue is box_pitch:ADDTEXTFIELD("85").
	set pitchvalue:style:width to 100.
	set pitchvalue:style:height to 18.

local box_APalt is wndw:addhlayout().
	local APalt_label is box_APalt:addlabel("Max Turn alt (km)").
	local APaltvalue is box_APalt:ADDTEXTFIELD("500").
	set APaltvalue:style:width to 100.
	set APaltvalue:style:height to 18.

local box_LVL is wndw:addhlayout().
	local LVL_label is box_LVL:addlabel("Level angle").
	local LVLvalue is box_LVL:ADDTEXTFIELD("-15").
	set LVLvalue:style:width to 100.
	set LVLvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {
		set val to azivalue:text.
		set val to val:tonumber(0).
		set sv_intAzimith to val.

		set val to pitchvalue:text.
		set val to val:tonumber(0).
		set sv_anglePitchover to val.

		set val to APaltvalue:text.
		set val to val:tonumber(0).
		set apPitchdown to val*1000.

		set val to LVLvalue:text.
		set val to val:tonumber(0).
		set pitchdown to val.

	wndw:hide().
  	set isDone to true.
}

Print "Start Heading: " + sv_intAzimith.
Print "Start Pitch: " + sv_anglePitchover. 
Print "Level turn at: " + apPitchdown + "m".
Print "Pitching at: " + pitchdown.
Local sv_ClearanceHeight is 30. //tower clearance height

//Prelaunch

Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll). 

//Liftoff
	
STAGE. //Ignite main engines
Print "Starting engines".
Local EngineStartTime is TIME:SECONDS.
Local MaxEngineThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList. //Get List of Engines in the vessel
FOR eng IN engList {  //Loops through Engines in the Vessel
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { //Check to see if the engine is in the current Stage
		SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngineThrust. 
	}
}
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngineThrust{ // until upto thrust or the engines have attempted to get upto thrust for more than 5 seconds.
	Set CurrEngineThrust to 0.
	FOR eng IN engList {  //Loops through Engines in the Vessel
		IF eng:STAGE >= STAGE:NUMBER { //Check to see if the engine is in the current Stage
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. //add thrust to overall thrust
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 5 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start up Failed...Making Safe".
		Shutdown. //ends the script
	}
}
Print "Releasing Clamps".
Wait until Stage:Ready .
STAGE. // Relase Clamps
PRINT "Lift off!!".
local LchAlt is ALT:RADAR.

// Clear tower
Wait UNTIL ALT:RADAR > sv_ClearanceHeight + LchAlt.
Wait UNTIL SHIP:Q > 0.015. 
LOCK STEERING TO HEADING(sv_intAzimith, sv_anglePitchover).
Wait 20.
LOCK STEERING TO SRFPROGRADE.
Until SHIP:Q < 0.015{
	Wait 0.2.
}
Stage. // realese fairings
Print "Fairing relase:"+(TIME:SECONDS - EngineStartTime).
wait 2.

// unguided setup
until ship:apoapsis > apPitchdown{
	Wait 0.1.
}
LOCK STEERING TO HEADING(sv_intAzimith, pitchdown).
until (TIME:SECONDS - EngineStartTime) > 145{
	wait 0.5.
}
set ship:control:roll to 1.//spin stabilise
Until AVAILABLETHRUST < 1{
	Wait 0.1.
}
Print "MECO and Release: " + (TIME:SECONDS - EngineStartTime).
Stage.//Release first stage
Wait until Stage:Ready.
Print "Booster Finshed".
Shutdown. //ends the script