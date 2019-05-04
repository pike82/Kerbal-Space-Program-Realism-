Local RSS_partlist is list().
Local partlist is List().
wait 2.
LIST Parts IN partList. 
FOR Part IN partList {  
	IF Part:tag >= "BoostTank" { 
		RSS_partlist:add(Part).
	}
}

// Get Booster Values
Print core:tag.
local wndw is gui(300).
set wndw:x to 400. //window start position
set wndw:y to 20.

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
	local pitchvalue is box_pitch:ADDTEXTFIELD("86.75").
	set pitchvalue:style:width to 100.
	set pitchvalue:style:height to 18.

local box_APalt is wndw:addhlayout().
	local APalt_label is box_APalt:addlabel("Max Turn alt (km)").
	local APaltvalue is box_APalt:ADDTEXTFIELD("170").
	set APaltvalue:style:width to 100.
	set APaltvalue:style:height to 18.

local box_LVL is wndw:addhlayout().
	local LVL_label is box_LVL:addlabel("Level angle").
	local LVLvalue is box_LVL:ADDTEXTFIELD("-8").
	set LVLvalue:style:width to 100.
	set LVLvalue:style:height to 18.

local box_LVL_Sp is wndw:addhlayout().
	local LVL_Sp_label is box_LVL_Sp:addlabel("Level Speed").
	local LVL_Spvalue is box_LVL_Sp:ADDTEXTFIELD("6000").
	set LVL_Spvalue:style:width to 100.
	set LVL_Spvalue:style:height to 18.

local box_TAR is wndw:addhlayout().
	local TAR_label is box_TAR:addlabel("Launch Target").
	local TARvalue is box_TAR:ADDTEXTFIELD("Earth").
	set TARvalue:style:width to 100.
	set TARvalue:style:height to 18.

local box_OFF is wndw:addhlayout().
	local OFF_label is box_OFF:addlabel("Launch offset").
	local OFFvalue is box_OFF:ADDTEXTFIELD("0.2").
	set OFFvalue:style:width to 100.
	set OFFvalue:style:height to 18.

local box_Circ is wndw:addhlayout().
	local Circ_label is box_Circ:addlabel("Booster Cric?").
	local Circvalue is box_Circ:ADDTEXTFIELD("True").
	set Circvalue:style:width to 100.
	set Circvalue:style:height to 18.

local box_Circ_Sp is wndw:addhlayout().
	local Circ_Sp_label is box_Circ_Sp:addlabel("Booster Cric Speed").
	local Circ_Spvalue is box_Circ_Sp:ADDTEXTFIELD("7700").
	set Circ_Spvalue:style:width to 100.
	set Circ_Spvalue:style:height to 18.

local box_PEalt is wndw:addhlayout().
	local PEalt_label is box_PEalt:addlabel("Booster Circ alt (km)").
	local PEaltvalue is box_PEalt:ADDTEXTFIELD("170").
	set PEaltvalue:style:width to 100.
	set PEaltvalue:style:height to 18.

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

		set val to LVL_Spvalue:text.
		set val to val:tonumber(0).
		set pitchdown_Sp to val.

		set val to TARvalue:text.
		set val to body(val).
		set L_TAR to val.

		set val to OFFvalue:text.
		set val to val:tonumber(0).
		set L_OFF to val.

		set val to Circvalue:text.
		set Circ_T to val.

		set val to Circ_Spvalue:text.
		set val to val:tonumber(0).
		set Circ_Sp to val.

		set val to PEaltvalue:text.
		set val to val:tonumber(0).
		set PEEnd to val*1000.

	wndw:hide().
  	set isDone to true.
}

Print "Start Heading: " + sv_intAzimith.
Print "Start Pitch: " + sv_anglePitchover. 
Print "Level turn at: " + apPitchdown + "m".
Print "Pitching at: " + pitchdown.
Print "Target: " + L_TAR.
Print "Offset: " + L_OFF.
Local sv_ClearanceHeight is 130. //tower clearance height

// Mission Values

ff_launchwindow(L_TAR, L_OFF).
Local sv_ClearanceHeight is 50. //tower clearance height

//Prelaunch

Wait 1.
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
LIST ENGINES IN engList. 
FOR eng IN engList {  
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { 
		SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngineThrust. 
	}
}
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngineThrust{ 
	Set CurrEngineThrust to 0.
	FOR eng IN engList { 
		//Print eng:name.
		IF eng:STAGE >= STAGE:NUMBER { 
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
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
LOCK STEERING TO HEADING(sv_intAzimith, 90).
Wait UNTIL SHIP:Q > 0.015. 
LOCK STEERING TO HEADING((sv_intAzimith-0), sv_anglePitchover).
Print "Pitchover: " + (TIME:SECONDS - EngineStartTime).
Wait 10.
lock pitch to 90 - VANG(SHIP:UP:VECTOR, SHIP:VELOCITY:SURFACE).
LOCK STEERING TO heading(sv_intAzimith, pitch) .
Print "Gravity Turn: " + (TIME:SECONDS - EngineStartTime).
local stagewait is 30.
local staging is false.
local timeRem is ff_burn_time(ff_stage_delta_v(RSS_partlist)).
Print "current dV" + ff_stage_delta_v(RSS_partlist).
until staging{
	Set timeRem to ff_burn_time(ff_stage_delta_v(RSS_partlist)).
	Print "Time remaining: " + timerem.
	If stageWait > timeRem or (TIME:SECONDS - EngineStartTime) > 160{ //Stage wait is actually the amount of burn time left in the tanks before stating the half staging
		PRINT "Half Staging".
		Set staging to true.
		STAGE. // Decouple the half stage
	}
	wait 1.
}

Until SHIP:Q < 0.01{
	Wait 0.2.
}
Print "Fairings Delpolyed: " + (TIME:SECONDS - EngineStartTime).
Stage.
until ship:apoapsis > apPitchdown{
	Wait 0.1.
}
Print "locking Pitch to zero".
LOCK STEERING TO HEADING(sv_intAzimith, 0).

If circ_t = "True"{
	Print "Booster Circ".
	until SHIP:VELOCITY:ORBIT:mag > pitchdown_Sp{
		Wait 0.1.
	}
	Print "locking Pitch down".
	LOCK STEERING TO HEADING(sv_intAzimith, pitchdown).
	until SHIP:VELOCITY:ORBIT:mag > Circ_Sp{
		Wait 0.1.
	}
	LOCK STEERING TO HEADING(sv_intAzimith, 0).
	Print "7700 m/s".
	Print "next loop".
	FOR eng IN engList { 
		If eng:FLAMEOUT{
		} 
		Else {
			Print eng:name + "2".
			if eng:name = "liquidEngine" or eng:name = "Bluedog.Atlas.LR105"{ //Check to see if the engine is in the current Stage
				eng:shutdown.
				Print "Engine Shut down".
			}
		}
		wait 0.01.
	}
	Print "MECO and vernier control: " + (TIME:SECONDS - EngineStartTime).
	until ship:periapsis > PEEnd{
		Wait 0.1.
		if AVAILABLETHRUST < 1 {Break.}
	}	
} Else{
	Until AVAILABLETHRUST < 1{
		Wait 0.1.
	}
}
Print "Ascent finished".
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
wait 1.0.
Stage.
Print "Orbit Reached".
Shutdown. //ends the script

function ff_launchwindow{
Parameter target is Earth, ascendLongDiff is 0.2.
	local IncPoss is true.
	local incDiff is 0.
	Local offset is 0.
	Local shiplon is 400.
	If Target = Earth{
		Return 0.
	}
	if target = sun{ // launch to ecliptic
		Until IncPoss = false{
			Print "Launching to Ecliptic".
			Print body:rotationangle.
			Print abs(180 - body:rotationangle).
			if abs(180 - body:rotationangle) < ascendLongDiff{
				set warp to 0.
				wait 3.
				Print "warp 0".
				Return 0.
			}
			if abs(180 - body:rotationangle)-ascendLongDiff < 0.2 {
				set warp to 1.
				Print 1.
			}
			else if abs(180 - body:rotationangle)-ascendLongDiff < 1 {
				set warp to 2.
				Print 2.
			}
			else if abs(180 - body:rotationangle)-ascendLongDiff < 10 {
				set warp to 3.
				Print 3.
			}
			else {
				set warp to 4.
				Print 4.
			}
			Wait 1.
		}
	}

	//  Lock the angle difference to the solar prime  
	lock DeltaLAN to mod((360-target:orbit:lan) + body:rotationangle,360). // gets the modulas (remainder from integer division) to get the angle to the LAN
	// Obtain the ship Longitude in a 360 degree reference (default is -180 to 180)
	if longitude < 0{
		local shiplon is abs(longitude).
	}
	else {
		local shiplon is 360-longitude.
	}
	if target:orbit:inclination < abs(latitude) { //If the inclination of the target is less than the lattidue of the ship it will not pass over the site as the max latitude of its path is too low.
		Set IncPoss to False.
		Set incDiff to ship:orbit:inclination-target:orbit:inclination.
		Print "Latitude unable to allow normal Launch to inclination!!!".
		Print incDiff.
		Print ship:orbit:inclination.
		Print target:orbit:inclination.
		Print IncPoss.
		wait 5.
	}
	else {// A normal launch is possible with the target passing overhead.
		Local offset is hf_tricalc(target).
	}
	local diffPlaneAng is 1000. //Set higher than the max inclination so it enters the loop
	local newdiffPlaneAng is 1000.
	Print IncPoss.
	until diffPlaneAng < (incDiff + ascendLongDiff){
		if IncPoss = False {
			Set diffPlaneAng to vang(hf_normalvector(ship),hf_normalvector(target)).// finds the angle between the orbital planes
			Print "Relative Inclination:   " + diffPlaneAng.
			print "Minimum R. Inclination: " + incDiff.
		}
		else{
			//Set diffPlaneAng to vang(hf_normalvector(ship),hf_normalvector(target)).
			Set diffPlaneAng to abs((shiplon + offset) - DeltaLAN).
			print "Relative LAN to Target: " + diffPlaneAng.
		}
		if diffPlaneAng <(incDiff + ascendLongDiff) +0.4 and diffPlaneAng > (incDiff + ascendLongDiff) + 0.2{
			set warp to 1.
			Print 2.
		}
		else if diffPlaneAng <(incDiff + ascendLongDiff) +1 and diffPlaneAng > (incDiff + ascendLongDiff) + 0.4{
			set warp to 2.
			Print 3.
		}
		else if diffPlaneAng < 10 and diffPlaneAng > (incDiff + ascendLongDiff) + 1{
			set warp to 3.
			Print 4.
		}
		Else if diffPlaneAng > 10{
			set warp to 4.
			Print 5.
		}

		wait 1.
	}
	set warp to 0.
	Print vang(hf_normalvector(ship),hf_normalvector(target)).
	wait 5.
	return vang(hf_normalvector(ship),hf_normalvector(target)).
}
function hf_tricalc{
	Parameter target.
	local a is latitude.
	local alpha is target:orbit:inclination.
	local b is 0.
	local c is 0.
	local bell is 90.
	local gamma is 0.
	if sin(a)*sin(bell)/sin(alpha) >1 {
		set b to 90.
		}
	else{
		set b to arcsin(sin(a)*sin(bell)/sin(alpha)).
	}
	set c to 2*arctan(tan(.5*(a-b))*(sin(.5*(alpha+bell))/sin(.5*(alpha-bell)))).
	return c.
}
function hf_normalvector{
	parameter ves.
	Local vel is velocityat(ves,time:seconds):orbit.
	Local norm is vcrs(vel,ves:up:vector). 
	return norm:normalized.// gives vector pointing towards centre of body from ship
}

Function ff_stage_delta_v {
Parameter RSS_partlist is list().
//Calculates the amount of delta v for the current stage    
local m is ship:mass * 1000. // Starting mass (kg)
local g is 9.80665.
local engine_count is 0.
local isp is 0. // Engine ISP (s)
local RSS is False.
local fuelmass is 0.
	// obtain ISP
	LIST engines IN engList.
	for en in engList 
	if en:ignition and not en:flameout {
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	
	// obtain RSS yes or no.
	for res IN Stage:Resources{
		if res:name = "HTP"{
			Set RSS to true.
		}
	}
	Print "RSS" + RSS.
	If RSS = true{
	//for real fuels 
		local fuels is list("LQDOXYGEN", "LQDHYDROGEN", "KEROSENE", "Aerozine50", "UDMH", "NTO", "MMH", 
			"HTP", "IRFNA-III", "NitrousOxide", "Aniline", "Ethanol75", "LQDAMMONIA", "LQDMETHANE", 
			"CLF3", "CLF5", "DIBORANE", "PENTABORANE", "ETHANE", "ETHYLENE", "OF2", "LQDFLUORINE", 
			"N2F4", "FurFuryl", "UH25", "TONKA250", "TONKA500", "FLOX30", "FLOX70", "", "FLOX88", 
			"IWFNA", "IRFNA-IV", "AK20", "AK27", "CaveaB", "MON1", "MON3", "MON10", "MON15", "MON20", "Hydyne", "TEATEB").
		for tankPart in RSS_partlist{
			for res in tankpart:RESOURCES{
				for f in fuels{
					if f = res:NAME{
						SET fuelMass TO fuelMass + ((res:DENSITY*res:AMOUNT)*1000).
						Print "fuel mass" + fuelmass.
					}
				}
			}
		}
	} Else {
	//for stock fuels
		local fuels is list("LiquidFuel", "Oxidizer", "SolidFuel", "MonoPropellant").
		for res in STAGE:RESOURCES{
			for f in fuels{
				if f = res:NAME{
					SET fuelMass TO fuelMass + res:DENSITY*res:AMOUNT.
				}
			}
		}
	}
	//TODO:Think about removing RCS components or making it an input term as this could be a significant proportion of the deltaV which is not used.
	return (isp * g * ln(m / (m - fuelMass))).
}./// End Function

function ff_burn_time {
parameter dV.
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local m is ship:mass * 1000. // Starting mass (kg)
	local e is constant():e. // Base of natural log
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	
	//TODO: look at comapring the dv with the ff_stage_delta_v. If less look at the engine in the next stage and determine the delta_v and time to burn until the dv has been meet.
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	
	if engine_count = 0{
		return 1. //return something to prevent error.
	}
	
	set isp to isp / engine_count. //assumes only one type of engine in cluster
	set thrust to thrust * 1000. // Engine Thrust (kg * m/s²)
	return g * m * isp * (1 - e^(-dV/(g*isp))) / thrust.
}/// End Function