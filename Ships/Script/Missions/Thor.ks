// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_MoonEND is wndw:addhlayout().
	local MoonEND_label is box_MoonEND:addlabel("Moon PE start (s)").
	local MoonENDvalue is box_MoonEND:ADDTEXTFIELD("90").
	set MoonENDvalue:style:width to 100.
	set MoonENDvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {

		set val to MoonENDvalue:text.
		set val to val:tonumber(0).
		set endPE to val.

	wndw:hide().
  	set isDone to true.
}
Global boosterCPU is "ThorS".
Print "Waiting for activation".
//wait for active
Local holdload is false. 
until holdload = true {
	Set holdload to true. //reset to true and rely on previous stage to turn false
	local PROCESSOR_List is list().
	LIST PROCESSORS IN PROCESSOR_List. // get a list of all connected cores
	for Processor in PROCESSOR_List {
		if Processor:TAG = boosterCPU{ //checks to see if previous stage is present
			Set holdload to false.
		}
	}
	wait 0.2.
}
Print "Thor active".
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
SET SHIP:CONTROL:FORE to 0.
ff_COMMS().
unlock steering.

//commence Landing routine
Lock steering to retrograde.
stage.
wait until stage:ready.
wait 1.0.
stage.
ff_CAB(endPE).
ff_SuBurn().
wait 2.
ff_SuBurn().
wait 400.
Shutdown.

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
parameter dV, isp is 0, thrust is 0, engine_count is 0. // For RSS/RO engine values must be given unless they are actually burning.
lock throttle to 0.
Print "Burntime".
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local m is ship:mass * 1000. // Starting mass (kg)
	local e is constant():e. // Base of natural log
	
	//TODO: look at comapring the dv with the ff_stage_delta_v. If less look at the engine in the next stage and determine the delta_v and time to burn until the dv has been meet.
	If engine_count = 0{ // only evaluate is figures not given
		list engines in all_engines.
		for en in all_engines {
			if en:ignition and not en:flameout {
				set thrust to thrust + en:availablethrust.
				set isp to isp + en:isp.
				set engine_count to engine_count + 1.
			}
		}
	}
	if engine_count = 0{
		return 1. //return something to prevent error.
	}
	set isp to isp / engine_count. //assumes only one type of engine in cluster
	set thrust to thrust * 1000. // Engine Thrust (kg * m/s²)
	Print isp.
	Print Thrust.
	return g * m * isp * (1 - e^(-dV/(g*isp))) / thrust.
}/// End Function

FUNCTION hf_mAngle{
PARAMETER a.
  UNTIL a >= 0 { SET a TO a + 360. }
  RETURN MOD(a,360).
  
}
FUNCTION ff_COMMS {
	PARAMETER event is "activate", stagewait IS 0.1, ShipQtgt is 0.0045.
	// "deactivate"
	IF SHIP:Q < ShipQtgt {
		FOR antenna IN SHIP:MODULESNAMED("ModuleRTAntenna") {
			IF antenna:HASEVENT(event) {
				antenna:DOEVENT(event).
				PRINT event + " Antennas".
				WAIT stageWait.
			}	
		}.
	}
} // End of Function

function ff_mdot {
	Parameter RSS_true is 0.
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	If engine_count = 0{
		Set engine_count to 1.
	}
	set isp to isp / engine_count.
	set thrust to thrust* 1000.// Engine Thrust (kg * m/s²)
	If RSS_true = 0{
		return (thrust/(g * isp)). //kg of change	
	}
	return RSS_true.
}/// End Function

Function ff_CAB{ 
	//this landing tries to burn purely horizontal and uses a pid to determine the desired downwards velocity and cancel it out through a pitch change. It does not stop the throttle or point upwards, that is upto the user to code in or allow a transistion into another function.

	Parameter ThrottelStartTime is 0.1, SafeAlt is 10, TargetLatLng is "Null", EndHorzVel is 0. // throttle start time is the time it take the trottle to get up to full power TODO: have this also take into account the rotation of the body so it can target a specific landing spot.
	
	Set PEVec to velocityat(Ship, ETA:PERIAPSIS + TIME:SECONDS):Surface.
	Set Horzvel to PEVec:mag. // its known at PE the verVel is Zero so all velocity must in theory be horizontal	

	local BurnStartTime is time:seconds + ETA:PERIAPSIS.
	
	Until time:seconds > (BurnStartTime-ThrottelStartTime){
		clearscreen.
		Print "Burn Horizontal Velocity to Cancel:" + PEVec:mag.
		Print "Wating for CAB Start in :" + (BurnStartTime - Time:seconds-ThrottelStartTime).
		Lock steering to ship:retrograde. 
		wait 0.001.
	}
	
	Set PIDVV to PIDLOOP(0.03, 0, 0.05, -0.1, 0.1).//SET PID TO PIDLOOP(KP, KI, KD, MINOUTPUT, MAXOUTPUT).	
	Set PIDVV:SETPOINT to 0. // we want the altitude to remain constant so no vertical velocity.
	Set highpitch to 0.

	//lock steering to retrograde * r(-highPitch, 0, 0):vector.
	Lock Horizon to VXCL(UP:VECTOR, -VELOCITY:SURFACE). //negative makes it retrograde
	LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(-highPitch,
                        VCRS(horizon,BODY:POSITION))*horizon,
						FACING:TOPVECTOR).//lock to retrograde at horizon
	
	Set Basetime to time:seconds.
	Lock Throttle to 1.0.
	Until ETA:PERIAPSIS > 500{
		//Create PID to adjust the craft pitch (without thrusting downward) which maintains a vertical velocity of zero and regulates the velocity of burn height change if not zero reventing a pitch above the horizontal.		
		Clearscreen.
		Print "Undertaking inital CAB".
		Print "Ground Speed: " + SHIP:GROUNDSPEED.
		Print "Pitch: " + highpitch.
		wait 0.01.
	}
	Until SHIP:GROUNDSPEED < 1100{
		//Create PID to adjust the craft pitch (without thrusting downward) which maintains a vertical velocity of zero and regulates the velocity of burn height change if not zero reventing a pitch above the horizontal.		
		Set dpitch TO PIDVV:UPDATE(TIME:SECONDS, verticalspeed). //Get the PID on the AlT diff as desired vertical velocity
		Set highpitch to max(highpitch + dpitch,0). // Ensure the pitch does not go below zero as gravity will efficently lower the veritcal velocity if required
		Clearscreen.
		Print "Undertaking CAB".
		Print "Ground Speed: " + SHIP:GROUNDSPEED.
		Print "Pitch: " + highpitch.
		wait 0.01.
	}
	Set PIDVV:SETPOINT to -100. // we want the altitude to start reducing.
	Until SHIP:GROUNDSPEED < 50 {
		//Create PID to adjust the craft pitch (without thrusting downward) which maintains a vertical velocity of zero and regulates the velocity of burn height change if not zero reventing a pitch above the horizontal.		
		Set dpitch TO PIDVV:UPDATE(TIME:SECONDS, verticalspeed). //Get the PID on the AlT diff as desired vertical velocity
		Set highpitch to max(highpitch + dpitch,0). // Ensure the pitch does not go below zero as gravity will efficently lower the veritcal velocity if required
		Clearscreen.
		Print "Undertaking retrograde transistion".
		Print "Ground Speed: " + SHIP:GROUNDSPEED.
		Print "Pitch: " + highpitch.
		wait 0.01.
	}
	Lock steering to retrograde.
	Print "Canceling ground speed".
	Until SHIP:GROUNDSPEED < 10 {
		wait 0.01.
	}
	Lock Throttle to 0.0.
} //End of Function

Function ff_SuBurn {

	Parameter ThrottelStartUp is 0.1, SafeAlt is 5, EndVelocity is (-1.5). // end velocity must be negative
	Lock Throttle to 0.0.
	local Flight_Arr is lexicon().
	set Flight_Arr to hf_fall(1.5).
	Lock steering to retrograde.
	Until Flight_Arr["fallDist"] + SafeAlt + (ThrottelStartUp * abs(ship:verticalspeed)) > (ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT) { // until the radar height is at the suicide burn height plus safe altitude and an allowance for the engine to throttle up to max thrust
		//Run screen update loop to inform of suicide burn wait.
		Set Flight_Arr to hf_fall(1.5).
		Clearscreen.
		Print "gl_fallTime:" + Flight_Arr["fallTime"].
		Print "gl_fallVel:" + Flight_Arr["fallVel"].
		Print "gl_fallDist:" + Flight_Arr["fallDist"].
		Print "gl_fallBurnTime:" + ff_burn_time(Flight_Arr["fallVel"],258, 1.5).
		Print "Radar Alt:" + (ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT).
		Wait 0.001.
	}
	//Burn Height has been reached start the burn
	until 0 {
		If verticalspeed > EndVelocity{
		 Lock Throttle to 0.0.
		} else {
			Lock Throttle to 1.0.
		}.
		
		if abs(verticalspeed) < 20 {
			LOCK STEERING to HEADING(90,90). // Lock in upright posistion and fixed rotation
		}.
		if ((ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT) < 0.25) or (Ship:Status = "LANDED"){ // this is used if the burn is intended to land the craft.
			Lock Throttle to 0.
			Unlock Throttle.
			Break.
		}.
		Wait 0.01.
	} // end Until
	// Note: if the ship does not meet these conditions the throttle will still be locked at 1, you will need to ensure a landing has taken place or add in another section in the runtime to ensure the throttle does not stay at 1 an make the craft go back upwards.
} //End of Function

function ff_Vel_Exhaust {
	Parameter RSS_true is 0.
	local g is 9.80665.  
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	If engine_count = 0{
		Set engine_count to 1.
	}
	set isp to isp / engine_count.
	If RSS_true = 0{
		return g *isp.///thrust). //
	}
	return RSS_true*g.
}/// End Function

Function hf_Fall{
	Parameter s_thrust is ship:AVAILABLETHRUST.
//Fall Predictions and Variables
	local gl_Grav is ff_Gravity().
	local fallTime is ff_quadraticPlus(-gl_Grav["Avg"]/2, -ship:verticalspeed, ship:Altitude - SHIP:GEOPOSITION:TERRAINHEIGHT).//r = r0 + vt - 1/2at^2 ===> Quadratic equiation 1/2*at^2 + bt + c = 0 a= acceleration, b=velocity, c= distance
	local fallVel is abs(ship:verticalspeed) + (gl_Grav["Avg"]*fallTime).//v = u + at
	local fallAcc is (s_thrust/ship:mass). // note is is assumed this will be undertaken in a vaccum so the thrust and ISP will not change. Otherwise if undertaken in the atmosphere drag will require a variable thrust engine so small variations in ISP and thrust won't matter becasue the thrust can be adjusted to suit.
	local fallDist is (fallVel^2)/ (2*(fallAcc)). // v^2 = u^2 + 2as ==> s = ((v^2) - (u^2))/2a 

	local arr is lexicon().
	arr:add ("fallTime", fallTime).
	arr:add ("fallVel", fallVel).
	arr:add ("fallAcc", fallAcc).
	arr:add ("fallDist", fallDist).
	
	Return(arr).
}

function ff_Gravity{
	Parameter Surface_Elevation is SHIP:GEOPOSITION:TERRAINHEIGHT.
	Set SEALEVELGRAVITY to body:mu / (body:radius)^2. // returns the sealevel gravity for any body that is being orbited.
	Set GRAVITY to body:mu / (ship:Altitude + body:radius)^2. //returns the current gravity experienced by the vessel	
	Set AvgGravity to sqrt(		(	(GRAVITY^2) +((body:mu / (Surface_Elevation + body:radius)^2 )^2)		)/2		).// using Root mean square function to find the average gravity between the current point and the surface which have a squares relationship.
	local arr is lexicon().
	arr:add ("SLG", SEALEVELGRAVITY).
	arr:add ("G", GRAVITY).
	arr:add ("AVG", AvgGravity).
	Return (arr).
}

function ff_quadraticPlus {
	parameter a, b, c.
	return (b - sqrt(max(b ^ 2 - 4 * a * c, 0))) / (2 * a).
}

Function ff_Avionics_off{
	Local P is SHIP:PARTSNAMED(core:part:Name)[0].
	Local M is P:GETMODULE("ModuleProceduralAvionics").
	M:DOEVENT("Shutdown Avionics").
}

Function ff_Avionics_on{
	Local P is SHIP:PARTSNAMED(core:part:Name)[0].
	Local M is P:GETMODULE("ModuleProceduralAvionics").
	M:DOEVENT("Activate Avionics").
}