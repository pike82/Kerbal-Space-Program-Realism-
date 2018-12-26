// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("55").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("170").
	set ENDvalue:style:width to 100.
	set ENDvalue:style:height to 18.

local box_MoonEND is wndw:addhlayout().
	local MoonEND_label is box_MoonEND:addlabel("Planet PE END (km)").
	local MoonENDvalue is box_MoonEND:ADDTEXTFIELD("200").
	set MoonENDvalue:style:width to 100.
	set MoonENDvalue:style:height to 18.

local box_TAR is wndw:addhlayout().
	local TAR_label is box_TAR:addlabel("Mission Target").
	local TARvalue is box_TAR:ADDTEXTFIELD("Mercury").
	set TARvalue:style:width to 100.
	set TARvalue:style:height to 18.

local box_Res is wndw:addhlayout().
	local Res_label is box_Res:addlabel("Restart Location").
	local Resvalue is box_Res:ADDTEXTFIELD("0").
	set Resvalue:style:width to 100.
	set Resvalue:style:height to 18.

local somebutton is wndw:addbutton("Confirm").
set somebutton:onclick to Continue@.

// Show the GUI.
wndw:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
	WAIT 1.
}

Function Continue {

		set val to WAITvalue:text.
		set val to val:tonumber(0).
		set apwait to val.
		
		set val to ENDvalue:text.
		set val to val:tonumber(0).
		set endheight to val*1000.

		set val to MoonENDvalue:text.
		set val to val:tonumber(0).
		set endPE to val*1000.

		set val to TARvalue:text.
		set val to body(val).
		set L_TAR to val.

		set val to Resvalue:text.
		set val to val:tonumber(0).
		set runmode to val.

	wndw:hide().
  	set isDone to true.
}

Global boosterCPU is "Aethon3".

If runmode = 0{

	Print "Waitng for activation".
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
	Print "Explorer active".
	Lock Throttle to 0.
	Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
	ff_COMMS().
	RCS on.
	wait 10.
	//Circularise burn
	Lock Horizon to VXCL(UP:VECTOR, VELOCITY:SURFACE). //negative velocity makes it retrograde
	LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(0,
				VCRS(horizon,BODY:POSITION))*horizon,
				FACING:TOPVECTOR).//lock to prograde along horizon

	until (ETA:apoapsis) < apwait{
		wait 0.5.
	}
	Lock Throttle to 1.//start ullage
	wait 5.
	Stage.//start engine
	until ship:periapsis > endheight{
		Wait 0.1.
	}
	Local englist is List().
	LIST ENGINES IN engList. 
	FOR eng IN engList {  
		Print "eng:STAGE:" + eng:STAGE.
		Print STAGE:NUMBER.
		IF eng:STAGE >= STAGE:NUMBER { 
				eng:shutdown. 
		}
	}
	Print "Engine shutdown".
	Lock Throttle to 0.
	RCS off.
	Set runmode to 1.	
}
If runmode = 1{
	Local transnode is ff_transfer(L_TAR).
	Print "transnode: " + transnode.
	add node(transnode[0], transnode[1], transnode[2], transnode[3]).
	Set runmode to 1.5.
}

If runmode = 1.5{
	Local counter is 0.
	Until counter > 1200{
		Clearscreen.
		Print "Refine Node before: " + (1200-counter).
		wait 1.
		Set Counter to counter +1.
	}
	Set runmode to 2.
}

If runmode = 2{
	local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag / 2, 300, 76, 1)).
	Print "burn starts at: " + startTime.
	wait 5.
	wait until time:seconds > startTime - 120.
	RCS on.
	lock steering to nextnode:burnvector.
	wait until time:seconds > startTime-10.//RCS ullage Start
	SET SHIP:CONTROL:FORE TO 0.8.
	wait 10.
	Local englist is List().
	LIST ENGINES IN engList. 
	FOR eng IN engList {  
		Print "eng:STAGE:" + eng:STAGE.
		Print STAGE:NUMBER.
		IF eng:STAGE >= STAGE:NUMBER { 
			eng:activate. 
			Print "Engine". 
		}
	}
	lock throttle to 1.
	SET SHIP:CONTROL:FORE TO 0.0.
	until hf_isManeuverComplete(nextnode) {
	  	wait 0.01.
	}
	lock throttle to 0.
	unlock steering.
	RCS off.
	remove nextnode.
	Stage.//release second stage
	wait until stage:ready.
	Stage.//activate rcs engine
	Set runmode to 2.5.
}

If runmode = 2.5{
	until orbit:body = sun{
		Wait 10.
	}
	Local corr_time is time:seconds + (ship:orbit:period / 6).
	Print "Correction man at:" + corr_time.
	wait 5.
	add node(corr_time,0,0,0).
	until nextnode:eta < 5 {
		Wait 10.
	}
	remove nextnode.
	Set runmode to 3.
}

If runmode = 3{
	Local transnode is ff_transfer(L_TAR).
	local transmnv is node(transnode[0], transnode[1], transnode[2], transnode[3]).
	add transmnv.
	Set runmode to 3.5.
}

If runmode = 3.5{
	Local counter is 0.
	Until counter > 1200{
		Clearscreen.
		Print "Refine Node before: " + (1200-counter).
		wait 1.
		Set Counter to counter +1.
	}
	Set runmode to 4.
}

If runmode = 4{
	local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag / 2, 281, 3.64, 1)).
	Print "burn starts at: " + startTime.
	wait 5.
	wait until time:seconds > startTime - 60.
	RCS on.
	lock steering to nextnode:burnvector.
	wait until time:seconds > startTime.
	lock throttle to 1.
	until hf_isManeuverComplete(nextnode) {
			if ship:orbit:HASNEXTPATCH {
				if ship:orbit:nextPatch:periapsis < endPE {
					Break.
				}
		}
		wait 0.001.
	}
	lock throttle to 0.
	unlock steering.
	RCS off.
	remove nextnode.
	Set runmode to 5.
}

If runmode = 5{
	Set corr_time to time:seconds + ship:orbit:nextPatchEta.
	wait 5.
	until time:seconds +60 > corr_time {
		Wait 1.
	}

	Print "In SOI correction burn".
	wait 60.
	local normalVec is vcrs(ship:velocity:orbit,-body:position).
	local radialVec is vcrs(ship:velocity:orbit,normalVec).
	Print "Waiting for PE burn".
	// Lock sinc to ship:orbit:inclination.
	If ship:orbit:periapsis < endPE{
		Print "PE Change".
		RCS on.
		lock Steering to -radialVec.
		wait 20.
		lock throttle to 1.
		Until ship:orbit:periapsis > endPE{
			Wait 0.01.
		}
		lock throttle to 0.
		RCS off.
	}
	wait 1.0.
	If ship:orbit:periapsis > endPE{
		Print "PE Change 2".
		RCS on.
		lock Steering to radialVec.
		wait 20.
		lock throttle to 1.
		Until ship:orbit:periapsis > endPE{
			Wait 0.01.
		}
		lock throttle to 0.
		RCS off.
	}
	Set runmode to 6.
}

If Runmode = 6{

	Print "PE Burn Setup".
	Local orbspeed is sqrt(Body:MU/(endPE + body:radius)).
	Local BurnSpeed is velocityat(ship, eta:periapsis):orbit:mag - orbspeed.
	Set corr_time to time:seconds + eta:periapsis - (ff_Burn_Time(nextnode:deltaV:mag / 2, 281, 3.64, 1)).
	Print "Dv: " +BurnSpeed.
	Print corr_time. 
	wait 5.
	Local counter is 0.
	Until counter > 120{
		Clearscreen.
		Print "Refine Node before: " + (120-counter).
		wait 1.
		Set Counter to counter +1.
	}
	local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag / 2, 281, 3.64, 1)).
	Print "burn starts at: " + startTime.
	wait 5.
	wait until time:seconds > startTime - 70.
	lock steering to nextnode:deltav.
	RCS on.
	wait 70.
	wait until time:seconds > startTime.
	Lock Throttle to 1.
	until hf_isManeuverComplete(nextnode) {
			if ship:orbit:HASNEXTPATCH {
				if ship:orbit:nextPatch:periapsis < endPE {
					Break.
				}
		}
		wait 0.001.
	}
	lock throttle to 0.
	unlock steering.
	RCS off.
	remove nextnode.
	set runmode to 7.
}

if runmode = 7{
	wait 400.
	Shutdown.
}

function ff_Transfer {
  Parameter target. //
	Local First_Est is list().
	Set First_Est to ff_HohmannSun(L_TAR, endPE). 
	local first_dv is sqrt((First_Est[1]^2) + (2*body:mu/(body:radius+altitude)))-ship:orbit:velocity:orbit:mag.
 	local Ej_theta is constant:pi/2 - arccos((body:mu)/(body:mu + ((body:radius+altitude)*First_Est[1]^2))).
	Print "Ejection angle: " + Ej_theta.
	wait 60.
	Global direct is first_est[2].
	Print "direct: " + direct.
	Print first_dv.
	wait 10.
	Local start is first_est[0] + 60.
	Local end is first_est[0] + 10000.
	If orbit:apoapsis <0{ // orbit period = inf
		Set end to first_est[0] + 60 + 400.
	}	else{
				If orbit:period < 10000{
					Set end to first_est[0] + 60 + orbit:period.
				}
				if orbit:period > 10000{
					Set end to first_est[0] + 60 + 40000.
				}
	}
  local startSearchTime is hf_ternarySearchparam(
    hf_angleToplanet@, Ej_theta,
    start, end, 1, false
  ).
	If Ej_theta < 0 {
		Set startSearchTime to (startSearchTime + (ship:orbit:period/2)).
	}
  local transfer is list(startSearchTime, 0, 0, first_dv).// RADIALOUT, NORMAL, PROGRADE 
  return transfer.
}

function hf_angleToplanet {
  parameter t, Ejec_theta.
	local ang is vectorAngle(
		velocityat(ship,t):orbit,
		velocityat(ship:body,t):orbit
  ).
	local result is 0.
	If direct = 1{
		set result to abs(ang - Ejec_theta).
	}Else {
		set result to abs((180 - ang) - Ejec_theta).
	}
  return result.
}

function hf_ternarySearchparam {
  parameter f, f_para, left, right, absolutePrecision, maxVal is true. // MaxVal is used to dertmine if max or min is desired
  until false {
    if abs(right - left) < absolutePrecision {
      return (left + right) / 2.
    }
    local leftThird is left + (right - left) / 3.
    local rightThird is right - (right - left) / 3.
		if maxval = true{
			if f(leftThird,f_para) < f(rightThird, f_para) {
				set left to leftThird.
			} else {
				set right to rightThird.
			}
		}
		if maxval = false{
			if f(leftThird, f_para) > f(rightThird, f_para) {
				set left to leftThird.
			} else {
				set right to rightThird.
			}
		}
  }
}

function hf_isManeuverComplete {
  parameter mnv.
  if not(defined originalVector) or originalVector = -1 {
    declare global originalVector to mnv:burnvector.
  }
  if vang(originalVector, mnv:burnvector) > 90 {
    declare global originalVector to -1.
    return true.
  }
  return false.
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

FUNCTION ff_HohmannSun{

PARAMETER tgt, t_pe is 0, trans_bod is sun, inc_tgt is 0. // trans_bod should be sun for planet transfers
	list bodies.
	Local Curr_time is time:seconds.
	Set Ship_Orbit to ORBITAT(SHIP:body,Curr_time).
	LOCAL tgt_Orbit is ORBITAT(tgt,Curr_time).
	LOCAL r1 is Ship_Orbit:SEMIMAJORAXIS.
	LOCAL r2 is tgt_Orbit:SEMIMAJORAXIS + t_pe.

	LOCAL dvDepart is SQRT(trans_bod:MU/r1) * (SQRT((2*r2)/(r1+r2)) -1). // wiki Dv1 Equation
	LOCAL dvArrive is SQRT(trans_bod:MU/r1) * (1- SQRT((2*r2)/(r1+r2))). // wiki Dv2 Equation
	local dir is 0.
	
	if r2 < r1 { 
		SET dvDepart TO -dvDepart. // this allows for transfers to a lower orbit
		SET dvArrive TO -dvArrive.
		set dir to 1.
	}
	
	if -dvDepart = dvArrive {
		set dvArrive to 0. // allows for transfers within the same SOI where the dv arrive and depart are the same.
	}
	local dv is dvDepart + dvArrive.
	LOCAL Trans_time is CONSTANT:PI * SQRT( ((r1+r2)^3) / (8 * trans_bod:MU) ). // wiki transfer orbit time Equation
	LOCAL Tgt_travel_ang is (Trans_time / tgt_Orbit:PERIOD)* 360. // the angle the tgt moves during the transist assuming a circular orbit
	LOCAL desired_phi is 180 - Tgt_travel_ang. // we want to meet the target at apoapsis so the target need to travel and end 180 degrees from where we start.
	LOCAL rel_ang_Change is (360 / Ship_Orbit:PERIOD) - (360 / tgt_Orbit:PERIOD). // the degrees the tgt moves each orbit by the ship each second.
	LOCAL ship_pos is positionat(SHIP:body, Curr_time)-trans_bod:position. //current position of the ship body
	LOCAL tgt_pos is positionat(tgt, Curr_time)-trans_bod:position. //current position of the target
	LOCAL start_phi is VANG(ship_pos,tgt_pos). // the current angle between the ship and the tgt.
	LOCAL ship_normal IS VCRS(VELOCITYAT(SHIP:body,curr_time):ORBIT,ship_pos).// the plane of the ship body
	LOCAL ship_tgt_cross IS VCRS(ship_pos,tgt_pos).//// plane of the transfer (ie. incination diference)
	if VDOT(ship_normal, ship_tgt_cross) > 0 { 
		SET start_phi TO 360 - start_phi. 
	} 
	LOCAL phi_delta is hf_mAngle(start_phi - desired_phi). //this determines how far off the best phase angle is.
	if rel_ang_Change < 0 { 
		SET phi_delta TO phi_delta - 360. //adjust for negative angle change values
	}
	Local node_time is Curr_time + (phi_delta / rel_ang_Change).
  return list(node_time, dv, dir).
}

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
				if antenna:HASFIELD("target"){
					antenna:Setfield("target", "Earth").
				}
				PRINT event + " Antennas".
				WAIT stageWait.
			}	
		}.
	}
} // End of Function
