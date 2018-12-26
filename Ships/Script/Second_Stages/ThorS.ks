// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("76").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("170").
	set ENDvalue:style:width to 100.
	set ENDvalue:style:height to 18.

local box_MoonEND is wndw:addhlayout().
	local MoonEND_label is box_MoonEND:addlabel("Moon PE END (km)").
	local MoonENDvalue is box_MoonEND:ADDTEXTFIELD("20").
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

		set val to WAITvalue:text.
		set val to val:tonumber(0).
		set apwait to val.

		set val to ENDvalue:text.
		set val to val:tonumber(0).
		set endheight to val*1000.

		set val to MoonENDvalue:text.
		set val to val:tonumber(0).
		set endPE to val*1000.

	wndw:hide().
  	set isDone to true.
}
Global boosterCPU is "Aethon3".
Global PrimTarget is moon.

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
Print "Thor active".
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
ff_COMMS().
LOCK STEERING TO PROGRADE.
RCS on.

//Circularise burn
Lock Horizon to VXCL(UP:VECTOR, VELOCITY:SURFACE). //negative velocity makes it retrograde
LOCK STEERING TO LOOKDIRUP(ANGLEAXIS(0,
			VCRS(horizon,BODY:POSITION))*horizon,
			FACING:TOPVECTOR).//lock to prograde along horizon

until (ETA:apoapsis) < apwait{
	wait 0.5.
}
SET SHIP:CONTROL:FORE TO 0.9.//start ullage using RCS
wait 6.
Lock Throttle to 1.
SET SHIP:CONTROL:FORE to 0.
Stage.//start engine
until ship:periapsis > endheight{
	Wait 0.01.
}
Lock Throttle to 0.
RCS off.

////Transfer to moon
Local transnode is ff_transfer().
Local counter is 0.
	Until counter > 120{
		Clearscreen.
		Print "Refine Node before: " + (120-counter).
		wait 1.
		Set Counter to counter +1.
	}
local startTime is time:seconds + nextnode:eta - (ff_Burn_Time(nextnode:deltaV:mag/ 2, 278, 35.1, 1)).
Print (ff_Burn_Time(nextnode:deltaV:mag/ 2, 278, 35.1, 1)).
Print "burn starts at: " + startTime.
Print nextnode:orbit:nextPatch:inclination.
wait 5.
warpto(startTime - 150).
wait until time:seconds > startTime - 65.
RCS on.
SAS on.
wait 1. 
Set SASMODE to "MANEUVER". //SAS tend to use less RCS than KOS steering but must have connection and good SAS core
wait until time:seconds > startTime-6.//RCS ullage Start
SAS off.
lock steering to nextnode:burnvector.
SET SHIP:CONTROL:FORE TO 0.9.//start ullage using RCS
wait 6.
Lock Throttle to 1.//Start main engine
SET SHIP:CONTROL:FORE to 0.
until hf_isManeuverComplete(nextnode) {
	if ship:orbit:HASNEXTPATCH {
		if ship:orbit:nextPatch:periapsis < endPE {
			lock throttle to 0.
			Break.
			Print "Break intiated".
		}
	}
  wait 0.001.
}
lock throttle to 0.
unlock steering.
remove nextnode.
RCS off.
wait 10.
Set corr_time to time:seconds + ship:orbit:nextPatchEta.
wait 5.
warpto(corr_time - 25).
until time:seconds +60 > corr_time {
	Wait 1.
}

Print "In SOI correction burn: " + time:clock.
wait 600.
local normalVec is vcrs(ship:velocity:orbit,-body:position).
local radialVec is vcrs(ship:velocity:orbit,normalVec).
SAS on. 
RCS on.
wait 1.
SET SASMODE TO "RadialIn".//SAS tend to use less RCS than KOS steering but must have connection and good SAS core
wait 30.
SAS off.
lock Steering to radialVec.
Local nowPe is ship:orbit:periapsis.
SET SHIP:CONTROL:FORE TO 0.9.
wait 1.0.
SET SHIP:CONTROL:FORE TO 0.0.
Print nowPE.
Print ship:orbit:periapsis.
Local RCSDir is "fwd".
if ship:orbit:periapsis < nowPe{
Set RCSDir to "back".
}
If ship:orbit:periapsis < endPE {
	Print "PE increase required".
	if RCSDir = "fwd"{
		SET SHIP:CONTROL:FORE TO 0.9.
	}
	if RCSDir = "Back"{
		SET SHIP:CONTROL:FORE TO -0.9.
	}
	Until ship:orbit:periapsis > endPE{
		Wait 0.01.
	}
}
SET SHIP:CONTROL:FORE TO 0.0.
If ship:orbit:periapsis > endPE{
	Print "PE decrease required".
	if RCSDir = "fwd"{
		SET SHIP:CONTROL:FORE TO -0.9.
	}
	if RCSDir = "Back"{
		SET SHIP:CONTROL:FORE TO 0.9.
	}
	Until ship:orbit:periapsis < endPE{
		Wait 0.01.
	}
	SET SHIP:CONTROL:FORE to 0.

}
RCS off.
SET SHIP:CONTROL:FORE to 0.
unlock steering.
Print "Waiting for PE burn".
Print "PE Burn Setup".
Local orbspeed is sqrt(Body:MU/(endPE + body:radius)).
Local BurnSpeed is velocityat(ship, eta:periapsis):orbit:mag - orbspeed.
Set corr_time to time:seconds + eta:periapsis.
Print eta:periapsis.
Print "Dv: " +BurnSpeed.
Print corr_time. 
wait 5.
warpto (corr_time-180).
until eta:periapsis < 40{
	wait 2.
}
Print "Starting PE Burn".
Set warp to 0.
Lock steering to retrograde.
RCS on.
wait 20.
SET SHIP:CONTROL:FORE TO 0.9.
wait 5.
Print "Throttle up".
lock throttle to 1.
wait 0.5.
SET SHIP:CONTROL:FORE to 0.
Print AVAILABLETHRUST.
wait 10.
until (ship:orbit:apoapsis < 1.2*endPE) and (ship:orbit:apoapsis > 0){
	wait 0.1.
	if (AVAILABLETHRUST*1000) < 1 {
		Stage.
		wait until stage:ready.
		wait 0.1.
		Stage.
		break.
	}
}
lock throttle to 0.
RCS off.
wait 10.
Stage.//move to lander only

//Commence lander script
wait 10.
Shutdown.
function ff_Transfer {
  Parameter target is PrimTarget, First_Est is ff_Hohmann(moon). //
	Local start is time:seconds + 60.
	Local end is time:seconds + 10000.
	If orbit:apoapsis <0{ // orbit period = inf
		Set end to time:seconds + 60 + 400.
	}	else{
				If orbit:period < 10000{
					Set end to time:seconds + 60 + orbit:period.
				}
				if orbit:period > 10000{
					Set end to time:seconds + 60 + 40000.
				}
	}
  local startSearchTime is hf_ternarySearchparam(
    hf_angleToMoon@, target,
    start, end, 1
  ).
  local transfer is ff_seek (startSearchTime, ff_freeze(0),ff_freeze(0),first_est, hf_moonTransferScore@).// t, RADIALOUT, NORMAL, PROGRADE, func 
  return transfer.
}

function hf_angleToMoon {
  parameter t, Target is PrimTarget.
  return vectorAngle(
    Ship:body:position - positionAt(ship, t),
    Ship:body:position - positionAt(Target, t)
  ).
}

function hf_moonTransferScore {
  parameter mnv, targ is PrimTarget.
  local result is 0.
  if mnv:orbit:hasNextPatch {
	  	if mnv:burnvector:mag < 3160{
    		set result to -abs((mnv:orbit:nextPatch:periapsis - endPE)) -((mnv:burnvector:mag)*100)- ((mnv:orbit:nextPatch:inclination)*10).
	 	} Else{
			Set result to -10*abs((mnv:orbit:nextPatch:periapsis - endPE)) -((mnv:burnvector:mag)*100)- ((mnv:orbit:nextPatch:inclination)*10).
		}
  } else {
    set result to -10000000*hf_distanceToMoonAtApoapsis(mnv, targ).
  }
  print "results: " + result.
  Print targ.
  return result.
}

function hf_distanceToMoonAtApoapsis {
  parameter mnv, targ is PrimTarget.
  local apoapsisTime is hf_ternarySearch(
    hf_altitudeAt@,
    time:seconds + mnv:eta, 
    time:seconds + mnv:eta + (mnv:orbit:period / 2),
    1
  ).
  return (positionAt(ship, apoapsisTime) - positionAt(targ, apoapsisTime)):mag.
}

function hf_altitudeAt {
  parameter t.
  return ship:body:altitudeOf(positionAt(ship, t)).
}

function hf_ternarySearch {
  parameter f, left, right, absolutePrecision.
  until false {
    if abs(right - left) < absolutePrecision {
      return (left + right) / 2.
    }
    local leftThird is left + (right - left) / 3.
    local rightThird is right - (right - left) / 3.
    if f(leftThird) < f(rightThird) {
      set left to leftThird.
    } else {
      set right to rightThird.
    }
  }
}

function hf_ternarySearchparam {
  parameter f, f_para, left, right, absolutePrecision.
  until false {
    if abs(right - left) < absolutePrecision {
      return (left + right) / 2.
    }
    local leftThird is left + (right - left) / 3.
    local rightThird is right - (right - left) / 3.
    if f(leftThird,f_para) < f(rightThird,f_para) {
      set left to leftThird.
    } else {
      set right to rightThird.
    }
  }
}

function ff_freeze {
	parameter n. 
	return lex("frozen", n).
}/// End Function

function ff_seek {
	parameter t, r, n, p, fitness, fine is False,
			  data is list(t, r, n, p),
			  fit is hf_orbit_fitness(fitness@).  // time, radial, normal, prograde, fitness are the parameters passed in for the node to be found. passes fitness through as a delegate to orbital fitness in this case { parameter mnv. return -mnv:orbit:eccentricity. } is passed through as a local function but any scorring evaluation can be passed through
	set data to ff_optimize(data, fit, 30).
	Print "Seek 30".
	wait 2.
	set data to ff_optimize(data, fit, 10).
	Print "Seek 10".
	wait 2.
	set data to ff_optimize(data, fit, 1). 
	Print "Seek 1".
	wait 2.
	If Fine{
		set data to ff_optimize(data, fit, 0.1). // search in 0.1m/s incriments
		Print "Seek 0.1".
	}
	fit(data). //sets the final manuver node and returns its parameters
	wait 0. 
	return data. // returns the manevour node parameters to where the function was called
}/// End Function

function ff_optimize {
	parameter data, fitness, step_size,
	winning is list(fitness(data), data),
	improvement is hf_best_neighbor(winning, fitness, step_size). 
	until improvement[0] <= winning[0] { 
	  set winning to improvement. 
	  set improvement to hf_best_neighbor(winning, fitness, step_size). 
	}
	return winning[1]. // returns the second column of the winning list "(data)", instead of "fitness(data)"
 }/// End Function

// identifies if the paramter is frozen
function hf_frozen {
	parameter v. 
	return (v+""):indexof("frozen") <> -1.
}/// End Function

// Returns paramters from the frozen lexicon
function hf_unfreeze {
	parameter v. 
	if hf_frozen(v) return v["frozen"]. 
	else return v.
}/// End Function
	
function hf_orbit_fitness {
	parameter fitness. // the parameter used to evaluate fitness
	return {
		parameter data.
		until not hasnode { 
			remove nextnode. // Used to remove any existing nodes
			wait 0. 
		} 
		Print "orb fit create node".
		local new_node is node(
		hf_unfreeze(data[0]), hf_unfreeze(data[1]),
		hf_unfreeze(data[2]), hf_unfreeze(data[3])). 
		add new_node. 
		//Print new_node.
		wait 0.
		return fitness(new_node). // returns the manevour node parameters to where the function was called
	}.
}/// End Function
	
function hf_best_neighbor {
	parameter best, fitness, step_size. 
	for neighbor in hf_neighbors(best[1], step_size) { 
		local score is fitness(neighbor). 
		if score > best[0] set best to list(score, neighbor). 
	}
	return best. 
}/// End Function

function hf_neighbors {
	parameter data, step_size, results is list().
	for i in range(0, data:length) if not hf_frozen(data[i]) { 
		local increment is data:copy.
		local decrement is data:copy.
		set increment[i] to increment[i] + step_size. 
		set decrement[i] to decrement[i] - step_size. 
		results:add(increment).
		results:add(decrement).
	}
	return results. // Return the list of neighbours for the data that can be changed (i.e. unfrozen)
}  /// End Function	

function hf_isManeuverComplete {
  parameter mnv.
  if not(defined originalVector) or originalVector = -1 {
    declare global originalVector to mnv:burnvector.
  }
  if vang(originalVector, mnv:burnvector) > 45 {
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
	//TODO:Think about removing RCS components or making it an input term as this could be a significant proportion of the deltaV which is not used.
	return (isp * g * ln(m / (m - fuelMass))).
}./// End Function

function ff_burn_time {
parameter dV, isp is 0, thrust_s is 0, engine_count is 0. // For RSS/RO engine values must be given unless they are actually burning.
lock throttle to 0.
Print "Burntime".
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local m is ship:mass * 1000. // Starting mass (kg)
	local e is constant():e. // Base of natural log
	local thrust is thrust_s.
	//TODO: look at comapring the dv with the ff_stage_delta_v. If less look at the engine in the next stage and determine the delta_v and time to burn until the dv has been meet.
	If engine_count = 0{ // only evaluate is figures not given
	print "engine calc".
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
	Print m.
	Print isp.
	Print Thrust.
	return g * m * isp * (1 - e^(-dV/(g*isp))) / thrust.
}/// End Function

FUNCTION ff_Hohmann{

PARAMETER tgt, t_pe is 0, trans_bod is Ship:BODY, inc_tgt is 0. // trans_bod should be sun for planet transfers
	
	Local Curr_time is time:seconds.
	LOCAL Ship_Orbit is ORBITAT(SHIP,Curr_time). //ORBITAT(orbitable,time) is KOS in-built function
	LOCAL tgt_Orbit is ORBITAT(tgt,Curr_time).
	LOCAL r1 is Ship_Orbit:SEMIMAJORAXIS.
	LOCAL r2 is tgt_Orbit:SEMIMAJORAXIS + t_pe.

	LOCAL dvDepart is SQRT(trans_bod:MU/r1) * (SQRT((2*r2)/(r1+r2)) -1). // wiki Dv1 Equation
	LOCAL dvArrive is SQRT(trans_bod:MU/r1) * (1- SQRT((2*r2)/(r1+r2))). // wiki Dv2 Equation
	
	if r2 < r1 { 
		SET dvDepart TO -dvDepart. // this allows for transfers to a lower orbit
		SET dvArrive TO -dvArrive.
	}
	
	if -dvDepart = dvArrive {
		set dvArrive to 0. // allows for transfers within the same SOI where the dv arrive and depart are the same.
	}

	local dv is dvDepart + dvArrive.
	LOCAL Trans_time is CONSTANT:PI * SQRT( ((r1+r2)^3) / (8 * trans_bod:MU) ). // wiki transfer orbit time Equation
	LOCAL Tgt_travel_ang is (Trans_time / tgt_Orbit:PERIOD)* 360. // the angle the tgt moves during the transist assuming a circular orbit
	LOCAL desired_phi is 180 - Tgt_travel_ang. // we want to meet the target at apoapsis so the target need to travel and end 180 degrees from where we start.
	LOCAL rel_ang_Change is (360 / Ship_Orbit:PERIOD) - (360 / tgt_Orbit:PERIOD). // the degrees the tgt moves each orbit by the ship each second.
	LOCAL ship_pos is positionat(SHIP, Curr_time)-ship:body:position. //current position of the ship
	LOCAL tgt_pos is positionat(tgt, Curr_time)-tgt:body:position. //current position of the target
	LOCAL start_phi is VANG(ship_pos,tgt_pos). // the current angle between the ship and the tgt.
	LOCAL ship_normal IS VCRS(VELOCITYAT(SHIP,curr_time):ORBIT,ship_pos).// the plane of the ship
	LOCAL ship_tgt_cross IS VCRS(ship_pos,tgt_pos).//// plane of the transfer (ie. incination diference)
	
	if VDOT(ship_normal, ship_tgt_cross) > 0 { 
		SET start_phi TO 360 - start_phi. 
	} // this checks to see if the planes are pointed in the same direction or are pointed opposite to one another so it is known if ship is leading or lagging the tgt. 

	LOCAL phi_delta is hf_mAngle(start_phi - desired_phi). //this determines how far off the best phase angle is.
	if rel_ang_Change < 0 { 
		SET phi_delta TO phi_delta - 360. //adjust for negative angle change values
	}
	Local node_time is Curr_time + (phi_delta / rel_ang_Change).
  return dv.

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
				PRINT event + " Antennas".
				WAIT stageWait.
			}	
		}.
	}
} // End of Function
