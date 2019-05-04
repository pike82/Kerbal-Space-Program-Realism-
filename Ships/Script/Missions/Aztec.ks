// Get Mission Values

local wndw is gui(300).
set wndw:x to 700. //window start position
set wndw:y to 120.

local label is wndw:ADDLABEL("Enter Mission Values").
set label:STYLE:ALIGN TO "CENTER".
set label:STYLE:HSTRETCH TO True. // Fill horizontally

local box_WAIT is wndw:addhlayout().
	local WAIT_label is box_WAIT:addlabel("AP WAIT").
	local WAITvalue is box_WAIT:ADDTEXTFIELD("35").
	set WAITvalue:style:width to 100.
	set WAITvalue:style:height to 18.

local box_END is wndw:addhlayout().
	local END_label is box_END:addlabel("PE END (km)").
	local ENDvalue is box_END:ADDTEXTFIELD("170").
	set ENDvalue:style:width to 100.
	set ENDvalue:style:height to 18.

local box_MoonEND is wndw:addhlayout().
	local MoonEND_label is box_MoonEND:addlabel("Moon PE END (km)").
	local MoonENDvalue is box_MoonEND:ADDTEXTFIELD("2000").
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

Global boosterCPU is "Aethon".

Print "Restart before AP: " + apwait + "s".
Print "Stop burn at: " + endheight + "m".
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
Print "Aztec active".
LOCK STEERING TO PROGRADE.
RCS on.
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
ff_COMMS().
//Circularise burn

until (ETA:apoapsis) < apwait{
	wait 0.5.
}
Lock Throttle to 1.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
Stage.//start ullage
Wait until Stage:Ready.
wait 0.5.
Stage.//start engine

until ship:periapsis > endheight{
	Wait 0.1.
}
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
RCS off.
////TODO: USe RCS to align with moon via transfer@ then shoot then ullage and burn to moon.
Local transnode is ff_transfer(moon).
local transmnv is node(transnode[0], transnode[1], transnode[2], transnode[3]).
add transmnv.
local startTime is time:seconds + transmnv:eta - (ff_Burn_Time(transmnv:deltaV:mag, 231*0.85, 21.28*0.85, 1) / 2).
Print time:seconds + transmnv:eta.
Print transmnv:eta.
Print transmnv:deltaV:mag.
Print (ff_Burn_Time(transmnv:deltaV:mag, 231, 21.28, 1) / 2).
Print "burn starts at: " + startTime.
wait until time:seconds > startTime - 180.
Set warp to 0.
RCS on.
lock steering to transmnv:burnvector.
wait until time:seconds > startTime-0.2.//RCS ullage Start
lock throttle to 1.
Stage. //Start ullage engines
Wait until Stage:Ready.
stage.//Start main engines
until hf_isManeuverComplete(transmnv) {
	if ship:orbit:HASNEXTPATCH {
		if ship:orbit:nextPatch:periapsis < endPE {
			Break.
		}
	}
	if AVAILABLETHRUST < 1{
		Stage.
		Wait 1.
	}
  wait 0.001.
}
lock throttle to 0.
unlock steering.
RCS off.
remove transmnv.

////TODO: USe RCS final stage to make correction and slow down and hit the moon.
Local corr_time is time:seconds + (ship:orbit:nextPatchEta / 2).
Print "Correction man at:" + corr_time.
warpto(corr_time - 15).
until time:seconds > corr_time {
	Wait 1.
}

Local transnode is ff_transfer(moon, 0).
local transmnv is node(transnode[0], transnode[1], transnode[2], transnode[3]).
add transmnv.
local startTime is time:seconds + transmnv:eta - (ff_Burn_Time(transmnv:deltaV:mag, 198, 1, 1) / 2).
Print "burn starts at: " + startTime.
warpto(startTime - 15).
wait until time:seconds > startTime - 10.
Set warp to 0.
RCS on.
lock steering to transmnv:burnvector.
wait until time:seconds > startTime.
lock throttle to 1.
until hf_isManeuverComplete(transmnv) {
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
remove transmnv.

Set corr_time to time:seconds + ship:orbit:nextPatchEta.
until time:seconds +60 > corr_time {
	Wait 1.
}

Print "In SOI correction burn".
wait 60.
lock Steering to retrograde.
RCS on.
wait 20.
lock throttle to 1.
Until orbit:periapsis < endPE{
	Wait 1.
}
Lock throttle to 0.
RCS off.
wait 120.
Print "Waiting for PE burn".
lock Steering to retrograde.
Until (eta:periapsis < 90) or altitude < 20000{
	Wait 1.
}
RCS on.
wait 5.
lock throttle to 1.
wait 400.
Shutdown.

////TODO: Fix up below to allow transfer to specific PE and inclination

function ff_Transfer {
  Parameter target, First_Est is ff_Hohmann(moon). //
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
  local transfer is list(startSearchTime, 0, 0, first_est).// RADIALOUT, NORMAL, PROGRADE TODO: undertake homman trans fer calc to get intial prograge value to deduce find time.
  set transfer to hf_improveConverge(transfer, hf_protectFromPast(hf_moonTransferScore@, target)).
  return transfer.
}

function hf_angleToMoon {
  parameter t, Target.
  return vectorAngle(
    Ship:body:position - positionAt(ship, t),
    Ship:body:position - positionAt(Target, t)
  ).
}

function hf_moonTransferScore {
  parameter data, target.
  local mnv is node(data[0], data[1], data[2], data[3]).
  add mnv.
  local result is 0.
  if mnv:orbit:hasNextPatch {
    set result to mnv:orbit:nextPatch:periapsis.// + (mnv:orbit:nextPatch:inclination*10).
  } else {
    set result to hf_distanceToMoonAtApoapsis(mnv, target).
  }
  remove mnv.
  return result.
}

function hf_distanceToMoonAtApoapsis {
  parameter mnv, target.
  local apoapsisTime is hf_ternarySearch(
    hf_altitudeAt@,
    time:seconds + mnv:eta, 
    time:seconds + mnv:eta + (mnv:orbit:period / 2),
    1
  ).
  return (positionAt(ship, apoapsisTime) - positionAt(target, apoapsisTime)):mag.
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

function hf_eccentricityScore {
  parameter data.
  local mnv is node(time:seconds + eta:apoapsis, 0, 0, data[0]).
  add mnv.
  local result is mnv:orbit:eccentricity.
  remove mnv.
  return result.
}

function hf_improveConverge {
  parameter data, scoreFunction.
  for stepSize in list(50, 5, 0.5) {
    until false {
      local oldScore is scoreFunction(data).
      set data to hf_improve(data, stepSize, scoreFunction).
      if oldScore <= scoreFunction(data) {
        break.
      }
    }
  }
  return data.
}

function hf_improve {
  parameter data, stepSize, scoreFunction.
  local scoreToBeat is scoreFunction(data).
  local bestCandidate is data.
  local candidates is list().
  local index is 0.
  until index >= data:length {
    local incCandidate is data:copy().
    local decCandidate is data:copy().
    set incCandidate[index] to incCandidate[index] + stepSize.
    set decCandidate[index] to decCandidate[index] - stepSize.
    candidates:add(incCandidate).
    candidates:add(decCandidate).
    set index to index + 1.
  }
  for candidate in candidates {
    local candidateScore is scoreFunction(candidate).
    if candidateScore < scoreToBeat {
      set scoreToBeat to candidateScore.
      set bestCandidate to candidate.
    }
  }
  return bestCandidate.
}

function hf_protectFromPast {
  parameter originalFunction, target.
  local replacementFunction is {
    parameter data.
    if data[0] < time:seconds + 15 {
      return 2^64.
    } else {
      return originalFunction(data, target).
    }
  }.
  return replacementFunction@.
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

FUNCTION ff_Hohmann{

PARAMETER tgt, t_pe is 0, trans_bod is Ship:BODY, inc_tgt is 0. // trans_bod should be sun for planet transfers
	
	Local Curr_time is time:seconds.
	Print "Curr_time: "+ Curr_time.
	LOCAL Ship_Orbit is ORBITAT(SHIP,Curr_time). //ORBITAT(orbitable,time) is KOS in-built function
	LOCAL tgt_Orbit is ORBITAT(tgt,Curr_time).
	LOCAL r1 is Ship_Orbit:SEMIMAJORAXIS.
	LOCAL r2 is tgt_Orbit:SEMIMAJORAXIS + t_pe.

	LOCAL dvDepart is SQRT(trans_bod:MU/r1) * (SQRT((2*r2)/(r1+r2)) -1). // wiki Dv1 Equation
	LOCAL dvArrive is SQRT(trans_bod:MU/r1) * (1- SQRT((2*r2)/(r1+r2))). // wiki Dv2 Equation
	Print dvDepart.
	Print dvArrive.
	
	if r2 < r1 { 
		SET dvDepart TO -dvDepart. // this allows for transfers to a lower orbit
		SET dvArrive TO -dvArrive.
	}
	
	if -dvDepart = dvArrive {
		set dvArrive to 0. // allows for transfers within the same SOI where the dv arrive and depart are the same.
	}
	Print "dvArrive: " + dvArrive.

	local dv is dvDepart + dvArrive.
	Print "dv int:" + dv.
	LOCAL Trans_time is CONSTANT:PI * SQRT( ((r1+r2)^3) / (8 * trans_bod:MU) ). // wiki transfer orbit time Equation
	Print "Trans_time: "+ Trans_time.
	Print tgt_Orbit:PERIOD.
	LOCAL Tgt_travel_ang is (Trans_time / tgt_Orbit:PERIOD)* 360. // the angle the tgt moves during the transist assuming a circular orbit
	Print Tgt_travel_ang.
	LOCAL desired_phi is 180 - Tgt_travel_ang. // we want to meet the target at apoapsis so the target need to travel and end 180 degrees from where we start.
	Print desired_phi.
	LOCAL rel_ang_Change is (360 / Ship_Orbit:PERIOD) - (360 / tgt_Orbit:PERIOD). // the degrees the tgt moves each orbit by the ship each second.
	Print rel_ang_Change.
	LOCAL ship_pos is positionat(SHIP, Curr_time)-ship:body:position. //current position of the ship
	LOCAL tgt_pos is positionat(tgt, Curr_time)-tgt:body:position. //current position of the target
	
	LOCAL start_phi is VANG(ship_pos,tgt_pos). // the current angle between the ship and the tgt.
	Print start_phi.
	
	LOCAL ship_normal IS VCRS(VELOCITYAT(SHIP,curr_time):ORBIT,ship_pos).// the plane of the ship
	LOCAL ship_tgt_cross IS VCRS(ship_pos,tgt_pos).//// plane of the transfer (ie. incination diference)
	
	Print ship_normal.
	Print ship_tgt_cross.
	
	Print VDOT(ship_normal, ship_tgt_cross).
	
	if VDOT(ship_normal, ship_tgt_cross) > 0 { 
		SET start_phi TO 360 - start_phi. 
	} // this checks to see if the planes are pointed in the same direction or are pointed opposite to one another so it is known if ship is leading or lagging the tgt. 

	LOCAL phi_delta is hf_mAngle(start_phi - desired_phi). //this determines how far off the best phase angle is.
	Print "phi_delta: " + phi_delta.
	if rel_ang_Change < 0 { 
		SET phi_delta TO phi_delta - 360. //adjust for negative angle change values
	}
	Print rel_ang_Change.
	Print (phi_delta / rel_ang_Change).
	Print Curr_time.
	Local node_time is Curr_time + (phi_delta / rel_ang_Change).
	//Print node_time.
	//LOCAL first_est is NODE(node_time, 0, 0, dv). // this creates the node (best refined by a hill climb) which can be used to gain a good first approximation of the time required to speed up the solution.
	//Print "Node time: " + node_time.
	Print "DV: " + dv.
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