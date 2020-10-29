; FlowLogo, copyright 2014 Juan Carlos Castilla

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXTENSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [matrix py]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VARIABLE DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
breed [farmerwells farmerwell]
breed [townwells townwell]


globals
[
  ;;;;;;groundwater model formulation;;;;;;
  max-error
  max-head
  min-head
  max-K
  min-K
  iterations
  aquifer-width
  A
  A-row-list
  A-row-reduced-list
  A-row-reduced-matrix
  A-column-list
  A-column-reduced-list
  A-reduced
  inverse-A
  C
  C-row-list
  C-row-reduced-list
  C-row-reduced-matrix
  C-reduced
  solution-vector
  number-of-unknowns
  K-patch-read

  ;;;;;;;;lists of patches;;;;;;;
  no-flow-cells-list
  fixed-head-cells-list
  remove-cells-list
  remove-cells-list-corrected
  active-cells-remap
  active-cells-remap-indexes

  ;;;;;;;multiplier functions;;;;;;;
  sine-recharge
  sine-well

  ;;;;;;river budget term;;;;;;;;
  riv-budget

  ;;;;;;well pumping budget term;;;;;;;;
  well-budget

  ;;;;;;SOCIAL MODEL;;;;;;
  year
  month
  Qfarmer

]

patches-own
[
  ;;;;;;;;source and sink terms;;;;;;;

  Qwell                                    ;; well pumping rate                                        [L3/T]
  Qwell-temp                               ;; temporary dummy variable to initialize heads

  Qinjection                               ;; well injection rate                                      [L3/T]
  Qinjection-temp                          ;; temporary dummy variable to initialize heads

  Recharge                                 ;; recharge                                                 [L3/L2*T] ~ [L/T]
  Recharge-temp                            ;; temporary dummy variable to initialize heads

  ET                                       ;; evapotranspiration, function of depth to water table     [L3/L2*T] ~ [L/T]
  ET-max-rate-temp                         ;; temporary dummy variable to initialize heads
  ET-max-rate-patch
  ET-extinction-depth-patch
  ET-land-surface-elevation-patch

  DRAIN                                    ;; drain discharge, depends on h-d             [L3/T] Note: divide by cell area in sourceterm equation
  DRAIN-elevation-patch
  DRAIN-conductance-patch

  RIV                                      ;; leakage from river                          [L3/L2*T] ~ [L/T]
  RIV-elevation-patch
  RIV-bottom-patch
  RIV-conductance-patch

  ;;;;;;patch hydraulic parameters;;;;;;;
  S-patch                                  ;; storativity of each patch
  S-patch-temp                             ;; temporary dummy variable to solve for steady state
  K-patch                                  ;; conductivity of each patch
  T-patch                                  ;; transmissivity of each patch

  ;;;;;;;;;type of patch;;;;;;;;
  interior-node?                           ;; is this an active patch?
  adjacent-to-boundary?                    ;; is this patch adjacent to a boundary?
  interior-but-corner?                     ;; out of the interior nodes, is it in the corner?

  ;;;;;;patch bc's;;;;;
  no-flow?                                 ;; is this a no-flow cell?
  fixed-head?                              ;; is this a fixed head cell?
  fixed-flux?                              ;; is this a fixed flux cell?

  ;;;;;;patch neighbours;;;;
  N-neighbour
  S-neighbour
  E-neighbour
  W-neighbour

  ;;;;;;patch features;;;;;
  well?
  injection?
  recharge?
  ET?
  DRAIN?
  RIV?

  ;;;;;;;head values;;;;;;
  H                                       ;; heads at the new timestep
  Hinew                                   ;; heads at the new iteration
  Hinitial                                ;; a reference H to calculate drawdowns

  ;;;;;;solution process parameters;;;;;
  TN
  TS
  TE
  TW
  TC
  Ncof
  Scof
  Ecof
  Wcof
  Ccof
  SourceTerm
  RHS
  patch-number-tag

  ;;;;;;;;;SOCIAL MODEL;;;;;;;;;

]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to refresh-view
  if view = "patch numbering" [ask patches with [interior-node? = true] [set plabel precision patch-number-tag 1 set plabel-color black]]
  if view = "boundary conditions" [view-bc ask patches with [no-flow? = true][set pcolor black] ask patches with [fixed-head? = true][set pcolor blue]ask patches with [RIV? = true][set pcolor cyan] ask patches with [well? = true][set pcolor red] ask patches with [injection? = true][set pcolor blue]]
  if view = "wells" [ask patches with [interior-node? = true][set plabel ""] ask patches with [well? = true][set plabel-color black set plabel "W"] ask patches with [injection? = true][set plabel-color black set plabel "R"]]
  if view = "T (values)" [ask patches [set plabel "" set plabel precision T-patch 0 set plabel-color black]]
  if view = "K (values)" [ask patches [set plabel "" set plabel precision K-patch 0 set plabel-color black]]
  if view = "K (contours)" [setup-maxmin-K ask patches with [interior-node? = true and no-flow? = false and ET? = false and fixed-head? = false and DRAIN? = false and RIV? = false] [set pcolor scale-color brown K-patch min-K max-K set plabel precision K-patch 0 set plabel-color black] ask patches with [no-flow? = true][set pcolor black]]
  if view = "S (values)" [ask patches [set plabel "" set plabel precision S-patch 5 set plabel-color black]]

  if view = "heads (values)" [ask patches [set plabel precision H 0 set plabel-color black]]
  if view = "heads (contours)" [setup-maxmin-heads ask patches with [interior-node? = true and no-flow? = false and ET? = false and fixed-head? = false and DRAIN? = false and RIV? = false] [setup-maxmin-heads set pcolor scale-color gray H min-head max-head set plabel precision H 0 set plabel-color black] ask patches with [no-flow? = true][set pcolor black] ask patches with [ET? = true][set plabel precision H 0 set plabel-color black]]

  if view = "areal recharge" [ask patches [set plabel "" set plabel precision Recharge 4 set plabel-color black]]

  if view = "ET" [ask patches [set plabel "" ask patches with [ET? = true][set plabel precision Recharge 4 set plabel-color black set pcolor green]]]

  if view = "DRAIN conductance" [ask patches with [DRAIN? = true][set plabel DRAIN-conductance-patch]]
  if view = "DRAIN flow [m3/d] and head values" [ask patches with [DRAIN? = true][set plabel precision DRAIN 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]                    ;; cells that are not drains show the head value
  if view = "DRAIN flow [L/s] and head values" [ask patches with [DRAIN? = true][set plabel precision (DRAIN * 1000 / 86400) 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]    ;; cells that are not drains show the head value

  if view = "RIV flow [m3/d] and head values" [ask patches with [RIV? = true][set plabel precision RIV 0 if RIV < 0 [set pcolor red] if RIV >= 0 [set pcolor cyan]] ask patches with [RIV? = false][set plabel precision H 0 set plabel-color black]]                    ;; cells that are not drains show the head value
  if view = "RIV flow [L/s] and head values" [ask patches with [RIV? = true][set plabel precision (RIV * 1000 / 86400) 0 if RIV < 0 [set pcolor red] if RIV >= 0 [set pcolor cyan]] ask patches with [RIV? = false][set plabel precision H 0 set plabel-color black]]    ;; cells that are not drains show the head value
  if view = "RIV flow [L/s] and drawdowns" [ask patches with [RIV? = true][set plabel precision (RIV * 1000 / 86400) 0 if RIV < 0 [set pcolor red] if RIV >= 0 [set pcolor cyan]] ask patches with [RIV? = false][set plabel precision (Hinitial - H) 0 set plabel-color black]]    ;; cells that are not drains show the head value

  if view = "RIV stage and head values" [ask patches with [DRAIN? = true][set plabel precision RIV-elevation-patch 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]      ;; cells that are not drains show the head value
  if view = "RIV bottom and head values" [ask patches with [DRAIN? = true][set plabel precision RIV-bottom-patch 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]        ;; cells that are not drains show the head value
  if view = "RIV conductance" [ask patches with [DRAIN? = true][set plabel precision RIV-conductance-patch 0] ask patches with [DRAIN? = false][set plabel ""]]                                                ;; cells that are not drains are blank
end


to reset-view
  ask patches [set pcolor white set plabel "" set plabel-color black view-bc]
end


to view-bc
  ifelse left-bc = "no-flow"
  [ask patches with [pxcor = 0][set pcolor black]]
  [ask patches with [pxcor = 0][set pcolor blue]]

  ifelse right-bc = "no-flow"
  [ask patches with [pxcor = max-pxcor][set pcolor black]]
  [ask patches with [pxcor = max-pxcor][set pcolor blue]]

  ifelse top-bc = "no-flow"
  [ask patches with [pycor = max-pycor][set pcolor black]]
  [ask patches with [pycor = max-pycor][set pcolor blue]]

  ifelse bottom-bc = "no-flow"
  [ask patches with [pycor = min-pycor][set pcolor black]]
  [ask patches with [pycor = min-pycor][set pcolor blue]]

  ask patches with [fixed-head? = true][set pcolor blue]
  ask patches with [ET? = true][set pcolor brown]
  ask patches with [DRAIN? = true][set pcolor magenta]
  ask patches with [RIV? = true][set pcolor cyan]
end


to setup-maxmin-heads                                                    ;; finds max and min H values for colour-coding
  set max-head [H] of max-one-of patches with [interior-node? = true and no-flow? = false][H]
  set min-head [H] of min-one-of patches with [interior-node? = true and no-flow? = false][H]
end


to setup-maxmin-K                                                        ;; finds max and min K values for colour-coding
  set max-K [K-patch] of max-one-of patches [K-patch]
  set min-K [K-patch] of min-one-of patches [K-patch]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET INITIAL HEADS TO CALCULATE DRAWDOWNS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-initial-heads
  ask patches with [interior-node? = true][set Hinitial H]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET BCs / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to change-to-fixed-head
   if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set pcolor blue
        set fixed-head? true
        set no-flow? false
        set H fixed-head-value-new
        set K-patch K
        ifelse aquifer-type = "confined"
        [set T-patch (K-patch * aquifer-thickness)]       ;; sets transmissivity and storage for confined conditions
        [set T-patch (K-patch * H)]                      ;; sets transmissivity and storage for unconfined conditions
        set plabel-color black
        set plabel H
      ]
    ]
end


to change-to-no-flow
   if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set pcolor black
        set fixed-head? false
        set no-flow? true
      ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET HYDRAULIC PARAMETERS / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-K
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set K-patch K-input
        set plabel-color black
        set plabel K-patch
      ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET DRAIN / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-DRAIN-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set DRAIN? true
        set DRAIN-elevation-patch DRAIN-elevation
        set DRAIN-conductance-patch DRAIN-conductance
        set plabel-color black
        set pcolor magenta
        set plabel DRAIN-conductance-patch
      ]
    ]
end

to clear-DRAIN-patches-all
  ask patches with [DRAIN? = true]
  [
    set DRAIN? false
    set DRAIN 0
    set DRAIN-elevation-patch 0
    set DRAIN-conductance-patch 0
    set pcolor white
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET RIV / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-RIV-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set RIV? true
        set RIV-elevation-patch RIV-elevation
        set RIV-bottom-patch RIV-bottom
        set RIV-conductance-patch RIV-conductance
        set plabel-color black
        set pcolor cyan
        set plabel RIV-elevation-patch
      ]
    ]
end

to clear-RIV-patches-all
  ask patches with [RIV? = true]
  [
    set RIV? false
    set RIV 0
    set RIV-elevation-patch 0
    set RIV-bottom-patch 0
    set RIV-conductance-patch 0
    set pcolor white
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET ET / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-ET-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set ET? true
        set ET-max-rate-patch ET-max-rate
        set ET-extinction-depth-patch ET-extinction-depth
        set ET-land-surface-elevation-patch ET-land-surface-elevation
        set plabel-color black
        set pcolor brown
        set plabel ET-max-rate-patch
      ]
    ]
end


to clear-ET-patches-all
  ask patches with [ET? = true]
  [
    set ET? false
    set ET-max-rate 0
    set ET-extinction-depth 0
    set ET-land-surface-elevation 0
    set pcolor white
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET FIXED FLUX — POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to place-fixed-flux
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
        [
          set pcolor magenta
          set fixed-flux? true
          set injection? true
          set Qinjection Qfixed-flux
          set plabel-color black
          set plabel Qinjection
        ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET WELLS — POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to place-pumping-well
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set pcolor red
        set well? true
        set Qwell Qwell-input
        ;set screen-level screen-level-input
        set plabel-color black
        set plabel Qwell
      ]
    ]
end


to place-injection-well
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
        [
          set pcolor blue
          set injection? true
          set Qinjection Qinjection-input
          set plabel-color black
          set plabel Qinjection
        ]
    ]
end


;; the following routines allow clearing the view from stresses and resetting them using the interface buttons

to hide-stresses-patches
  ask patches with [well? = true or injection? = true]
  [
    if well? = true [set pcolor white set plabel-color red]
    if injection? = true [set pcolor white set plabel-color blue]
  ]
end


to hide-stresses-labels
  ask patches with [well? = true or injection? = true]
  [
    if well? = true [set plabel ""]
    if injection? = true [set plabel ""]
  ]
end


to clear-stresses
  ask patches with [well? = true or injection? = true]
  [
    if well? = true [set Qwell 0 hide-stresses-patches hide-stresses-labels set well? false]
    if injection? = true [set Qinjection 0 hide-stresses-patches hide-stresses-labels set injection? false]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET RECHARGE / POINT-AND-CLICK ON THE INTERFACE AND ALL CELLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-recharge-all-patches
  ask patches with [interior-node? = true][set Recharge areal-recharge]
end


to set-recharge-single-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set Recharge areal-recharge
        set plabel-color black
        set plabel Recharge
      ]
    ]
end


to clear-recharge
  ask patches with [interior-node? = true][set Recharge 0]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MULTIPLIER FUNCTIONS / RECHARGE AND WELLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calculate-multiplier-sine-recharge                                            ;; assumes that recharge occurs in (southern hemisphere) winter
    let B-recharge (2 * pi / 365)
    let C-recharge 81.75
    set sine-recharge sin (B-recharge * (ticks - C-recharge) * 180 / pi)         ;; note that the sine function here is in degrees, not in radians
    if sine-recharge < 0 [set sine-recharge 0]
end


to calculate-multiplier-sine-wells                                               ;; assumes that pumping occurs in (southern hemisphere) summer
    let B-well (2 * pi / 365)
    let C-well -81.75
    set sine-well sin (B-well * (ticks - C-well) * 180 / pi)                     ;; note that the sine function here is in degrees, not in radians
    if sine-well < 0 [set sine-well 0]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-model
  ca
  reset-ticks
  setup-world
  setup-bc
  setup-initial-heads
  setup-hydraulic-parameters
  setup-sources
  setup-maxmin-heads
  setup-neighbours
  setup-patch-numbering
  setup-patches-adjacent-to-boundary
  refresh-view
end


to setup-world
  resize-world 0 (N + 1) 0 (M + 1)                            ;; defines number of cells in X (N) and Y (M) direction, leaves additional patches for the boundary conditions NOTE: N=M
  ask patches [set pcolor white]                              ;; set initial colour of patches to white
  ask patches [set well? false set recharge? false]           ;; initially none of the patches have wells or recharge, setup-pumping and setup-recharge modify this tag
  set number-of-unknowns N * M

  if PCG? = TRUE
  [
  ]
end


to setup-bc
  ask patches [set no-flow? false set fixed-head? false set interior-node? true]                                                            ;; initialize these location indicators: all nodes are interior and no bc tags - easier for further coding

  ask patches [if (pxcor = min-pxcor) or (pxcor = max-pxcor) or (pycor = max-pycor) or (pycor = min-pycor) [set interior-node? false]]      ;; tag interior and boundary nodes

  ifelse left-bc = "no-flow"
  [ask patches with [pxcor = 0][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pxcor = 0][set pcolor blue set fixed-head? true set interior-node? false set H left-bc-head]]

  ifelse right-bc = "no-flow"
  [ask patches with [pxcor = max-pxcor][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pxcor = max-pxcor][set pcolor blue set fixed-head? true set interior-node? false set H right-bc-head]]

  ifelse top-bc = "no-flow"
  [ask patches with [pycor = max-pycor][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pycor = max-pycor][set pcolor blue set fixed-head? true set interior-node? false set H top-bc-head]]

  ifelse bottom-bc = "no-flow"
  [ask patches with [pycor = min-pycor][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pycor = min-pycor][set pcolor blue set fixed-head? true set interior-node? false set H bottom-bc-head]]
end


to setup-initial-heads
  ask patches with [interior-node? = true] [set H initial-heads]                               ;; set the initial heads in interior nodes
end


to setup-hydraulic-parameters                                       ;; sets a homogeneous conductivity to the whole model using the input box on the interface (avoids having cells with no K value)
  ask patches with [no-flow? = false]
  [
    set K-patch K
    ifelse aquifer-type = "confined"
    [set T-patch (K-patch * aquifer-thickness) set S-patch S]       ;; sets transmissivity and storage for confined conditions
    [set T-patch (K-patch * H) set S-patch Sy]                      ;; sets transmissivity and storage for unconfined conditions
  ]
end


to setup-sources
  ask patches [set well? false]         ;; initialize patch tags without wells
  ask patches [set injection? false]    ;; initialize patch tags without injection
  ask patches [set recharge? false]     ;; initialize patch tags without recharge
  ask patches [set ET? false]           ;; initialize patch tags without ET
  ask patches [set DRAIN? false]        ;; initialize patch tags without DRAIN
  ask patches [set RIV? false]          ;; initialize patch tags without leakage from river

  ;; Sources and stresses can be added directly through the interface, otherwise manually (directly in the code) here:

  ;; pumping wells, units of [m3/day] ~ [L3/T]
  ;; injection wells, units of [m3/day] ~ [L3/T]
  ;; recharge cells, units of [L3/L2*T] ~ [L/T]
  ;; ET cells, units of [L3/L2*T] ~ [L/T]
  ;; DRAIN cells, units of [L3/T]
  ;; RIV cells, units of [L3/T]

end


to setup-neighbours
  ask patches with [interior-node? = true]
  [
    set N-neighbour patch-at 0 1                                          ;; neighbour North of this patch
    set S-neighbour patch-at 0 -1                                         ;; neighbour South of this patch
    set E-neighbour patch-at 1 0                                          ;; neighbour East of this patch
    set W-neighbour patch-at -1 0                                         ;; neighbour West of this patch
  ]
end


to setup-patch-numbering
  ask patches with [interior-node? = true]
  [set patch-number-tag pxcor + (M - pycor) * N]
end


to setup-patches-adjacent-to-boundary
  ask patches with [interior-node? = true]
  [
    ifelse (pxcor = (min-pxcor + 1) or pxcor = (max-pxcor - 1) or pycor = (min-pycor + 1) or pycor = (max-pycor - 1))
    [set adjacent-to-boundary? true]
    [set adjacent-to-boundary? false]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SOLUTION ITERATION LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  iterate
  refresh-view
  tick
end

to iterate
  if aquifer-type = "unconfined" [prepare-equations solve-once update-unconfined-transmissivities]
  if aquifer-type = "confined" [solve-once]
end


to update-unconfined-transmissivities
  ask patches [set T-patch (K-patch * H)]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POPULATE THE MODEL WITH INITIAL HEADS — STEADY STATE RUN WITH NO STRESSES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to initialize-heads                                                    ;; runs the model ONCE, with all stresses = 0 to set up the initial heads
    reset-ticks

    ask patches
    [
      if aquifer-type = "confined"                                     ;; sets transmissivity and storage for confined conditions
      [set T-patch (K-patch * aquifer-thickness) set S-patch S]

      if aquifer-type = "unconfined"                                   ;; sets transmissivity and storage for unconfined conditions
      [set T-patch (K-patch * H) set S-patch Sy]

      set Qwell-temp Qwell                                             ;; save the original values to restore after the init, they are needed for the steady/transient simulation
      set Qinjection-temp Qinjection
      set Recharge-temp Recharge
      set S-patch-temp S-patch
      set ET-max-rate-temp ET-max-rate-patch

      set Qwell 0                                                      ;; we set these parameters to zero for the init, which is steady state without any stresses
      set Qinjection 0
      set Recharge 0
      set S-patch 0
      set ET-max-rate-patch 0
    ]

    ask patches with [ET? = true]                                      ;; sets the ET cell to a fixed-head condition to give a reasonalble steady-state solution considering long-term equilibrium at H = ground elevation - extinction depth
    [
      set H (ET-land-surface-elevation - ET-extinction-depth-patch)
      set fixed-head? true
    ]

    prepare-equations                                                  ;; set up the equations for this solution
    iterate

    refresh-view

    ;; now prepare equations for the main simulation

    if solver = "steady-state" [prepare-equations-steadystate]                                                   ;; note that the fixed-head assumption for ET cells is maintained here
    if solver = "transient" [ask patches with [ET? = true][set fixed-head? false] prepare-equations-transient]   ;; remove the fixed-head assumption of ET cells for subsequent transient simulations

end


to prepare-equations-steadystate
  ask patches
  [
    set Qwell Qwell-temp
    set Qinjection Qinjection-temp
    set Recharge Recharge-temp
    set S-patch 0                                          ;; S=0 for steady state simulations and stresses are activated
    set ET-max-rate-patch ET-max-rate-temp
  ]
  prepare-equations
end


to prepare-equations-transient
  ask patches
  [
    set Qwell Qwell-temp
    set Qinjection Qinjection-temp
    set Recharge Recharge-temp
    set S-patch S-patch-temp
    set ET-max-rate-patch ET-max-rate-temp
  ]                                                        ;; restores original parameters for the simulation
  prepare-equations
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PREPARE EQUATIONS AND DEFINE THE INVERSE OF MATRIX A ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; executed ONCE for CONFINED conditions (T does not change during the simulation) and ITERATIVELY for unconfined conditions (T=Kxh varies during the simulation)

to prepare-equations
  calculate-conductances
  modify-conductances-for-bcs
  build-matrix-A
  remove-inactive-cells-from-A-matrix
  if PCG? = FALSE [calculate-inverse-A]
  if PCG? = TRUE
  [
  ]
end


to calculate-conductances                                                ;; sets the interblock transmissivities using the harmonic mean
ask patches with [interior-node? = true]
[
  set TN [T-patch] of N-neighbour                                        ;; T of patch North of here
  set TS [T-patch] of S-neighbour                                        ;; T of patch South of here
  set TE [T-patch] of E-neighbour                                        ;; T of patch East of here
  set TW [T-patch] of W-neighbour                                        ;; T of patch West of here
  set TC [T-patch] of self

  ;; using the harmonic mean of conductivity

  set Ncof (1 / (delta ^ 2)) * (2 * TC * TN) / (TC + TN)                 ;; Coeff for head North of this patch
  set Scof (1 / (delta ^ 2)) * (2 * TC * TS) / (TC + TS)                 ;; Coeff for head South of this patch
  set Ecof (1 / (delta ^ 2)) * (2 * TC * TE) / (TC + TE)                 ;; Coeff for head East of this patch
  set Wcof (1 / (delta ^ 2)) * (2 * TC * TW) / (TC + TW)                 ;; Coeff for head West of this patch
  set Ccof -1 * (Ncof + Scof + Ecof + Wcof + S-patch / delta-t)          ;; Coeff for head at this patch
]
end


to modify-conductances-for-bcs
ask patches with [interior-node? = true]
[
  ;; conductance to/from no-flow boundaries are zero

  if [no-flow?] of N-neighbour = true [set Ncof 0]
  if [no-flow?] of S-neighbour = true [set Scof 0]
  if [no-flow?] of E-neighbour = true [set Ecof 0]
  if [no-flow?] of W-neighbour = true [set Wcof 0]

  set Ccof -1 * (Ncof + Scof + Ecof + Wcof + (S-patch / delta-t))                ;; needed to reflect any of the above changes
]
end


to build-matrix-A                                                                ;; AxB=C ~ NOTE: N=M
  set number-of-unknowns N * M

  ;set C matrix:make-constant (number-of-unknowns) 1 0                           ;; a column vector with 0's: here goes the SourceTerm + terms derived from bc's
  set A matrix:make-constant number-of-unknowns number-of-unknowns 0             ;; a N^2xM^2 matrix of 0's: here we will add the equation coeffs

  ask patches with [interior-node? = true][set interior-but-corner? false]       ;; set all the interior nodes to false, we tag the corner ones later

  ;; COMPLETE THE "A" MATRIX

  ;; interior patches not adjacent to a boundary have all the the terms
  ask patches with [interior-node? = true and adjacent-to-boundary? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]

  ;; top left corner
  ask patches with [interior-node? = true and pycor = max-pycor - 1 and pxcor = min-pxcor + 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term

    set interior-but-corner? true
  ]

  ;; top right corner
  ask patches with [interior-node? = true and pycor = max-pycor - 1 and pxcor = max-pxcor - 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term

    set interior-but-corner? true
  ]

  ;; bottom left corner
  ask patches with [interior-node? = true and pycor = min-pycor + 1 and pxcor = min-pxcor + 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term

    set interior-but-corner? true
  ]

  ;; bottom right corner
  ask patches with [interior-node? = true and pycor = min-pycor + 1 and pxcor = max-pxcor - 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term

    set interior-but-corner? true
  ]

  ;; top row
  ask patches with [interior-node? = true and pycor = max-pycor - 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]

  ;; bottom row
    ask patches with [interior-node? = true and pycor = min-pycor + 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
  ]

  ;; left column
    ask patches with [interior-node? = true and pxcor = min-pxcor + 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]

  ;; right column
    ask patches with [interior-node? = true and pxcor = max-pxcor - 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]
end

to calculate-inverse-A
  set inverse-A matrix:inverse A
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ITERATION SUBROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to solve-once
  calculate-source-term                       ;; comprises the sources (wells, ET) and sinks (injection, recharge)
  calculate-RHS                               ;; obtain the RHS of each equation
  build-matrix-C                              ;; these only need to be calculated at the beggining of each iteration
  reduce-matrix-C                             ;; eliminate the sourceterms corresponding to no-flow and fixed-head cells
  solve-system-of-equations                   ;; solve the system of equations AxB=C
  list-of-active-cells                        ;; lists the cells to be updated with new heads
  extract-Hinew                               ;; the new heads are mapped into the Hinew variable of the grid
  update-heads-to-grid                        ;; H <-- Hinew / the iteration has ended so we update the H values for the iterations corresponding to the next timestep
end


to calculate-source-term

  ifelse (sine-recharge-multiplier? = true and solver = "transient") [calculate-multiplier-sine-recharge][set sine-recharge 1]
  ifelse (sine-well-multiplier? = true and solver = "transient") [calculate-multiplier-sine-wells][set sine-well 1]

  if solver = "steady-state"                                      ;; ***IMPORTANT***: when using ET, DRAIN and RIV (head-dependent) cells, set initial conditions using a long-term transient simulation
  [
  ]

  if solver = "transient"                                         ;; recalculate the ET and DRAIN terms before each iteration
  [
    ask patches with [ET? = true][calculate-ET-discharge]
    ask patches with [DRAIN? = true][calculate-DRAIN-discharge]
    ask patches with [RIV? = true][calculate-RIV-term]
  ]

  ;;FOR THE CELLS THAT ARE NOT FARMER WELLS I.E. TOWN WELLS PUMP CONSTANTLY THROUGHOUT THE YEAR
  ask patches with [interior-node? = true]
  [
    set SourceTerm ((Qwell - Qinjection) / (delta ^ 2)) + (- Recharge ) + (ET) + (DRAIN / (delta ^ 2)) + (RIV / (delta ^ 2))      ;; units consistent [L3/L2*T] ~ [L/T]

    ;; look at neighbours for fixed-head boundary, we know the heads here and therefore move this term to the RHS

    if [fixed-head?] of N-neighbour = true [set SourceTerm (SourceTerm - ([H] of N-neighbour) * (Ncof))]
    if [fixed-head?] of S-neighbour = true [set SourceTerm (SourceTerm - ([H] of S-neighbour) * (Scof))]
    if [fixed-head?] of E-neighbour = true [set SourceTerm (SourceTerm - ([H] of E-neighbour) * (Ecof))]
    if [fixed-head?] of W-neighbour = true [set SourceTerm (SourceTerm - ([H] of W-neighbour) * (Wcof))]
  ]


  ;;FIXED FLUX CELLS
  ask patches with [interior-node? = true and injection? = true and fixed-flux? = true]
  [
    set SourceTerm ((Qwell - Qinjection) / (delta ^ 2)) + (- Recharge * sine-recharge) + (ET) + (DRAIN / (delta ^ 2)) + (RIV / (delta ^ 2))      ;; units consistent [L3/L2*T] ~ [L/T]

    ;; look at neighbours for fixed-head boundary, we know the heads here and therefore move this term to the RHS

    if [fixed-head?] of N-neighbour = true [set SourceTerm (SourceTerm - ([H] of N-neighbour) * (Ncof))]
    if [fixed-head?] of S-neighbour = true [set SourceTerm (SourceTerm - ([H] of S-neighbour) * (Scof))]
    if [fixed-head?] of E-neighbour = true [set SourceTerm (SourceTerm - ([H] of E-neighbour) * (Ecof))]
    if [fixed-head?] of W-neighbour = true [set SourceTerm (SourceTerm - ([H] of W-neighbour) * (Wcof))]
  ]
end


to calculate-RHS
  ask patches with [interior-node? = true]
  [set RHS (-1 * S-patch) / delta-t * (H) + SourceTerm]
end


to build-matrix-C
  set C matrix:make-constant (number-of-unknowns) 1 0
  ask patches with [interior-node? = true]
  [
    let my-position ([patch-number-tag] of self - 1)                ;; NOTE: indexing of the matrix extension starts at 0, not 1
    matrix:set C (my-position) 0 [RHS] of self                      ;; set Q term of the RHS of the equation
  ]
end


to solve-system-of-equations
  if PCG? = FALSE [set solution-vector matrix:times inverse-A C]
  if PCG? = TRUE
  [
  ]
end


to extract-Hinew
  let i 0
  while [i <= length active-cells-remap - 1]
  [
    ask patches with [patch-number-tag = (item i active-cells-remap)][set Hinew matrix:get (solution-vector) i 0]
    set i i + 1
  ]
end


to update-heads-to-grid
  ask patches with [interior-node? = true and no-flow? = false and fixed-head? = false][set H Hinew]                    ;; replace the old head value with the new one
end


to calculate-ET-discharge                                                                                             ;; recalculates the ET term at each iteration
  ask patches with [ET? = true]
  [
   if H > ET-land-surface-elevation-patch [set ET ET-max-rate-patch]                                                  ;; if head is greater than land surface elevation

   if H >= (ET-land-surface-elevation-patch - ET-extinction-depth-patch)                                              ;; if head is between land surface and extinction depth
   and H <= (ET-land-surface-elevation-patch)
   [set ET ET-max-rate-patch * (H - (ET-land-surface-elevation-patch - ET-extinction-depth-patch)) / ET-extinction-depth-patch]   ;; a linear relationship between the max rate and zero

   if H < (ET-land-surface-elevation-patch - ET-extinction-depth-patch) [set ET 0]                                    ;; if head is lower than extinction depth
  ]

end


to calculate-DRAIN-discharge                                                                                          ;; recalculates the springflow at each iteration NOTE: this values is in m3/day, needs to be divided by the cell area in the sourceterm equation
  ask patches with [DRAIN? = true]
  [
   if H > DRAIN-elevation-patch [set DRAIN DRAIN-conductance-patch * (H - DRAIN-elevation-patch)]
   if H <= DRAIN-elevation-patch [set DRAIN 0]
  ]
end

to calculate-RIV-term                                                                                         ;; recalculates the springflow at each iteration NOTE: this values is in m3/day, needs to be divided by the cell area in the sourceterm equation
  ask patches with [RIV? = true]
  [
   if H > RIV-elevation-patch [set RIV RIV-conductance-patch * (H - RIV-elevation-patch)]
   if H <= RIV-elevation-patch and H > RIV-bottom-patch [set RIV RIV-conductance-patch * (RIV-elevation-patch - H) * -1]
   if H <= RIV-elevation-patch and H < RIV-bottom-patch [set RIV RIV-conductance-patch * (RIV-elevation-patch - RIV-bottom-patch) * -1]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REMOVE INACTIVE CELLS FROM CALCULATIONS / SPEEDS UP SOLUTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to remove-inactive-cells-from-A-matrix
  list-inactive-cells
  define-rows-to-remove
  remove-rows
  transform-A-from-row-list-to-matrix
  remove-columns
  transform-A-from-column-list-to-matrix
  set A A-reduced
end


to remove-rows
  transform-A-to-row-list
  remove-rows-from-A
end


to remove-columns
  transform-A-to-column-list
  remove-columns-from-A
end


to reduce-matrix-C
  transform-C-to-row-list
  remove-rows-from-C
  transform-C-from-row-list-to-matrix
  set C C-reduced
end


to list-inactive-cells                                                              ;; these are either no-flow or fixed head cells in the model. We need to remove these from the calculations
  ask patches with [interior-node? = true]
  [
    set no-flow-cells-list (list [patch-number-tag] of patches with [no-flow? = true and interior-node? = true])
    set fixed-head-cells-list (list [patch-number-tag] of patches with [fixed-head? = true and interior-node? = true])
    set remove-cells-list sentence no-flow-cells-list fixed-head-cells-list
    set remove-cells-list sort sentence item 0 remove-cells-list item 1 remove-cells-list
  ]
end


to define-rows-to-remove                                                            ;; creates a list containing the index of patches that we will remove. Note that the indexing begints at 0. It considers the fact that the remove-item function removes one row at a time and the indexes change during this process
  let i 0
  set remove-cells-list-corrected []
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list) - i - 1
    set remove-cells-list-corrected lput index-of-cell-to-remove remove-cells-list-corrected
    set i i + 1
  ]
end


to transform-A-to-row-list                                                         ;; transforms matrix A into list form (rows)
  set A-row-list matrix:to-row-list A
end


to remove-rows-from-A                                                              ;; removes the rows one by one
  let i 0
  set A-row-reduced-list A-row-list
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list-corrected)
    set A-row-reduced-list remove-item index-of-cell-to-remove A-row-reduced-list
    set i i + 1
  ]
end

to transform-A-from-row-list-to-matrix                                              ;; back to matrix form
  set A-row-reduced-matrix matrix:from-row-list A-row-reduced-list
end


to transform-A-to-column-list                                                      ;; transforms matrix A into list form (columns)
  set A-column-list matrix:to-column-list A-row-reduced-matrix
end


to remove-columns-from-A                                                           ;; removes the rows one by one
  let i 0
  set A-column-reduced-list A-column-list
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list-corrected)
    set A-column-reduced-list remove-item index-of-cell-to-remove A-column-reduced-list
    set i i + 1
  ]
end


to transform-A-from-column-list-to-matrix                                              ;; back to matrix form
  set A-reduced matrix:from-column-list A-column-reduced-list
end


to transform-C-to-row-list
   set C-row-list matrix:to-row-list C
end


to remove-rows-from-C
  let i 0
  set C-row-reduced-list C-row-list
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list-corrected)
    set C-row-reduced-list remove-item index-of-cell-to-remove C-row-reduced-list
    set i i + 1
  ]
end


to transform-C-from-row-list-to-matrix
    set C-reduced matrix:from-row-list C-row-reduced-list
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ACTIVE CELLS FOR MAPPING SOLUTION VECTOR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to list-of-active-cells
  set active-cells-remap (list [patch-number-tag] of patches with [no-flow? = false and fixed-head? = false and interior-node? = true])
  set active-cells-remap sort item 0 active-cells-remap
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WRITE HEADS TO FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to write-output-heads
  file-delete "heads.txt"
  file-open "heads.txt"
  let j 1
  while [j <= N]
  [
    let i 1
    while [i <= M]
    [
      if i <= M [ask patch i j [file-write H]]
      if i = M [ask patch i j [file-print ""]]
      set i i + 1
    ]
    set j j + 1
  ]
  file-close
  file-flush
end
@#$#@#$#@
GRAPHICS-WINDOW
171
58
771
659
-1
-1
18.5
1
10
1
1
1
0
0
0
1
0
31
0
31
0
0
1
ticks
30.0

BUTTON
784
586
953
619
 1- SETUP
setup-model
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
124
1358
281
1391
K
K
1
100
10.0
1
1
[m/day]
HORIZONTAL

SLIDER
1238
604
1366
637
delta-t
delta-t
1
365
1.0
1
1
[days]
HORIZONTAL

TEXTBOX
128
1340
352
1358
HYDRAULIC PARAMETERS
12
0.0
1

INPUTBOX
186
727
260
787
N
30.0
1
0
Number

INPUTBOX
276
727
348
787
M
30.0
1
0
Number

INPUTBOX
103
770
153
830
delta
750.0
1
0
Number

TEXTBOX
1018
587
1207
606
SIMULATION PARAMETERS
12
0.0
1

CHOOSER
1042
452
1134
497
left-bc
left-bc
"no-flow" "fixed-head"
1

CHOOSER
1318
452
1410
497
right-bc
right-bc
"no-flow" "fixed-head"
1

CHOOSER
1226
452
1318
497
top-bc
top-bc
"no-flow" "fixed-head"
0

CHOOSER
1132
452
1228
497
bottom-bc
bottom-bc
"no-flow" "fixed-head"
0

INPUTBOX
1226
498
1318
558
top-bc-head
50.0
1
0
Number

INPUTBOX
1042
498
1134
558
left-bc-head
30.0
1
0
Number

INPUTBOX
1318
498
1410
558
right-bc-head
70.0
1
0
Number

INPUTBOX
1132
498
1229
558
bottom-bc-head
20.0
1
0
Number

MONITOR
186
787
260
832
Size X [km]
(delta * N) / 1000
17
1
11

MONITOR
276
787
348
832
Size Y [km]
(delta * M) / 1000
17
1
11

CHOOSER
493
734
759
779
view
view
"wells" "K (values)" "K (contours)" "T (values)" "S (values)" "heads (values)" "heads (contours)" "areal recharge" "ET" "DRAIN conductance" "DRAIN flow [m3/d] and head values" "DRAIN flow [L/s] and head values" "RIV flow [m3/d] and head values" "RIV flow [L/s] and head values" "RIV flow [L/s] and drawdowns" "RIV storage and head values" "RIV bottom and head values" "RIV conductance" "patch numbering" "boundary conditions"
5

BUTTON
564
810
673
843
NIL
reset-view
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
1125
604
1237
649
solver
solver
"steady-state" "transient"
1

INPUTBOX
1365
604
1453
664
initial-heads
50.0
1
0
Number

SLIDER
124
1390
281
1423
aquifer-thickness
aquifer-thickness
1
100
20.0
1
1
[m]
HORIZONTAL

CHOOSER
1015
604
1127
649
aquifer-type
aquifer-type
"confined" "unconfined"
0

BUTTON
784
650
953
683
3 - RUN
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
282
1358
358
1423
S
1.0E-4
1
0
Number

BUTTON
793
258
947
291
place pumping well
place-pumping-well
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
794
310
948
343
place injection well
place-injection-well
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
146
15
906
40
FlowLogo - A 2D Groundwater Flow Package for Coupled GW-ABM Simulations
16
0.0
1

TEXTBOX
128
1666
348
1685
DISCHARGE AND RECHARGE WELLS
12
0.0
1

TEXTBOX
674
1690
856
1746
(input well discharge or injection rate, then press button and click on model window to add well or injection point)
11
0.0
1

TEXTBOX
212
692
362
710
MODEL DIMENSIONS
12
0.0
1

TEXTBOX
222
712
313
730
Number of cells
11
0.0
1

TEXTBOX
594
717
630
735
VIEWS
12
0.0
1

INPUTBOX
358
1358
434
1423
Sy
0.3
1
0
Number

BUTTON
784
617
953
650
2- INITIAL HEADS
initialize-heads
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
266
762
281
780
X
11
0.0
1

TEXTBOX
91
751
175
769
Size of cells [m]
11
0.0
1

BUTTON
500
1684
585
1717
hide patches
hide-stresses-patches
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
124
1684
197
1750
Qwell-input
1000.0
1
0
Number

INPUTBOX
198
1684
271
1750
Qinjection-input
500.0
1
0
Number

BUTTON
500
1718
585
1751
hide labels
hide-stresses-labels
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
584
1684
669
1750
clear all wells
clear-stresses
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1278
588
1330
606
timestep
11
0.0
1

TEXTBOX
290
1424
351
1442
confined S
11
0.0
1

TEXTBOX
358
1422
432
1440
unconfined S
11
0.0
1

TEXTBOX
122
1924
230
1954
AREAL RECHARGE
12
0.0
1

INPUTBOX
120
1940
209
2006
areal-recharge
0.2
1
0
Number

TEXTBOX
122
2010
356
2028
Recharge units: [m3/m2*day] = [m/day]
11
0.0
1

TEXTBOX
128
1754
319
1782
Pumping/Injection units: [m3/day]\nScreen level: [m] above reference
11
0.0
1

INPUTBOX
268
1684
347
1750
screen-level-input
20.0
1
0
Number

TEXTBOX
126
1556
276
1586
BOUNDARY CONDITIONS
12
0.0
1

BUTTON
258
1574
473
1607
change this cell to: fixed head
change-to-fixed-head
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
124
1574
260
1640
fixed-head-value-new
100.0
1
0
Number

BUTTON
258
1608
473
1641
change this cell to: no-flow
change-to-no-flow
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1326
1386
1494
1419
export heads to file
write-output-heads
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
278
1474
412
1507
set K for this cell
set-K
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
124
1474
279
1534
K-input
10.0
1
0
Number

TEXTBOX
1552
1720
1867
2196
INSTRUCTIONS:\n\n1. Set general parameters for the whole model: number of cells, hydraulic parameters, initial heads and boundary conditions at the perimeter (these can be modified later) using the input boxes and sliders on the interface\n\n2. Press 'SETUP'\n\n3. Add discharge/recharge wells, areal recharge cells, no-flow/fixed-head boundaries and heterogenous hydraulic parameters by using the corresponding buttons and then pointing-and-clicking on the model view\n\n4. Use the 'view' switch to check that all the parameters have been correctly set for the problem at hand\n\n5. Set aquifer type (confined/unconfined), solver (steady-state/transient) and timestep\n\n6. Press 'INITIAL HEADS. This routine deactivates all stresses and areal recharge and runs the model to obtain a set of initial heads in stady-state conditions. NOTE: For unconfined conditions, wait until the heads have converged. \n\n7. Press 'INITIAL HEADS' again to deactivate before running any simulations\n\n8. Press 'RUN' to start the simulation. Additional wells can be added while the simulation is running, using the 'place pumping well' or place 'injection well option'. NOTE: set the dischrage/recharge rate BEFORE pressing the 'place pumping/injection well' buttons\n
11
105.0
0

TEXTBOX
124
1456
410
1486
HYDRAULIC CONDUCTIVITY
12
0.0
1

BUTTON
886
1488
984
1521
NIL
reset-ticks
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
432
1358
685
1423
set these hydraulic parameters for all cells
setup-hydraulic-parameters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
574
1954
774
1987
sine-recharge-multiplier?
sine-recharge-multiplier?
1
1
-1000

BUTTON
408
1940
558
2006
clear all recharge values
clear-recharge
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
208
1940
408
1973
set recharge for all patches
set-recharge-all-patches
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
208
1974
408
2007
set this value for a single patch
set-recharge-single-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
864
1734
1170
1818
This multiplier is a sine wave of recharge centered around the month of June. Rain is zero during the summer, end of spring and the begining of autumn. This multiplier is applied to all the patches with recharge. Settings can be modified in the code
11
0.0
1

SWITCH
870
1694
1054
1727
sine-well-multiplier?
sine-well-multiplier?
0
1
-1000

TEXTBOX
124
2240
375
2258
EVAPOTRANSPIRATION
12
0.0
1

SLIDER
334
2256
506
2289
ET-extinction-depth
ET-extinction-depth
0
10
5.0
1
1
NIL
HORIZONTAL

INPUTBOX
124
2256
254
2316
ET-land-surface-elevation
50.0
1
0
Number

BUTTON
504
2256
610
2289
add ET cell
set-ET-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
610
2256
714
2289
clear all ET cells
clear-ET-patches-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
254
2256
336
2316
ET-max-rate
0.001
1
0
Number

TEXTBOX
130
2316
280
2334
ET units = [m/day]
11
0.0
1

TEXTBOX
124
2038
299
2068
DRAIN/SPRING
12
0.0
1

INPUTBOX
122
2054
217
2114
DRAIN-elevation
40.0
1
0
Number

INPUTBOX
218
2054
331
2114
DRAIN-conductance
50.0
1
0
Number

TEXTBOX
124
2114
316
2132
Drain conductance units: [m2/day]
11
0.0
1

BUTTON
798
364
951
397
add drain cell
set-drain-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
280
2160
378
2220
RIV-conductance
50.0
1
0
Number

INPUTBOX
124
2160
205
2220
RIV-elevation
50.0
1
0
Number

INPUTBOX
204
2160
283
2220
RIV-bottom
47.0
1
0
Number

TEXTBOX
124
2142
274
2160
LOSING/GAINING RIVER CONDITION
12
0.0
1

BUTTON
378
2160
480
2193
add RIV cell
set-RIV-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
444
2054
567
2087
clear all DRAIN cells
clear-DRAIN-patches-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
478
2160
581
2193
clear all RIV cells
clear-RIV-patches-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
565
778
675
811
NIL
refresh-view
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1326
1416
1494
1449
export world
export-world \"exampleGWABM\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1326
1448
1494
1481
import world
import-world \"exampleGWABM\"\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1326
1512
1494
1545
NIL
set-initial-heads
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
987
1378
1044
1423
YEAR
YEAR
17
1
11

MONITOR
1042
1378
1103
1423
MONTH
MONTH
17
1
11

INPUTBOX
122
1822
207
1900
Qfixed-flux
2000.0
1
0
Number

BUTTON
208
1840
358
1875
place fixed flux cell
place-fixed-flux
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
124
1804
312
1827
FIXED-FLUX
12
0.0
1

SWITCH
1156
1494
1259
1527
PCG?
PCG?
1
1
-1000

BUTTON
1148
1442
1252
1476
init Rserve
rserve:init 6311 \"localhost\"   \nprint rserve:isConnected                                     ;; connect to Rserve to solve flow equations using PCG package in R
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

house bungalow
false
0
Rectangle -7500403 true true 210 75 225 255
Rectangle -7500403 true true 90 135 210 255
Rectangle -16777216 true false 165 195 195 255
Line -16777216 false 210 135 210 255
Rectangle -16777216 true false 105 202 135 240
Polygon -7500403 true true 225 150 75 150 150 75
Line -16777216 false 75 150 225 150
Line -16777216 false 195 120 225 150
Polygon -16777216 false false 165 195 150 195 180 165 210 195
Rectangle -16777216 true false 135 105 165 135

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="all combinations" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;nearest neighbour&quot;"/>
      <value value="&quot;full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="well-protection-areas">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-scheme">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="social uncertainty" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SGS BASE+FMS+SLU" repetitions="2" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
set SGS-filename (word "K_grid_r" behaviorspace-run-number ".txt")
prepare-SGS</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="repetitions BASE+SLU" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;nearest neighbour&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="well-protection-areas">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-scheme">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="repetitions BASE+RAV" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;nearest neighbour&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="well-protection-areas">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-scheme">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="repetitions BASE+FMS" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="well-protection-areas">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-scheme">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="repetitions BASE+WPA" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;nearest neighbour&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="well-protection-areas">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-scheme">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="repetitions BASE+SUB" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;nearest neighbour&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="well-protection-areas">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-scheme">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="repetitions BASE+SLU+FMS" repetitions="20" runMetricsEveryStep="false">
    <setup>let old-SLU strategic-land-use
let old-RAV risk-aversion
let old-FMS information-scheme
let old-WPA well-protection-areas
let old-SUB subsidy-scheme
let old-h farmer-heterogeneity
import-world "exampleGWABM"
set strategic-land-use old-SLU
set risk-aversion old-RAV
set information-scheme old-FMS
set well-protection-areas old-WPA
set subsidy-scheme old-SUB
set farmer-heterogeneity old-h
random-seed new-seed</setup>
    <go>go</go>
    <final>output-results</final>
    <enumeratedValueSet variable="strategic-land-use">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-aversion">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="information-scheme">
      <value value="&quot;full&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="well-protection-areas">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="subsidy-scheme">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmer-heterogeneity">
      <value value="1.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
