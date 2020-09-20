FINI
/CLEAR
/TITLE,Wind Turbine Tower: Model 1; Date: 4/4/2017
!Define structural analysis
/NOPR
KEYW,PR_STRUC,1
KEYW,PR_THERM,0
KEYW,PR_FLUID,0
KEYW,PR_ELMAG,0

! Unit : kg, m, s
/UNITS,SI
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!KEEP THE TOOLBAR WITH WWT BUTTON
ABBRES,NEW,'TOOLBAR',' ',' '

/PREP7
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Load geometry data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*DIM,geotab,table,76,7,,id,spec
*TREAD,geotab(0,0),WTTGeo,txt,,1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Load node data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*DIM,nodetab,table,76,1,,No,Id
*TREAD,nodetab(0,0),Node,txt,,1
!*MSG,UI,nodetab(76,1)
!%G
!/eof
!=====================================
!DEFINE MATERIAL PROPERTIES
!=====================================
ES355 = 2.1e+011
DS355 = 7847.094
!Material for 75 segments
*do,i,1,75
	MPTEMP,1,0
	MPDATA,EX,i,1,ES355
	MPDATA,PRXY,i,1,0.3
	MPDATA,DENS,i,1,DS355
	MPDATA,DMPR,i,,0.02
*enddo
!Material for virtual beams connecting lumped mass
MPTEMP,1,0
MPDATA,EX,76,1,1e-8
MPDATA,PRXY,76,1,0
MPDATA,DENS,76,1,1e-8
!Define damping for structure
DMPRAT, 0.03

!=====================================
!DEFINE TYPE ELEMENT
!=====================================
!Define shell element for 75 segments
ET,1,SHELL181
!Define shell sections for 75 segments
*do,i,1,75
	sect,i,shell,,SEG%i%
	secdata, geotab(i,3),i,0,3  !!!!!!!!!!!!!!!!!!!OFFSET THE FLANGE HERE
	secoffset,USER,geotab(i,7)
*enddo
!Define lumped mass element
ET,2,MASS21

!=====================================
!MODELLING GEOMETRY
!=====================================
!Draw centers and conner points of circle
!Conner points latter used for line definition
*do,i,1,76
	K,i,0,geotab(i,5),0
	!Make point for lines and base points later to take acc
	K,i+76,-geotab(i,1)/2,geotab(i,5),0
*enddo

!Draw the lines by LEFT side of tower
*do,i,1,75
	LSTR,i+76,i+77
	LESIZE,i, , ,geotab(i,6), , , , ,1
*enddo

!Draw circle
*do,i,1,76
	!If i=36 then i+38 is the point in the bottom
	!That make circle out of horizontal plane
	!So use point 75 to be the upper direction point of last circle
	*IF,i,EQ,76,THEN
		!Create the top point as hub location
		!For reference of last circle direction
		K,kpinqr(0,14)+1,0,79.85,0
		CIRCLE, i, geotab(i,1)/2,kpinqr(0,14), , ,
	*ELSE
		CIRCLE, i, geotab(i,1)/2,i+1, , ,
	*ENDIF
*enddo

!Extrude the line on the path circle
!to become the surface
!Merge all coincident KEYPOINTS
NUMMRG, KP, 1E-2, , , LOW

!Create surface and define mesh size along lines
*do,i,1,76
	*IF,i,LT,76,THEN
		LESIZE,4*i+72, , ,9, , , , ,1
		LESIZE,4*i+73, , ,9, , , , ,1
		LESIZE,4*i+74, , ,9, , , , ,1
		LESIZE,4*i+75, , ,9, , , , ,1
		!The order of lines affects the normal vector of
		!surface
		ADRAG, i,,,,,,4*i+75, 4*i+74, 4*i+73, 4*i+72
	*ELSE
		!Nothing but mesh the last circle
		LESIZE,4*i+72, , ,9, , , , ,1
		LESIZE,4*i+73, , ,9, , , , ,1
		LESIZE,4*i+74, , ,9, , , , ,1
		LESIZE,4*i+75, , ,9, , , , ,1
	*ENDIF
*enddo
!Merge all coincident KEYPOINTS
NUMMRG, KP, 1E-4, , , LOW
!COMPRESS THE KEYPOINT NUMBER
NUMCMP,KP

!!!MESH ALL AREA
!!!BEFORE MESH, SHOW THE ATTRIBUTE
!!!DO NOT DELETE THIS, OTHERWISE ERROR
!SLIST,   1,  36, ,BRIEF,all
*do,i,1,75
	TYPE,   1
	MAT,    i
	ESYS,       0
	SECNUM,   i
	AMESH,4*i-3,4*i,1
*enddo
!Set boundary condition
NSEL,S,LOC,Y,0
D,ALL, , , , , ,UX,UY,UZ,ROTX,ROTY,ROTZ
EPLOT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!CHECKING MESH IS VERY IMPORTANT
MCHECK
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!=====================================
!DEFINE LUMPED MASS
!=====================================
!!!THESE COMMAND CAN BE ERROR WHEN YOU ADD MORE NODES
!Create lines connect lumped mass
!Define element for these lines
ET,3,BEAM188

!Define section of beams
SECTYPE,   76, BEAM, CTUBE, BEAM, 0
SECOFFSET, CENT
SECDATA,0.001,0.001+1e-8

!Draw lumped mass nodes
N,ndinqr(0,14)+1,-3,79.85,0
N,ndinqr(0,14)+1,4.825,79.85,0
N,ndinqr(0,14)+1,0,79.85,0
!*MSG,UI,ndinqr(0,14)
!%G

!Create line elements on top tower
*do,i,1,38
	TYPE, 3
	MAT, 76
	ESYS, 0
	SECNUM, 76
	TSHAP,LINE
	!Create line element
	NSEL,S,NODE,,ndinqr(0,14)
	NSEL,A,NODE,,i+ndinqr(0,14)-39
	E,ndinqr(0,14),i+ndinqr(0,14)-39
*enddo

!Define rigid region
*do,i,1,38
	NSEL,S,NODE,,ndinqr(0,14)
	NSEL,A,NODE,,i+ndinqr(0,14)-39
	CERIG, ndinqr(0,14), i+ndinqr(0,14)-39,ALL
*enddo
!Define constant mass value
R,1,39800,39800,39800, , , ,
R,2,68000,68000,68000, , , ,

!Assign mass to nodes
Type,2
Real, 1
E, ndinqr(0,14)-2
Type, 2
Real, 2
E,  ndinqr(0,14)-1
!*MSG,UI,ndinqr(0,14)-1
!%G

!=====================================
!SOVLE MODAL ANALYSIS
!=====================================
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Set damage scenario
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!The number of damage location
ndl = 1
!The number of damage severity
!in each location, not include intact
nds = 0

!Define table value
!*DIM,FRETAB,,10,nds+1
!DAMTAB is for ANN output layer
!1st row is the damage location
!2nd row is the damage severity
!*DIM,DAMTAB,,2,nds+1
!*DIM,DISTAB,,76,nds+1
!76 is the number of nodes
!10 is the number of modes
!nds is the number of damage cases
!*DIM,DISTABX,,76,10,nds+1,NODE,MODE,CASE
!*DIM,DISTABY,,76,10,nds+1,NODE,MODE,CASE
!*DIM,DISTABZ,,76,10,nds+1,NODE,MODE,CASE

!Define table containing displacement
*DIM,ACCTAB,TABLE,660001,11

/EOF
ES355 = 2.1e+011
*do,i,1,ndl
	*do,j,1,nds+1
		/PREP7
		MPTEMP,1,0
		MPDATA,EX,i,1,ES355*(1-0.01*j)
		FINISH
		/SOL
		ANTYPE,2
		MODOPT,LANB,10
		MODOPT,LANB,10,0,0, ,OFF
		/STATUS,SOLU
		ALLSEL,ALL,ALL
		SOLVE
		FINISH
		/POST1
		MACTRA
		EXPTRA,i,j
		!DAMTAB(1,j) = i
		!DAMTAB(2,j) = j-1
		!*GET,FRETAB((1:10:1),j),MODE,(1:10:1),FREQ
		!*USE,MACDISX,DISTABX,nodetab,j
		!*USE,MACDISY,DISTABY,nodetab,j
		!*USE,MACDISZ,DISTABZ,nodetab,j
	*enddo
	!Export data
	!*USE, EXPDAM,i,DAMTAB
	!*USE, EXPFRE,i,FRETAB
	!*USE, EXPDISX,i,DISTABX
	!*USE, EXPDISY,i,DISTABY
	!*USE, EXPDISZ,i,DISTABZ
	FINISH
	/PREP7
	MPTEMP,1,0
	MPDATA,EX,i,1,ES355
*enddo
SAVE