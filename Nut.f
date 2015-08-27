c=======================================================================
c Soil nutrient uptake model with switches file processing. 
c Modified from OurPrism2.f by Jonathan Rees.
c See file LICENSE for license.
c For method description, please see
c    Yanai, R.D.  1994.  
c    A steady-state model of nutrient uptake accounting for newly-grown roots.
c    Soil. Sci. Soc. Am. J. 58:1562-1571.
c    doi:10.2136/sssaj1994.03615995005800050041x
c    http://hydrology1.nmsu.edu/nm-soil-water-model/Steady-state_model.pdf
c Kindly reference this article when reporting on uses of this software.

c User documentation is in a separate file.  Don't get caught without it.

c Switch options: (default if not specified is "calculated")
c   Fixed <value>
c   Input
c   Calculated
c   Calculated <parameter>
c   Linear <initial>
c   Exponential <initial>
c   Output

c State variables and parameters:
c  Ruth     Carol
c   name     name    Units
c  ----     ------   -----
c  alpha    RtAbso   cm/s       Root absorbing power
c  b        SolBuf   1          Soil buffer capacity (cm3/cm3)
c  C0       CSurf    mol/cm3    Concentration at root surface
c  Cav      SChem    mol/cm3    Average nutrient concentration
c  De       DiffCo   cm2/s      Effective diffusion coefficient
c  dt       Interval s          Duration of a timestep
c  f        --       1          Fugacity (for computing v0) (NYI)
c  f_slope  --       1/s        Change in f (NYI)
c  growth   DlyGrwth 1/s        Growth rate (exponential; cm/cm/s)
c  growth   --       cm/s       Growth rate (linear; cm/s)
c  Km       Km       mol/cm3    Solution concentration at which net influx
c  L        cm       cm         Root length
c  r0       RtRad    cm         Root radius
c  rx       RadAve   cm         Average radial distance to the next root's 
c                                zone of influence
c  source   --       mol/cm3/s  Constant nutrient supply/depletion (Cav pump)
c  t        --       s          Time
c  theta    --       1          Water content (cm3/cm3)   NYI
c  U        UPTK     mol        Uptake (this is the main output of the model)
c  Unew     Unew     mol        Uptake of new roots
c  steps    --       1          Number of timesteps to run
c  v0       VelIn    cm/s       Inward radial velocity of water at root surf
c  Vmax     Imax     mol/cm2/s  Maximum influx at high concentration is 1/2 Vmax
c  w0       RtWUp    cm3/s      Rate of water uptake
c
c Obsolescent (left over from OurPrism2):
c  --       Cmin     mol/cm3    Concentration at which alpha goes to 0
c  iter     Iteration --        Number of steps so far
c  oalpha   RtAbso   cm/s       Root absorbing power (calculated iteratively)


c To promote portability, I try to code to the FORTRAN 90 standard.  I
c have checked conformance using MS Fortran's "Adhere to Fortran 90
c Language Standard" option with array bounds and other checks enabled.
c 
c Language features used:
c -- IF (...) THEN ... ELSEIF ... ENDIF
c -- STATUS= in OPEN statements
c -- Blank lines
c -- "!" comments
c -- Underscore as an alphabetic character.
c 
c Language features not used:
c -- REAL*8 isn't official (this is an IBMish extension).  Replaced with
c    DOUBLE PRECISION.
c -- Statements not to go past column 72.
c -- I assume that only the first 6 characters of an identifier are
c    significant.
c -- Fortran 90 probably has some structured looping constructs, but I
c    have not bothered to learn them.
c 
c Style notes:
c 
c -- File extension ".fi" means "Fortran include" and is the extension
c    conventionally used by Microsoft Fortran Powerstation.
c 
c -- I start statements in column 7, not in column 8 (as in many of
c    the TREGRO files).  Less whitespace (up to a point) means less
c    wasted space and therefore makes programs more readable, because
c    you can take in more information at a glance.
c 
c -- Indentation: 2 characters for progressive indentation of IF's and
c    loops.
c 
c -- Case:  Upper case is traditional for FORTRAN, but unnecessary
c    these days.  I am undecided which is more readable.
c    Use upper or lower case 'c' for comments?
c 
c -- At present all the source code is in one file, Nut.f, with one
c    include file, Nut.fi.  This is better for development since it
c    makes global searches a little easier.  Also it's silly to repeat
c    the same long comments at the top of each file.  There is a
c    glossary of variables at the top, but it is not as detailed as
c    Carol's.


c *------------------------------------------------------------
c * Static information: Names of state variables.
c *------------------------------------------------------------

      BLOCK DATA MUMBLE
      INCLUDE 'Nut.fi'

c This list of variable name strings must be kept in sync with the
c corresponding PARAMETER definitions in Nut.fi.

      DATA VARNAMES /
     >  'alpha',
     >  'b',
     >  'C0',
     >  'Cav',
     >  'Cmin',
     >  'De',
     >  'Dl',
     >  'dt',
     >  'f',
     >  'f_slope',
     >  'growth',
     >  'iter',
     >  'Kd',
     >  'Km',
     >  'L',
     >  'oalpha',
     >  'r0',
     >  'rho',
     >  'rx',
     >  'source',
     >  'steps',
     >  't',
     >  'theta',
     >  'U',
     >  'Unew',
     >  'v0',
     >  'Vmax',
     >  'w0', 
     >  'Lv' /
      DATA UNITNAMES /
     >  'cm/s',         ! alpha
     >  '1',            ! b
     >  'mol/cm3',      ! C0
     >  'mol/cm3',      ! Cav
     >  'mol/cm3',      ! Cmin
     >  'cm2/s',        ! De
     >  'cm3/g',        ! Dl
     >  's',            ! dt
     >  '1',            ! f
     >  '1/s',          ! f_slope
     >  '1/s',          ! growth
     >  '1',            ! iter
     >  'cm3/g',        ! Kd
     >  'mol/cm3',      ! Km
     >  'cm',           ! L
     >  'cm/s',         ! oalpha
     >  'cm',           ! r0
     >  'g/cm3',        ! rho
     >  'cm',           ! rx
     >  'mol/cm3/s',    ! source
     >  '1',            ! steps
     >  's',            ! t
     >  '1',            ! theta
     >  'mol',          ! U
     >  'mol',          ! Unew
     >  'cm/s',         ! v0
     >  'mol/cm2/s',    ! Vmax
     >  'cm3/s',        ! w0
     >  '1/cm2'/        ! Lv  (cm/cm3)
      END


c *------------------------------------------------------------
c * Nut: The main program.
c *------------------------------------------------------------

      PROGRAM NUT
      IMPLICIT NONE
      INCLUDE 'Nut.fi'

c Functions used:
      LOGICAL READIN
      INTEGER READFIELDS, RTRIM, VARNUM, IMEMQ


c Internal variables
      LOGICAL EXISTP
      CHARACTER*60 CFNAME, RFNAME
      INTEGER I, ITER, STEPS, J, NI
      CHARACTER*80 COMMENT
      DOUBLE PRECISION VAL
      DOUBLE PRECISION f_init, theta_init

c For input switches file processing: NF = number of fields
      INTEGER NF, MAXF
      PARAMETER (MAXF=NVARS+1)
      CHARACTER*15 FIELDS(MAXF)

      TAB = CHAR(9)

      WRITE (STDOUT, 901)
 901    FORMAT (' Soil Nutrient Uptake Model'/
     >          ' Original program by Carol Zollweg'/
     >          ' Modifications by Jonathan Rees, August 1997'/)

c     *------------------------------------------------
c     * Read name of the control file (which holds both switches and inputs).
c     *------------------------------------------------
 10   WRITE (STDOUT, *) 'Please enter the name of the control file: '
      READ (STDIN, *) CFNAME
      J = RTRIM(CFNAME)
      CFNAME = CFNAME(1:j)
      OPEN (1, FILE=CFNAME, STATUS='OLD', ERR=15)
      GOTO 18
 15   WRITE (STDOUT, *) '!! Couldn''t find control file ', CFNAME
      GOTO 10

 18   WRITE (STDOUT, *) '...Reading control information and data from ',
     >                  CFNAME

c     *------------------------------------
c     * Open the results file 
c     *------------------------------------
 20   WRITE (STDOUT, *) 'Please enter the name of the results file: '
      READ (STDIN, *) RFNAME
      J = RTRIM(RFNAME)
      RFNAME = RFNAME(1:J)

      INQUIRE (FILE=RFNAME, EXIST=EXISTP)
      IF (EXISTP) THEN
        WRITE (STDOUT, *) 'A file by that name already exists.  ',
     >                    'Replace it? (y/n)'
        READ (STDIN, *) COMMENT
        IF (COMMENT .EQ. 'y' .OR. COMMENT .EQ. 'yes' .OR.
     >      COMMENT .EQ. 'Y' .OR. COMMENT .EQ. 'YES') GOTO 23
        GOTO 20
      ENDIF

 23   OPEN (3, FILE=RFNAME, ERR=25)
      REWIND (3)      !relictual. should be unnecessary
      GOTO 28
 25   WRITE (STDOUT, *) '!! Couldn''t open results file ', RFNAME,
     >  ' for writing'
      GOTO 20

 28   WRITE (STDOUT, *) '...Writing results to ', RFNAME

c     *---------------------------------------------
c     * Process control information
c     *---------------------------------------------

c The first few lines are an identifying comment.  They are simply copied
c to the result file.
c     LOOP
 30     READ (1, '(A80)', END=777) COMMENT
        J = RTRIM(COMMENT)
        WRITE (3, 33) COMMENT(1:MAX(J,1))
 33       FORMAT (A)
        IF (J .GT. 0) GOTO 30
c     END LOOP

c Initialize the variable status table, etc.
      DO 45 I = 1, NVARS
        STATUS(I) = CALCULATED
        VALS(I) = 0.0
        SCALE(I) = 1.0
 45   CONTINUE
      NUMINVARS = 0

c Default values
      VALS(Vdt) = 24 * 60 * 60    !seconds in a day

c Subsequent lines are parameter status specifiers:
c   name units status value...
c     LOOP
 50     NF = READFIELDS(1, FIELDS, MAXF)
        IF (NF .LT. 0) GOTO 777
        IF (NF .EQ. 0) GOTO 59
        IF (NF .LT. 3) 
     >    WRITE(STDOUT, *) '!! Invalid status line syntax -- ',
     >                     FIELDS(1)

c Fields are:   <name> <units> <status> [<parameters>]
        I = VARNUM(FIELDS(1))
        IF (I .EQ. 0) THEN
          WRITE (STDOUT, *) '!! Unrecognized variable name ', 
     >                      FIELDS(1), ' in switches table'
          GOTO 50
        ENDIF

c Perform unit normalization
        IF (FIELDS(2) .EQ. 'day') THEN
          FIELDS(2) = 's'
          SCALE(I) = 1.0/(24.0 * 60.0 * 60.0)
        ELSE IF (FIELDS(2) .EQ. '1/day') THEN
          FIELDS(2) = '1/s'
          SCALE(I) = (24.0 * 60.0 * 60.0)
        ELSE IF (FIELDS(2) .EQ. 'm') THEN
          FIELDS(2) = 'cm'
          SCALE(I) = 0.01
        ENDIF

c Check unit against permissible units
        IF (FIELDS(2) .NE. UNITNAMES(I)) THEN
          WRITE (STDOUT, *) '!! Unrecognized unit ', FIELDS(2),
     >                      ' for variable ', FIELDS(1)
          WRITE (STDOUT, *) '!! (Expected e.g. ', UNITNAMES(I), ')'
        ENDIF

c Read the initial value and scale it accordingly
        IF (NF .LT. 4 .OR. RTRIM(FIELDS(4)) .EQ. 0) THEN
          VALS(I) = 0.0
        ELSE
          READ(FIELDS(4), *) VAL
          VALS(I) = VAL / SCALE(I)
        ENDIF

c The status field is "input", "fixed", "calculated", etc.
        IF (FIELDS(3) .EQ. 'input') THEN
          STATUS(I) = INPUT
          NUMINVARS = NUMINVARS + 1
          IF (NF .GT. 3)
     >      WRITE (STDOUT, *) '!! Extraneous parameter for ',
     >        FIELDS(1)
        ELSE IF (FIELDS(3) .EQ. 'calculated') THEN
          STATUS(I) = CALCULATED
        ELSE 
          IF (NF .LT. 4) WRITE (STDOUT, *) '!! Missing parameter for ',
     >      FIELDS(1)
          IF (FIELDS(3) .EQ. 'fixed') THEN
            STATUS(I) = FIXED
          ELSE IF (FIELDS(3) .EQ. 'linear') THEN
            STATUS(I) = LINEAR
          ELSE IF (FIELDS(3) .EQ. 'exponential') THEN
            STATUS(I) = EXPONENTIAL
          ELSE
            WRITE (STDOUT, *) '!! Invalid status: ', FIELDS(3)
            WRITE (STDOUT, *) '!! (Wanted input, fixed, calculated, ',
     >                        'linear, or exponential)'
          ENDIF
        ENDIF
        GOTO 50
c     END LOOP

c Blank line encountered.
 59   CONTINUE

c     *--------------------------------
c     * Process the column names line.  This determines what will be
c     * written to the results file, and also specifies the order of
c     * the fields in the input records.
c     *--------------------------------

      NCOLS = READFIELDS(1, FIELDS, MAXF)
      NI = 0
      DO 60 J = 1, NCOLS
        I = VARNUM(FIELDS(J))
        IF (I .EQ. 0) THEN
          WRITE (STDOUT, *) '!! Unrecognized variable name in inputs ',
     >                      'file: [', FIELDS(J), ']'
        ELSE IF (STATUS(I) .EQ. INPUT) THEN
          NI = NI + 1
          INVARS(NI) = I
        ENDIF
        COLVARS(J) = I
 60   CONTINUE

      IF (NI .NE. NUMINVARS) THEN
        WRITE (STDOUT, *) '!! Wrong number of inputs: switches list',
     >        ' has', NUMINVARS, ', but column headings list has ', NI
      ENDIF

c The results file should have at least one column, yes?
      IF (NCOLS .EQ. 0) THEN
        WRITE (STDOUT, *) '!! Warning: no output columns specified!'
        GOTO 98
      ENDIF

c The results file should have an uptake column, yes?
      IF (IMEMQ(VU, COLVARS, NCOLS) .EQ. 0) THEN
        WRITE (STDOUT, *) '!! Warning: no column specified for ',
     >    'uptake (U)'
      ENDIF

      if(debugp) then
        print *, '| columns: ', 
     >    (varnames(colvars(j)), j = 1, ncols)
        if (numinvars .gt. 0) then
          print *, '| inputs:  ', 
     >      (varnames(invars(j)), j = 1, numinvars)
        else
          print *, '| no inputs'
        endif
      endif

c     *---------------------------------
c     * Initialize, etc.
c     *---------------------------------

c Begin writing the results file (comment has already been written, above)

      DO 70 I = 1, NCOLS
        FIELDS(I) = VARNAMES(COLVARS(I))
 70   CONTINUE
      CALL WRITEFIELDS(3, FIELDS, NCOLS)

c Compute format to be used in output routine
      J = 1
      FORM(1:J) = '('
      DO 71 I = J, (NCOLS-1)*10 + J, 10
        FORM(I+1:I+10) = 'g12.6,1h-,'
        FORM(I+9:I+9) = TAB
   71 CONTINUE
      FORMLEN = NCOLS*10 + J + 1
      FORM(FORMLEN:FORMLEN) = ')'

      if (debugp)
     >  print *, '| Format = [', FORM(1:FORMLEN), ']'

c  Sanity check
      IF (NUMINVARS .LE. 0 .AND. STATUS(Vsteps) .NE. FIXED)
     >  WRITE (STDOUT, *) '!! No termination condition ',
     >                    '(no inputs, and steps isn''t fixed)'

c     *-----------------------------------------------------------------
c     * Begin iterating
c     *-----------------------------------------------------------------

c  The following are conversions to integer
      ITER = 1
      STEPS = VALS(Vsteps)
      if(debugp) print *, '| init done'

c     BEGIN LOOP
 90   CONTINUE

c  Read in the ith parameter set (if there is one)
        IF (NUMINVARS .GT. 0) THEN
          IF (.NOT. READIN(1)) GOTO 98
        ENDIF

        VALS(Viter) = ITER
        if(debugp) print *, 'Iteration #', ITER

c  Perform one time step (time t to time t+dt) of uptake model
        CALL RUNMODEL

c  Get initial values of f and theta to establish linear relationship
        IF (ITER .EQ. 1) THEN
          f_init = VALS(Vf)
          theta_init = VALS(Vtheta)
        ENDIF

c  Write results
        WRITE (3, FORM) (VALS(COLVARS(I)) * SCALE(COLVARS(I)),
     >                   I = 1, NCOLS)

c  Stop if it's time to stop
        IF (STATUS(Vsteps) .EQ. FIXED .AND. ITER .GE. STEPS)
     >    GOTO 98

c  Prepare for next iteration
        CALL UPDATE(f_init, theta_init)
        VALS(Vt) = VALS(Vt) + VALS(Vdt)
        ITER = ITER + 1

        GOTO 90
c     END LOOP

c     *-----------------------------------------------------------------
c     * End iterating
c     *-----------------------------------------------------------------

c  Close control and results files
 98   CLOSE(1)
      CLOSE(3)
      WRITE (STDOUT, *) 'Simulation completed.'
      WRITE (STDOUT, *) 'Ran', ITER, ' iterations.'

      GOTO 778

 777  WRITE (STDOUT,*) '!! Not enough stuff in control file'

 778  CONTINUE
      END


c *---------------------------------------------------------------------
c * RUNMODEL oversees execution of one iteration of the uptake model.
c *---------------------------------------------------------------------

      SUBROUTINE RUNMODEL
      IMPLICIT NONE
      INCLUDE 'Nut.fi'

c Set up inputs to the uptake model.
      alpha     = VALS(Valpha)
      b         = VALS(Vb)
      Cav       = VALS(VCav)
      De        = VALS(VDe) 
      Dl        = VALS(VDl)
      dt        = VALS(Vdt)
      f         = VALS(Vf) 
      growth    = VALS(Vgrowth)
      Km        = VALS(VKm)
      L         = VALS(VL)
      r0        = VALS(Vr0)
      rho       = VALS(Vrho)
      rx        = VALS(Vrx)
      source    = VALS(Vsource)
      theta     = VALS(Vtheta)
      Unew      = VALS(VUnew)
      v0        = VALS(Vv0)
      Vmax      = VALS(VVmax)
      w0        = VALS(Vw0)

c Obsolescent?
      Cmin      = VALS(VCmin)    ! Cmin (mol/cm3)

      if(debugp) print *, '| L:', L
      if(debugp) print *, '| r0:', r0
      if(debugp) print *, '| c0:', C0

c MODEL does the real work of computing values of dependent variables.
      CALL MODEL

c Copy calculated values back into the VALS vector.
      VALS(Valpha)  = alpha
      VALS(Vb)      = b
      VALS(VCav)    = Cav
      VALS(VC0)     = C0
      VALS(VDe)     = De
      VALS(VL)      = L
      VALS(Vrx)     = rx
      VALS(Vv0)     = v0
      VALS(VU)      = U
      VALS(VUnew)   = Unew

      RETURN
      END


c=======================================================================
c Derived from OurPrism2 file UpTk.f


c *------------------------------------------------------------
c * Do all computations for one step of the uptake model.
c *
c * Calculate the root length for both new and established roots, the
c * root density, and the average radial distance to the next root's
c * zone of influence.  The type of root used depends on the soil
c * horizon.
c * The root length (cm) = m/gC * gC for the root * 100cm/m     
c * The root density (m/m**3) = m/gC * gC for the root / soil volume
c * The average radial distance to the next root's zone of      
c * influence, rx, (cm) =                                       
c *      1/sqrt(Pi * RootDens (m/m**3)/10000cm**2/m**2)
c *---------------------------------------------------------------------


      SUBROUTINE MODEL
      IMPLICIT NONE

c Common variables
      INCLUDE 'Nut.fi'
c Function
      DOUBLE PRECISION CALCPc
c Internal variables
      DOUBLE PRECISION Deb, A, rav
      DOUBLE PRECISION GAMMA
      DOUBLE PRECISION DeltaL, Pc, RTRATIO
      DOUBLE PRECISION UEST
c      DOUBLE PRECISION Lv
c Temporaries
      DOUBLE PRECISION AA, AB, AC, AD, AE, AF, FOO

c EVENSMLR is important.  v0 goes down as low as 10^-11 (for Robinia
c  pseudoacacia), so the threshold should probably be smaller than that.
c  If v0 is smaller than that but nonzero, we should probably
c  generate a warning, because user should be informed that the
c  the diffusion-only formula will be invoked.
      DOUBLE PRECISION CM3_L, EVENSMLR
        PARAMETER (CM3_L = 1000.0, EVENSMLR = 1E-12)

      if(debugp) print *, '| L:', L, '  (MODEL)'
      if(debugp) print *, '| r0:', r0

c     *-----------------------------------------------------------*
c     * If there are no roots, there is no uptake!  Exit routine. *
c     *-----------------------------------------------------------*
      IF (L .LE. 0.0d0) THEN
        WRITE (STDOUT,*) '!! Root length <= 0'
        RETURN
      ENDIF

c     *-----------------------------------------------------------------
c     * Compute b if requested.
c     *-----------------------------------------------------------------
      IF (STATUS(Vb) .EQ. CALCULATED) THEN
        b = theta + rho*Kd
      ENDIF

c     *-----------------------------------------------------------------
c     * Compute De if requested.
c     *-----------------------------------------------------------------
      IF (STATUS(VDe) .EQ. CALCULATED) THEN
        Deb = Dl * theta * f
        De = Deb / b
      ELSE
        Deb = De * b
      ENDIF

c     *-----------------------------------------------------------------
c     * Compute rx (zone of influence), if required.
c     * Lv is root length per unit volume (cm of root per cm^3 of soil).
c     *-----------------------------------------------------------------
      IF (STATUS(Vrx) .EQ. CALCULATED) THEN
        rx = 1.0d0 / SQRT(PI * Lv)
      ENDIF

c     *---------------------------------
c     * Calculate the root ratio rx/r0. 
c     *---------------------------------
      RTRATIO = rx / r0
      if(debugp) print *, '| RTRATIO:', RTRATIO

c     *------------------------------------------------------------
c     * Calculate v0 and gamma.  v0 equals
c     * the water uptake w0 (cm3/s) divided by the root surface      
c     * area (cm2).  Don't allow the inward velocity to equal zero.
c     * Root surface area calculated with established roots       
c     * only to make it consistent with root water uptake w0.
c     * Root water uptake is calculated with established roots.    
c     *------------------------------------------------------------
      IF (STATUS(Vv0) .EQ. CALCULATED) THEN
        v0 = MAX(EVENSMLR, w0 / (2 * PI * r0 * L))
        if(debugp) print *, '| v0 (calculated):', v0
      ENDIF
      gamma = (r0 * v0)/Deb
      if(debugp) print *, '| gamma:', gamma


      IF (STATUS(Voalpha) .EQ. CALCULATED) THEN
c     *-----------------------------------------------------------------
c     * Calculate the root absorbing power (alpha) as a function of
c     * Vmax and Km by iterating because alpha is a function of the
c     * concentration at the root surface and that concentration is a
c     * function of alpha.
c     *-----------------------------------------------------------------
c       -- UPALPHA computes alpha, C0, and Pc.
        CALL UPALPHA(EVENSMLR,      
     >               0.1d0,      ! OLDC0 (OLDCSURF)
     >               Vmax, Km, Cmin, gamma,             
     >               v0, RTRATIO, r0, Deb, b, Cav,
     >               STATUS(VALPHA) .EQ. CALCULATED,
     >               alpha,  oalpha, C0, Pc)
        if(debugp) print *, '| C0 (iterative):', C0
        if(debugp) print *, '| alpha (iterative):', oalpha
        if(debugp) print *, '| Pc (iterative):', Pc
      ENDIF

      IF (STATUS(Valpha) .EQ. CALCULATED) THEN
c       -- CALCALPHA computes alpha and C0, but not Pc.
        CALL CALCALPHA(gamma, RTRATIO)
        if(debugp) print *, '| C0 (direct):', C0
        if(debugp) print *, '| alpha (direct):', alpha
        Pc = C0 / Cav
        if(debugp) print *, '| Pc (direct):', Pc
      ELSE
c       -- If alpha is known, we can use it to compute Pc and C0.
        Pc = CALCPc(gamma, RTRATIO, Deb, EVENSMLR)
        C0 = Pc * Cav
      ENDIF

c     *-----------------------------------------------------------------
c     * Calculate rav = the radius where the concentration equals
c     * the average concentration. If v0 is close to zero, use a
c     * different equation.
c     *-----------------------------------------------------------------
      IF (v0 .LE. EVENSMLR) THEN
        AA = Deb/(alpha * r0)
        AB = (1 - Pc) / Pc
        FOO = EXP(AA * AB)
      ELSE
        AA = 2 / (2 - GAMMA)
        AC = -1 / GAMMA
        AD = RTRATIO**(2 - GAMMA) - 1
        AE = (RTRATIO * RTRATIO) - 1
        FOO = ((AA * (AD/AE))**AC)
      ENDIF
      rav = r0 * FOO
      if (debugp) print *, '| rav:', rav

c     *------------------------------------------------------------*
c     * Calculate the uptake due to the established roots (U_est). *
c     *------------------------------------------------------------*
      Uest = L * 2 * PI * r0 * alpha * C0 * dt
      if (debugp) print *, '| Uest:', Uest

c     *-----------------------------------------------------------
c     * Calculate the uptake by the new roots (if any).
c     * To do this, first calculate A = the amount of solute removed
c     * from the depletion zone per unit length of new root.  If v0
c     * is very close to zero, use a different equation.
c     *-----------------------------------------------------
      IF (STATUS(VUnew) .EQ. CALCULATED) THEN
        IF (v0 .LE. EVENSMLR) THEN
          AB = r0 * r0
          AC = rav * rav
          AD = AC - AB
          AE = Pc * ALPHA * r0 / Deb
          AF = LOG(rav / r0)
          A = PI * b * Cav *
     >         (((1 - Pc) * AD) - (AE * ((AC * (AF - 0.5)) + AB*0.5)))
        ELSE
          AA = 1 - Pc*alpha/v0
          AB = rav*rav - r0*r0
          AC = 2*Pc/v0
          AD = (v0 - ALPHA)/(2 - GAMMA)
          AE = ((rav/r0) ** (2 - GAMMA)) - 1
          A = PI * b * Cav *
     >         (AA*AB - (AC * AD * r0*r0 * AE))
        ENDIF
        if (debugp) print *, '| A:', A

c     *-----------------------------------------------------------
c     * Length of roots that are brand new.
c     *-----------------------------------------------------------
        DeltaL = L * (growth * dt)

c     *-----------------------------------------------------------
c     * Don't allow uptake by new roots to be negative.
c     **** Ruth questions this.  Maybe it _should_ be allowed to go negative.
c     *-----------------------------------------------------------
        Unew = MAX(A * DeltaL, 0.0d0)
        if (debugp) print *, '| Unew:', Unew
      ENDIF

c     *---------------------------------------------------------*
c     * The total amount of nutrient taken up by the root is
c     * the amount up by the established roots plus the amount
c     * taken up by the new roots.
c     *---------------------------------------------------------*
      U = Uest + Unew
      if (debugp) print *, '| U:', U

      RETURN
      END

c=======================================================================
c Alpha computation copied mindlessly from Ruth's handwritten notes.
      SUBROUTINE CALCALPHA(gamma, RTRATIO)
      IMPLICIT NONE
      DOUBLE PRECISION gamma, RTRATIO

c Common variables.
      INCLUDE 'Nut.fi'

c Internal variables.
      DOUBLE PRECISION TEMP1, TEMP2
      DOUBLE PRECISION AA, BB
      DOUBLE PRECISION ROOT1, ROOT2

c Compute intermediate quantities.
        TEMP1 = RTRATIO**(2 - GAMMA) - 1
        TEMP2 = (RTRATIO * RTRATIO) - 1
      AA = (2 / (2 - GAMMA)) * (TEMP1 / TEMP2)
      if(debugp) print *, '| AA:', AA

      BB = v0 * Cav
      if(debugp) print *, '| BB:', BB

      CALL QUADRATIC(AA * v0,
     >               AA*Km*v0 + (1-AA)*Vmax - BB,
     >               BB * Km,
     >               root1, root2)
      if(debugp) print *, '| roots are', root1, root2

      C0 = MAX(root1, root2)
      IF (C0 .LT. 0) THEN
        WRITE (STDOUT, *) '!! negative roots in C0 computation'
      ENDIF

      alpha = Vmax / (Km + C0)

      RETURN
      END

c=======================================================================
c Update state variables for next iteration.

      SUBROUTINE UPDATE(f_init, theta_init)
      IMPLICIT NONE
      DOUBLE PRECISION f_init, theta_init

c Common and internal variables.
      INCLUDE 'Nut.fi'

c Formula for Cav copied from UpComp.f
      IF (STATUS(VCav) .EQ. CALCULATED) THEN
        Cav = Cav - U / (L * PI * (rx * rx) * b)
      ENDIF

      IF (STATUS(VCav) .EQ. LINEAR)
     >  Cav = Cav + (source * dt)

c Root growth can be linear of exponential
      IF (STATUS(VL) .EQ. LINEAR) THEN
        L = L + (growth * dt)
      ELSE IF (STATUS(VL) .EQ. EXPONENTIAL) THEN
        L = L * (1.0d0 + (growth * dt))
      ENDIF

c Update fugacity, which is linear in theta
      IF (STATUS(Vf) .EQ. CALCULATED) THEN
        f = f_init + f_slope*(theta - theta_init)
      ENDIF

      VALS(VCav) = Cav
      VALS(VL) = L
      VALS(Vf) = f

      END


c=======================================================================
c     *----------------------------------------------------------*
c     * Calculate the ratio Pc = C0/Cav.
c     * If v0 <= EvenSmlr a different calculation for Pc is
c     * used based on the diffusion-only case of Baldwin et al.
c     * 1973 (eqn III).  This equation is independent of  v0
c     * and gamma.
c     *----------------------------------------------------------*
      DOUBLE PRECISION FUNCTION CALCPc(gamma, RTRATIO, Deb, EVENSMLR)
      IMPLICIT NONE
      DOUBLE PRECISION gamma, RTRATIO, Deb, EVENSMLR
      INCLUDE 'Nut.fi'

      DOUBLE PRECISION Pc, AA, AB, AC, AD, AE

      IF (v0 .LE. EVENSMLR) THEN
c       rx = RTRATIO * r0      -- this has already been computed
        AA = alpha * r0 / (2 * Deb)
        AB = rx * rx
        AC = AB * alpha * r0 / Deb
        AD = AB - r0 * r0 
        AE = LOG(rx/r0)
        Pc = 1/(1 - AA + AC/AD * AE)
        if(debugp) print *, '| Pc (small v0): ', Pc
      ELSE
        AA = v0 - alpha
        AB = 2 / (2 - gamma)
        AC = RTRATIO ** (2 - gamma)
        AD = RTRATIO * RTRATIO
        AE = (AC - 1.0) / (AD - 1.0)
        Pc = v0 / (alpha + AA * AB * AE)
        if(debugp) print *, '| Pc (normal): ', Pc
      ENDIF

      CALCPc = Pc
      RETURN
      END

c=======================================================================
c *-----------------------------------
c * Quadratic equation solver.
c * A numerical analyst would probably tell you that this is absolutely
c * the worst way to solve a quadratic equation, due to its
c * vulnerability to overflow, underflow, and roundoff errors.  A better
c * way might be found in a reference such as "Numerical Recipes," which I
c * haven't yet consulted.
c *-----------------------------------

      SUBROUTINE QUADRATIC(A, B, C, R1, R2)
      IMPLICIT NONE
      DOUBLE PRECISION A, B, C, R1, R2
      DOUBLE PRECISION DISCRIM, S

      DISCRIM = ((B * B) - (4 * A * C))
      IF (DISCRIM .LT. 0) THEN
        PRINT *, '!! Quadratic equation has complex roots'
        RETURN
      ENDIF
      S = SQRT(DISCRIM)
      R1 = (-B + S)/(2*A)
      R2 = (-B - S)/(2*A)
      RETURN
      END


c=======================================================================
c I/O utilities.

c *-----------------------------------
c * Read a line of input parameters.
c * Returns .true. normally, .false. at end of file.
c *-----------------------------------

      LOGICAL FUNCTION READIN(UNIT)
      IMPLICIT NONE
      INTEGER UNIT

      INCLUDE 'Nut.fi'
      INTEGER I
      DOUBLE PRECISION INPUTS(NVARS)

      READ (UNIT, *, END=777) (INPUTS(I), I=1,NUMINVARS)
      DO 20 I = 1, NUMINVARS
        VALS(INVARS(I)) = INPUTS(I)
  20  CONTINUE
      READIN = .TRUE.
      RETURN
c 
 777  WRITE (STDOUT, *) 'End of input records'
      READIN = .FALSE.
      END


c *-------------------------------------------
c * Separate a line into tab-separated fields.
c * MAXF = maximum number of fields (i.e. size of output array).
c * Returns the number of fields read, or -1 on EOF.
c *-------------------------------------------
      INTEGER FUNCTION READFIELDS(UNIT, FIELDS, MAXF)
      IMPLICIT NONE
      INTEGER UNIT, MAXF
      CHARACTER*(*) FIELDS(MAXF)

      CHARACTER*320 BUFFER
      CHARACTER*1 TAB
      INTEGER POS, J, NF, FIN, RTRIM
c         
      TAB = CHAR(9)
c         
      POS = 1
      NF = 0
      READ (UNIT, '(A150)', END=9) BUFFER 
      FIN = RTRIM(BUFFER)
 1    IF (POS .LE. FIN .AND. NF .LT. MAXF) THEN
        J = INDEX(BUFFER(POS:FIN), TAB)
        IF (J .GT. 0) THEN
           NF = NF + 1
           FIELDS(NF) = BUFFER(POS:POS+J-2)
           POS = POS + J
           GOTO 1
        ELSE
           NF = NF + 1
           FIELDS(NF) = BUFFER(POS:FIN)
        ENDIF
      ENDIF

      READFIELDS = NF
      RETURN

c end of file
 9    READFIELDS = -1
      RETURN
      END


c *---------------------------------------------------------------------
c * Write NF fields, separated by tabs. 
c * [Trailing whitespace isn't written.  -- yes it is]
c *---------------------------------------------------------------------

      SUBROUTINE WRITEFIELDS(UNIT, FIELDS, NF)
      IMPLICIT NONE
      INTEGER UNIT, NF
      CHARACTER*15 FIELDS(NF)

      CHARACTER*320 BUFFER
      INTEGER POS, F, I
c      INTEGER RTRIM
      CHARACTER*1 TAB
c   
      TAB = CHAR(9)
      POS = 1
      DO 10 F = 1, NF
c        I = RTRIM(FIELDS(F))
        I = 15     !TEMPORARY CHEAT, TRIAL PERIOD ONLY
        BUFFER(POS:POS+I-1) = FIELDS(F)(1:I)
           POS = POS + I
           IF (F < NF) THEN
              BUFFER(POS:POS) = TAB
              POS = POS + 1
           ENDIF
 10     CONTINUE
      WRITE (UNIT, 11) BUFFER(1:POS-1)
 11     FORMAT (A)
      RETURN
      END


c *---------------------------------------------------------------------
c * Find position of rightmost non-whitespace character in string
c *---------------------------------------------------------------------
      FUNCTION RTRIM(S)
      IMPLICIT NONE
      CHARACTER*(*) S
      INTEGER POS, RTRIM
      POS = LEN(S)
c     LOOP
 10     IF (S(POS:POS) .NE. ' ') THEN
          RTRIM = POS
          RETURN
        ENDIF
        POS = POS - 1
        IF (POS .LE. 0) THEN
          RTRIM = 0
          RETURN
        ENDIF
        GOTO 10
c     END LOOP

      END


c *---------------------------------------------------------------------
c * Determine whether a given integer occurs in a vector.
c * (MEMQ is a similar function in the Lisp programming language.)
c *---------------------------------------------------------------------
      INTEGER FUNCTION IMEMQ(N, V, LEN)
      INTEGER N, LEN, V(LEN)
      INTEGER I
      DO 1 I = 1, LEN
        IF (V(I) .EQ. N) THEN
          IMEMQ = I
          RETURN
        ENDIF
    1 CONTINUE
      IMEMQ = 0
      RETURN
      END


c *------------------------------------------------------------
c * Find the index of a variable given its name.
c *------------------------------------------------------------
      INTEGER FUNCTION VARNUM(NAME)
      IMPLICIT NONE
      INCLUDE 'Nut.fi'
      CHARACTER*(*) NAME
      INTEGER I

      DO 1 I = 1, NVARS
        IF (NAME .EQ. VARNAMES(I)) THEN
          VARNUM = I
          RETURN
        ENDIF
    1 CONTINUE
      VARNUM = 0
      RETURN
      END


c=======================================================================
c=======================================================================
c From file UpAlpha.f

c FLUSH FLUSH FLUSH FLUSH FLUSH FLUSH FLUSH FLUSH FLUSH FLUSH FLUSH FLUSH
c Replace with quadratic formula

c INPUTS:  EVENSMLR, INC0, Vmax, KM, CMIN, GAMMA, v0, 
c          RTRATIO, r0, Deb, B, Cav,
c          CALC_alpha,                  -- flag
c          INALPHA,      -- observed only if CALC_alpha is .FALSE.
c
c OUTPUTS:
c          ALPHA, C0, Pc


c Pc = The ratio between the concentration of the 
c substance at the root surface and the      
c average solution concentration.

      SUBROUTINE UPALPHA(EVENSMLR,   INC0,    Vmax, 
     >                   Km,         Cmin,     GAMMA,
     >                   v0,         RTRATIO,  r0,    
     >                   Deb,        b,        Cav,   
     >                   CALC_ALPHA, INALPHA,  ALPHA,        
     >                   C0,      Pc)
      IMPLICIT NONE
c Parameters
      DOUBLE PRECISION EVENSMLR, INC0, Vmax
      DOUBLE PRECISION Km, Cmin, GAMMA
      DOUBLE PRECISION RTRATIO, r0, Deb, b, Cav, v0
      LOGICAL   CALC_ALPHA
      DOUBLE PRECISION INALPHA
      DOUBLE PRECISION ALPHA, C0, Pc

c Function
      DOUBLE PRECISION CALCPc

c Internal variables
      DOUBLE PRECISION OLDC0
      INTEGER ITER
      LOGICAL DONE

c Constants
      DOUBLE PRECISION CRITERION
        PARAMETER (CRITERION = 0.001d0)
      INTEGER MAXITER
        PARAMETER (MAXITER = 100)
      LOGICAL DEBUGP
        PARAMETER (DEBUGP = .FALSE.)

c Initialize
      ITER = 0 
      OLDC0 = INC0

c     *--------------------------------------------------------------* 
c     * If alpha is to be calculated, then check if concentration is * 
c     * below minimum indicating that the root absorbing power is    *
c     * zero.  If it is, indicate that the routine converged in zero *
c     * iterations.  Otherwise, indicate that the routine has not    *
c     * converged.                                                   *
c     *--------------------------------------------------------------* 
      IF (CALC_alpha) THEN 
        IF (Cav .LE. Cmin) THEN
          ALPHA = 0.0
          DONE = .TRUE.
        ELSE
          DONE = .FALSE.
        ENDIF

C     *---------------------------------------------------------*
C     * If alpha is not to be calculated make sure that routine *
C     * does not iterate.                                       *
C     *---------------------------------------------------------*
      ELSE
        DONE = .TRUE.
      ENDIF

c     LOOP
 10     CONTINUE
c     *-----------------------------------------------*
c     * Increment the number of iterations completed. *
c     *-----------------------------------------------*
        ITER = ITER + 1
C     *-------------------------------------------------------*
C     * If alpha is to be calculated then calculate the root  *
C     * absorbing power based on the old value of             *
C     * concentration at the root surface. Otherwise, set it  *
C     * to the input value.   Vmax / (Km + c0)                *
C     *-------------------------------------------------------*
        IF (CALC_ALPHA) THEN
           ALPHA = Vmax / (Km + OLDC0)
        ELSE
           ALPHA = INALPHA
        ENDIF
        if(debugp) print *, '| alpha in loop: ', alpha

        Pc = CALCPC(gamma, RTRATIO, Deb, EVENSMLR)

C     *----------------------------------------------------------*
C     * Calculate the new concentration of the substance at the  *
C     * root surface: C_0 = P_c * C_av
C     *----------------------------------------------------------*
        C0 = Pc * Cav
C     *----------------------------------------------------*
C     * Is this new concentration close enough to the old  *
C     * concentration to consider the solution converged?  *
C     * (Only check if alpha is being calculated)          *
C     *----------------------------------------------------*
        IF (CALC_ALPHA) THEN
          if(debugp) print *, '| criterion: ', criterion
          IF (ABS((C0 - OLDC0) / (OLDC0 + Km)) .LE.
     >          CRITERION) THEN
            DONE = .TRUE.
          ELSE
            OLDC0 = C0
          ENDIF
        ENDIF

C     *---------------------------------------------------------*
C     * If the maximum number of iterations has been run, allow *
C     * routine to end.                                         *
C     *---------------------------------------------------------*
        IF (ITER .GE. MAXITER) DONE = .TRUE.

        IF (.NOT.DONE) GOTO 10
c     END LOOP

      RETURN
      END
