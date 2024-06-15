c*****Preprocessor to generate the source code of the program MMC
      character*80 line,line1,blankline,filename
      character*1 NN,TN,NA,NL,TS,IB,CB,VC,PG,FG,PX,HU,FR,RF,DD,
     -  LO,DF,RT,PH,PS,R16,F95,TR,MS,QP,FE,ED,GS,DM,GM,ME,R1,AU
      character*1 ans
      character*1
     -  iopdefNN,iopdefTN,iopdefNA,iopdefNL,iopdefTS,iopdefFR,
     -  iopdefDB,iopdefUX,iopdefXX,iopdefUG,iopdefAX,iopdefPS,
     -  iopdefEF,iopdef16,iopdefDM,iopdefHP,iopdefI2,iopdefVC,
     -  iopdefIB,iopdefG7,iopdefPG,iopdefFG,iopdefRF,iopdefAB,
     -  iopdefDD,iopdefGM,iopdefGCE,iopdefFRE,iopdefTOR,iopdefLPT,
     -  iopdefPXA,iopdefGSV,iopdefGNS,iopdefPHS,iopdefEDC,iopdefRT,
     -  iopdefDF,iopdefEPE,iopdefTRL,iopdefVNN,iopdefTNN,iopdefMNE,
     -  iopdefR1,iopdefTAU,iopdefNS
      character*2 sym
      character*4 SYSTEM
      character*5 tfl(2)
      dimension id1(70),id2(40)
      common /values/ isize(70),iopt(40),maxo,maxs
      character*4 oslist(9)
      character*17 osllist(9)
      character*2 sizesym,optname
      character*10 sizename
      character*25 optlname
      character*38 sizelname
      common /names/ sizename(70),sizelname(70),sizesym(70),
     -  optname(40),optlname(40)
      common /interactive/ inter
      data NN /' '/,TN /' '/,NA /' '/,TS /' '/,IB /' '/,CB /' '/,
     -  VC /' '/,PG /' '/,FG /' '/,PX /' '/,HU /' '/,FR /' '/,
     -  RF /' '/,LO /' '/,DF /' '/,RT /' '/,PH /' '/,PS /' '/,
     -  TR /' '/,MS /' '/,QP /' '/,FE /' '/,ED /' '/,GS /' '/,
     -  DM /' '/,GM /' '/,R16 /' '/,ME /' '/,R1 /' '/,AU /' '/,
     -  F95 /' '/
      data iopdefGCE /' '/,iopdefFRE /' '/,iopdefTOR /' '/,
     -  iopdefLPT /' '/,iopdefPXA /' '/,iopdefGNS /' '/,iopdefPHS /' '/,
     -  iopdefGSV /' '/,iopdefEDC /' '/,iopdefRT /' '/,iopdefDF /' '/,
     -  iopdefEPE /' '/,iopdefTRL /' '/,iopdefVNN /' '/,iopdefTNN /' '/,
     -  iopdefGM /' '/,iopdef16 /' '/,iopdefMNE /' '/,iopdefR1 /' '/,
     -  iopdefTAU /' '/,iopdefNS /' '/
      data tfl /'FALSE','TRUE'/
      data oslist /'IRIX','G77 ','AIX ','EFC ','HP  ',
     -  'UNIX','ABSF','G95 ','GF  '/
      data osllist /'SGI Irix         ','Gnu Fortran-77   ',
     -  'IBM AIX          ','Intel Fortran    ','Hewlett-Packard  ',
     -  'Any Unix (Compaq)','Absoft Fortran90 ','Gnu Fortran-95   ',
     -  'G-Fortran        '/
c------------------------------------------------------------------------------
c
c       ===== INSTRUCTIONS TO CHANGE THE DEFAULT OPTIONS/VALUES =====
c
c     To change the OPERATING SYSTEM/COMPILER selection, change the lines
c       iopdefSYS=1
c       SYSTEM = 'IRIX'
c     to one of the followings:
c       iopdefSYS=2
c       'G77 ' (Linux Gnu Fortran)
c
c       iopdefSYS=3
c       'AIX ' (IBM - AIX)
c
c       iopdefSYS=4
c       'EFC ' (Intel Fortran)
c
c       iopdefSYS=5
c       'HP  ' (Hewlett-Packard)
c
c       iopdefSYS=6
c       'UNIX' (Generic Unix)
c
c       iopdefSYS=7
c       'ABSF' (Absoft compiler)
c
c       iopdefSYS=8
c       'G95 ' (Gnu Fortran-95)
c
c     To change a CODE SELECTION OPTION (i.e., uncomment C@** lines):
c       1. Find the setopt call whose next to last argument is comment symbol
c       2. Change the last argument of the setdim call from iopdef to 'F' or 'T'
c          and eliminate the (now superfluous) setdefeopt call.
c     For example, to eliminate force calculations (the current default is to
c     include them) the C@FR comments have to be kept. 
c     The corresponding calls are
c       call setdefopt(6,'FR','T',iopdefFR)
c       call setopt('solvent force calculation (force-biasing)',41,
c    -    FR,'@FR',iopdefFR)
c     This has to be changed to
c       call setdefopt(6,'FR','F',iopdefFR)
c       call setopt('solvent force calculation (force-biasing)',41,
c    -    FR,'@FR',iopdefFR)
c
c     To change a SIZE PARAMETER (i.e., set the value for a #** symbol):
c       1. Find the setdim call whose 3rd argument is the symbol to be changed
c          and the next to last argument is the corresponding variable name
c       2. Change the right-hand side of the assignment in the statement
c          above the setdim call to the value needed and eliminate the
c          condition before it.
c     For example, to change the maximum number of solute atoms (#ST) to 10000
c     change
c       if (idmaxslt .eq. -1) idmaxslt=4000
c       call setdim('solute atoms',12,'ST',0,maxslt,idmaxslt,1)
c     to
c       idmaxslt=10000
c       call setdim('solute atoms',12,'ST',0,maxslt,idmaxslt,1)
c     or to limit the number of different proximity distributions (#GQ) to 40
c     change
c       if (idmxpxgslt .eq. -1) idmxpxgslt=maxslt
c       call setdim('different proximity RDFs and QCDFs',34,'GQ',
c    -    maxslt,mxpxgslt,idmxpxgslt,1)
c     to
c       idmxpxgslt=40
c       call setdim('different proximity RDFs and QCDFs',34,'GQ',
c    -    maxslt,mxpxgslt,idmxpxgslt,1)
c------------------------------------------------------------------------------
      do ic=1,80
        blankline(ic:ic)=' '
      end do
      write (6,2000)
c-----Input filename
200   write (6,2006)
      filename=blankline
      read (5,1000) filename
      icl=80
      do while (icl .gt. 1 .and. filename(icl:icl) .eq. ' ')
        icl=icl-1
      end do
      if (icl.eq. 1) then
c       Use default
        icl=3
        filename(1:3)='mmc'
      end if
      filename(icl+1:icl+4)='.for'
      namlen=icl+4
c-----Open files
      open(unit=10,status='old',file=filename(1:namlen),
     -   form='formatted',iostat=ios)
      if (ios .ne. 0) then
        print *,'Problem opening ',filename(1:namlen)
        go to 200
      end if
      namlenc=namlen-2
      open(unit=20,status='new',file=filename(1:namlenc),
     -  form='formatted',iostat=ios)
      if (ios .ne. 0)
     -  open(unit=20,status='old',file=filename(1:namlenc),
     -   form='formatted',iostat=ios)
      if (ios .ne. 0) then
        print *,'Problem opening ',filename(1:namlenc)
        stop
      end if
      rewind 20
      call askyn('Do you want a full list of the symbols and values',49,
     -  1,-1,ifull)
      nv=0
      do is=1,maxs
        if (isize(is) .ne. -1) nv=nv+1
      end do
c     For nv > 5 it is assumed that isize and iopt are already initialized
c     (see key PRCO SAVE)
      if (nv .gt. 5) then
        write (6,2001)
        read (5,1000) ans
        if (iopt(14) .eq. 1) R16='T'
        if (ans .eq. 'n' .or. ans .eq. 'N') go to 100
        inter=1
      else
        call askyn('Do you want to set options & sizes interactively',
     -    48,1,-1,inter)
      end if
      call getdefsizes(
     - idmaxmol    ,idmaxatmol  ,idmxpxslt   ,idmaxsltmol ,idmaxwnnu   ,
     - idmaxnst    ,idmaxnsv    ,idmaxest    ,idmaxesv    ,idmaxloopslt,
     - idmaxwnnv   ,idmaxslt    ,idmaxgslt   ,idmaxtslt   ,idmaxslv    ,
     - idmaxss     ,idmaxat     ,idmaxtrgrgr ,idmaxstg    ,idmaxsvg    ,
     - idmaxsst    ,idmaxmst    ,idmaxgrid   ,idmaxpfgr   ,idmaxcggr   ,
     - idmaxorgr   ,idmaxxgr    ,idmaxygr    ,idmaxzgr    ,idmaxcav    ,
     - idmaxlin    ,idmaxausp   ,idmaxauit   ,idmaxavit   ,idmaxtors   ,
     - idmaxatyp   ,idmaxatypu  ,idmaxstmol  ,idmaxtgrid  ,idmaxwrgrid ,
     - idmaxgvv    ,idmaxdrgrid ,idmaxdagrid ,idmaxpegrid ,idmxpxgslt  ,
     - idmaxcavps  ,idmaxpfsum  ,idmaxmatch  ,idmaxtagrid ,idmxfeslt   ,
     - idmaxhunsite,idmxlooptor ,idmxdiffmol ,idmxdiffcr  ,idmxrescr   ,
     - idmaxwidslt ,idmaxphsmol ,idmaxhmneig ,idmaxmolfg  ,idmaxath    ,
     - idmaxmapgrid,idmaxhbgrid ,idmaxatsave, idmaxaucsave,idmaxgrdclst,
     - idmaxrndginp,idmaxnnlist,idmaxsitehb)
      call getdefopt(
     -  iopdefNN,iopdefTN,iopdefNA,iopdefNL,iopdefTS,iopdefFR,
     -  iopdefDB,iopdefUX,iopdefXX,iopdefUG,iopdefAX,iopdefPS,
     -  iopdefEF,iopdef16,iopdefDM,iopdefHP,iopdefI2,iopdefVC,
     -  iopdefIB,iopdefG7,iopdefPG,iopdefFG,iopdefRF,iopdefAB,
     -  iopdefDD)
c=====START Customization section
c-----Select operating system
      iopdefSYS=1
      SYSTEM = 'GF  '
c     F95='F'
300   if (inter .gt. 0) then
101     write (6,2005) (i,osllist(i),oslist(i),i=1,7)
        call getint('Your systems number (1-9)',25,iopdefSYS,isys)
        if (isys .lt. 1 .or. isys .gt. 9) then
          print *,'ERROR: invalid choice'
          go to 101
        end if
        SYSTEM=oslist(isys)
        iopdefSYS=isys
      end if
c-----Select system sizes
      if (idmaxmol .eq. -1) idmaxmol=4000
      call setdim('solvent molecules+1',17,'MO',0,maxmol,idmaxmol,1)
      if (mod(maxmol,2) .eq. 1) maxmol=maxmol+1
      if (idmaxslt .eq. -1) idmaxslt=6200
      call setdim('solute atoms',12,'ST',0,maxslt,idmaxslt,1)
      if (idmaxrndginp .eq. -1) idmaxrndginp=100000
      call setdim('random mumbers read',19,'RN',0,maxrndginp,
     -  idmaxrndginp,1)
c-----Select active functionalities
c     Active code segments - choose 'T' (true) or 'F' (false)
      call setdefopt(0,'GX','T',iopdefGCE)
      call setopt('cavity-biased (T,V,mu) ensemble calculation',43,
     -  CB,'#GX',iopdefGCE)
      call setdefopt(19,'IB','T',iopdefIB)
      call setopt('(T,P,N) ensemble calculation',28,IB,'@IB',iopdefIB)
      call setdefopt(0,'FE','T',iopdefFRE)
      call setopt('any free-energy simulation option',33,FE,'#FE',
     -  iopdefFRE)
      call setdefopt(0,'TR','T',iopdefTOR)
      call setdefopt(0,'LT','T',iopdefLPT)
      if (iopdefTOR .eq. 'F' .and. iopdefLPT .eq. 'F') then
        call setdefopt(0,'TR','T',iopdefTRL)
        call setopt('any intersolute move (PART or PARD)',35,MS,
     -    '#TR',iopdefTRL)
      else
        MS='T'
      end if
      if (MS .eq. 'T') then
        call setopt('torsion move arrays',19,TR,'#TR',iopdefTOR)
        if (TR .eq. 'T') then
          call setopt('torsion loop move arrays',24,LO,'#LT',iopdefLPT)
        else
          LO='F'
        end if
      else
        TR='F'
        LO='F'
        TN='F'
      end if
      call setdefopt(0,'SX','T',iopdefPXA)
      call setopt('proximity analysis arrays',25,PX,'#SX',iopdefPXA)
      call setdefopt(0,'GV','T',iopdefGSV)
      call setopt('general solvent arrays',22,GS,'#GV',iopdefGSV)
      call setdefopt(22,'FG','F',iopdefFG)
      call setopt('field-gradient calculation',26,FG,'@FG',iopdefFG)
      call setdefopt(21,'PG','T',iopdefPG)
      call setopt('cavity grid analysis',20,PG,'@PG',iopdefPG)
      if (iopdefpg .eq. 'T') then
        idmaxgrdclst=2000
        call setdim('number of cavities + number of pocket',37,
     -    'GC',0,maxgrdclst,idmaxgrdclst,1)
      else
        maxgrdclst=1
      end if
      call setdefopt(0,'MH','F',iopdefGNS)
      call setopt('generic site calculation',24,HU,'#MH',iopdefGNS)
      call setdefopt(23,'ME','F',iopdefMNE)
      call setopt('minimum energy saving',21,ME,'#NE',iopdefMNE)
      call setdefopt(23,'RF','F',iopdefRF)
      call setopt('reaction-field correction',25,RF,'@RF',iopdefRF)
      call setdefopt(25,'DD','F',iopdefDD)
      call setopt('distance-dependent dielectric',29,DD,'@DD',iopdefDD)
      if (RF .eq. 'T') then
        call setdefopt(26,'1R','F',iopdefR1)
        call setopt('1/r dielectric',14,R1,'@1R',iopdefR1)
      else
        call setdefopt(26,'1R','F',iopdefR1)
        call setopt('1/r dielectric',14,R1,'@1R',iopdefR1)
      end if
      call setdefopt(0,'GM','F',iopdefGM)
      call setopt('energy-map grid',15,GM,'@GM',iopdefGM)
      call setdefopt(0,'MS','T',iopdefPHS)
      call setopt('primary hydration shell',23,PH,'#MS',iopdefPHS)
      call setdefopt(0,'DT','F',iopdefEDC)
      call setopt('energy decomposition calculation',32,ED,'#DT',
     -  iopdefEDC)
      call setdefopt(15,'DM','F',iopdefDM)
      call setopt('MPI calls (for now, very limited',32,DM,'@DM',
     -  iopdefDM)
      if (ED .eq. 'T') TN='T'
      call setdefopt(0,'RC','F',iopdefRT)
      call setopt('structures for residence time calculation',41,RT,
     -  '#RC',iopdefRT)
      call setdefopt(0,'DC','F',iopdefDF)
      call setopt('diffusion constant calculation',30,DF,'#DC',iopdefDF)
      call setdefopt(0,'AU','F',iopdefTAU)
      if (TR .eq. 'T')
     -  call setopt('torsion angle autocorrelation calculation',41,AU,
     -    '#AU',iopdefTAU)
      call setdefopt(6,'FR','T',iopdefFR)
      call setopt('solvent force calculation (force-biasing)',41,
     -  FR,'@FR',iopdefFR)
      call setdefopt(5,'TS','F',iopdefTS)
      call setopt('solute torque calculation (whole solute rot)',44,
     -  TS,'@TS',iopdefTS)
      NN='T'
      if (maxmol .gt. 20000) then
        if (maxmol .gt. 25000) then
          NN='F'
          write (6,2012)
        else
          write (6,2008) 4*(maxmol/1000)**2/31
        end if
      end if
      if (NN .eq. 'T' .and. DM .eq. 'T') then
        NN='F'
        print *,'For now, MPI run can not use solvent NN map'
      end if
      if (NN .eq. 'T') then
        call setdefopt(1,'NN','T',iopdefNN)
        if (iopdefNN .eq. 'T' .and. iopdefVNN .eq. ' ') then
           iopdefVNN='T'
        else
           iopdefVNN='F'
        end if
        call setopt('solvent-solvent near-neighbor map',33,NN,'@NN',
     -    iopdefVNN)
      end if
      call setdefopt(2,'TN','F',iopdefTN)
      if (MS .eq. 'T' .and. ED .eq. 'F' .or. iopdefTN .eq. 'T') then
        if (iopdefTNN .eq. ' ') iopdefTNN='T'
        call setopt('solute-solute near-neighbor map',31,TN,'@TN',
     -    iopdefTNN)
      end if
      if (NN .eq. 'T' .or. TN .eq. 'T') then
c       Select near-neighbor map handling: either arithmetic or logical code
        if (SYSTEM .ne. 'IRIX') then
          call setdefopt(3,'NA','T',iopdefNA)
          call setopt('arithmetic handling of bitmaps',30,NA,'@NA',
     -      iopdefNA)
          if (NA .eq. 'T') NL='F'
          if (NA .eq. 'F') NL='T'
        end if
      else
        NA = 'T'
        NL = 'F'
      end if
      call setdefopt(18,'VC','T',iopdefVC)
      call setopt('vectorizable search',19,VC,'@VC',iopdefVC)
      call setdefopt(12,'PS','F',iopdefPS)
      call setopt('SGI autoparallelizable',22,PS,'@PS',iopdefPS)
      call setdefopt(0,'  ','F',iopdef16)
      call setopt('quadruple precision instead of double',37,R16,
     -  'R16',iopdef16)
      call setdefopt(0,'TN','F',iopdefEPE)
      call setopt('QPEN/EPEN potentials',20,QP,'#TN',iopdefEPE)
c     SGI autoparallelizable version enabled
      PS = 'F'
      if (TR .eq. 'T') then
        if (idmaxtors .eq. -1) idmaxtors=200
        call setdim('torsion angles',14,'TR',0,maxtors,idmaxtors,1)
        if (idmaxtagrid .eq. -1) idmaxtagrid=1
        call setdim('torsion angle distribution grids',32,'TD',0,
     -    maxtagrid,idmaxtagrid,1)
        itfac=max0(1,maxslt/500)
        if (idmaxtslt .eq. -1) idmaxtslt=itfac*maxslt
        call setdim('atoms in the full torsion list',30,'TA',
     -    0,maxtslt,idmaxtslt,1)
c       For lots of torsions, it has to be a multiple of maxslt!
      else
        maxtors=2
        maxtagrid=1
        maxtslt=1
      end if
      if (PH .eq. 'T') then
        maxphsmol=maxmol
      else
        maxphsmol=1
      end if
      if (PX .eq. 'T') then
c       Proximity analysis enabled
c       mxpxslt - #SX : maximum no of solute atoms for proximity analysis
        mxpxslt=maxslt
        if (idmxpxgslt .eq. -1) idmxpxgslt=maxslt
        call setdim('different proximity RDFs and QCDFs',34,'GQ',
     -    maxslt,mxpxgslt,idmxpxgslt,1)
c       Other default may be mxpxgslt=40
      else
        mxpxslt=2
        mxpxgslt=2
      end if
      if (CB .eq. 'T') then
c       (T,V,MU) - ensemble cavity arrays
        if (idmaxxgr .eq. -1) idmaxxgr=250
        call setdim('cavity grids in the X direction',31,
     -    'GX',0,maxxgr,idmaxxgr,1)
        if (idmaxygr .eq. -1) idmaxygr=250
        call setdim('cavity grids in the Y direction',31,
     -    'GY',0,maxygr,idmaxygr,1)
        if (idmaxzgr .eq. -1) idmaxzgr=250
        call setdim('cavity grids in the Z direction',31,
     -    'GZ',0,maxzgr,idmaxzgr,1)
        maxcav5p=(maxxgr*maxygr*maxzgr)/20
        if (idmaxcavps .eq. -1) idmaxcavps=max0(3000,maxcav5p)
        call setdim('cavities with pref. sampl. weight',33,
     -    'PP',0,maxcavps,idmaxcavps,1)
c                (or less)
      else
        maxxgr = 2
        if (idmaxxgr .eq. -1) idmaxxgr=36  
        call setdim('angular grids for Widom fcg addition (if any)',45,
     -    'GX',0,maxxgr,idmaxxgr,1)
        maxygr = 2
        maxzgr = 2
        maxcavps = 3
      end if
      if (GM .eq. 'T') then
        if (idmaxmapgrid .eq. -1) idmaxmapgrid=161
        call setdim('grid points in the energy map',29,
     -    'GM',0,maxmapgrid,idmaxmapgrid,1)
        if (idmaxhbgrid .eq. -1) idmaxhbgrid=30
        call setdim('grid points in the hydrogen-bond map',36,
     -    'GH',0,maxhbgrid,idmaxhbgrid,1)
      else
        maxmapgrid=1
        maxhbgrid=1
      end if
      if (HU .eq. 'T') then
        if (idmaxhunsite .eq. -1) idmaxhunsite=min0(2000,maxmol)
        call setdim('generic sites',13,'MH',maxmol,maxhunsite,
     -    idmaxhunsite,1)
        if (idmaxhmneig .eq. -1) idmaxhmneig=100
        call setdim('neighbors for full initial match',32,'NH',
     -    0,maxhmneig,idmaxhmneig,1)
        if (idmaxsitehb .eq. -1) idmaxsitehb=30
        call setdim('sites hydrogen-bonded to a residue',34,'NS',
     -    0,maxsitehb,idmaxsitehb,1)
      else
        maxhunsite = 2
        maxhmneig = 1
        maxsitehb = 1
      end if
      if (FG .eq. 'T') then
        if (PX .eq. 'F') then
          if (idmxpxgslt .eq. -1) idmxpxgslt=maxslt
          call setdim('different solute proximity regions',34,'GQ',
     -      maxslt,mxpxgslt,idmxpxgslt,1)
        end if
        if (idmaxmolfg .eq. -1) idmaxmolfg=maxmol
        call setdim('field-gradient solvent molecules',32,
     -    'MG',maxmol,maxmolfg,idmaxmolfg,1)
        if (maxmolfg .lt. maxmol) write (6,2011)
        rmb=float(12*maxmolfg)*float(mxpxgslt)/1.e+06
        if (rmb .gt. 50.0) write (6,2010) rmb
      else
        maxmolfg=1
      end if
      if (DF .eq. 'T') then
        if (idmxdiffcr .eq. -1) idmxdiffcr=30
        call setdim('structures in diffusion calculation',35,
     -    'DC',1000,mxdiffcr,idmxdiffcr,1)
      else
        mxdiffcr = 1
      end if
      if (RT .eq. 'T') then
        if (idmxrescr .eq. -1) idmxrescr=120
        call setdim('structures in residence time calculation/31+1',45,
     -    'RC',1000000,mxrescr,idmxrescr,1)
      else
        mxrescr = 1
      end if
      if (AU .eq. 'T' .or. RT .eq. 'T') then
        if (idmaxaucsave .eq. -1) idmaxaucsave=10000
        call setdim('terms in the autocorrelation function',37,
     -      'AU',0,maxaucsave,idmaxaucsave,1)
        if (AU .eq. 'T') print *,'NOTE: the maximum number of angles ',
     -    'saved per torsion is also ',maxaucsave
      else
        maxaucsave=1
      end if
c-----Other solute-related arrays
      if (idmaxsltmol .eq. -1) idmaxsltmol=min0(maxslt,1000)
      call setdim('solute molecules',16,'MM',
     -  maxslt,maxsltmol,idmaxsltmol,1)
      if (FG .eq. 'T') then
        rmb=float(12*mxpxgslt)*float(maxsltmol)/1.e+06
        if (rmb .gt. 50.0) write (6,2009) rmb
      end if
      if (idmaxgslt .eq. -1) idmaxgslt=max0(1,maxslt/5)
      call setdim('solute groups (residues)',24,'GR',maxslt,
     -  maxgslt,idmaxgslt,1)
      if (idmaxatmol .eq. -1) idmaxatmol=maxslt
      call setdim('atoms per solute molecules',26,'MA',
     -  maxslt,maxatmol,idmaxatmol,1)
      if (idmaxtrgrgr .eq. -1) idmaxtrgrgr=maxslt
      call setdim('solute groups for torsion option',32,'TL',
     -  0,maxtrgrgr,idmaxtrgrgr,1)
      if (GS .eq. 'F') then
        maxslv=5
        maxsvg=1
        maxstg=1
      else
        maxstg=maxslt
        if (idmaxsvg .eq. -1) idmaxsvg=4
        call setdim('solvent centers in a general solvent',36,'GV',
     -    0,maxsvg,idmaxsvg,1)
        maxslv=max0(5,maxsvg)
      end if
      if (FE .eq. 'T') then
        if (idmxfeslt .eq. -1) idmxfeslt=min0(maxslt,500)
        call setdim('free-energy solute atoms',24,'FE',
     -    maxslt,mxfeslt,idmxfeslt,1)
        if (idmaxwidslt .eq. -1) idmaxwidslt=min0(maxsltmol,6)
        call setdim('Widom solute molecules',22,'MW',
     -    maxsltmol,maxwidslt,idmaxwidslt,1)
        if (idmaxauit .eq. -1) idmaxauit=75
        call setdim('adaptive umbrella sampling iterations',37,'WI',
     -    0,maxauit,idmaxauit,1)
        if (idmaxmatch .eq. -1) idmaxmatch=25
        call setdim('adaptive umbrella sampling runs to match',40,'WM',
     -    0,maxmatch,idmaxmatch,1)
        if (idmaxcggr .eq. -1) idmaxcggr=100
        call setdim('coupling parameter distribution grids',37,'WG',
     -    0,maxcggr,idmaxcggr,1)
        if (idmaxorgr .eq. -1) idmaxorgr=100
        call setdim('energy difference distribution grids',36,'OR',
     -    0,maxorgr,idmaxorgr,1)
c       (Overlap Ratio method)
      else
        mxfeslt=1
        maxwidslt=1
        maxauit=1
        maxmatch=3
        maxcggr=2
        maxorgr=1
      end if
      if (QP .eq. 'T') then
        if (idmaxnst .eq. -1) idmaxnst=1
        call setdim('nuclei on solute for EPEN',25,'TN',
     -    maxslt,maxnst,idmaxnst,1)
        if (idmaxest .eq. -1) idmaxest=1
        call setdim('electrons on solute for EPEN',28,'TE',
     -    maxslt,maxest,idmaxnst,1)
        if (idmaxnsv .eq. -1) idmaxnsv=1
        call setdim('nuclei on solvent for EPEN',26,'VN',
     -    maxslv,maxnsv,idmaxnsv,1)
        if (idmaxesv .eq. -1) idmaxesv=1
        call setdim('electrons on solvent for EPEN',29,'VE',
     -    maxslv,maxesv,idmaxesv,1)
      else
        maxnst=1
        maxest=1
        maxnsv=1
        maxesv=1
      end if
      if (idmaxslv .eq. -1) idmaxslv=1
      if (ED .eq. 'T') then
        maxsst=maxslt
      else
        if (idmaxsst .eq. -1) idmaxsst=1
        call setdim('solutes for energy decomp. and sens. anal',41,
     -    'DT',maxslt,maxsst,idmaxsst,1)
      end if
      if (idmaxmst .eq. -1) idmaxmst=1
      call setdim('solvent molecules for sensitivity analysis',42,
     -  'DM',maxmol,maxmst,idmaxmst,1)
      maxmst = maxmol
c-----Distribution function dimensions
      if (PX .eq. 'T') then
        if (idmaxgrid .eq. -1) idmaxgrid=220
        call setdim('proximity g(r) grids',20,'RG',0,maxgrid,idmaxgrid,
     -    1)
        if (idmaxtgrid .eq. -1) idmaxtgrid=220
        call setdim('total g(r) grids',16,'TG',0,maxtgrid,idmaxtgrid,1)
        if (idmaxwrgrid .eq. -1) idmaxwrgrid=220
        call setdim('solvent-solvent proximity g(r) grids',36,'VG',
     -    0,maxwrgrid,idmaxwrgrid,1)
        if (idmaxgvv .eq. -1) idmaxgvv=3
        call setdim('solvent-solvent proximity g(r) types',36,'NG',
     -    maxslv,maxgvv,idmaxgvv,1)
        if (idmaxdrgrid .eq. -1) idmaxdrgrid=220
        call setdim('proximity radial dipole correlation grids',41,'DG',
     -    0,maxdrgrid,idmaxdrgrid,1)
        if (idmaxdagrid .eq. -1) idmaxdagrid=90
        call setdim('proximity angular dipole correlation grids',42,
     -    'LG',0,maxdagrid,idmaxdagrid,1)
        if (idmaxpegrid .eq. -1) idmaxpegrid=100
        call setdim('proximity solute-solvent pair energy grids',42,
     -    'GE',0,maxpegrid,idmaxpegrid,1)
      else
        maxgrid=1
        maxtgrid=1
        maxwrgrid=1
        maxgvv=1
        maxdrgrid=1
        maxdagrid=1
        maxpegrid=1
      end if
      if (idmaxatypu .eq. -1) idmaxatypu=100
      call setdim('solute atom types used',22,'UU',max0(100,maxslt),
     -  maxatypu,idmaxatypu,1)
      if (idmaxpfgr .eq. -1) idmaxpfgr=300
      call setdim('preferential sampling grids',27,'PG',
     -  0,maxpfgr,idmaxpfgr,1)
      if (idmaxpfsum .eq. -1) idmaxpfsum=10
      call setdim('preferential sampling sub sums',30,'PS',
     -  maxpfgr,maxpfsum,idmaxpfsum,1)
      if (idmaxavit .eq. -1) idmaxavit=200
      call setdim('block averages',14,'MI',0,maxavit,idmaxavit,1)
      nbitsperword=32
      if (idmaxnnlist .eq. -1) idmaxnnlist=32
      call setdim('1st+2nd+3rd neighbors',21,'NL',0,maxnnlist,
     -  idmaxnnlist,1)
C=====END custimization section
c     Change maxatyp only when the corresponding data statements are changed
c     maxatyp - #AT : maximum number of atom types
      maxatyp = 350
C=====START derived quantity definition - modify at your own risk
c     maxlin - #W2: #WI*(#WI+7)/2 if IBM minimizer fmfp is used for AUS
c                   #WI**2  if linear n-step matching is used for AUS
      maxlin = maxauit * ( maxauit + 7 ) / 2
c     maxausp - #WS: #WI*#WG/2+1 : size of storage array for individual
c                         probability distributions for AUS
      maxausp = maxauit * maxcggr / 2 + 1
c     maxwnnv - #VW : maximum no of computer words for solvent neighbour
c     bit list (the divider is the computer wordsize - 1)
      maxwnnv = ( maxmol / (nbitsperword-1) ) + 1
c     maxwnnu - #UW : maximum no of computer words for solute neighbour
c     bit list (the divider is the computer wordsize - 1)
      maxwnnu = ( maxslt /(nbitsperword-1) ) + 1
c     maxwnnu = 1
c     maxss - #VT  : greater of #SV and #ST
      maxss = max0(maxslt,maxslv)
      if (maxslv .lt. 5) then
        print *,'ERROR: # of solvent atoms/molecule is less then 5'
        stop
      end if
      maxslv4 = maxslv - 4
c     maxat - #NA : maximum no of centers (atoms and pseudoatoms)
      maxat = maxslt + maxslv * ( maxmol - 1 )
c     maxcav - #CV  : maximum no of cavities
      maxcav = maxxgr * maxygr * maxzgr
c     maxstmol - #UV  : greater of #NA, #MO, #ST and #MH
      maxstmol = max0(maxat,maxslt,maxmol,maxhunsite,32)
      maxath=max0(maxat,maxslt+maxhunsite*maxslv)
      if (ME .eq. 'T') then
        maxatsave=maxat
      else
        maxatsave=1
      end if
      if (DF .eq. 'T' .or. RT .eq. 'T') then
        if (PX .eq. 'F') then
          print *,'Diffusion and residence time calculations need PX=T'
          if (inter .gt. 0) then
            print *,'Repeat the input (NOTE: all defaults are changed ',
     -        'to their value just set'
            go to 300
          else
            stop
          end if
        end if
c       mxdiffmol - #MD : maximum number of molecules for diffusion const calc
c                         and/or residence time calculation
        mxdiffmol = maxmol
      else
        mxdiffmol = 2
      end if
c     if (RT .eq. 'T') then
c       Residence time arrays is a bitmap
c       mxrescr=mxrescr/31+1
c     end if
      totdiffres=mxdiffmol*mxrescr+6*mxdiffcr+4*mxdiffcr+
     -  9*mxdiffcr*mxdiffmol
      totdiffres=4.0*totdiffres*mxpxgslt/1000000.0
      if (totdiffres .gt. 500.0)
     -   write (6,2013) totdiffres,mxpxgslt,mxdiffcr,mxrescr
c     mxlooptor - #LT  : maximum no of torsion involved with loop moves
c     maxloopslt - #LS  : maximum no of torsion involved with loop moves
      if (LO .eq. 'T') then
        mxlooptor = maxtors
        maxloopslt = maxslt
      else
        mxlooptor = 1
        maxloopslt = 1
      end if
c     Summary print of sizes
c     Calculate and print memory and requirement and checkpoint file sizes
      write (6,2002) maxmol,maxslt,maxgslt,maxsltmol,maxslv,maxauit,
     -  maxxgr,maxygr,maxzgr,maxtors,maxtslt,mxpxslt,mxpxgslt
C=====END derived properties section
      call findsym('MO',sizesym,maxs,index,isize,maxmol)
      call findsym('MA',sizesym,maxs,index,isize,maxatmol)
      call findsym('SX',sizesym,maxs,index,isize,mxpxslt)
      call findsym('MM',sizesym,maxs,index,isize,maxsltmol)
      call findsym('UW',sizesym,maxs,index,isize,maxwnnu)
      call findsym('TN',sizesym,maxs,index,isize,maxnst)
      call findsym('VN',sizesym,maxs,index,isize,maxnsv)
      call findsym('TE',sizesym,maxs,index,isize,maxest)
      call findsym('VE',sizesym,maxs,index,isize,maxesv)
      call findsym('VW',sizesym,maxs,index,isize,maxwnnv)
      call findsym('ST',sizesym,maxs,index,isize,maxslt)
      call findsym('GR',sizesym,maxs,index,isize,maxgslt)
      call findsym('TA',sizesym,maxs,index,isize,maxtslt)
      call findsym('SV',sizesym,maxs,index,isize,maxslv)
      call findsym('V4',sizesym,maxs,index,isize,maxslv4)
      call findsym('VT',sizesym,maxs,index,isize,maxss)
      call findsym('NA',sizesym,maxs,index,isize,maxat)
      call findsym('TL',sizesym,maxs,index,isize,maxtrgrgr)
      call findsym('GT',sizesym,maxs,index,isize,maxstg)
      call findsym('GV',sizesym,maxs,index,isize,maxsvg)
      call findsym('DT',sizesym,maxs,index,isize,maxsst)
      call findsym('DM',sizesym,maxs,index,isize,maxmst)
      call findsym('RG',sizesym,maxs,index,isize,maxgrid)
      call findsym('PG',sizesym,maxs,index,isize,maxpfgr)
      call findsym('WG',sizesym,maxs,index,isize,maxcggr)
      call findsym('OR',sizesym,maxs,index,isize,maxorgr)
      call findsym('GX',sizesym,maxs,index,isize,maxxgr)
      call findsym('GY',sizesym,maxs,index,isize,maxygr)
      call findsym('GZ',sizesym,maxs,index,isize,maxzgr)
      call findsym('CV',sizesym,maxs,index,isize,maxcav)
      call findsym('W2',sizesym,maxs,index,isize,maxlin)
      call findsym('WS',sizesym,maxs,index,isize,maxausp)
      call findsym('WI',sizesym,maxs,index,isize,maxauit)
      call findsym('MI',sizesym,maxs,index,isize,maxavit)
      call findsym('TR',sizesym,maxs,index,isize,maxtors)
      call findsym('AT',sizesym,maxs,index,isize,maxatyp)
      call findsym('UU',sizesym,maxs,index,isize,maxatypu)
      call findsym('UV',sizesym,maxs,index,isize,maxstmol)
      call findsym('TG',sizesym,maxs,index,isize,maxtgrid)
      call findsym('VG',sizesym,maxs,index,isize,maxwrgrid)
      call findsym('ND',sizesym,maxs,index,isize,maxgvv)
      call findsym('DG',sizesym,maxs,index,isize,maxdrgrid)
      call findsym('LG',sizesym,maxs,index,isize,maxdagrid)
      call findsym('GE',sizesym,maxs,index,isize,maxpegrid)
      call findsym('GQ',sizesym,maxs,index,isize,mxpxgslt)
      call findsym('PP',sizesym,maxs,index,isize,maxcavps)
      call findsym('PS',sizesym,maxs,index,isize,maxpfsum)
      call findsym('WM',sizesym,maxs,index,isize,maxmatch)
      call findsym('TD',sizesym,maxs,index,isize,maxtagrid)
      call findsym('FE',sizesym,maxs,index,isize,mxfeslt)
      call findsym('MH',sizesym,maxs,index,isize,maxhunsite)
      call findsym('LT',sizesym,maxs,index,isize,mxlooptor)
      call findsym('LS',sizesym,maxs,index,isize,maxloopslt)
      call findsym('MD',sizesym,maxs,index,isize,mxdiffmol)
      call findsym('DC',sizesym,maxs,index,isize,mxdiffcr)
      call findsym('RC',sizesym,maxs,index,isize,mxrescr)
      call findsym('MW',sizesym,maxs,index,isize,maxwidslt)
      call findsym('MS',sizesym,maxs,index,isize,maxphsmol)
      call findsym('NH',sizesym,maxs,index,isize,maxhmneig)
      call findsym('MG',sizesym,maxs,index,isize,maxmolfg)
      call findsym('HA',sizesym,maxs,index,isize,maxath)
      call findsym('GM',sizesym,maxs,index,isize,maxmapgrid)
      call findsym('GH',sizesym,maxs,index,isize,maxhbgrid)
      call findsym('NE',sizesym,maxs,index,isize,maxatsave)
      call findsym('AU',sizesym,maxs,index,isize,maxaucsave)
      call findsym('GC',sizesym,maxs,index,isize,maxgrdclst)
      call findsym('RN',sizesym,maxs,index,isize,maxrndginp)
      call findsym('NL',sizesym,maxs,index,isize,maxnnlist)
      call findsym('NS',sizesym,maxs,index,isize,maxsitehb)
      print *
c     Code activation
c     Debug code must be turned on 'manually'
      call activatecode('F','DB',optlname,optname,iopt,maxo)
c     Activate solvent-solvent near-neighbour bitmap
      call activatecode(NN,'NN',optlname,optname,iopt,maxo)
c     Activate solute-solute near-neighbour bitmap
      call activatecode(TN,'TN',optlname,optname,iopt,maxo)
c     Either C@NL or C@NA has to be turned on when C@NN or C@TN is on
      if (NN .eq. 'T' .or. TN .eq. 'T') then
        call findsym('NA',optname,maxo,indexa,id2,-1)
        call findsym('NL',optname,maxo,indexl,id2,-1)
c       Deafult: arithemetic
        iopt(indexa)=1
        iopt(indexl)=0
        if (SYSTEM .eq. 'IRIX') then
          iopt(indexa)=1
          iopt(indexl)=0
          print *, optlname(indexa),' code is active'
         end if
      end if
c     Activate solute torque calculation (if slt is to be rotated)
      call activatecode(TS,'TS',optlname,optname,iopt,maxo)
c     Activate (TPN) ensemble calculations
      call activatecode(IB,'IB',optlname,optname,iopt,maxo)
c     Activate reaction-field correction
      call activatecode(RF,'RF',optlname,optname,iopt,maxo)
c     Activate distence-dependent dielectric calculation
      call activatecode(DD,'DD',optlname,optname,iopt,maxo)
c     Activate 1/r dielectric calculation
      call activatecode(R1,'1R',optlname,optname,iopt,maxo)
c     Activate Field gradient calculation code
      call activatecode(FG,'FG',optlname,optname,iopt,maxo)
c     Activate cavity grid analysys
      call activatecode(PG,'PG',optlname,optname,iopt,maxo)
c     Activate solvent force calculation code
      call activatecode(FR,'FR',optlname,optname,iopt,maxo)
c     Activate autoparallelizable code
      call activatecode(PS,'PS',optlname,optname,iopt,maxo)
c
c     Scalar-vector code selection
      call setonoff(VC,'VC','NV',optname,optlname,iopt,id2,maxo)
c     1/r dielectric or not selection
      call setonoff(R1,'1R','NR',optname,optlname,iopt,id2,maxo)
c     MPI/not MPI selection
      call setonoff(DM,'DM','ND',optname,optlname,iopt,id2,maxo)
c     Operating system/platform code selection and Unix flavor (if any)
      if (SYSTEM .eq. 'IRIX') then
        call activatecode('T','UG',optlname,optname,iopt,maxo)
      else if (SYSTEM .eq. 'G77 ') then
        call activatecode('T','G7',optlname,optname,iopt,maxo)
      else if (SYSTEM .eq. 'G95 ') then
        call activatecode('T','G5',optlname,optname,iopt,maxo)
      else if (SYSTEM .eq. 'GF  ') then
        call activatecode('T','GF',optlname,optname,iopt,maxo)
        F95='T'
      else if (SYSTEM .eq. 'EFC ') then
        call activatecode('T','EF',optlname,optname,iopt,maxo)
        F95='T'
      else if (SYSTEM .eq. 'AIX ') then
        call activatecode('T','AX',optlname,optname,iopt,maxo)
        call activatecode('T','UX',optlname,optname,iopt,maxo)
      else if (SYSTEM .eq. 'HP  ') then
        call activatecode('T','HP',optlname,optname,iopt,maxo)
        call activatecode('T','UX',optlname,optname,iopt,maxo)
        print *, 'YOU HAVE TO SET THE --> +U77 <-- COMPILATION OPTION'
      else if (SYSTEM .eq. 'ABSF') then
        call activatecode('T','AB',optlname,optname,iopt,maxo)
      else if (SYSTEM .eq. 'UNIX') then
        call activatecode('T','UX',optlname,optname,iopt,maxo)
      end if
100   if (ifull .eq. 1 .or. nv .gt. 2) then
        do i=1,maxs
          if (sizesym(i) .ne. '**') write (6,2003)
     -      i,sizename(i),sizesym(i),isize(i),sizelname(i)
        end do
        do i=1,maxo
          ioptw=iopt(i)
          if (iopt(i) .eq. -1) ioptw=1
          if (optname(i) .ne. '  ')
     -      write (6,2004) i,optlname(i),optname(i),tfl(ioptw+1)
        end do
      end if
      print *
      rnint=2*maxgvv+3*maxslv+73*maxslt+8*maxgslt+20*maxsltmol+
     -  42*maxtors+21*maxmol+2*maxgrid+maxpfgr+2*maxpfsum+maxcavps+
     -  20*maxatyp+2*maxcggr+8*maxauit+maxausp+2*maxstmol+3*maxtrgrgr+
     -  2*maxorgr+2*maxmst+maxcav+maxcavps+maxxgr*maxygr*maxzgr+
     -  2*maxxgr+maxtslt+maxtagrid*maxtors+11*mxlooptor+2*maxphsmol+
     -  maxrndginp
      if (NN .eq. 'T') rnint=rnint+maxwnnv*maxmol+3*maxmol
      if (TN .eq. 'T') rnint=rnint+maxwnnu*maxslt
      if (DM .eq. 'T') rnint=rnint+maxmol
      rnreal=3*maxat+55*maxmol+6*maxatmol+3*maxgslt+12*maxslv+41*maxslt+
     -  19*maxtors+12*maxsltmol+3*maxpfgr+maxcavps+3*maxatyp+5*maxavit+
     -  7*maxcggr+2*maxausp+3*maxsvg*maxsvg+3*maxstg*maxsvg+maxsvg+
     -  maxatypu*maxatypu+maxauit+3*maxtslt+10*maxloopslt+3*maxphsmol+
     -  9*maxhunsite+7500+2*maxaucsave*maxtors
      rndbl=10*maxsltmol+7*maxmol+16*maxtors+3*maxslt+3*maxpfsum+
     -  7*maxavit+9*maxcggr+9*maxsst*maxslv+2*maxxgr+24*mxlooptor
      if (FG .eq. 'T') rndbl=rndbl+12*maxmolfg*mxpxgslt+
     -  12*maxsltmol*mxpxgslt+2*maxsltmol+6*maxmolfg
      if (TS .eq. 'T') rndbl=rndbl+8*maxslt
      rnc4=4*maxslt+2*maxslv+10*maxatyp
      if (R16 .eq. 'T') rndbl=rndbl*2
      rnbytyckp=(4*rnint+4*rnreal+8*rndbl+4*rnc4)/1000000.0
      rnint=maxgrid*mxpxgslt+maxtgrid*mxpxgslt+15*mxpxslt+2*maxslt+
     -  maxdagrid*mxpxgslt+15*mxpxgslt+maxpegrid*mxpxgslt+
     -  maxwrgrid*mxpxgslt*maxgvv+
     -  4*maxsst+mxrescr*mxpxgslt*mxdiffmol+maxat+maxgrid*mxpxgslt
      rnreal=4*maxslt+3*maxslv+20*mxpxslt
      rndbl=maxdrgrid*mxpxgslt+7*mxpxslt+11*maxavit+3*maxslt+maxat+
     -  47*maxsst
      rnc4=3*maxslt
      if (R16 .eq. 'T') rndbl=rndbl*2
      rnbytypxc=totdiffres+(4*rnint+4*rnreal+8*rndbl+4*rnc4)/1000000.0
      if (PX .eq. 'F') then
        write (6,2007) 'C',rnbytyckp
      else
        write (6,2007) 'Total c',rnbytyckp+rnbytypxc
        write (6,2007) 'Simulation c',rnbytyckp
        write (6,2007) 'Analysis   c',rnbytypxc
      end if
      rnint=maxat+3*maxstmol+10*maxhunsite+4*maxmol+2*maxhunsite*maxmol+
     -  3*32*mxrescr+27*maxhbgrid**3
      rnreal=3*maxhunsite+2*maxstmol+3*maxhunsite*maxmol+
     -  mxpxgslt+mxrescr*16
      rnbytyadd=(4*rnint+4*rnreal)/1000000.0
      write (6,2014) 'Additional',rnbytyadd
      write (6,2014) 'Total',rnbytyadd+rnbytyckp+rnbytypxc
      if (rnbytyckp+rnbytypxc+rnbytyadd .gt. 1000.0) 
     -  print *,'WARNING: over 1Gb of memory will be required to run'
c     Option to change
      if (inter .gt. 0) then
        print *
        call askyn('Do yo want to modify some choices',33,1,0,modans)
        if (modans .eq. 1) then
          print *,'NOTE: all defaults are changed to their value set ',
     -      'in the last pass'
          go to 300
        end if
      end if
      nline=0
      do while (.true.)
        line=blankline
        read (10,1000,end=999) line
        nline=nline+1
c       Find last nonblank
        icl=80
        do while (icl .gt. 1 .and. line(icl:icl) .eq. ' ')
          icl=icl-1
        end do
c       Eliminate comments with requested symbols
        ic0=0
        do while (line(ic0+2:ic0+2) .eq. '@')
          sym=line(ic0+3:ic0+4)
          call findsym(sym,optname,maxo,index,id2,-1)
          if (index .gt. 0) then
            if (iopt(index) .eq. 1) then
c             Eliminate comment
              line(ic0+1:ic0+icl-4)=line(ic0+5:icl)
              icl=icl-4
            else
              ic0=ic0+4
            end if
          else
            ic0=ic0+4
          end if
        end do
CCC     if (line(2:2) .ne. '@') then
c         Check for #
          ic0=2
          do while (ic0 .le. icl-2)
            if (line(ic0:ic0) .eq. '#' .and.
     -          line(ic0+1:ic0+2) .ne. '  ') then
              sym=line(ic0+1:ic0+2)
              call findsym(sym,sizesym,maxs,index,id1,-1)
              if (index .gt. 0) then
                if (isize(index) .lt. 0) then
                  print *,'ERROR: symbol ',sym,' has no value assigned'
                  call writenum(line1,ic0,len,1)
                else
                  call writenum(line1,ic0,len,isize(index))
                end if
                if (len .ne. 3) then
                  lencha=len-3
                  lenchop=max0(0,icl+lencha-80)
c                 print *,line
c                 print *,'len,lencha,ic0,index=',len,lencha,ic0,index
                  lenc1=(icl-lenchop)-(ic0+3)
                  if (lenc1 .ge. 0)
     -              line(ic0+len:ic0+len+lenc1)=line(ic0+3:ic0+3+lenc1)
                  icl=icl+lencha
                  if (icl .gt. 80) then
                    print *,'Line ',nline,' is truncated to 80 chars'
                    icl=80
                  end if
                  if (lencha .lt. 0) then
                    do ic=icl+1,80
                      line(ic:ic)=' '
                    end do
                  end if
                end if
                line(ic0:ic0+len-1)=line1(ic0:ic0+len-1)
                ic0=ic0+len-1
              end if
            end if
            ic0=ic0+1
          end do
          if (R16 .eq. 'T') then
            ii0=0
            do while (line(ii0+2:ii0+2) .eq. '@')
              ii0=ii0+4
            end do
            if (line(ii0+1:ii0+12) .eq. '      real*8' .or.
     -          line(ii0+1:ii0+12) .eq. '      Real*8') then
              line(ii0+15:icl+1)=line(ii0+14:icl)
              line(ii0+11:ii0+14)='*16 '
              icl=icl+1
            end if
            ic0=2
            do while (ic0 .le. icl-1)
              icinc=1
              if (line(ic0:ic0) .eq. 'd') then
c               dsqrt,dabs,dlog,dexp,dacos,dasin,dcos,dsin,dcosh,dsinh
                if (icl-ic0 .ge. 1) then
                  if (line (ic0-1:ic0+1) .eq. '.d0') then
                    line (ic0-1:ic0+1)='.q0'
                  end if
                  if (icl-ic0 .ge. 3) then
                    if (line (ic0:ic0+3) .eq. 'dexp') then
                      icinc=3
                      line (ic0:ic0+icinc)='qexp'
                    else if (line (ic0:ic0+3) .eq. 'dabs') then
                      icinc=3
                      line (ic0:ic0+icinc)='qabs'
                    else if (line (ic0:ic0+3) .eq. 'dlog') then
                      icinc=3
                      line (ic0:ic0+icinc)='qlog'
                    else if (line (ic0:ic0+3) .eq. 'dcos') then
                      icinc=3
                      line (ic0:ic0+icinc)='qcos'
                    else if (line (ic0:ic0+3) .eq. 'dsin') then
                      icinc=3
                      line (ic0:ic0+icinc)='qsin'
                    end if
                  end if
                  if (icl-ic0 .ge. 4) then
                    if (line (ic0:ic0+4) .eq. 'dsqrt') then
                      icinc=4
                      line (ic0:ic0+icinc)='qsqrt'
                    else if (line (ic0:ic0+4) .eq. 'dcosh') then
                      icinc=4
                      line (ic0:ic0+icinc)='qcosh'
                    else if (line (ic0:ic0+4) .eq. 'dsinh') then
                      icinc=4
                      line (ic0:ic0+icinc)='qsinh'
                    else if (line (ic0:ic0+4) .eq. 'dacos') then
                      icinc=4
                      line (ic0:ic0+icinc)='qacos'
                    else if (line (ic0:ic0+4) .eq. 'dasin') then
                      icinc=4
                      line (ic0:ic0+icinc)='qasin'
                    else if (line (ic0:ic0+4) .eq. 'datan') then
                      icinc=4
                      line (ic0:ic0+icinc)='qatan'
                    else if (line (ic0:ic0+4) .eq. 'dmax1') then
                      icinc=4
                      line (ic0:ic0+icinc)='qmax1'
                    else if (line (ic0:ic0+4) .eq. 'dmin1') then
                      icinc=4
                      line (ic0:ic0+4)='qmin1'
                    end if
                  end if
                  if (icl-ic0 .ge. 5) then
                    if (line (ic0:ic0+5) .eq. 'dfloat') then
                      icinc=5
                      line (ic0:ic0+icinc)='qfloat'
                    end if
                  end if
                  if (icl-ic0 .ge. 9) then
                    if (line (ic0:ic0+9) .eq. 'dsign(1.d0') then
                      icinc=9
                      line (ic0:ic0+icinc)='qsign(1.q0'
                    end if
                  end if
                end if
              end if
              ic0=ic0+icinc
            end do
          end if
          if (F95 .eq. 'T') then
c           Change declaration formats
            ii0=0
            do while (line(ii0+2:ii0+2) .eq. '@')
              ii0=ii0+4
            end do
            if (line(ii0+1:ii0+16) .eq. '      character*') then
              if (line(ii0+17:ii0+19) .eq. '(*)') then
                line(ii0+23:ii0+icl+3)=line(ii0+20:ii0+icl)
                line(ii0+16:ii0+22)='(len=*)'
                icl=icl+3
              else
                ic0=18
                do while (idigit(line(ii0+ic0:ii0+ic0)) .eq. 1)
                  ic0=ic0+1
                end do
                icdel=ic0-17
                line(ii0+23+icdel:ii0+icl+5)=line(ii0+18+icdel:ii0+icl)
                line(ii0+20+1:ii0+20+icdel)=line(ii0+17:ii0+16+icdel)
                line(ii0+16:ii0+20)='(len='
                line(ii0+21+icdel:ii0+21+icdel+1)=') '
                icl=icl+5
              end if
            else if (line(ii0+1:ii0+12) .eq. '      real*8' .or. 
     -               line(ii0+1:ii0+12) .eq. '      Real*8') then
              line(ii0+15:ii0+icl+1)=line(ii0+14:ii0+icl)
              line(ii0+11:ii0+14)='(8) '
              icl=icl+1
            else if (line(ii0+1:ii0+13) .eq. '      real*16') then
              line(ii0+16:ii0+icl+2)=line(ii0+14:ii0+icl)
              line(ii0+11:ii0+15)='(16) '
              icl=icl+2
            else if (line(ii0+1:ii0+15) .eq. '      integer*2') then
              line(ii0+18:ii0+icl+1)=line(ii0+17:ii0+icl)
              line(ii0+14:ii0+17)='(2) '
              icl=icl+1
            end if
          end if
          if (iopt(29) .eq. 1) then
c           Not MPI/distributed memory - blank out 'if (MYRANK .eq. 0)'
            ic0=1
            call nextchar(line,ic0,80)
            if (ic0 .lt. 40) then
              if (line(ic0:ic0+19) .eq. 'if (MYRANK .eq. 0) w') then
                line(ic0:icl-18)=line(ic0+18:icl)
                icl=icl-18
              end if
            end if
          end if
          if (line(1:1) .ne. 'c' .and. line(1:1) .ne. 'C') then
c           Check for line length
            ic=1
            do while (ic .le. icl .and. line(ic:ic) .ne. '!')
              ic=ic+1
            end do
            ic=min0(ic,icl)
            if (ic .gt. 72)
     -        write (6,2015) nline,line(1:icl),filename(1:namlen)
          end if
          write (20,1000) line(1:icl)
CCC     end if
      end do
999   print *
      print *,'Fortran executable to be compiled: ',filename(1:namlenc)
      if (SYSTEM .eq. 'EFC ')
     -  print *,'NOTE: Use the compilation code -Vaxlib'
      stop
1000  format(a)
2000  format(' Preprocessor for MMC',20x,'(Version: 02/24/2021)')
2001  format(' Compilation information is contained in the arrays ',
     -  'iopt and isize',/,' Do you want to modify any of it (y/n)? ',$)
2002  format(/,' Maximum number of molecules:',i6,/,
     -   ' Maximum number of solute centers, all copies:',i6,/,
     -   ' Maximum number of solute groups, all copies:',i6,/,
     -   ' Maximum number of solute molecules, all copies:',i6,/,
     -   ' Maximum number of solvent centers per solvent:',i6,/,
     -   ' Maximum number of AUS iterations:',i3,/,
     -   ' Maximum number of X, Y, Z cavity grids:',3i4,/,
     -   ' Maximum number of torsions:',i6,/,
     -   ' Maximum number of atoms in the torsion list:',i6,/,
     -   ' Maximum number of solute atoms for proximity analysis:',i6,
     -   /,' Maximum number of different proximity RDFs and QCDFs:',i6)
2003  format(i3,1x,a10,' (',a2,') =',i10,1x,a)
2004  format(i3,1x,a25,' (',a2,'): ',a)
2005  format(' The following systems can be chosen:',/,
     -  (i2,1x,a17,1x,'(',a4,')'))
2006  format(' Name of the source file (without the .for) [mmc]=',$)
2007  format(1x,a,'heckpoint file size=',f8.1,' Mb')
2008  format(' NOTE: enabling the solvent-solvent near-neigbor bitmap',
     -  ' adds',i6,' Mb ',/,7x,'to the memory and disk space used')
2009  format(' NOTE: solute field-gradient calculations add',f8.1,
     -  ' Mb memory and disk space',/,7x,'- make sure that the maximum',
     -  ' number of solute molecules is not too big')
2010  format(' NOTE: solvent field-gradient calculations add',f8.1,
     -  ' Mb memory and disk space',/,7x,'- make sure that the maximum',
     -  ' number of different solute proximity ',/,7x, 'regions and',
     -  ' of field-gradient solvent molecules are not too big')
2011  format(' NOTE: when #MG is less than the number of solvents,',
     -  'use FLDG NOSV',/,7x,'(to skip field gradient calculations at ',
     -  'the solvent sites)')
2012  format(' Solvent-solvent near-neigbor bitmap be disabled since',/,
     - ' it will not work with this many solvents')
2013  format(/,' NOTE: residence time/diffusion calculation arrays ',
     -  'will take up ',f8.1,' Mb',/,' - you may want to reduce the ',
     -  'number of proximity distributions (',i6,') or',/,'   reduce ',
     -  'the maximum number of frames to be allowed (',i7,'/',i7,')')
2014  format(1x,a,' memory requirement=',f8.1,' Mb')
2015  format(' WARNING: line ',i6,':',/,1x,a,/,
     -  10x,'contains statement longer than 72 characters.',/,
     -  10x,'Edit ',a,' or use apropriate compilation option')
      end
      subroutine getdefsizes(
     - idmaxmol    ,idmaxatmol  ,idmxpxslt   ,idmaxsltmol ,idmaxwnnu   ,
     - idmaxnst    ,idmaxnsv    ,idmaxest    ,idmaxesv    ,idmaxloopslt,
     - idmaxwnnv   ,idmaxslt    ,idmaxgslt   ,idmaxtslt   ,idmaxslv    ,
     - idmaxss     ,idmaxat     ,idmaxtrgrgr ,idmaxstg    ,idmaxsvg    ,
     - idmaxsst    ,idmaxmst    ,idmaxgrid   ,idmaxpfgr   ,idmaxcggr   ,
     - idmaxorgr   ,idmaxxgr    ,idmaxygr    ,idmaxzgr    ,idmaxcav    ,
     - idmaxlin    ,idmaxausp   ,idmaxauit   ,idmaxavit   ,idmaxtors   ,
     - idmaxatyp   ,idmaxatypu  ,idmaxstmol  ,idmaxtgrid  ,idmaxwrgrid ,
     - idmaxgvv    ,idmaxdrgrid ,idmaxdagrid ,idmaxpegrid ,idmxpxgslt  ,
     - idmaxcavps  ,idmaxpfsum  ,idmaxmatch  ,idmaxtagrid ,idmxfeslt   ,
     - idmaxhunsite,idmxlooptor ,idmxdiffmol ,idmxdiffcr  ,idmxrescr   ,
     - idmaxwidslt ,idmaxphsmol ,idmaxhmneig ,idmaxmolfg  ,idmaxath    ,
     - idmaxmapgrid,idmaxhbgrid ,idmaxatsave ,idmaxaucsave,idmaxgrdclst,
     - idmaxrndginp,idmaxnnlist,idmaxsitehb)
      common /values/ isize(70),iopt(40),maxo,maxs
      idmaxmol    =isize(01)
      idmaxatmol  =isize(02)
      idmxpxslt   =isize(03)
      idmaxsltmol =isize(04)
      idmaxwnnu   =isize(05)
      idmaxnst    =isize(06)
      idmaxnsv    =isize(07)
      idmaxest    =isize(08)
      idmaxesv    =isize(09)
      idmaxloopslt=isize(10)
      idmaxwnnv   =isize(11)
      idmaxslt    =isize(12)
      idmaxgslt   =isize(13)
      idmaxtslt   =isize(14)
      idmaxslv    =isize(15)
      idmaxss     =isize(16)
      idmaxat     =isize(17)
      idmaxtrgrgr =isize(18)
      idmaxstg    =isize(19)
      idmaxsvg    =isize(20)
      idmaxsst    =isize(21)
      idmaxmst    =isize(22)
      idmaxgrid   =isize(23)
      idmaxpfgr   =isize(24)
      idmaxcggr   =isize(25)
      idmaxorgr   =isize(26)
      idmaxxgr    =isize(27)
      idmaxygr    =isize(28)
      idmaxzgr    =isize(29)
      idmaxcav    =isize(30)
      idmaxlin    =isize(31)
      idmaxausp   =isize(32)
      idmaxauit   =isize(33)
      idmaxavit   =isize(34)
      idmaxtors   =isize(35)
      idmaxatyp   =isize(36)
      idmaxatypu  =isize(37)
      idmaxstmol  =isize(38)
      idmaxtgrid  =isize(39)
      idmaxwrgrid =isize(40)
      idmaxgvv    =isize(41)
      idmaxdrgrid =isize(42)
      idmaxdagrid =isize(43)
      idmaxpegrid =isize(44)
      idmxpxgslt  =isize(45)
      idmaxcavps  =isize(46)
      idmaxpfsum  =isize(47)
      idmaxmatch  =isize(48)
      idmaxtagrid =isize(49)
      idmxfeslt   =isize(50)
      idmaxhunsite=isize(51)
      idmxlooptor =isize(52)
      idmxdiffmol =isize(53)
      idmxdiffcr  =isize(54)
      idmxrescr   =isize(55)
      idmaxwidslt =isize(56)
      idmaxphsmol =isize(57)
      idmaxhmneig =isize(58)
      idmaxmolfg  =isize(50)
      idmaxath    =isize(60)
      idmaxmapgrid=isize(61)
      idmaxhbgrid =isize(62)
      idmaxatsave =isize(63)
      idmaxaucsave =isize(64)
      idmaxgrdclst =isize(65)
      idmaxrndginp =isize(66)
      idmaxnnlist  =isize(67)
      idmaxsitehb  =isize(68)
      return
      end
      subroutine getdefopt(
     -  iopdefNN,iopdefTN,iopdefNA,iopdefNL,iopdefTS,iopdefFR,
     -  iopdefDB,iopdefUX,iopdefXX,iopdefUG,iopdefAX,iopdefPS,
     -  iopdefEF,iopdef16,iopdefDM,iopdefHP,iopdefI2,iopdefVC,
     -  iopdefIB,iopdefG7,iopdefPG,iopdefFG,iopdefRF,iopdefAB,
     -  iopdefDD)
      character*1
     -  iopdefNN,iopdefTN,iopdefNA,iopdefNL,iopdefTS,iopdefFR,
     -  iopdefDB,iopdefUX,iopdefXX,iopdefUG,iopdefAX,iopdefPS,
     -  iopdefEF,iopdef16,iopdefDM,iopdefHP,iopdefI2,iopdefVC,
     -  iopdefIB,iopdefG7,iopdefPG,iopdefFG,iopdefRF,iopdefAB,
     -  iopdefDD
      common /values/ isize(70),iopt(40),maxo,maxs
      call assigndefopt(iopdefNN,iopt(01))
      call assigndefopt(iopdefTN,iopt(02))
      call assigndefopt(iopdefNA,iopt(03))
      call assigndefopt(iopdefNL,iopt(04))
      call assigndefopt(iopdefTS,iopt(05))
      call assigndefopt(iopdefFR,iopt(06))
      call assigndefopt(iopdefDB,iopt(07))
      call assigndefopt(iopdefUX,iopt(08))
      call assigndefopt(iopdefXX,iopt(09))
      call assigndefopt(iopdefUG,iopt(10))
      call assigndefopt(iopdefAX,iopt(11))
      call assigndefopt(iopdefPS,iopt(12))
      call assigndefopt(iopdefEF,iopt(13))
      call assigndefopt(iopdef16,iopt(14))
      call assigndefopt(iopdefDM,iopt(15))
      call assigndefopt(iopdefHP,iopt(16))
      call assigndefopt(iopdefI2,iopt(17))
      call assigndefopt(iopdefVC,iopt(18))
      call assigndefopt(iopdefIB,iopt(19))
      call assigndefopt(iopdefG7,iopt(20))
      call assigndefopt(iopdefPG,iopt(21))
      call assigndefopt(iopdefFG,iopt(22))
      call assigndefopt(iopdefRF,iopt(23))
      call assigndefopt(iopdefAB,iopt(24))
      call assigndefopt(iopdefDD,iopt(25))
      return
      end
      subroutine assigndefopt(idefopt,iopt)
      character*1 idefopt
      if (iopt .eq. -1) then
        idefopt=' '
      else if (iopt .eq. 0) then
        idefopt='F'
      else if (iopt .eq. 1) then
        idefopt='T'
      else
        print *,'Illegal option code:',iopt
        idefopt=' '
      end if
      return
      end
      subroutine setdefopt(iop,sizesyminp,iopdef0,iopdef)
      character*1 iopdef0,iopdef
      character*2 sizesyminp
      common /values/ isize(70),iopt(40),maxo,maxs
      character*2 sizesym,optname
      character*10 sizename
      character*25 optlname
      character*38 sizelname
      common /names/ sizename(70),sizelname(70),sizesym(70),
     -  optname(40),optlname(40)
      character*1 tfs(2)
      dimension isizex(70)
      data tfs /'F','T'/
      if (iopdef .eq. 'F' .or. iopdef .eq. 'T') return
      if (iop .gt. 0) then
c       Compilation option index is available
        if (iopt(iop) .eq. -1) then
          iopdef=iopdef0
        else
          iopdef=tfs(iopt(iop)+1)
        end if
      else
c       Find value associated with size symbol sizesyminp
        call findsym(sizesyminp,sizesym,maxs,index,isizex,maxmol)
        if ((index .eq. 0 .or. index .gt. 70) .and.
     -       sizesyminp .ne. '  ') then
          print *,'Symbol ',sizesyminp,' gave invalid index:',index
          iopdef=iopdef0
        else if (isize(index) .eq. -1) then
          iopdef=iopdef0
        else if (isize(index) .le. 2) then
          iopdef=tfs(1)
        else
          iopdef=tfs(2)
        end if
      end if
      return
      end
      subroutine setopt(desc,ldesc,optvar,sym,def)
      character*1 optvar,def
      character*3 sym
      character*(*) desc
      character*1 tf(2)
      character*200 line
      common /interactive/ inter
      data tf /'F','T'/
      if (inter .eq. 0) then
        optvar=def
      else
        if (def .eq. 'F') idef=-1
        if (def .eq. 'T') idef=+1
        ld=ldesc+9
        write (line(1:ld),1000) desc(1:ldesc)
        if (sym(1:1) .eq. '@') then
          line(ld+1:ld+1)='C'
          ld=ld+1
        end if
        line(ld+1:ld+3)=sym
        line(ld+4:ld+7)=') - '
        ld=ld+7
        call askyn(line(1:ld),ld,1,idef,ia)
        optvar=tf(ia+1)
        def=optvar
      end if
      return
1000  format('Enable ',a,' (')
      end
      subroutine setonoff(onoff,labon,laboff,optname,optlname,iopt,
     -  id2,maxo)
      character*1 onoff
      character*2 labon,laboff,optname(maxo)
      character*25 optlname(maxo)
      dimension id2(maxo),iopt(maxo)
c     Makes sure that complementary options are set correctly
      call findsym(labon,optname,maxo,indexv,id2,-1)
      call findsym(laboff,optname,maxo,indexs,id2,-1)
      if (onoff .eq. 'T') then
        iopt(indexv)=1
        iopt(indexs)=0
        print *, optlname(indexv),' code is active'
      else
        iopt(indexv)=0
        iopt(indexs)=1
        print *, optlname(indexs),' code is active'
      end if
      return
      end
      subroutine setdim(desc,ldesc,sym,lim,ivar,idef,ichdef)
      character*(*) desc
      character*2 sym
      common /interactive/ inter
      character*200 line
      if (inter .eq. 0) then
        ivar=idef
      else
        ld=ldesc+24
        write (line(1:ld),1000) desc(1:ldesc),sym
100     call getint(line(1:ld),ld,idef,ivar)
        if (ivar .le. 0) then
          print *,'ERROR: array sizes must be positive'
          go to 100
        else if (ivar .gt. lim .and. lim .gt. 0) then
          print *,'ERROR: this array size can not exceed ',lim
          go to 100
        end if
        if (ichdef .eq. 1) idef=ivar
      end if
      return
      ivar=idef
      return
1000  format('Maximum number of ',a,' (#',a2,')',$)
      end
      subroutine findsym(sym,list,llist,index,iarr,ival)
      character*2 sym,list
      dimension list(llist),iarr(llist)
c     print *,'FINDSYM sym=',sym,' val=',ival
      index=0
      do while (index .lt. llist)
        index=index+1
        if (sym .eq. list(index)) then
          if (ival .eq. -1) then
            iarr(index)=0
          else if (ival .eq. 0) then
            ival=iarr(index)
          else if (ival .gt. 0) then
            iarr(index)=max0(0,ival)
          end if
          return
        end if
      end do
      index=0
c     print *, 'ERROR: ',sym,' not found in list'
      end
      subroutine writenum(line,ic,len,num)
      character*80 line
      if (num .lt. 10) then
        write (line(ic:ic),101) num
        len=1
      else if (num .lt. 100) then
        write (line(ic:ic+1),102) num
        len=2
      else if (num .lt. 1000) then
        write (line(ic:ic+2),103) num
        len=3
      else if (num .lt. 10000) then
        write (line(ic:ic+3),104) num
        len=4
      else if (num .lt. 100000) then
        write (line(ic:ic+4),105) num
        len=5
      else if (num .lt. 1000000) then
        write (line(ic:ic+5),106) num
        len=6
      else if (num .lt. 10000000) then
        write (line(ic:ic+6),107) num
        len=7
      else if (num .lt. 100000000) then
        write (line(ic:ic+7),108) num
        len=8
      else
        write (line(ic:ic+8),109) num
        len=9
      end if

      return
101   format(i1)
102   format(i2)
103   format(i3)
104   format(i4)
105   format(i5)
106   format(i6)
107   format(i7)
108   format(i8)
109   format(i9)
      end
      subroutine activatecode(yesno,sym,optlname,optname,iopt,len)
      dimension iopt(len)
      character*1 yesno,tf(2)
      character*2 sym,optname(len)
      character*25 optlname(len)
      dimension id(100)
      data tf /'F','T'/
      ival=0
      call findsym(sym,optname,len,index,id,ival)
      if (index .eq. 0) then
        print *,'index=0 for sym=',sym
        return
      end if
      if (yesno .eq. ' ') yesno=tf(iopt(index)+1)
      if (yesno .eq. 'T') then
        iopt(index)=1
        print *, optlname(index),' code is active'
      else
        iopt(index)=0
        print *, optlname(index),' code is inactive'
      end if
      return
      end
      function idigit(in)
      character*1 in,idig(10)
      data idig /'0','1','2','3','4','5','6','7','8','9'/
      idigit=0
      do i=1,10
        if (in .eq. idig(i)) then
          idigit=1
          return
        end if
      end do
      return
      end
      subroutine askyn(q,lenq,iyn,idefans,ians)
      character*(*) q
      character*132 pline
      character*1 ans
      character*5 defans
c     idefans=-1: default no; idefans=+1: default yes
c     iyn=1: yes -> ians=1, no -> ians=0
c     iyn=0: yes -> ians=0, no -> ians=1
      if (idefans .eq. -1) then
        defans='[n] '
        lendef=4
      else if (idefans .eq. +1) then
        defans='[y] '
        lendef=4
      else
        defans=' '
        lendef=1
      end if
      pline(1:1)=' '
      pline(2:lenq+1)=q(1:lenq)
      pline(lenq+2:lenq+8)=' (y/n) '
      pline(lenq+9:lenq+8+lendef)=defans(1:lendef)
100   write (6,1000) pline(1:lenq+8+lendef)
      read (5,1001,end=99,err=99) ans
99    ians=0
      if (ans .ne. 'n' .and. ans .ne. 'N' .and. ans .ne. 'y' .and.
     -  ans .ne. 'Y') then
        if (idefans .eq. -1) then
          ans='n'
        else if (idefans .eq. 1) then
          ans='y'
        else
          print *,'Pls answer y or n'
          go to 100
        end if
      end if
      if (ans .eq. 'y' .or. ans .eq. 'Y') ians=1
      if (iyn .eq. 0) ians=1-ians
      return
1000  format(a,1x,$)
1001  format(a1)
      end
      subroutine getint(q,len,idefault,in)
      character*(*) q
      character*132 ansline,pline
      lenq=len
      pline(1:1)=' '
      pline(2:lenq+1)=q(1:lenq)
      if (idefault .ne. 999999) then
c       Put default on query
        pline(lenq+2:lenq+3)=' ['
        if (idefault .le. 9999) then
          write (pline(lenq+4:lenq+7),1002) idefault
          pline(lenq+8:lenq+9)=']='
          lenq=lenq+9
        else
          write (pline(lenq+4:lenq+11),1003) idefault
          pline(lenq+12:lenq+13)=']='
          lenq=lenq+13
        end if
      else
        pline(lenq+2:lenq+2)='='
        lenq=lenq+2
      end if
100   write (6,1000) pline(1:lenq)
      read (5,1001,end=99,err=99) ansline
      ii=1
      call nextchar(ansline,ii,132)
      i1=ii
      call nextblank(ansline,ii)
      i2=ii-1
      if (i1 .gt. i2) then
        if (idefault .eq. 99999) go to 100
        in=idefault
      else if (i2-i1 .eq. 0) then
        read (ansline(i1:i2),101,err=999) in
      else if (i2-i1 .eq. 1) then
        read (ansline(i1:i2),102,err=999) in
      else if (i2-i1 .eq. 2) then
        read (ansline(i1:i2),103,err=999) in
      else if (i2-i1 .eq. 3) then
        read (ansline(i1:i2),104,err=999) in
      else if (i2-i1 .eq. 4) then
        read (ansline(i1:i2),105,err=999) in
      else if (i2-i1 .eq. 5) then
        read (ansline(i1:i2),106,err=999) in
      else if (i2-i1 .eq. 6) then
        read (ansline(i1:i2),107,err=999) in
      else if (i2-i1 .eq. 7) then
        read (ansline(i1:i2),108,err=999) in
      else if (i2-i1 .eq. 8) then
        read (ansline(i1:i2),109,err=999) in
      else if (i2-i1 .eq. 9) then
        read (ansline(i1:i2),110,err=999) in
      else
        read (ansline(i1:i2),*,err=999) in
      end if
      return
99    in=idefault
      return
999   print *,'Invalid input for an integer'
      go to 100
101   format(i1)
102   format(i2)
103   format(i3)
104   format(i4)
105   format(i5)
106   format(i6)
107   format(i7)
108   format(i8)
109   format(i9)
110   format(i10)
1000  format(a,$)
1001  format(a132)
1002  format(i4)
1003  format(i8)
      end
      subroutine nextchar(line,ifc,len)
      character*(*) line
c     Finds the next nonblank in line
      if (ifc .gt. len-1) return
      ifc1=ifc
      do i=ifc1,len-1
        ifc=i
        if (line(i:i) .ne. ' ' .and. line(i:i) .ne. '  ') then
          if (line(i:i) .eq. '!') ifc=len
          return
        end if
      end do
      ifc=len
      return
      end
      subroutine nextblank(line,ifc)
      character*(*) line
c     Finds the next blank in line
      if (ifc .gt. 131) return
      ifc1=ifc
      do i=ifc1,131
        ifc=i
        if (line(i:i) .eq. ' ' .or. line(i:i) .eq. '   ') then
          if (line(i:i) .eq. '!') ifc=132
          return
        end if
      end do
      ifc=132
      return
      end
      block data
      common /values/ isize(70),iopt(40),maxo,maxs
      character*2 sizesym,optname
      character*10 sizename
      character*25 optlname
      character*38 sizelname
      common /names/ sizename(70),sizelname(70),sizesym(70),
     -  optname(40),optlname(40)
      data maxo /40/,maxs/70/
c     sizesym(i): the 2-character dimensioning symbol list
      data sizesym
     - /'MO','MA','SX','MM','UW','TN','VN','TE','VE','LS',
     -  'VW','ST','GR','TA','SV','VT','NA','TL','GT','GV',
     -  'DT','DM','RG','PG','WG','OR','GX','GY','GZ','CV',
     -  'W2','WS','WI','MI','TR','AT','UU','UV','TG','VG',
     -  'ND','DG','LG','GE','GQ','PP','PS','WM','TD','FE',
     -  'MH','LT','MD','DC','RC','MW','MS','NH','MG','HA',
     -  'GM','GH','NE','AU','GC','RN','NL','NS',2*'  '/
c     iasize(i): the actual value of the dimension symbol sizesym(i)
c     sizename(i): the name used in pre.f for the dimension symbol sizesym(i)
      data sizename
     -/'maxmol    ','maxatmol  ','mxpxslt   ','maxsltmol ','maxwnnu   ',
     - 'maxnst    ','maxnsv    ','maxest    ','maxesv    ','maxloopslt',
     - 'maxwnnv   ','maxslt    ','maxgslt   ','maxtslt   ','maxslv    ',
     - 'maxss     ','maxat     ','maxtrgrgr ','maxstg    ','maxsvg    ',
     - 'maxsst    ','maxmst    ','maxgrid   ','maxpfgr   ','maxcggr   ',
     - 'maxorgr   ','maxxgr    ','maxygr    ','maxzgr    ','maxcav    ',
     - 'maxlin    ','maxausp   ','maxauit   ','maxavit   ','maxtors   ',
     - 'maxatyp   ','maxatypu  ','maxstmol  ','maxtgrid  ','maxwrgrid ',
     - 'maxgvv    ','maxdrgrid ','maxdagrid ','maxpegrid ','mxpxgslt  ',
     - 'maxcavps  ','maxpfsum  ','maxmatch  ','maxtagrid ','mxfeslt   ',
     - 'maxhunsite','mxlooptor ','mxdiffmol ','mxdiffcr  ','mxrescr   ',
     - 'maxwidslt ','maxphsmol ','maxhmneig ','maxmolfg  ','maxath    ',
     - 'maxmapgrid','maxhbgrid ','maxatsave ','maxaucsave','maxgrdclst',
     - 'maxrndginp','maxnnlist ','maxsitehb ',2*'          '/
      data optname/'NN','TN','NA','NL','TS','FR','DB','UX','  ','UG',
     -  'AX','PS','EF','16','DM','HP','I2','VC','IB','G7','PG','FG',
     -  'RF','AB','DD','1R','G9','GF',2*'  ','ND','NV','NR',7*'  '/
      data optlname /
     -  'Solvent near-neighbor map','Solute near-neighbor map ',
     -  'Arithmetic bit-map code  ','Logical bit-map handling ',
     -  'Solute torque calculation','Force/torque calculations',
     -  'Debugging code           ','Generic Unix             ',
     -  '                         ','SGI Unix                 ',
     -  'AIX Unix                 ','SGI auto parallelization ',
     -  'Intel Fortan calls       ','Quadruple precision      ',
     -  'MPI-Distributed memory   ','Hewlett-Packard          ',
     -  'Integer*2                ','Vectorized search        ',
     -  'Isobaric ensemble        ','Gnu Fortran-77           ',
     -  'Cavity grid analysis     ','Field gradient calcs.    ',
     -  'Reaction-field correction','Absoft Fortran 90/95     ',
     -  'Mehler-Solmayer DD diele.','1/r dielectric           ',
     -  'Gnu Fortran-95           ','GFortran                 ',
     -  2*'                         ',
     -  'Not MPI                  ','Non-vectorized search    ',
     -  'Not 1/r dielectric       ',
     -  7*'                         '/
      data sizelname /
     -  'solvent molecules+1                   ',
     -  'atoms per solute molecule             ',
     -  'solute atoms for proximity analysis   ',
     -  'solute molecules                      ',
     -  'words for solute neighbour bit list   ',
     -  'nuclei on solute                      ',
     -  'nuclei on solvent                     ',
     -  'EPEN electrons on solute              ',
     -  'EPEN electrons on solvent             ',
     -  'number of solute molecules w loop move',
     -  'words for solvent neighbour bit list  ',
     -  'solute centers (all copies)           ',
     -  'solute groups (residues) - all copies ',
     -  'solute centers for torsion option     ',
     -  'solvent centers/solvent               ',
     -  'solute or solvent centers             ',
     -  'centers (atoms and pseudoatoms)       ',
     -  'solute groups within torsion groups   ',
     -  'solute centers with a general solvent ',
     -  'solvent centers in a general solvent  ',
     -  'solute centers for sensitivity analyss',
     -  'molecules for sensitivity analysis    ',
     -  'full g(r) and primary g(r) grid-points',
     -  'preferential sampling grid points     ',
     -  'coupling parameter distribution grids ',
     -  'energy difference distribution grids  ',
     -  'grids in the x dir for grid search    ',
     -  'grids in the y dir for grid search    ',
     -  'grids in the z dir for grid search    ',
     -  'cavities                              ',
     -  'adaptive US matching workspace        ',
     -  'stored probabilities                  ',
     -  'iterations allowed for adaptive US+1  ',
     -  'block average entries                 ',
     -  'torsions                              ',
     -  'atom types the program can store      ',
     -  'atom types in a given solute          ',
     -  'molecules or solute atoms             ',
     -  'total g(r) grid points                ',
     -  'grid points for solvent-solvent g(r)s ',
     -  'number of solvent-solvent g(r)s       ',
     -  'dipole correlation QCDF radial grids  ',
     -  'dipole correlation QCDF angular grids ',
     -  'solute-solvent PE QCDF energy grids   ',
     -  'different QCDFs                       ',
     -  'cavities with pref. sampl. weights    ',
     -  'preferential sampling weight sub sums ',
     -  'AUS iterations to match               ',
     -  'torsion angle distribution grids      ',
     -  'free energy solute atoms              ',
     -  'sites for Hungarian method matching   ',
     -  'torsion loops                         ',
     -  'molecules for diffusion and residence ',
     -  'structures for diffusion              ',
     -  'structures for residence time         ',
     -  'number of Widom solutes               ',
     -  'number of primary hydr shell molecules',
     -  'number of neighbors for full match try',
     -  'number of molecules for fg calculation',
     -  'number of representative atoms        ',
     -  'number of potential function map grids',
     -  'number of hydrogen-bond map grids     ',
     -  'number of atoms for min energy save   ',
     -  'number of frames saved for torsion auc',
     -  'number of cavity/pocket grid clusters ',
     -  'number of random numbers read         ',
     -  'number of 1st+2nd+3rd neighbors       ',
     -  'number of sites H-bonded to a residue ',
     -  2*'                                      '/
c     This can be replaced by the data statements printed by MMC
c     using the PRCO SAVE command - it will not be overridden by the
c     statements in the driver
c             MO         MA         SX         MM         UW         TN
c             VN         TE         VE         LS         VW         ST
c             GR         TA         SV         VT         NA         TL
c             GT         GV         DT         DM         RG         PG
c             WG         OR         GX         GY         GZ         CV
c             W2         WS         WI         MI         TR         AT
c             UU         UV         TG         VG         ND         DG
c             LG         GE         GQ         PP         PS         WM
c             TD         FE         MH         LT         MD         DC
c             RC         MW         MS         NH         MG         HA
c             GM         GH         NE         AU         GC         RN
c             NL         NS
      data isize /70*-1/
      data iopt /40*-1/
      end
