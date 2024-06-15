      program pxps
c*****Generate Postscript plots from proximity analysis results
c Version 1.0: M Mezei, based on
c     make_dynamics_distance_graph of Dan Strahs (strahs)
      integer number, numgr, lablen, numytic, iline, numstr
      integer grphnum, super(20), sub(20), ndatcol(5), ict(2,5)
      integer ian(10000), ix(10000)
      real label, topstart(2)
      real time(100000), value(100000), time2(10000), value2(10000)
      real miny, maxy, minx, maxx, graphs(8,2), coorx
      real ydist, xdist
      real xtic, ytic, xticst,  xticminor, yticminor
      real skip, fsize(20), fmaxsize, sizecurrent
      real dataline(5)
      integer i180d(5,5),inoz(5,5)
      character*100  outputfile, filename, inpfile, emptystring
      character*24 datestring
      character*10 fname(20), fontcurrent, greekfont, normalfont
      character*50 datanames(5)
      character*80 line
      character*200 titlestring, indgraphstring, largeemptystring
      character*200 tmpstring(20)
      character*8 datacol(5,5),labslt(10000)
      character*8 username
      character*9 version
      character*3 datatypes(5)
      character*1 current
      character*1 tab
      common /tab/ itab,tab
      character*4 tab4
      equivalence (tab4,itabb)
      data version /' 02/14/18'/
      data graphs/103.5,360,103.5,360,
     2            103.5,360,103.5,360,
     3            562.5,562.5,402.75,402.75,
     4            245.25,245.25,90,90/
      data topstart/103.5, 700.0/
      data ict /22,24,22,24,22,24,22,24,22,24/
      data datatypes /'gkr','xpe','tav','xtd','gvv'/
      data datanames /
     -  'Solute-solvent radial distribution functions      ',
     -  'Solute-solvent pair energy distribution functions ',
     -  'Mean solvent orientation as a function of R       ',
     -  'Distribution of first shell solvent orientations  ',
     -  'Solvent-solvent radial distribution functions     '/
      data datacol /
     -  'R       ','gpx(R)  ','Kpx(R)  ','gt(R)   ','Kt(R)   ',
     -  'Ep      ','xpx(Ep) ','Kpx(Ep) ','        ','        ',
     -  'R       ','TH(R)   ','        ','        ','        ',
     -  'TH      ','xpx(TH) ','        ','        ','        ',
     -  'R       ','gv1px(R)','gv2px(R)','gv3px(R)','gv4px(R)'/
      data i180d /0,0,0,0,0, 0,0,0,0,0, 0,1,0,0,0, 1,0,0,0,0,
     -            0,0,0,0,0/
      data inoz /0,0,0,0,0, 0,0,0,0,0, 0,1,0,0,0, 0,0,0,0,0,
     -            0,0,0,0,0/
      data ndatcol /5,3,2,2,5/
      data lableny2 /0/,ntick /0/,iend /0/,xtic /0.0/,b2 /0.0/,b3 /0.0/
c     INITIALIZE EVERYTHING: variables, I/O files, graphs
      greekfont =  "/Symbol   "
      normalfont = "/Helvetica" 
      do i =1, 100
       emptystring(i:i) = ' '
       outputfile(i:i) = ' '
       filename(i:i) = ' '
      end do
      do i =1, 200
       largeemptystring(i:i) = ' '
       titlestring(i:i) = ' '
       do j = 1, 20
         tmpstring(j)(i:i) = ' '
       end do
      end do
      write(6,*)
c     Get file name root
      write(6,1000) version
      read (5,2000) filename
      ic=lenchar(filename,1)
      inpfile(1:ic)=filename(1:ic)
      leninp=ic+4
      inpfile(ic+1:leninp)='.pxp'
      call openfile(20,'Root  ','OLD',inpfile,leninp)
      rewind 20
c     Read header information on .pxi file
      read (20,*) nsltgr
      read (20,1011) (ian(i),i=1,nsltgr)
      read (20,1012) (labslt(i),i=1,nsltgr)
      rewind 20
      print *,'Number of proximity regions analyzed=',nsltgr
c     Get plot title
      write(6,1001)
      read(5,2000) titlestring
      call askyn('Do you want grid lines drawn',28,1,-1,igridlines)
      if (igridlines .eq. 1)  print *,
     -  'Drawing grid lines excludes the possibility of a second plot'
c     Get plot type
1     call gettypes(datanames,datacol,ndatcol,1,5,idatatyp,idatacol)
      i2ndplot=0
      idatatyp2=0
      if (igridlines .eq. 0 .and. idatatyp .lt. 5 .and. idatatyp .ne. 3)
     -    then
        call askyn('Do you want a second function also plotted',42,
     -    1,+1,i2ndplot)
        if (i2ndplot .eq. 1) then
2         call gettypes(datanames,datacol,ndatcol,idatatyp,5,
     -      idatatyp2,idatacol2)
          if (datacol(1,idatatyp) .ne. datacol(1,idatatyp2)) then
            print *,'Can not plot these two functions together'
            go to 2
          end if
        end if
      end if
      if (i2ndplot .eq. 0) write (6,1014) datacol(idatacol,idatatyp),
     -  datacol(1,idatatyp)
      if (i2ndplot .eq. 1) write (6,1015) datacol(idatacol,idatatyp),
     -  datacol(idatacol2,idatatyp2),datacol(1,idatatyp)
c     Get plot list
      do i=1,nsltgr
        ix(i)=0
      end do
      ndupmax=0
      call askyn('Do you to give a list of proximity regions to plot',
     -  50,1,-1,listinp)
      if (listinp .eq. 1) then
        call getint('Number of proximity regions to plot',35,nsltgr,1,
     -    nsltgr,numgr)
        do i=1,numgr
          write (line,1003) i
          call getint(line,37,nsltgr,1,999999,ix(i))
          write (6,1002) labslt(ix(i))
        end do
      else
        call getint('First proximity region',22,1,1,nsltgr,ifx)
        call getint('Last proximity region',21,1,1,nsltgr,ilx)
        do i=ifx,ilx
c         Find out if data segment exists
          do while (line(ict(1,idatatyp):ict(2,idatatyp)) .ne. 
     -      datatypes(idatatyp) .or. is .ne. i)
              read (20,2000,end=300) line
              if (line(ict(1,idatatyp):ict(2,idatatyp)) .eq. 
     -            datatypes(idatatyp)) then
                    read (line(5:8),2001) is
                    if (idatatyp .ne. 5) then
                      read (line(26:29),2001) ndupis
                      if (ndupmax .lt. ndupis) ndupmax=ndupis
                    end if
              end if
          end do
c         print *,'FOUND:',li
          ix(i-ifx+1)=1
          go to 301
300       write (6,1008) datatypes(idatatyp),i
          rewind 20
301       continue
        end do
        if (i2ndplot .eq. 1 .and. idatatyp .ne. idatatyp2) then
          do i=ifx,ilx
c           Find out if data segment exists
            do while (line(ict(1,idatatyp):ict(2,idatatyp)) .ne. 
     -        datatypes(idatatyp) .or. is .ne. i)
                read (20,2000,end=310) line
                if (line(ict(1,idatatyp):ict(2,idatatyp)) .eq. 
     -              datatypes(idatatyp)) then
                      read (line(5:8),2001) is
                      if (idatatyp2 .ne. 5) then
                        read (line(26:29),2001) ndupis
                        if (ndupmax .lt. ndupis) ndupmax=ndupis
                      end if
                end if
            end do
c        print *,'FOUND:',line(ict(1,idatatyp2):ict(2,idatatyp2)),' ',is
            ix(i-ifx+1)=1
            go to 311
310         write (6,1008) datatypes(idatatyp2),i
            rewind 20
311         continue
          end do
        end if
        numgr=0
        do i=ifx,ilx
          if (ix(i-ifx+1) .eq. 1) then
            numgr=numgr+1
            ix(numgr)=i
          end if
        end do
        if (numgr .eq. 0) go to 900
      end if
      ivalfac=0
      ivalfac2=0
      if (ndupmax .eq. 1) then
        print *,'No multiplicity information on ',inpfile(1:leninp)
      else if (ndupmax .gt. 1) then
        if (datacol(idatacol,idatatyp)(1:1) .eq. 'K') then
          write (6,1009) datacol(idatacol,idatatyp)
          read (5,1100) current
          if (current .eq. 'y' .or. current .eq. 'Y') ivalfac=1
        end if
        if (i2ndplot .gt. 0) then
          if (datacol(idatacol2,idatatyp2)(1:1) .eq. 'K') then
            write (6,1009) datacol(idatacol2,idatatyp2)
            read (5,1100) current
            if (current .eq. 'y' .or. current .eq. 'Y') ivalfac2=1
          end if
        end if
      end if
      rewind 20
c     Read header information on .pxi file
      read (20,*) nsltgr
      read (20,1011) (ian(i),i=1,nsltgr)
      read (20,1012) (labslt(i),i=1,nsltgr)
      nloops=(numgr-1)/8+1
      igrf=1
      do iloops=1,nloops
        if (iloops .eq. nloops) then
          igrl=numgr
        else 
          igrl=igrf+7
        end if
        ifx=ix(igrf)
        ilx=ix(igrl)
        lenout=leninp-4
        outputfile(1:lenout)=filename(1:lenout)
        call addchar(outputfile,lenout,'_')
        call addnum(outputfile,lenout,ifx)
        call addchar(outputfile,lenout,'_')
        call addnum(outputfile,lenout,ilx)
        call addchar(outputfile,lenout,'_')
c       lenout=lenout+3
c       outputfile(lenout-2:lenout)=datatypes(idatatyp)
c       call addchar(outputfile,lenout,'_')
        ic=1
        do while (datacol(idatacol,idatatyp)(ic:ic) .ne. '(' .and.
     -            datacol(idatacol,idatatyp)(ic:ic) .ne. ' ')
          if (datacol(idatacol,idatatyp)(ic:ic) .ne. '@') then
            call addchar(outputfile,lenout,
     -        datacol(idatacol,idatatyp)(ic:ic))
            ic=ic+1
          end if
        end do
        if (i2ndplot .gt. 0) then
          call addchar(outputfile,lenout,'_')
          ic=1
          do while (datacol(idatacol2,idatatyp2)(ic:ic) .ne. '(' .and.
     -              datacol(idatacol2,idatatyp2)(ic:ic) .ne. ' ')
            if (datacol(idatacol2,idatatyp2)(ic:ic) .ne. '@') then
              call addchar(outputfile,lenout,
     -          datacol(idatacol2,idatatyp2)(ic:ic))
              ic=ic+1
            end if
          end do
        end if
        lenout=lenout+3
        outputfile(lenout-2:lenout)='.ps'
        call openfile(10,'PS    ','NEW',outputfile,lenout)
c output PostScript initializing code
       write(10,21) 
21     format("%!PS-Adobe-2.0")
       write(10,22) 
22     format("%%Title: Dan's Dynamics PostScript Script")
       write(10,23) 
23     format("%%Creator: ")
       call fdate(datestring)
       write(10,24) datestring
24     format("%%CreationDate: ",a24)
       write(10,25) 
25     format("%%Pages: 1")
       write(10,26) 
26     format("%%BoundingBox: 0 0 612 792")
       call getlog(username)
       write(10,27) username 
27     format("%%For: ",a8)
       write(10,28) 
28     format("%% Copyright Apple Computer, Inc. ",
     * "1989-92 All Rights Reserved")
       write(10,29) 
29     format("%%EndComments")

c initialize abbreviated definitions

       write(10,1100) "/RM { rmoveto } def"
       write(10,1100) "/RL { rlineto } def"
       write(10,1100) "/SLW { setlinewidth } def"
       write(10,1100) "/M { moveto } def"
       write(10,1100) "/SW { stringwidth } def"
       write(10,1100) "/SD { setdash } def"
       write(10,1100) "/FF { findfont } def"
       write(10,1100) "/SC { scalefont } def"
       write(10,1100) "/SF { setfont } def"
       write(10,1100) "/SHW { show } def"
       write(10,1100) "/SK { stroke } def"
       write(10,1100) "/NP { newpath } def"
      do igrphnum = igrf,igrl
       grphnum=igrphnum-igrf+1
       indgraphstring(1:200) = largeemptystring(1:200)
       indgraphstring(1:1)='#'
       write (indgraphstring(2:5),2001) ix(igrphnum)
       indgraphstring(7:14)=labslt(ix(igrphnum))
       line=largeemptystring(1:80)
c      Find data segment
       is=0
c      print *,'Searching dt=',datatypes(idatatyp),' is=',ix(igrphnum)
       do while (line(ict(1,idatatyp):ict(2,idatatyp)) .ne. 
     -   datatypes(idatatyp) .or. is .ne. ix(igrphnum))
           read (20,2000,end=200) line
           if (line(ict(1,idatatyp):ict(2,idatatyp)) .eq. 
     -         datatypes(idatatyp)) read (line(5:8),2001) is
       end do
c      print *,'FOUND:',line(ict(1,idatatyp):ict(2,idatatyp)),' ',is
       go to 202
200    write (6,1008) datatypes(idatatyp),ix(igrphnum)
       rewind 20
       go to 203
202    read (line(18:20),2002) number
       if (idatatyp .ne. 5) then
         read (line(26:29),2001) ndupis
         ndtc=ndatcol(idatatyp)
       else
         read (line(31:32),2003) ndtc
       end if
c      print *,'number=',number
c Central data input section
       do ig=1,number
         read(20,*,end=999) (dataline(i),i=1,ndtc)
         time(ig)=dataline(1)
         value(ig)=dataline(idatacol)
         if (ivalfac .eq. 1) value(ig)=value(ig)*ndupis
         if (idatatyp .eq. idatatyp2) then
           time2(ig)=dataline(1)
           value2(ig)=dataline(idatacol2)
           if (ivalfac2 .eq. 1) value2(ig)=value2(ig)*ndupis
         end if
c        print *,ig,value(ig)
       end do
       if (idatatyp .eq. idatatyp2) number2=number

       if (i2ndplot .eq. 1 .and. idatatyp .ne. idatatyp2) then
c        Find 2nd data segment
         is=0
c        print *,'Searching dt=',datatypes(idatatyp2),' is=',ix(igrphnum)
         do while (line(ict(1,idatatyp2):ict(2,idatatyp2)) .ne. 
     -     datatypes(idatatyp2) .or. is .ne. ix(igrphnum))
             read (20,2000,end=210) line
             if (line(ict(1,idatatyp2):ict(2,idatatyp2)) .eq. 
     -           datatypes(idatatyp2)) read (line(5:8),2001) is
         end do
c        print *,'FOUND:',line(ict(1,idatatyp2):ict(2,idatatyp2)),' ',is
         go to 212
210      write (6,1008) datatypes(idatatyp2),ix(igrphnum)
         rewind 20
         go to 203
212      read (line(18:20),2002) number2
         if (idatatyp2 .ne. 5) then
           read (line(26:29),2001) ndupis
           ndtc=ndatcol(idatatyp2)
         else
           read (line(31:32),2003) ndtc
         end if
c        print *,'number2=',number2
c Central data input section
         do ig=1,number2
          read(20,*,end=999) (dataline(i),i=1,ndtc)
          time2(ig)=dataline(1)
          value2(ig)=dataline(idatacol2)
          if (ivalfac2 .eq. 1) value2(ig)=value2(ig)*ndupis
c         print *,ig,value2(ig)
        end do
         rewind 20
      end if
       maxx=arrmax(time,number)
       minx=arrmin(time,number)
       maxy=arrmax(value,number)
       miny=arrmin(value,number)
       if (i2ndplot .gt. 0) then
         maxx2=arrmax(time2,number2)
         minx2=arrmin(time2,number2)
         maxy2=arrmax(value2,number2)
         miny2=arrmin(value2,number2)
       end if
c       print *,'minx,maxx=',minx,maxx
c       print *,'miny,maxy=',miny,maxy
c output PostScript for drawing box representing graph boundary

       write(10,*) 
       write(10,1100) "% Drawing of graph boundaries" 
       write(10,30) 
30     format("NP")
       write(10,'(f5.1,1x,f5.1,a7)') 
     *       graphs(grphnum,1),graphs(grphnum,2)," M"
       write(10,*) "0 108 RL"
       write(10,*) "153 0 RL"
       write(10,*) "0 -108 RL"
       write(10,*) "closepath"
       write(10,*) "0.1 SLW"
       write(10,31) 
31     format("SK")

c establish axis lengths; if not a 'nice' number (i.e. not modulo 
c    of a power of 10 two smaller than the log of the axis), 
c    add a deltax to make it nice
c    deltax are then zeroed if they are added at end of graph


c establish y-axis length by extending axis to nearest round numbers
c  at both ends. Then, double check to make sure total length is round number

       call gettics(value,number,y0,ydist,ytic,numytic,
     -  i180d(idatacol,idatatyp))
       if (i2ndplot .eq. 1)  then
          call gettics(value2,number2,y02,ydist2,ytic2,numytic2,
     -  i180d(idatacol2,idatatyp2))
       else
         ytic2=ytic
         numytic2=numytic
         ydist2=ydist
       end if
 
c output PostScript for drawing ticks on X axis
c  A) establish spacing for ticks and starting point for ticks

       if (abs(minx/maxx) .lt. 0.3) minx=0.0
       if (i180d(1,idatatyp) .eq. 1) then
c        Special treatment for degree scale
         ntic=6
         xtick=3.0
         xticst=0.0
         maxx=180.0
         coorx=maxx
       else
         icoorx=maxx-minx+0.01
         if (icoorx .le. 5) then
           xtic=1
         else if (icoorx .le. 10) then
           xtic=2
         else if (icoorx .le. 15) then
           xtic=3
         else if (icoorx .le. 20) then
         xtic=4
         else if (icoorx .le. 40) then
           xtic=8
         else if (icoorx .le. 50) then
           xtic=10
         else if (icoorx .le. 100) then
           xtic=20
         else if (icoorx .le. 200) then
           xtic=40
         end if
         ntick=5
         xticst=0.0 
         xticst0=minx
         maxx=minx+ntick*xtic
         coorx=maxx-minx
       end if
       xdist = 153.0/coorx
c       print *,'xtic,xdist=',xtic,xdist

c  B) as we place each tick, also write the grid pattern and place a label 
       do i = 0, ntick
c    1) write major ticks
         write(10,*)
         write(10,1100) "% Drawing of X-axis major tick"
         write(10,30) 
         write(10,'(f7.2,1x,f7.2," M")') 
     *    graphs(grphnum,1)+(xticst+i*xtic)*xdist,
     *     graphs(grphnum,2) 
         write(10,*) "0 4.5 RL"
         write(10,*) "0.1 SLW"
         write(10,31) 
         write(10,30) 
         write(10,'(f7.2,1x,f7.2," M")') 
     *    graphs(grphnum,1)+(xticst+i*xtic)*xdist,
     *    (graphs(grphnum,2) + 108.0) 
         write(10,*) "0 -4.5 RL"
         write(10,*) "0.1 SLW"
         write(10,31) 

c    2) write grid pattern
         if (igridlines .eq. 1) then
           write(10,*)
           write(10,1100) "% Drawing of X-axis grid line"
           write(10,30) 
           write(10,'(f7.2,1x,f7.2," M")')
     *      graphs(grphnum,1)+(xticst+i*xtic)*xdist,
     *       graphs(grphnum,2)
           write(10,*) "0 108 RL"
           write(10,*) "[0.5 2] 0 SD"
           write(10,*) "0.1 SLW"
           write(10,31) 
           write(10,*) "[] 0 SD"
         end if

c    3) write X-axis tick labels
         write(10,*)
         write(10,1100) "% Drawing of X-axis tick label"
         write(10,30)
         write(10,*) normalfont," FF"
cxx      write(10,*) "6 SC"
         write(10,*) "9 SC"
         write(10,*) "SF"
         label = xticst0 + (xtic * i)
         if (abs(label) .lt. 10.0) then
           lablen=1
         else if (abs(label) .lt. 100.0) then
           lablen=2
         else
           lablen=3
         end if
         if (label .lt. 0.0) lablen=lablen+1
         ilablen=lablen
         call writeintgtxt(10,ilablen,lablen,"SW pop neg",10)
         write(10,*) "2.0 div ",
     *     graphs(grphnum,1)+(xticst+i*xtic)*xdist,
     *     " add"
cxx      write(10,'(f7.2," M")') (graphs(grphnum,2)-7)
         write(10,'(f7.2," M")') (graphs(grphnum,2)-10)
         k=1
         lablen = lablen + 2
         call writerealtxt(10,label,lablen,k,"SHW",3)

c     4) write X-axis minor ticks
c       a) write minor ticks before first major tick and 
c            after last major tick (if necessary)

         xticminor = xtic / 4.0 
         if ((i.eq.0.and.xticst.gt.xticminor).or.
     *    (i.eq.4.and.(coorx-(4.*xtic)-xticst.gt.xticminor))) then
          if (i.eq.0) then
            k1 = (xticst/xticminor) * 1
            b2 = graphs(grphnum,1) + xticst 
            b3 = -1.
          elseif (i.eq.4) then
            k1 = ((coorx-4.*xtic-xticst)/xticminor) * 1
cd            if (((k1*xticminor)+(4.*xtic)+xticst).gt.coorx) k1 = k1 - 1
            b2 = graphs(grphnum,1) + 
     *            (xticst+i*xtic)*xdist
            b3 = 1.
          endif
          do k = 1, k1
           write(10,*)
           write(10,1100) "% Drawing of X-axis minor ticks"
           write(10,30)
           write(10,'(f7.2,1x,f7.2," M")') 
     *             (b2 + (k*xticminor*xdist*b3)), graphs(grphnum,2)
           write(10,*) "0 2.25 RL"
           write(10,*) "0.1 SLW"
           write(10,31)
           write(10,30)
           write(10,'(f7.2,1x,f7.2," M")')
     *             (b2 + (k*xticminor*xdist*b3)), 
     *             (graphs(grphnum,2)+108.0)
           write(10,*) "0 -2.25 RL"
           write(10,*) "0.1 SLW"
           write(10,31)
          end do
         endif
          
c       b) write X-axis minor ticks for interior of graph 
         if (i.ge.0.and.i.lt.4) then
          k1 = 3
          b2 = graphs(grphnum,1) +
     *            ((xticst+i*xtic)*xdist)
          do k = 1, k1
           write(10,*)
           write(10,1100) "% Drawing of X-axis minor ticks"
           write(10,30)
           write(10,'(f7.2,1x,f7.2," M")')
     *             (b2 + (k*xticminor*xdist)), graphs(grphnum,2)
           write(10,*) "0 2.25 RL"
           write(10,*) "0.1 SLW"
           write(10,31)
           write(10,30)
           write(10,'(f7.2,1x,f7.2," M")')
     *             (b2 + (k*xticminor*xdist)), (graphs(grphnum,2)+108.0)
           write(10,*) "0 -2.25 RL"
           write(10,*) "0.1 SLW"
           write(10,31)
          end do
         endif

       end do

c      print *,'Xtics done'
c output PostScript for drawing ticks on Y axis
c  A) establish spacing for as many ticks as necessary
c     no 'yticst' variable necessary since the axes were made 'nice'


c  B) as we place each tick, also write the grid pattern and place a label 
c      print *,'y0,numytic,ytic,ydist=',y0,numytic,ytic,ydist
       do i = 0, numytic
c    1) write major ticks
         write(10,*)
         write(10,1100) "% Drawing of Y-axis major tick"
         write(10,30) 
         write(10,'(f7.2,1x,f7.2," M")') graphs(grphnum,1),
     *       graphs(grphnum,2) + (i*ytic*ydist)
         write(10,*) "4.5 0 RL"
         write(10,*) "0.1 SLW"
         write(10,31) 
      end do
      do i = 0, numytic2
         write(10,30) 
         write(10,'(f7.2,1x,f7.2," M")') 
     *      (graphs(grphnum,1) + 153.0),
     *      (graphs(grphnum,2) + (i*ytic2*ydist2))
         write(10,*) "-4.5 0 RL"
         write(10,*) "0.1 SLW"
         write(10,31) 
      end do

c    2) write grid pattern
       if (igridlines .eq. 1) then
         do i = 0, numytic
           write(10,*)
           write(10,1100) "% Drawing of Y-axis grid line"
           write(10,30) 
           write(10,'(f7.2,1x,f7.2," M")') graphs(grphnum,1),
     *         graphs(grphnum,2) + (i*ytic*ydist)
           write(10,*) "153 0 RL"
           write(10,*) "[0.5 2] 0 SD"
           write(10,*) "0.1 SLW"
           write(10,31) 
           write(10,*) "[] 0 SD"
         end do
       end if

c    3) write Y-axis tick labels
         lableny=0
         call findfraclen(y0,lenfrac0)
         call findfraclen(ytic,lenfrac1)
         lenfrac=max0(lenfrac0,lenfrac1)
         call findlen(y0,lablen0,lenfrac)
         call findlen(y0+numytic*ytic,lablen1,lenfrac)
         lablen=max0(lablen0,lablen1)
         lableny=lablen
         do i = 0, numytic
           label = (ytic*i) + y0
           write(10,*)
           write(10,1100) "% Drawing of Y-axis tick label"
           write(10,30)
           write(10,*) normalfont," FF"
cxx        write(10,*) "6 SC"
           write(10,*) "9 SC"
           write(10,*) "SF"
           write(10,'(f7.2,1x,f7.2," M")') 
cxx  -       graphs(grphnum,1)-4.0*lableny,
     -       graphs(grphnum,1)-6.0*lableny,
     -       graphs(grphnum,2)+i*ytic*ydist
           if (lenfrac.eq.0) then
             k1 = label
             call writeintgtxt(10,k1,lablen,"SHW",3)
           else
             call writerealtxt(10,label,lablen,lenfrac,"SHW",3)
           endif
c          print *,'i,ytic,y0=',i,ytic,y0
         end do

         if (i2ndplot .eq. 1) then
           lentick=0
           call findfraclen(y02,lenfrac0)
           call findfraclen(ytic2,lenfrac1)
           lenfrac=max0(lenfrac0,lenfrac1)
           call findlen(y02,lablen0,lenfrac)
           call findlen(y02+numytic2*ytic2,lablen1,lenfrac)
           lablen=max0(lablen0,lablen1)
           lableny2=lablen
           do i = 0, numytic2
             label = (ytic2 * i ) + y02
             write(10,*)
             write(10,1100) "% Drawing of Y-axis tick label"
             write(10,30)
             write(10,*) normalfont," FF"
cxx          write(10,*) "6 SC"
             write(10,*) "9 SC"
             write(10,*) "SF"
             write(10,'(f7.2,1x,f7.2," M")') 
     -         graphs(grphnum,1)+153.0+3.0,
     -         graphs(grphnum,2)+i*ytic2*ydist2
             if (lenfrac.eq.0) then
                k1 = label
                call writeintgtxt(10,k1,lablen,"SHW",3)
             else
               call writerealtxt(10,label,lablen,lenfrac,"SHW",3)
             endif
c            print *,'i,ytic,y0=',i,ytic2,y02
            end do
          end if

c     4) write Y-axis minor ticks for interior of graph 
       do i = 0, numytic-1
         yticminor = ytic / 5.0
            k1 = 4
            b2 = graphs(grphnum,2) + (i*ytic*ydist)
            do k = 1, k1
              write(10,*)
              write(10,1100) "% Drawing of Y-axis minor ticks"
              write(10,30)
              write(10,'(f7.2,1x,f7.2," M")') graphs(grphnum,1),
     *              (b2 + (k*yticminor*ydist))
              write(10,*) "2.25 0 RL"
              write(10,*) "0.1 SLW"
              write(10,31)
            end do
       end do
   
       do i = 0, numytic2-1
         yticminor = ytic2 / 5.0
            k1 = 4
            b2 = graphs(grphnum,2) + (i*ytic2*ydist2)
            do k = 1, k1
              write(10,*)
              write(10,1100) "% Drawing of Y-axis minor ticks"
              write(10,30)
              write(10,'(f7.2,1x,f7.2," M")') 
     *             (graphs(grphnum,1) + 153.0),
     *             (b2 + (k*yticminor*ydist2))
              write(10,*) "-2.25 0 RL"
              write(10,*) "0.1 SLW"
              write(10,31)
            end do
       end do

      call drawcurve(time,value,number,inoz(idatacol,idatatyp),
     -  xticst0,graphs(grphnum,1),graphs(grphnum,2),xdist,ydist,y0,1)

      if (i2ndplot .gt. 0) 
     -  call drawcurve(time,value2,number,inoz(idatacol2,idatatyp2),
     -    xticst0,graphs(grphnum,1),graphs(grphnum,2),xdist,ydist2,y02,
     -    2)

c output PostScript for X-axis and Y-axis labels
       write(10,*)
       write(10,1100) "% Drawing of X-axis label"
       write(10,30)
       write(10,*) normalfont," FF"
cxx    write(10,*) "6 SC"
       write(10,*) "9 SC"
       write(10,*) "SF"
       write(10,1101) datacol(1,idatatyp)," SW pop neg"
       write(10,'(a18,f7.2,a5,f7.2,a2)') "153.0 add 2.0 div ", 
cxx  *         graphs(grphnum,1)," add ",(graphs(grphnum,2)-14.), " M"
     *         graphs(grphnum,1)," add ",(graphs(grphnum,2)-19.), " M"
       write(10,1101) datacol(1,idatatyp)," SHW"
       write(10,1100) "% Drawing of Y-axis label"
       write(10,30)
       write(10,*) normalfont," FF"
cxx    write(10,*) "6 SC"
       write(10,*) "9 SC"
       write(10,*) "SF"
       write(10,1101) datacol(idatacol,idatatyp)," SW pop neg"
cxx    xyl=graphs(grphnum,1)-lableny*4.0-3.0
       xyl=graphs(grphnum,1)-lableny*6.0-4.0
       yyl=graphs(grphnum,2)+108/2.0
       write (10,'(2f8.2,a2)') xyl,yyl,' M'
       write(10,*) "90 rotate"
       write(10,1101) datacol(idatacol,idatatyp)," SHW"
       write(10,*) "-90 rotate"
       if (i2ndplot .gt. 0) then
         write(10,1100) "% Drawing of Y-axis label"
         write(10,30)
         write(10,*) normalfont," FF"
cxx      write(10,*) "6 SC"
         write(10,*) "9 SC"
         write(10,*) "SF"
         write(10,1101) datacol(idatacol2,idatatyp2)," SW pop neg"
cxx      xyl2=graphs(grphnum,1)+153.0+7.0+lableny2*4.0 
         xyl2=graphs(grphnum,1)+153.0+8.0+lableny2*6.0 
         yyl2=graphs(grphnum,2)+108/2.0
         write (10,'(2f8.2,a2)')  xyl2,yyl2,' M'
         write(10,*) "90 rotate"
         write(10,1101) datacol(idatacol2,idatatyp2)," SHW"
         write(10,*) "-90 rotate"
c        Write sample lines
         write(10,30)
         write (10,*) '[] 0 SD'
         write (10,'(2f8.2,a2)') xyl,yyl-30.0,' M'
         write (10,'(2f8.2,a3)') 0.0,20.0,' RL'
         write(10,31)
         write(10,30)
         write (10,*) '[2] 1 SD'
         write (10,'(2f8.2,a2)') xyl2,yyl2-30.0,' M'
         write (10,'(2f8.2,a3)') 0.0,20.0,' RL'
         write(10,31)
         write (10,*) '[] 0 SD'
       end if

c output PostScript for graph title
       if (indgraphstring.ne.largeemptystring) then
         numstr = 1
         write(10,*)
         write(10,1100) "% Drawing of graph title"
         write(10,30)
         do i = 1, 20
           fsize(i) = -1.0
           fname(i) = "----------"
           tmpstring(i) = largeemptystring
           super(i) = 0
           sub(i) = 0
         end do
cxx      fsize(1) = 10.
         fsize(1) = 15.
         sizecurrent = fsize(1)
         fname(1) = normalfont
         fontcurrent = fname(1)
c scan string length
         iend=14
c scan number of lines indicated by string
         iline = 1
         do i = 1, iend-1
           if (indgraphstring(i:i+1).eq.'@m') iline = iline + 1
         end do
         k = 1
         skip = 0.0
c scan for special things (i.e font changes, size changes)
c  when done or encounter newline indicator, calculate total string width
c  and print string
         do i = 1, iend
           current = indgraphstring(i:i)
           if (current.ne.'@'.and.skip.eq.0.) then
                 tmpstring(numstr)(k:k) = current
                 k = k + 1
           elseif (current.eq.'@'.and.skip.eq.0.) then
c if '@' symbol found, peek at next position
             if (indgraphstring(i+1:i+1).eq.'@') then
               tmpstring(numstr)(k:k) = '@'
               k = k + 1
               skip = 1.0
             elseif (indgraphstring(i+1:i+1).eq.'f') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fname(numstr) = greekfont 
                fontcurrent = fname(numstr) 
                skip = 1.0
                k = 1
             elseif (indgraphstring(i+1:i+1).eq.'n') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fname(numstr) = normalfont
                fontcurrent = fname(numstr)
                skip = 1.0
                k = 1
             elseif (indgraphstring(i+1:i+1).eq.'+') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fsize(numstr) = sizecurrent * 0.75
                sizecurrent = fsize(numstr)
                super(numstr) = 1
                skip = 1.0
                k = 1
             elseif (indgraphstring(i+1:i+1).eq.'-') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fsize(numstr) = sizecurrent * 0.75
                sizecurrent = fsize(numstr)
                sub(numstr) = 1.0
                skip = 1.0
                k = 1
             elseif (indgraphstring(i+1:i+1).eq.'m'.and.iline.gt.1) then
                do i1 = 1, numstr
                  if (tmpstring(i1).eq.largeemptystring
     *                                .and.i1.lt.numstr) then
                    fsize(i1) = fsize(i1+1)
                    fname(i1) = fname(i1+1)
                    super(i1) = super(i1+1)
                    sub(i1) = sub(i1+1)
                    if (numstr.eq.1) then
                      fname(i1) = fontcurrent
                      fsize(i1) = sizecurrent
                    endif
                    tmpstring(i1)(1:200) = tmpstring(i1+1)(1:200)
                  elseif (tmpstring(i1).eq.largeemptystring
     *                                     .and.i1.eq.numstr) then
                    numstr = numstr - 1
                  endif
                  if (fsize(i1).eq.-1.0.and.i.gt.1) 
     *                                  fsize(i1) = fsize(i1-1)
                  if (fname(i1).eq."----------".and.i.gt.1) 
     *                                  fname(i) = fname(i1-1)
cxx               if (i.eq.1.and.fsize(i1).eq.-1.0) fsize(i1)=10.0
                  if (i.eq.1.and.fsize(i1).eq.-1.0) fsize(i1)=15.0
                  if (i.eq.1.and.fname(i1).eq."----------") 
     *                                  fname(i1) = normalfont
                end do
                write(10,*) "clear"
                fmaxsize = 0.0
                do i1 = 1, numstr
                  write(10,*) fname(i1)," FF"
                  write(10,*) fsize(i1)," SC"
                  if (fmaxsize.lt.fsize(i1)) fmaxsize = fsize(i1)
                  write(10,*) "SF"
                  do j = 200, 1, -1
                   if (tmpstring(i1)(j:j).ne.' ') goto 107
                  end do
107               continue
                  write(10,*) "(",tmpstring(i1)(1:j),") SW pop"
                end do
                do i1 = 1, (numstr-1)
                  write(10,*) "add"
                end do
                write(10,'(a23,f7.2,a4)') "153.0 exch sub 2.0 div ",
     *                graphs(grphnum,1)," add"
                write(10,'(f5.1,a1,f5.1,a15,f7.2,a5,f6.2,a6)')
     *            fmaxsize," ",((iline-1)*0.75)," mul 108.0 add ",
     *            graphs(grphnum,2)," add ",fmaxsize*0.3*(iline*1.),
     *            " add M"
                do i1 = 1, numstr
                  write(10,*) fname(i1)," FF"
                  write(10,*) fsize(i1)," SC"
                  write(10,*) "SF"
                  if (super(i1).eq.1) write(10,*) 
     *                             "0 ",fsize(i1)*0.5," RM"
                  if (sub(i1).eq.1) write(10,*) 
     *                             "0 ",fsize(i1)*0.5*-1.0," RM"
                  do j = 200, 1, -1
                   if (tmpstring(i1)(j:j).ne.' ') goto 109
                  end do
109               continue
                  write(10,*) "(",tmpstring(i1)(1:j),") SHW"
                end do
                iline = iline - 1
                fname(1) = fontcurrent
                fsize(1) = sizecurrent
                tmpstring(1) = largeemptystring
                do j = 2, 20
                  fsize(j) = -1.0
                  fname(j) = "----------"
                  tmpstring(j) = largeemptystring
                end do
                numstr = 1
                k = 1
                skip = 1.0
             elseif (indgraphstring(i+1:i+1).eq.'#') then
                if (tmpstring(numstr).ne.largeemptystring) 
     *                                       numstr = numstr + 1
                k1 = i + 2
102             continue
                if (indgraphstring(k1:k1).ne.'#'.and.k1.le.iend) then
                   k1 = k1 + 1
                   goto 102
                else
                   read(indgraphstring(i+2:k1-1),*) fsize(numstr)
                endif
                sizecurrent = fsize(numstr)
                skip = k1 - i 
                k = 1
             endif       
          elseif (skip.ge.1.) then
             skip = skip - 1.0
          endif
         end do

         do i = 1, numstr
           if (tmpstring(i).eq.largeemptystring.and.i.lt.numstr) then
             fsize(i) = fsize(i+1)
             fname(i) = fname(i+1)
             if (numstr.eq.1) then
              fsize(i) = sizecurrent
              fname(i) = fontcurrent
             endif
             tmpstring(i)(1:200) = tmpstring(i+1)(1:200)
           elseif (tmpstring(i).eq.largeemptystring
     *                             .and.i.eq.numstr) then
             numstr = numstr - 1
           endif
           if (fsize(i).eq.-1.0.and.i.gt.1) fsize(i) = fsize(i-1)
           if (fname(i).eq."----------".and.i.gt.1) 
     *                                      fname(i) = fname(i-1)
         end do
         write(10,*) "clear"
         fmaxsize = 0.0
         do i = 1, numstr
           write(10,*) fname(i)," FF"
           write(10,*) fsize(i)," SC"
           if (fmaxsize.lt.fsize(i)) fmaxsize = fsize(i)
           write(10,*) "SF"
           do j = 200, 1, -1
            if (tmpstring(i)(j:j).ne.' ') goto 104
           end do
104        continue
           write(10,*) "(",tmpstring(i)(1:j),") SW pop"
         end do
         do i = 1, (numstr-1)
           write(10,*) "add"
         end do
         write(10,'(a23,f7.2,a5)') "153.0 exch sub 2.0 div ",
     *                     graphs(grphnum,1)," add"
         write(10,'(a6,f7.2,a5,f6.2,a6)') "108.0 ",graphs(grphnum,2),
     *            " add ",fmaxsize*0.3," add M"
         do i = 1, numstr
           write(10,*) fname(i)," FF"
           write(10,*) fsize(i)," SC"
           write(10,*) "SF"
           if (super(i).eq.1) write(10,'(a2,f6.2,a3)')
     *                        "0 ",fsize(i)*0.5," RM"
           if (sub(i).eq.1) write(10,'(a2,f6.2,a3)')
     *                        "0 ",fsize(i)*0.5*-1.," RM"
           do j = 200, 1, -1
            if (tmpstring(i)(j:j).ne.' ') goto 106
           end do
106        continue
           write(10,*) "(",tmpstring(i)(1:j),") SHW"
         end do
       endif
203    continue
      end do

      
c output PostScript for page title
       if (titlestring.ne.largeemptystring) then
         numstr = 1
         write(10,*)
         write(10,1100) "% Drawing of page title"
         write(10,30)
         do i = 1, 20
           fsize(i) = -1.0
           fname(i) = "----------"
           tmpstring(i) = largeemptystring
           super(i) = 0
           sub(i) = 0
         end do
cxx      fsize(1) = 14.
         fsize(1) = 21.
         sizecurrent = fsize(1)
         fname(1) = normalfont
         fontcurrent = fname(1)
c scan string length
         do i = 200, 1, -1 
           if (titlestring(i:i).ne.' ') then
             iend = i
             goto 111
           endif
         end do
111      continue
c scan number of lines indicated by string
         iline = 1
         do i = 1, iend-1
           if (titlestring(i:i+1).eq.'@m') iline = iline + 1
         end do
         k = 1
         skip = 0.0
c scan for special things (i.e font changes, size changes)
c  when done or encounter newline indicator, calculate total string width
c  and print string
         do i = 1, iend
           current = titlestring(i:i)
           if (current.ne.'@'.and.skip.eq.0.) then
                 tmpstring(numstr)(k:k) = current
                 k = k + 1
           elseif (current.eq.'@'.and.skip.eq.0.) then
c if '@' symbol found, peek at next position
             if (titlestring(i+1:i+1).eq.'@') then
               tmpstring(numstr)(k:k) = '@'
               k = k + 1
               skip = 1.0
             elseif (titlestring(i+1:i+1).eq.'f') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fname(numstr) = greekfont 
                fontcurrent = fname(numstr) 
                skip = 1.0
                k = 1
             elseif (titlestring(i+1:i+1).eq.'n') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fname(numstr) = normalfont
                fontcurrent = fname(numstr)
                skip = 1.0
                k = 1
             elseif (titlestring(i+1:i+1).eq.'+') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fsize(numstr) = sizecurrent * 0.75
                sizecurrent = fsize(numstr)
                super(numstr) = 1
                skip = 1.0
                k = 1
             elseif (titlestring(i+1:i+1).eq.'-') then
                if (tmpstring(numstr).ne.largeemptystring)
     *                                       numstr = numstr + 1
                fsize(numstr) = sizecurrent * 0.75
                sizecurrent = fsize(numstr)
                sub(numstr) = 1.0
                skip = 1.0
                k = 1
             elseif (titlestring(i+1:i+1).eq.'m'.and.iline.gt.1) then
                do i1 = 1, numstr
                  if (tmpstring(i1).eq.largeemptystring
     *                                .and.i1.lt.numstr) then
                    fsize(i1) = fsize(i1+1)
                    fname(i1) = fname(i1+1)
                    super(i1) = super(i1+1)
                    sub(i1) = sub(i1+1)
                    if (numstr.eq.1) then
                      fname(i1) = fontcurrent
                      fsize(i1) = sizecurrent
                    endif
                    tmpstring(i1)(1:200) = tmpstring(i1+1)(1:200)
                  elseif (tmpstring(i1).eq.largeemptystring
     *                                     .and.i1.eq.numstr) then
                    numstr = numstr - 1
                  endif
                  if (fsize(i1).eq.-1.0.and.i.gt.1) 
     *                                  fsize(i1) = fsize(i1-1)
                  if (fname(i1).eq."----------".and.i.gt.1) 
     *                                  fname(i) = fname(i1-1)
cxx               if (i.eq.1.and.fsize(i1).eq.-1.0) fsize(i1)=14.0
                  if (i.eq.1.and.fsize(i1).eq.-1.0) fsize(i1)=21.0
                  if (i.eq.1.and.fname(i1).eq."----------") 
     *                                  fname(i1) = normalfont
                end do
                write(10,*) "clear"
                fmaxsize = 0.0
                do i1 = 1, numstr
                  write(10,*) fname(i1)," FF"
                  write(10,*) fsize(i1)," SC"
                  if (fmaxsize.lt.fsize(i1)) fmaxsize = fsize(i1)
                  write(10,*) "SF"
                  do j = 200, 1, -1
                   if (tmpstring(i1)(j:j).ne.' ') goto 117
                  end do
117               continue
                  write(10,*) "(",tmpstring(i1)(1:j),") SW pop"
                end do
                do i1 = 1, (numstr-1)
                  write(10,*) "add"
                end do
                write(10,'(a23,f7.2,a4)') "409.5 exch sub 2.0 div ",
     *                topstart(1)," add"
                write(10,'(f5.1,a1,f5.2,a5,f7.2,a13,f6.2,a6)')
     *            fmaxsize," ",((iline-1)*0.75)," mul ",
     *            topstart(2)," add 27.0 add",fmaxsize*0.3*(iline*1.),
     *            " add M"
                do i1 = 1, numstr
                  write(10,*) fname(i1)," FF"
                  write(10,*) fsize(i1)," SC"
                  write(10,*) "SF"
                  if (super(i1).eq.1) write(10,'(a2,f6.2,a3)') 
     *                             "0 ",fsize(i1)*0.5," RM"
                  if (sub(i1).eq.1) write(10,'(a2,f6.2,a7)') 
     *                             "0 ",fsize(i1)*0.5," neg RM"
                  do j = 200, 1, -1
                   if (tmpstring(i1)(j:j).ne.' ') goto 119
                  end do
119               continue
                  write(10,*) "(",tmpstring(i1)(1:j),") SHW"
                end do
                iline = iline - 1
                fname(1) = fontcurrent
                fsize(1) = sizecurrent
                tmpstring(1) = largeemptystring
                do j = 2, 20
                  fsize(j) = -1.0
                  fname(j) = "----------"
                  tmpstring(j) = largeemptystring
                end do
                numstr = 1
                k = 1
                skip = 1.0
             elseif (titlestring(i+1:i+1).eq.'#') then
                if (tmpstring(numstr).ne.largeemptystring) 
     *                                       numstr = numstr + 1
                k1 = i + 2
112             continue
                if (titlestring(k1:k1).ne.'#'.and.k1.le.iend) then
                   k1 = k1 + 1
                   goto 112
                else
                   read(titlestring(i+2:k1-1),*) fsize(numstr)
                endif
                sizecurrent = fsize(numstr)
                skip = k1 - i 
                k = 1
             endif       
          elseif (skip.ge.1.) then
             skip = skip - 1.0
          endif
         end do

113      continue
         do i = 1, numstr
           if (tmpstring(i).eq.largeemptystring.and.i.lt.numstr) then
             fsize(i) = fsize(i+1)
             fname(i) = fname(i+1)
             if (numstr.eq.1) then
              fsize(i) = sizecurrent
              fname(i) = fontcurrent
             endif
             tmpstring(i)(1:200) = tmpstring(i+1)(1:200)
           elseif (tmpstring(i).eq.largeemptystring
     *                             .and.i.eq.numstr) then
             numstr = numstr - 1
           endif
           if (fsize(i).eq.-1.0.and.i.gt.1) fsize(i) = fsize(i-1)
           if (fname(i).eq."----------".and.i.gt.1) 
     *                                      fname(i) = fname(i-1)
         end do
         write(10,*) "clear"
         fmaxsize = 0.0
         do i = 1, numstr
           write(10,*) fname(i)," FF"
           write(10,*) fsize(i)," SC"
           if (fmaxsize.lt.fsize(i)) fmaxsize = fsize(i)
           write(10,*) "SF"
           do j = 200, 1, -1
            if (tmpstring(i)(j:j).ne.' ') goto 114
           end do
114        continue
           write(10,*) "(",tmpstring(i)(1:j),") SW pop"
         end do
         do i = 1, (numstr-1)
           write(10,*) "add"
         end do
         write(10,'(a23,f7.2,a4)') "409.5 exch sub 2.0 div ",
     *                                    topstart(1)," add"
         write(10,'(a5,f7.2,a5,f5.2,a6)') "27.0 ",topstart(2),
     *              " add ",fmaxsize*0.3," add M"
         do i = 1, numstr
           write(10,*) fname(i)," FF"
           write(10,*) fsize(i)," SC"
           write(10,*) "SF"
           if (super(i).eq.1) write(10,'(a2,f6.2,a3)')
     *                        "0 ",fsize(i)*0.5," RM"
           if (sub(i).eq.1) write(10,'(a2,f6.2,a6)')
     *                        "0 ",fsize(i)*0.5," neg RM"
           do j = 200, 1, -1
            if (tmpstring(i)(j:j).ne.' ') goto 116
           end do
116        continue
           write(10,*) "(",tmpstring(i)(1:j),") SHW"
         end do
       endif
      write(10,*)
      write(10,91) 
91    format("%%Trailer")
      write(10,92) 
92    format("showpage")
 
      close(unit=10)
      igrf=igrl+1
      end do
900   write (6,1007)
      read (5,1100) current
      if (current .eq. 'y' .or. current .eq. 'Y') then
        rewind 20
        go to 1
      end if
      stop
999   print *,'Run out of data prematurely on ',inpfile(1:leninp)
      stop
1000  format('Proximity plot generator Version:',a9,/,
     -  'Data file name root=',$)
1001  format('Use @ for: @f=Greek fnt, @n=Normal fnt, @+=superscript',/,
     -  ' @-=subscript, @m=newline, @#n#=font size n, @@=@symbol',/,
     -  'Title of graph page: ')
1002  format(' Reegion ',a,' selected')
1003  format('Proximity region',i3,' selected (8/page)=')
1007  format('More plots (y/n)? ',$)
1008  format('Data set not found (skipped): ',a3,' is=',i4)
1009  format('Do you want to multiply ',a,' with the number of atoms ',
     -  'contributing (y/n)? ',$)
1011  format(20i4)
1012  format(10a8)
1014  format('Plotting ',a,' vs ',a)
1015  format('Plotting ',a,' and ',a,' vs ',a)
1100  format(a)
1101  format('(',a8,')',a)
2000  format(a)
2001  format(i4)
2002  format(i3)
2003  format(i2)
      end
      subroutine openfile(iunit,prompt,mode,filename,namlen)
      character*(*) filename
      character*6 prompt
      character*3 mode
100   if (namlen .eq. 0) then 
        write (6,1000) prompt
        read (5,1001) filename 
        namlen=lenchar(filename,1)
      end if
      open(unit=iunit,status=mode,file=filename(1:namlen),
     -  iostat=iopen)
      if (iopen .ne. 0) then
        write (6,1002) filename(1:namlen)
        namlen=0
        go to 100
      end if
200   if (namlen .eq. 0) namlen=lenchar(filename,1)
      if (iopen .eq. 0) print *,'File ',filename(1:namlen),' opened'
      return
1000  format(1x,a6,' file name=',$)
1001  format(a)
1002  format(' Problem opening file ',a)
      end
      function lenchar(char,ic1)
      character*(*) char
      ic=ic1
      do while (char(ic:ic) .ne. ' ')
        ic=ic+1
      end do
      lenchar=ic-1
c     print *,'LENCHAR char=',char,'lenchar=',lenchar
      return
      end
      subroutine addchar(name,length,char)
      character*1 char
      character*(*) name
      length=length+1
      name(length:length)=char
      return
      end
      subroutine addnum(name,length,num)
      character*(*) name
      if (num .lt. 10) then
        write (name(length+1:length+1),1001) num
        length=length+1
      else if (num .lt. 100) then
        write (name(length+1:length+2),1002) num
        length=length+2
      else if (num .lt. 1000) then
        write (name(length+1:length+3),1003) num
        length=length+3
      else if (num .lt. 10000) then
        write (name(length+1:length+4),1004) num
        length=length+4
      end if
      return
1001  format(i1)
1002  format(i2)
1003  format(i3)
1004  format(i4)
      end
      subroutine gettics(y,n,y0,ydist,ytic,numytic,i180)
      dimension y(n)
      ymax=0.0
      do i=1,n
        if (ymax .lt. y(i)) ymax=y(i)
      end do
      if (i180 .eq. 1) then
c       Special treatment for scale in degrees
        yrange=180.0
        numytic=6
      else
        if (ymax .eq. 0.0) ymax=1.0
        k=log10(ymax)
        if (ymax .lt. 1) k=k-1
c       print *,'y,k=',ymax,k
        numytic=ymax/10.0**(k)+1
        yrange=numytic*10.0**(k) + 0.00001
      end if
      ytic=yrange/numytic
c     print *,'numytic,ytic,yrange=',numytic,ytic,yrange
      if (numytic .lt. 3) then
        numytic=numytic*4
        ytic=ytic/4.0
      else if (numytic .lt. 5) then
        numytic=numytic*2
        ytic=ytic/2.0
      end if
      ydist=108.0/yrange
      y0=0.0
      return
      end
      function arrmin(a,n)
      dimension a(n)
      arrmin=a(1)
      do i=2,n
        if (arrmin .gt. a(i)) arrmin=a(i)
      end do
      return
      end
      function arrmax(a,n)
      dimension a(n)
      arrmax=a(1)
      do i=2,n
        if (arrmax .lt. a(i)) arrmax=a(i)
      end do
      return
      end
      subroutine findfraclen(ytic,lenfrac)
c     Finds the number of digits needed for the fractional part on the Y axis
      lenfrac=0
      if (ytic .lt. 1.0) then
        lenfrac=4
        iytic=10**4*ytic
        irem=0
        do while (irem .eq. 0 .and. lenfrac .gt. 0)
          iytico=iytic
          iytic=iytic/10
          irem=iytico-10*iytic
          if (irem .eq. 0) lenfrac=lenfrac-1
        end do
      end if
      return
      end
      subroutine findlen(label,lablen,lenfrac)
      real label
      if (label .eq. 0.0) then
        lablen = 1
      else if (abs(label).gt.1.0) then
        lablen = log10(abs(label))+1
      else
        lablen=1
      end if
      if (label.lt.0.0) lablen = lablen + 1
      if (lenfrac .ne. 0) lablen = lablen + 1 + lenfrac
c??   if (lableny .lt. lablen) lableny=lablen
c      write (77,2311) grphnum,i,label,lablen,lenfrac
c2311  format(' g=',i2,' i=',i2,' lab=',f10.4,' len,frac=',2i3)
      return
      end
      subroutine gettypes(datanames,datacol,ndatcol,ityp0,ntyp,
     -  idatatyp,idatacol)
      character*50 datanames(ntyp)
      character*8 datacol(5,ntyp)
      integer ndatcol(ntyp)
1     write (6,1005) (i,datanames(i),i=ityp0,ntyp)
      write (6,1001)
      read (5,*) idatatyp
      if (idatatyp .lt. ityp0 .or. idatatyp .gt. ntyp) then
        print *,'Invalid type'
        goto 1
      end if
      if (ndatcol(idatatyp) .eq. 2) then
        idatacol=2
        write (6,1004) datacol(2,idatatyp)
      else
101     write (6,1006) (i,datacol(i,idatatyp),i=1,ndatcol(idatatyp))
        write (6,1002)
        read (5,*) idatacol
        if (idatacol .lt. 1 .or. idatacol .gt. ndatcol(idatatyp)) then
          print *,'Invalid column'
          goto 101
        end if
      end if
      return
1004  format(' Data colum:',a8)
1005  format(' Plot types:',5(/,i2,2x,a50))
1001  format(' Plot type number=',$)
1006  format(' Data colums:',5(i2,1x,a8))
1002  format(' Data column number=',$)
      end
      subroutine drawcurve(time,value,n,inoz,xticst0,graphx,graphy,
     -  xdist,ydist,y0,linetype)
      dimension time(n),value(n)
c     output Postscript for drawing curve in graph
      write(10,*) 
      write(10,1100) "% Drawing of data curve"
      write(10,30) 
      if (linetype .eq. 1) write (10,*) '[] 0 SD'
      if (linetype .eq. 2) write (10,*) '[2] 1 SD'
      istart=1
c     Skip initial zeros if inz > 0
      if (inoz .eq. 1) then
        do while (value(istart) .eq. 0.0 .and. istart .lt. n)
          istart=istart+1
        end do
      end if
      xstart = graphx + ((time(istart)-xticst0)*xdist)
      ystart = ((value(istart) - y0) *ydist) + graphy
      write(10,'(f5.1,1x,f5.1," M")') xstart, ystart
      b = time(istart)
      c = value(istart)
      nz=0
      do i = istart+1, n
        if (value(i) .gt. 0.0 .or. nz .eq. 0) then
          if (value(i) .ne. 0.0 .and. inoz .eq. 1) nz=1
          write(10,'(f9.4,1x,f9.4," RL")') 
     *          xdist*(time(i)-b), ydist*(value(i) - c)
          b = time(i)
          c = value(i)
        end if
      end do
      write(10,*) "0.1 SLW"
      write(10,31) 
      return
30    format("NP")
31    format("SK")
1100  format(a)
      end
      subroutine writeintgtxt(iout,intg,len,text,ltext)
      character*(*) text
      character*80 line
      write (line,1000) intg,text(1:ltext)
      nd=9-len
      lentot=ltext+9+3
      do i=nd+2,lentot
       line(i-nd:i-nd)=line(i:i)
      end do
      write (iout,1001) line(1:lentot-nd)
      return
1000  format('(',i9,') ',a)
1001  format(a)
      end
      subroutine writerealtxt(iout,rin,len,lfract,text,ltext)
      character*(*) text
      character*80 line
      if (rin .ge. 0.0) r=rin+10**(-lfract-1)
      if (rin .lt. 0.0) r=rin-10**(-lfract-1)
      write (line,1000) r,text(1:ltext)
      lentot=ltext+20+3
      ndf=10-lfract
      do i=22,lentot
       line(i-ndf:i-ndf)=line(i:i)
      end do
      lentot=lentot-ndf
      nd=20-ndf-len
      do i=nd+2,lentot
       line(i-nd:i-nd)=line(i:i)
      end do
      write (iout,1001) line(1:lentot-nd)
      return
1000  format('(',f20.10,') ',a)
1001  format(a)
      end
      subroutine askyn(q,lenq,iyn,idefans,ians)
      character*(*) q
      character*132 pline
      character*1 ans
      character*5 defans
c     idefans=-1: default no; idefans=+1: default yes
c     iyn=1: yes -> ians=1, no -> ians=0
c     iyn=0: yes -> ians=0, no -> ians=1
      ians=0
        if (idefans .eq. -1) then
          defans=' [n] '
          lendef=5
        else if (idefans .eq. +1) then
          defans=' [y] '
          lendef=5
        else
          defans=' '
          lendef=1
        end if
        pline(1:1)=' '
        pline(2:lenq+1)=q(1:lenq)
        pline(lenq+2:lenq+7)=' (y/n)'
        pline(lenq+8:lenq+7+lendef)=defans(1:lendef)
100     write (6,1000) pline(1:lenq+7+lendef)
        read (5,1001,end=99,err=99) ans
99      if (ans .ne. 'n' .and. ans .ne. 'N' .and. ans .ne. 'y' .and.
     -    ans .ne. 'Y') then
          if (idefans .eq. -1) then
            ans='n'
          else if (idefans .eq. 1) then
            ans='y'
          else
            print *,'Pls answer y or n'
            go to 100
          end if
        end if
c     end if
      if (ans .eq. 'y' .or. ans .eq. 'Y') ians=1
      if (iyn .eq. 0) ians=1-ians
      return
1000  format(a,$)
1001  format(a1)
      end
      subroutine getint(q,len,idefault_inp,noneg,maxval,in)
      character*(*) q
      character*132 ansline,pline
      idefault=idefault_inp
      lenq=len
      if (len .gt. 0) then
        pline(1:1)=' '
        pline(2:lenq+1)=q(1:lenq)
        if (idefault .ne. 999999) then
c         Put default on query
          if (idefault .gt. maxval) idefault=maxval
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
      end if
100   if (len .gt. 0) write (6,1000) pline(1:lenq)
      read (5,1001,end=99,err=99) ansline
      ii=1
      call nextstring(ansline,ii,i1,i2,132)
      if (i1 .gt. i2) then
        if (idefault .eq. 999999) go to 100
        in=idefault
      else
        read (ansline(i1:i2),*,err=999) in
      end if
      if (noneg .eq. 1 .and. in .lt. 0) then
        write (6,2002) q(1:len)
        write (6,2000)
        go to 100
      end if
      if (in .gt. maxval) then
        write (6,2001) maxval
        go to 100
      end if
      return
99    in=idefault
      return
999   print *,'Invalid input for an integer'
      go to 100
1000  format(a,$)
1001  format(a132)
1002  format(i4)
1003  format(i8)
2000  format(7x,'Negative number is invalid')
2001  format(7x,'Number read exceeds limit (',i9,')')
2002  format(' ERROR for ',a,':')
      end
      subroutine nextblank(line,ifc,len)
      character*(*) line
c     Finds the next blank in line
      character*1 tab
      common /tab/ itab,tab
      if (ifc .gt. len-1) return
      ifc1=ifc
      do i=ifc1,len-1
        ifc=i
        if (line(i:i) .eq. ' ' .or. line(i:i) .eq. tab) then
          if (line(i:i) .eq. '!') ifc=len
          return
        end if
      end do
      ifc=132
      return
      end
      subroutine findchar(char,line,ic,len)
      character*1 char 
      character*(*) line
c     Finds the next character char in line
c     print *,'FINDCHAR char=',char,' ic=',ic,' len=',len
      do while (line(ic:ic) .ne. char .and. ic .lt. len)
        ic=ic+1
      end do
      return
      end
      subroutine nextstring(line,ifc,ic1,ic2,len)
      character*(*) line
c     print *,'NEXTSTRING ifc=',ifc,' line(ifc-ifc+9)=',line(ifc:ifc+9)
      call nextchar(line,ifc,len)
      if (line(ifc:ifc) .eq. '"') then
c       Look for closing quote
        ifc=ifc+1
        ic1=ifc
        call findchar('"',line,ifc,len)
        if (ifc .eq. len) then
          write (6,1000) ic1,line(1:len)
          stop
        end if
        ic2=ifc-1
        ifc=ifc+1
      else
        ic1=ifc
        call nextblank(line,ifc,len)
        ic2=ifc-1
      end if
      return
1000  format(' ERROR: no closing quote was found for quote at col ',i3,
     -  ' in line',/,1x,a)
      end
      subroutine nextchar(line,ifc,len)
      character*(*) line
c     Finds the next nonblank in line
      character*1 tab
      common /tab/ itab,tab
      if (ifc .gt. len-1) return
      ifc1=ifc
      do i=ifc1,len-1
        ifc=i
c       if (line(i:i) .eq. ' ') print *,'nextc i=',i,' blank'
c       if (line(i:i) .eq. tab) print *,'nextc i=',i,' tab'
        if (line(i:i) .ne. ' ' .and. line(i:i) .ne. tab) then
          if (line(i:i) .eq. '!') ifc=len
          return
        end if
      end do
      ifc=len
      return
      end
      block data
      character*1 tab
      common /tab/ itab,tab
      data itab /151587081/
      end
