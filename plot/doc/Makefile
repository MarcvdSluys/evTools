#Compile the program and put the executable in ~/usr/bin

#No text/space after statements below!
COMP = gfortran#  Use GNU fortran compiler
COMP = gfortran-4.4.3#
#COMP = ifort#     Use Intel fortran compiler


#ifort:
ifeq ($(COMP),ifort)
   #To develop code  (don't mix -check bounds with -O>0!):
   #CFLAGS = -free -O0  -vec-report0 -warn alignments -warn declarations -warn uncalled -warn usage -check bounds -traceback #-check format -check output_conversion
   CFLAGS = -free -O0  -vec-report0 -traceback -warn all -check all  -check noarg_temp_created

   #To debug code:
   #CFLAGS = -free -O0  -vec-report0 -g

   #To use optimum run speed
   #CFLAGS = -free -O3 -vec-report0 -traceback
   #CFLAGS = -free -O3 -arch SSE2 -vec-report0 -traceback           #SSE2 needs -lsvml
   ##CFLAGS = -free -O3 -arch SSE2 -ipo -vec-report0 -traceback

   CFLAGS += -nogen-interfaces  #For ifort v.11

   #LFLAGS = -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 
   #LFLAGS = -lpgplot_ifort -lX11 -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 
   LFLAGS = -lpgplot
   #LFLAGS = -ipo -vec-report0 -lpgplot_ifort -lX11 -lm -lifcore -limf -lirc -lsvml -lifport -Xlinker 
endif


#gfortran: similar in speed to ifort, but file reading is ~2.5x slower!
#ifeq ($(COMP),gfortran)
ifneq (,$(filter $(COMP),gfortran gfortran-4.4.3))
   #CFLAGS = -O0 -ffree-form -ffree-line-length-none
   CFLAGS = -O0 -ffree-form -ffree-line-length-none   -Wall 
   #CFLAGS = -O -ffree-form -ffree-line-length-none
   #CFLAGS = -O3 -ffree-form -ffree-line-length-none
   LFLAGS = -lpgplot
endif




STDOBJ = functions.o
PLTOBJ = plotfunctions.o plt_functions.o
MDLOBJ = 
MODOBJ = 

%.o: %.f Makefile
	$(COMP) $(CFLAGS) -c $<


all: plotmdl plotmdln plotmod plotplt plotpltn plotpltp

exit:



plotmdl: $(STDOBJ) $(MDLOBJ) plotmdl.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plotmdl $(STDOBJ) $(MDLOBJ) plotmdl.o

plotmdln: $(STDOBJ) $(MDLOBJ) plotmdln.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plotmdln $(STDOBJ) $(MDLOBJ) plotmdln.o

plotmod: $(STDOBJ) $(MODOBJ) plotmod.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plotmod $(STDOBJ) $(MODOBJ) plotmod.o

plotplt: $(STDOBJ) $(PLTOBJ) plotplt.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plotplt $(STDOBJ) $(PLTOBJ) plotplt.o

plotpltn: $(STDOBJ) $(PLTOBJ) plotpltn.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plotpltn $(STDOBJ) $(PLTOBJ) plotpltn.o

plotpltp: $(STDOBJ) $(PLTOBJ) plotpltp.o
	$(COMP) $(LFLAGS) -o ~/usr/bin/plotpltp $(STDOBJ) $(PLTOBJ) plotpltp.o






clean:
	rm -f *.o *.mod

cleanwarnings:
	rm -f *__genmod.f90 *__genmod.mod
