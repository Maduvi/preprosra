# This will help you compile everything here.
FC      = gfortran
CPPFLAGS = -I/usr/lib64/gfortran/modules
LDFLAGS  = -L/usr/lib64 -lnetcdff
FFLAGS  = -g
COMPILE  = $(FC) $(FFLAGS) $(CPPFLAGS) -c
LINK     = $(FC) $(LDFLAGS) -o
OBJECT1  = codemat.o IOmodul.o sra2nc.o
OBJECT2  = codemat.o IOmodul.o nc2sra.o

all: nc2sra sra2nc

sra2nc: $(OBJECT1)
	$(LINK) sra2nc.exe $(OBJECT1)

nc2sra: $(OBJECT2)
	$(LINK) nc2sra.exe $(OBJECT2)

%.o: %.f90
	$(COMPILE) $<

clean-all: clean
	rm -f *.exe

clean:
	rm -f *.o *.mod 
