# This will help you compile everything here.
FC      = gfortran
CPPFLAGS = -I/usr/lib64/gfortran/modules
LDFLAGS  = -L/usr/lib64 -lnetcdff
FFLAGS  = -g
COMPILE  = $(FC) $(FFLAGS) $(CPPFLAGS) -c
LINK     = $(FC) $(LDFLAGS) -o
OBJECT  = codemat.o IOmodul.o sra2nc.o

all: cdl

cdl: sra2nc
	./sra2nc.exe

sra2nc: $(OBJECT)
	$(LINK) sra2nc.exe $(OBJECT)

%.o: %.f90
	$(COMPILE) $<

clean-all: clean
	rm -f sra2nc.exe *.cdl

clean:
	rm -f *.o *.mod 
