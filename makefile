TARGET = test

SRCDIR  = src
TESTDIR = test

DEFAULT = byte


flag =-Is

native:
	python3 testFileGenerator.py
	corebuild $(flag) $(SRCDIR),$(TESTDIR) $(TESTDIR)/$(TARGET).native

byte:
	python3 testFileGenerator.py
	corebuild $(flag) $(SRCDIR),$(TESTDIR) $(TESTDIR)/$(TARGET).byte

clean:
	corebuild -clean; rm *.native; rm *.byte


