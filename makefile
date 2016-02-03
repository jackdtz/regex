TARGET = test

SRCDIR  = src
TESTDIR = test

DEFAULT = byte


flag =-Is

native:
	corebuild $(flag) $(SRCDIR),$(TESTDIR) $(TESTDIR)/$(TARGET).native

byte:
	corebuild $(flag) $(SRCDIR),$(TESTDIR) $(TESTDIR)/$(TARGET).byte

clean:
	corebuild -clean; rm *.native; rm *.byte


