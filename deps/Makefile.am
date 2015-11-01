include $(top_srcdir)/config/utility.am
include $(top_srcdir)/common.am

EXTRA_DIST = \
	$(DEPS_SRC)

DEPS_SRC = \
	$(shell cd $(srcdir); find . -name '*.ez')

DEPS = $(basename $(DEPS_SRC))

all : $(DEPS)

debug:
	echo $(DEPS)

$(DEPS) : % : %.ez
	$(UNZIP) -o $<
	touch $@

clean :
	$(foreach D,$(DEPS),! test -d $(D) || rm -rf $(D);)
