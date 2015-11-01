include $(top_srcdir)/config/utility.am
include $(top_srcdir)/common.am

EXTRA_DIST = \
	$(initd_SCRIPTS:=.in) \
	$(reachetc_DATA:=.in)

initddir = $(SIPX_SERVICEDIR)
initd_SCRIPTS = \
	reach

reachetc_DATA = \
	vm.args \
	sys.config \
	lager.config

$(initd_SCRIPTS) $(reachetc_DATA) : % : %.in Makefile
	@$(call SearchAndReplace,$<,$@,$(EXTRA_REPLACE))

CLEANFILES = $(initd_SCRIPTS) $(noinst_DATA)

