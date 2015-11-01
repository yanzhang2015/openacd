SUBDIRS = \
	reach_core \
	reach_dialplan \
	reach_freeswitch \
	reach_ouc \
	reach_spx \
	reach_web


docs : % : %-recurse 

%-recurse :
	list='$(SUBDIRS)'; for subdir in $$list; do \
	  test "$$subdir" = . || ($(am__cd) $$subdir && $(MAKE) $(AM_MAKEFLAGS) $*) || eval 'exit 1'; \
	done

