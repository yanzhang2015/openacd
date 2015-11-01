include $(top_srcdir)/config/utility.am
include $(top_srcdir)/common.am
include $(top_srcdir)/erlang-app.am

EXTRA_DIST = \
	src \
	include

all : ebin $(ERLAPP_BEAM) ebin/$(ERLAPP_NAME).app

check : erl-check;

docs: clean-docs
	erl -pa ebin/ -noshell\
		-eval 'edoc:application($(ERLAPP_NAME), ".", [$(EDOC_OPTS)]), init:stop().'                

clean-docs:
	rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

clean : clean-ebin clean-docs;
