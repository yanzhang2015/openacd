include $(top_srcdir)/config/utility.am
include $(top_srcdir)/common.am
include $(top_srcdir)/erlang-app.am

EXTRA_DIST = \
	src \
	include \
	priv \
	templates \
	test \
	package.json \
	Gruntfile.js \
	site

COMPILE_FIRST = \
	ebin/ouc_rstat_store.beam

TEMPLATES_SRC = \
	$(shell cd $(srcdir); find templates -name '*.dtl')

TEMPLATES = $(foreach T,$(TEMPLATES_SRC),ebin/$(notdir $(basename $(T)))_dtl.beam)

all : link-priv ebin $(COMPILE_FIRST) $(TEMPLATES) $(ERLAPP_BEAM) ebin/$(ERLAPP_NAME).app site-dist

link-priv:
	test "$(srcdir)" = "$(builddir)" || rsync -ra --delete $(srcdir)/priv $(builddir)

site-dist:
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/package.json $(builddir)/package.json
	test "$(srcdir)" = "$(builddir)" || cp $(srcdir)/Gruntfile.js $(builddir)/Gruntfile.js
	npm install
	node_modules/grunt-cli/bin/grunt --srcdir="$(srcdir)" --builddir="$(builddir)" build


check : erl-check;

docs: clean-docs
	erl -pa ebin/ -noshell\
		-eval 'edoc:application($(ERLAPP_NAME), ".", [$(EDOC_OPTS)]), init:stop().'

clean-docs:
	rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

clean : clean-ebin clean-docs;
