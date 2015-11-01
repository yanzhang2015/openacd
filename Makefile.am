include config/utility.am
include config/project.am
include common.am
include erlang-app.am

@PACKAGE@_EXTRA_DIST = \
	src \
	relx \
	relx.config.in \
	start-dev.sh

# To use make vars in reach-app.spec.in
dist_EXTRA_REPLACE = $(EXTRA_REPLACE)

SUBDIRS = \
	deps \
	files \
	apps \
	.

DEPS_SRC = \
	$(shell cd $(srcdir); find ez_deps -name '*.ez')

DEPS = \
	$(foreach D,$(DEPS_ez),deps/$(basename $(D)))

$(DEPS) : deps/% : ez_deps/%.ez
	! test -d deps || mkdir deps
	$(UNZIP) -d deps -o $<

all : $(DEPS) ebin ebin/reach.beam ebin/user_default.beam ebin/reach.app

MODULES = reach, user_default

relx.config : relx.config.in Makefile
	@$(call SearchAndReplace,$<,$@,$(EXTRA_REPLACE))

rel : relx.config
	! test -d _rel || rm -rf _rel
	$(srcdir)/relx -V -l deps

# Create these empty dirs on install if not already there
emptydirs = \
	$(reachbindir) \
	$(reachlogdir) \
	$(reachrundir) \
	$(reachdbdir) \
	$(reachkeydir) \
	$(reachpipedir) \
	$(reachsitedir)

DESTDIR_emptydirs = \
	$(addprefix $(DESTDIR),$(emptydirs))

$(DESTDIR_emptydirs) :
	$(INSTALL) -d $@

install-data-hook : rel $(DESTDIR_emptydirs) rel-install site-install

rel-install :
	cp -p _rel/reach/bin/start_clean.boot $(DESTDIR)$(reachbindir)/start_clean.boot
	rsync -ra --delete _rel/reach/lib/ $(DESTDIR)$(reachlibdir)/lib/
	rsync -ra --delete _rel/reach/erts-$(ERTS_VSN)/ $(DESTDIR)$(reachlibdir)/erts-$(ERTS_VSN)/
	rsync -ra --delete _rel/reach/releases/ $(DESTDIR)$(reachlibdir)/releases/

site-install:
	rsync -ra --delete apps/reach_ouc/site_dist/ $(DESTDIR)$(reachsitedir)

clean : clean-ebin
	! test -d deps || rm -rf deps
	! test -d _rel || rm -rf _rel

