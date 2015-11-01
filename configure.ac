AC_PREREQ([2.63])
AC_INIT([reach-app], [15.08])
AC_CONFIG_AUX_DIR(config)
m4_include([config/general.m4])
m4_include([config/sipXlib.m4])
m4_include([config/sipXlib2.m4])
AM_INIT_AUTOMAKE(foreign tar-ustar)
AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

AC_ERLANG_PATH_ERL
AC_ERLANG_PATH_ERLC

# Common +bin_opt_info +warn_missing_spec
AC_SUBST([ERLC_OPTS], ["-Werror +debug_info +warn_export_all \
  +warn_export_vars +warn_shadow_vars +warn_obsolete_guard"])

SF_NEED_PROG([SSH_KEYGEN], [ssh-keygen], [authentication key generation, management and conversion [autodetected]])
SF_NEED_PROG([UNZIP], [unzip], [list, test and extract compressed files in a ZIP archive [autodetected]])

AC_CONFIG_FILES([
  Makefile
  files/Makefile
  deps/Makefile
  apps/Makefile
  apps/reach_core/Makefile
  apps/reach_dialplan/Makefile
  apps/reach_freeswitch/Makefile
  apps/reach_ouc/Makefile
  apps/reach_spx/Makefile
  apps/reach_web/Makefile
])
AC_OUTPUT
