Name: reach-app
Version: 15.08
Summary: Reach Call Center
Release: 4051.79c07
Group: Applications/Communications
Vendor: Reach
Packager: Douglas Hubler
License: CPAL
AutoReqProv: no
URL: http://github.com/ezuce/reach
Source: %{name}-%{version}.tar.gz

BuildRequires: erlang
BuildRequires: openssh
BuildRequires: unzip
BuildRequires: rsync
BuildRequires: nodejs
BuildRequires: npm

Requires: openssh
Requires: chkconfig
Requires: reach-redis
Requires: nodejs

Obsoletes: openacd
Obsoletes: erlang-oacd_dialplan
Obsoletes: erlang-oacd_freeswitch
Obsoletes: erlang-oacd_ouc
Obsoletes: erlang-oacd_spx
Obsoletes: erlang-oacd_web
Obsoletes: erlang-openacd
Obsoletes: oucxopenacd
Obsoletes: oucxopenacd-site

Prefix: %_prefix
BuildRoot: %{_tmppath}/%name-%version-root

%description
Reach is a skills-based, Call Center software based on FreeSWITCH and built in erlang.

%prep
%setup -q

%build
%configure SIPXPBXUSER=sipx
make

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT

%preun
: %{_sysconfdir}/init.d/reach stop

%files
%defattr(-,root,root,-)
%dir %attr(755,sipx,sipx) %{_localstatedir}/log/sipxpbx
%dir %attr(755,sipx,sipx) %{_localstatedir}/sipxdata/reach/db
%dir %attr(755,sipx,sipx) %{_localstatedir}/run/sipxpbx
%dir %attr(755,sipx,sipx) %{_localstatedir}/www/reach
%dir %attr(700,sipx,sipx) %{_localstatedir}/sipxdata/key
%dir %attr(700,sipx,sipx) %{_localstatedir}/sipxdata/reach/pipe
%{_libdir}/reach/bin/*
%{_libdir}/reach/erts-*/*
%{_libdir}/reach/lib/*
%{_libdir}/reach/releases/*
%{_localstatedir}/www/reach/*
%attr(755,root,root) %{_sysconfdir}/init.d/reach
%config(noreplace) %{_sysconfdir}/sipxpbx/reach/sys.config
%config(noreplace) %{_sysconfdir}/sipxpbx/reach/lager.config
%config(noreplace) %{_sysconfdir}/sipxpbx/reach/vm.args

%post
if grep -q :on <<<`/sbin/chkconfig reach --list 2>&1`; then
    /sbin/chkconfig --del reach
    /sbin/chkconfig reach off
fi

%changelog
* Tue Oct 22 2013 Jan Vincent Liwanag <jvliwanag@ezuce.com>
- Initial release
- Add chkconfig dep and reset run level on post
- Remove chkconfig dep, should be started only by supervisor
