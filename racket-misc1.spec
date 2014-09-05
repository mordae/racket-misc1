%global pkgsdir %{_datadir}/racket/extra-pkgs
%global docsdir %{_docdir}/racket

%global collection misc1

Name:		racket-misc1
Version:	20140904
Release:	1%{?dist}
Summary:	miscellaneous utilities

Group:		Development/Libraries
License:	LGPLv3
URL:		http://github.com/mordae/racket-misc1
Source0:	%{name}-%{version}.tar.gz

BuildArch:	noarch

BuildRequires:	racket racket-packaging
Requires:	racket

%description
Library with convenience syntactic forms and procedures.

%prep
%setup -q

%build
%{_libexecdir}/racket-build %{collection}

%install
%{_libexecdir}/racket-install %{collection}

%post
raco link --installation --root %{pkgsdir}/%{collection}
raco setup --no-user --doc-index --only %{collection} >/dev/null

%preun
raco link --installation --remove %{pkgsdir}/%{collection}

%postun
raco setup --no-user --doc-index --tidy >/dev/null

%files
%{pkgsdir}/*
%doc %{docsdir}/*

%changelog
