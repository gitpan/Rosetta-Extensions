#!perl

use 5.008001; use utf8; use strict; use warnings;

use ExtUtils::MakeMaker;

warn <<__endquote;
------------------------------------------------------------
This distribution requires a live database to be tested against.  To gain the
best understanding of whether Rosetta::Extensions will work with your preferred
database engine and/or working environment, you should edit this distribution's
test configuration file "t_setup.pl" prior to running 'make test' and/or 'perl
Makefile.PL'.  The setup details that you input should match a visible database
engine that you have full privileges on, including the ability to create schema
objects, and select or modify data.  If you do not edit "t_setup.pl", such as
when you are a fully-automatic CPAN tester, then the test suite will use the
newest SQLite v3 database engine by default, and the executed tests will only be
as thorough as the SQLite v3 feature set supports.
------------------------------------------------------------
__endquote

WriteMakefile(
	NAME	=> 'Rosetta::Extensions',
	VERSION => '0.15', # for the distribution itself, not of individual modules
	PREREQ_PM => {
		'Rosetta' => '0.41',
		'DBI' => '1.48',
		'DBD::SQLite' => '1.08', # default database engine used by the test suite
	},
);

1;
