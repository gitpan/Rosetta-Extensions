use ExtUtils::MakeMaker;

warn <<__endquote;
------------------------------------------------------------
This distribution requires a live database to be tested against.  Please edit
the configuration file "t_setup.pl" that comes with this distribution prior to
running 'make test' (it could also be done prior to running 'perl
Makefile.PL').  The setup details that you input should match a visible
database engine that you have full privileges on, including the ability to
create schema objects, and select or modify data.
------------------------------------------------------------
__endquote

WriteMakefile(
	NAME	=> 'Rosetta::Extensions',
	VERSION => '0.13', # for the distribution itself, not of individual modules
	PREREQ_PM => {
		'Rosetta' => '0.39',
		'DBI' => '1.45',
	},
);
