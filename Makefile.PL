use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	NAME	=> 'Rosetta::Extensions',
	VERSION => '0.12', # for the distribution itself, not of individual modules
	PREREQ_PM => {
		'Rosetta' => '0.38',
		'DBI' => '1.45',
	},
);
