use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	NAME	=> 'Rosetta::Extensions',
	VERSION => '0.10', # for the distribution itself, not of individual modules
	PREREQ_PM => {
		'Rosetta' => '0.37',
		'DBI' => '1.43',
	},
);
