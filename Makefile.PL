use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	NAME	=> 'Rosetta::Extensions',
	VERSION => '0.11', # for the distribution itself, not of individual modules
	PREREQ_PM => {
		'Rosetta' => '0.37',
		'SQL::Routine' => '0.46', # This explicit dependency is for Rosetta-Extensions-0.11 only.
		'DBI' => '1.43',
	},
);
