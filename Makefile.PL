use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	NAME	=> 'Rosetta::Extensions',
	VERSION => 0.05, # for the distribution itself, not of individual modules
	PREREQ_PM => {
		'Locale::KeyedText' => 0.04,
		'SQL::SyntaxModel' => 0.22,
		'Rosetta' => 0.14,
		'DBI' => 1, # don't know yet what version is the minimum
	},
);
