use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
	NAME	=> 'Rosetta::Extensions',
	VERSION => 0.06, # for the distribution itself, not of individual modules
	PREREQ_PM => {
		'Locale::KeyedText' => 0.06,
		'SQL::SyntaxModel' => 0.24,
		'Rosetta' => 0.16,
		'DBI' => 1, # don't know yet what version is the minimum
	},
);
