# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Rosetta_Extensions.t'

######################### We start with some black magic to print on failure.

BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use Rosetta::Utility::SQLBuilder 0.09;
use Rosetta::Engine::Generic 0.06;
use Rosetta::Engine::Generic::L::en 0.06;
$loaded = 1;
print "ok 1\n";
use strict;
use warnings;

######################### End of black magic.

# Set this to 1 to see complete result text for each test
my $verbose = shift( @ARGV ) ? 1 : 0;  # set from command line

######################################################################
# Here are some utility methods:

my $test_num = 1;  # same as the first test, above

sub result {
	$test_num++;
	my ($worked, $detail) = @_;
	$verbose or 
		$detail = substr( $detail, 0, 50 ).
		(length( $detail ) > 47 ? "..." : "");	print "@{[$worked ? '' : 'not ']}ok $test_num $detail\n";
}

sub message {
	my ($detail) = @_;
	print "-- $detail\n";
}

sub error_to_string {
	my ($message) = @_;
	ref($message) or return( $message ); # if this isn't an object
	my $translator = Locale::KeyedText->new_translator( ['Rosetta::Engine::Generic::L::', 
		'Rosetta::L::', 'SQL::Routine::L::'], ['en'] );
	my $user_text = $translator->translate_message( $message );
	unless( $user_text ) {
		return( "internal error: can't find user text for a message: ".
			$message->as_string()." ".$translator->as_string() );
	}
	return( $user_text );
}

######################################################################

message( "START TESTING Rosetta-Extensions" );

######################################################################

eval {
	message( "No functional tests are written yet; they will come later" );
};
$@ and result( 0, "TESTS ABORTED: ".error_to_string( $@ ) );

######################################################################

message( "DONE TESTING Rosetta-Extensions" );

######################################################################

1;
