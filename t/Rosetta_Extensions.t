#!/usr/bin/perl

use strict; use warnings;
use Rosetta::Validator '0.38';

BEGIN { $| = 1; }

######################################################################
# First ensure the modules to test will compile, are correct versions:

use Rosetta::Engine::Generic '0.09';
use Rosetta::Engine::Generic::L::en '0.07';
use Rosetta::Utility::SQLBuilder '0.12';

######################################################################
# Here are some utility methods:

my $test_num = 0;

sub print_result {
	my ($result) = @_;
	$test_num ++;
	my ($feature_key, $feature_status, $feature_desc_msg, $val_error_msg, $eng_error_msg) = 
		@{$result}{'FEATURE_KEY', 'FEATURE_STATUS', 'FEATURE_DESC_MSG', 'VAL_ERROR_MSG', 'ENG_ERROR_MSG'};
	my $result_str = 
		($feature_status eq 'PASS' ? "ok $test_num (PASS)" : 
			$feature_status eq 'FAIL' ? "not ok $test_num (FAIL)" : 
			"ok $test_num (SKIP)").
		" - $feature_key - ".object_to_string( $feature_desc_msg ).
		($val_error_msg ? ' - '.object_to_string( $val_error_msg ) : '').
		($eng_error_msg ? ' - '.object_to_string( $eng_error_msg ) : '');
	print "$result_str\n";
}

sub object_to_string {
	my ($message) = @_;
	if( ref($message) and UNIVERSAL::isa( $message, 'Rosetta::Interface' ) ) {
		$message = $message->get_error_message();
	}
	if( ref($message) and UNIVERSAL::isa( $message, 'Locale::KeyedText::Message' ) ) {
		my $translator = Locale::KeyedText->new_translator( ['Rosetta::Engine::Generic::L::', 
			'Rosetta::Validator::L::', 'Rosetta::L::', 'SQL::Routine::L::'], ['en'] );
		my $user_text = $translator->translate_message( $message );
		unless( $user_text ) {
			return( "internal error: can't find user text for a message: ".
				$message->as_string()." ".$translator->as_string() );
		}
		return( $user_text );
	}
	return( $message ); # if this isn't the right kind of object
}

######################################################################
# Now perform the actual tests:

eval {
	my $total_tests_per_invoke = Rosetta::Validator->total_possible_tests();
	print "1..$total_tests_per_invoke\n";

	my $config_filepath = shift( @ARGV ); # set from first command line arg; '0' means none
	my $rh_config = {};
	if( $config_filepath ) {
		$rh_config = do $config_filepath;
		unless( ref($rh_config) eq 'HASH' ) {
			my $err_str = "can't obtain test configuration specs from Perl file '".$config_filepath."'; ";
			if( defined( $rh_config ) ) {
				$err_str .= "result is not a hash ref, but '$rh_config'";
			} elsif( $@ ) {
				$err_str .= "compilation or runtime error of '$@'";
			} else {
				$err_str .= "file system error of '$!.'";
			}
			die "$err_str\n";
		}
	}

	my $trace_to_stdout = shift( @ARGV ) ? 1 : 0; # set from second command line arg

	my $validator = Rosetta::Validator->new();
	$trace_to_stdout and $validator->set_trace_fh( \*STDOUT );
	$validator->set_engine_name( 'Rosetta::Engine::Generic' );
	$validator->set_engine_config_options( $rh_config );

	$validator->perform_tests();

	foreach my $result (@{$validator->get_test_results()}) {
		print_result( $result );
	}
};
$@ and print "TESTS ABORTED: ".object_to_string( $@ ); # errors in test suite itself, or core modules

######################################################################

1;
