#!perl

use 5.008001; use utf8; use strict; use warnings;

BEGIN { $| = 1; }
use Rosetta::Validator '0.41';
BEGIN {
	my $total_possible = Rosetta::Validator->total_possible_tests();
	$total_possible += 1; # extra nums for our own non-Validator tests
	print "1..$total_possible\n";
}

######################################################################
# First ensure the modules to test will compile, are correct versions:

use Rosetta::Engine::Generic '0.12';
use Rosetta::Engine::Generic::L::en '0.08';
use Rosetta::Utility::SQLBuilder '0.15';

######################################################################
# Here are some utility methods:

my $test_num = 0;
$test_num ++; # test 1 is that SQL::Validator->main() doesn't die

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
			return "internal error: can't find user text for a message: ".
				$message->as_string()." ".$translator->as_string();
		}
		return $user_text;
	}
	return $message; # if this isn't the right kind of object
}

sub import_setup_options {
	my ($setup_filepath) = @_;
	my $err_str = "can't obtain test setup specs from Perl file '".$setup_filepath."'; ";
	my $setup_options = do $setup_filepath;
	unless( ref($setup_options) eq 'HASH' ) {
		if( defined( $setup_options ) ) {
			$err_str .= "result is not a hash ref, but '$setup_options'";
		} elsif( $@ ) {
			$err_str .= "compilation or runtime error of '$@'";
		} else {
			$err_str .= "file system error of '$!'";
		}
		die "$err_str\n";
	}
	unless( scalar( keys %{$setup_options} ) ) {
		die $err_str."result is a hash ref that contains no elements\n";
	}
	eval {
		Rosetta->validate_connection_setup_options( $setup_options ); # dies on problem
	};
	if( my $exception = $@ ) {
		unless( $exception->get_message_key() eq 'ROS_I_V_CONN_SETUP_OPTS_NO_ENG_NM' ) {
			die $err_str."result is a hash ref having invalid elements; ".
				object_to_string( $exception )."\n";
		}
	}
	$setup_options->{'data_link_product'} ||= {};
	# Shouldn't be an Engine set already, but if there is, we override it.
	$setup_options->{'data_link_product'}->{'product_code'} = 'Rosetta::Engine::Generic';
	return $setup_options;
}

######################################################################
# Now perform the actual tests:

my $setup_filepath = shift( @ARGV ) || 't_setup.pl'; # set from first command line arg; '0' means use default name
my $trace_to_stdout = shift( @ARGV ) ? 1 : 0; # set from second command line arg

my $setup_options = eval {
	return import_setup_options( $setup_filepath );
};
if( my $exception = $@ ) {
	warn "-- NOTICE: could not load any test setup options from file '$setup_filepath': $exception";
	warn "-- NOTICE: defaulting to test with a file-based SQLite database named 'test'\n";
	$setup_options = {
		'data_storage_product' => {
			'product_code' => 'SQLite',
			'is_file_based' => 1,
		},
		'data_link_product' => {
			'product_code' => 'Rosetta::Engine::Generic',
		},
		'catalog_instance' => {
			'file_path' => 'test',
		},
	};
	unlink( "test" ); # remove any existing file from previous run of this test
}

my $trace_fh = $trace_to_stdout ? \*STDOUT : undef;

my $test_results = eval {
	return Rosetta::Validator->main( $setup_options, $trace_fh );
};
if( my $exception = $@ ) {
	# errors in test suite itself, or core modules, it seems
	print "not ok 1 (FAIL) - Rosetta::Validator->main() execution - ".
		object_to_string( $exception )."\n";
} else {
	print "ok 1 (PASS) - Rosetta::Validator->main() execution\n";
	foreach my $result (@{$test_results}) {
		print_result( $result );
	}
}

######################################################################

1;
