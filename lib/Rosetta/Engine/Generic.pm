=head1 NAME

Rosetta::Engine::Generic - A catch-all Engine for any DBI-supported SQL database

=cut

######################################################################

package Rosetta::Engine::Generic;
use 5.006;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = '0.03';

use Locale::KeyedText 0.06;
use SQL::SyntaxModel 0.24;
use Rosetta 0.16;
use DBI;
use Rosetta::Utility::SQLBuilder 0.06;

use base qw( Rosetta::Engine );

######################################################################

=head1 DEPENDENCIES

Perl Version: 5.006

Standard Modules: I<none>

Nonstandard Modules: 

	Locale::KeyedText 0.06 (for error messages)
	SQL::SyntaxModel 0.24
	Rosetta 0.16
	DBI (v1.43 or higher recommended)
	Rosetta::Utility::SQLBuilder 0.06

I<I prefer to simply require that people have DBI v1.43 or later (newest
version at this writing) but I am not requiring a version yet since it may not
be easy for many people, such as ISP customers, to upgrade right now.>

=head1 COPYRIGHT AND LICENSE

This file is part of the Rosetta database abstraction framework.

Rosetta is Copyright (c) 1999-2004, Darren R. Duncan.  All rights reserved.
Address comments, suggestions, and bug reports to B<perl@DarrenDuncan.net>, or
visit "http://www.DarrenDuncan.net" for more information.

Rosetta is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License (GPL) version 2 as published by the
Free Software Foundation (http://www.fsf.org/).  You should have received a
copy of the GPL as part of the Rosetta distribution, in the file named
"LICENSE"; if not, write to the Free Software Foundation, Inc., 59 Temple
Place, Suite 330, Boston, MA 02111-1307 USA.

Linking Rosetta statically or dynamically with other modules is making a
combined work based on Rosetta.  Thus, the terms and conditions of the GPL
cover the whole combination.  As a special exception, the copyright holders of
Rosetta give you permission to link Rosetta with independent modules,
regardless of the license terms of these independent modules, and to copy and
distribute the resulting combined work under terms of your choice, provided
that every copy of the combined work is accompanied by a complete copy of the
source code of Rosetta (the version of Rosetta used to produce the combined
work), being distributed under the terms of the GPL plus this exception.  An
independent module is a module which is not derived from or based on Rosetta,
and which is fully useable when not linked to Rosetta in any form.

Any versions of Rosetta that you modify and distribute must carry prominent
notices stating that you changed the files and the date of any changes, in
addition to preserving this original copyright notice and other credits.
Rosetta is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of Rosetta would
appreciate being informed any time you create a modified version of Rosetta
that you are willing to distribute, because that is a practical way of
suggesting improvements to the standard version.

=cut

######################################################################
######################################################################

# Names of properties for objects of the Rosetta::Engine::Generic class are declared here:
my $PROP_PAYLOAD = 'payload'; # If Eng fronts a LITERAL Intf, put payload it represents here.
my $PROP_DBI_OBJ = 'dbi_obj'; # Typically either undefined or a 'dbh' or a 'sth' we are wrapping.
my $PROP_SQL_BUILDER = 'sql_builder'; # Assoc with a connection; the SQLBuilder object we use.

# Names of the allowed Interface types go here:
my $INTFTP_ERROR       = 'Error'; # What is returned if an error happens, in place of another Intf type
my $INTFTP_TOMBSTONE   = 'Tombstone'; # What is returned when execute() destroys an Interface
my $INTFTP_APPLICATION = 'Application'; # What you get when you create an Interface out of any context
	# This type is the root of an Interface tree; when you create one, you provide an 
	# "application_instance" SQL::SyntaxModel Node; that provides the necessary context for 
	# subsequent "command" or "routine" Nodes you pass to any child Intf's "prepare" method.
my $INTFTP_PREPARATION = 'Preparation'; # That which is returned by the 'prepare()' method
my $INTFTP_ENVIRONMENT = 'Environment'; # Parent to all CONNECTION INTFs impl by same Engine
my $INTFTP_CONNECTION  = 'Connection'; # Result of executing a 'connect' command
my $INTFTP_TRANSACTION = 'Transaction'; # Result of asking to start a new transaction
my $INTFTP_LITERAL     = 'Literal'; # Result of execution that isn't one of the above, like an IUD
	# This type can be returned as the grand-child for any of [APPL, ENVI, CONN, TRAN].
	# This type is returned by the execute() of any Command that doesn't return one of 
	# the above 4 context INTFs, except for those that return CURSOR|ROW.
	# Any commands that stuff new Nodes in the current SSM Container, such as the 
	# *_LIST or *_INFO or *_CLONE Commands, will return a new Node ref as the payload.
	# Any commands that simply do a yes/no test, such as *_VERIFY, or DB_PING, 
	# simply have a boolean payload.
	# IUD commands usually return this, plus method calls; payload may be a hash ref of results.
my $INTFTP_ROW         = 'Row'; # Result of executing a query that returns one row
my $INTFTP_CURSOR      = 'Cursor'; # Result of executing a query that would return rows to the caller

# Names of features that Rosetta::Engine::Generic claims to support:
my %SUPPORTED_FEATURES = map { ($_ => 1) } qw(
	CONN_BASIC 
	TRAN_BASIC 

	DOMAIN_LIST 
	DOMAIN_DEFN_BASIC

	TABLE_LIST
	TABLE_DEFN_BASIC
	TABLE_UKEY_BASIC TABLE_UKEY_MULTI

	QUERY_BASIC
	QUERY_RETURN_SPEC_COLS QUERY_RETURN_COL_EXPRS
	QUERY_WHERE
	QUERY_JOIN_BASIC QUERY_JOIN_OUTER_LEFT
	QUERY_WINDOW_ORDER
);

######################################################################

sub new {
	my ($class) = @_;
	my $engine = bless( {}, ref($class) || $class );
	$engine->{$PROP_PAYLOAD} = undef;
	$engine->{$PROP_DBI_OBJ} = undef;
	$engine->{$PROP_SQL_BUILDER} = undef;
	return( $engine );
}

######################################################################

sub destroy {
	my ($engine, $interface) = @_;
	my $intf_type = $interface->get_interface_type();
	if( $intf_type eq $INTFTP_CONNECTION ) {
		$engine->close_dbi_connection( $engine->{$PROP_DBI_OBJ} );
	}
	# Assume Interface won't let us be called if child Interfaces (and Engines) exist
	%{$engine} = ();
}

sub DESTROY {
	my ($engine) = @_;
	if( ref($engine->{$PROP_DBI_OBJ}) eq 'DBI::db' ) { # a 'Connection'
		$engine->close_dbi_connection( $engine->{$PROP_DBI_OBJ} );
	}
	%{$engine} = ();
}

######################################################################

sub prepare {
	my ($engine, $interface, $routine_defn) = @_;
	my $preparation = undef;
	my $intf_type = $interface->get_interface_type();
	my $node_type = $routine_defn->get_node_type();
	if( $node_type eq 'routine' ) {
		unless( $intf_type eq $INTFTP_TRANSACTION ) {
			$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_GEN_RTN', 
				{ 'ITYPE' => $intf_type } );
		}
		$preparation = $engine->prepare_routine( $interface, $routine_defn );
	} elsif( $node_type eq 'command' ) {
		my $cmd_type = $routine_defn->get_enumerated_attribute( 'command_type' );
		if( $intf_type eq $INTFTP_ENVIRONMENT ) {
			# Note that 'environment' is synonymous with 'application', 
			# but an Engine->prepare() should never be called with an 'application' Interface.
			if( $cmd_type eq 'DB_LIST' ) {
				$preparation = $engine->prepare_cmd_db_list( $interface, $routine_defn );
			} elsif( $cmd_type eq 'DB_INFO' ) {
				$preparation = $engine->prepare_cmd_db_info( $interface, $routine_defn );
			} elsif( $cmd_type eq 'DB_OPEN' ) {
				$preparation = $engine->prepare_cmd_db_open( $interface, $routine_defn );
			} else {
				$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_THIS_CMD', 
					{ 'ITYPE' => $intf_type, 'CTYPE' => $cmd_type } );
			}
		} elsif( $intf_type eq $INTFTP_CONNECTION ) {
			if( $cmd_type eq 'DB_CLOSE' ) {
				$preparation = $engine->prepare_cmd_db_close( $interface, $routine_defn );
			} elsif( $cmd_type eq 'TRA_OPEN' ) {
				$preparation = $engine->prepare_cmd_tra_open( $interface, $routine_defn );
			} else {
				$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_THIS_CMD', 
					{ 'ITYPE' => $intf_type, 'CTYPE' => $cmd_type } );
			}
		} elsif( $intf_type eq $INTFTP_TRANSACTION ) {
			if( $cmd_type eq 'TRA_CLOSE' ) {
				$preparation = $engine->prepare_cmd_tra_close( $interface, $routine_defn );
			} elsif( $cmd_type eq 'TABLE_CREATE' ) {
				$preparation = $engine->prepare_cmd_table_create( $interface, $routine_defn );
			} elsif( $cmd_type eq 'TABLE_DELETE' ) {
				$preparation = $engine->prepare_cmd_table_delete( $interface, $routine_defn );
			} else {
				$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_THIS_CMD', 
					{ 'ITYPE' => $intf_type, 'CTYPE' => $cmd_type } );
			}
		} else {
			$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_THIS_CMD', 
				{ 'ITYPE' => $intf_type, 'CTYPE' => $cmd_type } );
		}
	} else {
		$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_SSM_NODE', 
			{ 'ITYPE' => $intf_type, 'NTYPE' => $node_type } );
	}
	return( $preparation );
}

######################################################################

sub open_dbi_connection {
	my ($engine, $dbi_driver, $local_dsn, $login_user, $login_pass) = @_;
	my $use_auto_commit = $engine->get_static_const_use_auto_commit(); # usually 0
	my $dbi_dbh = DBI->connect( 
		"DBI:".$dbi_driver.":".$local_dsn,
		$login_user,
		$login_pass,
		{ RaiseError => 1, AutoCommit => $use_auto_commit },
	); # throws exception on failure
	return( $dbi_dbh );
}

sub close_dbi_connection {
	my ($engine, $dbi_dbh) = @_;
	my $use_auto_commit = $engine->get_static_const_use_auto_commit(); # usually 0
	unless( $use_auto_commit ) {
		$dbi_dbh->rollback(); # explicit call, since behaviour of disconnect undefined
	}
	$dbi_dbh->disconnect(); # throws exception on failure
}

######################################################################

sub clean_up_dbi_driver_string {
	# This code is partly derived from part of DBI->install_driver().
	my ($engine, $driver_name) = @_;
	# This line converts an undefined value to a defined empty string.
	defined( $driver_name ) or $driver_name = '';
	# This line removes any leading or trailing whitespace.
	$driver_name =~ s|^\s*(.*?)\s*$|$1|;
	# This line extracts the 'driver' from 'dbi:driver:*' strings.
	$driver_name =~ m|^DBI:(.*?):|i; $1 and $driver_name = $1;
	# This line extracts the 'driver' from 'DBD::driver' strings.
	$driver_name =~ m|^DBD::(.*)$|; $1 and $driver_name = $1;
	# This line extracts the 'driver' from any other bounding characters.
	$driver_name =~ s|([a-zA-Z0-9_]*)|$1|;
	return( $driver_name );
}

######################################################################

sub install_dbi_driver {
	my ($engine, $cat_link_inst_opt_dbi_driver, $cat_inst_node) = @_;

	# First gather the hint we were given for what driver to use.
	# Try the catalog link instance option first, and the catalog instance data storage product second.
	my $driver_hint = $cat_link_inst_opt_dbi_driver;
	unless( $driver_hint ) {
		my $dsp_node = $cat_inst_node->get_node_ref_attribute( 'product' );
		$driver_hint = $dsp_node->get_literal_attribute( 'product_code' );
	}

	# This trims the hint to essentials if it is formatted like specific DBI driver strings.
	$driver_hint = $engine->clean_up_dbi_driver_string( $driver_hint );

	# If driver hint is empty because it just contained junk characters before, then use a default.
	unless( $driver_hint ) {
		$driver_hint = 'ODBC'; # This is the fall-back we use, as stated in module documentation.
	}

	# This tests whether the driver hint exactly matches the key part of the name of a 
	# DBI driver that is installed on the system; the driver is also installed if it exists.
	eval {
		DBI->install_driver( $driver_hint );
	};
	unless( $@ ) {
		# No errors, so the DBI driver exists and is now installed.
		return( $driver_hint );
	}
	my $semi_original_driver_hint = $driver_hint; # save for error strings if any

	# If we get here then the driver hint does not exactly match a DBI driver name, 
	# so we will have to figure out one to use by ourselves.

	# Let's start by trying a few capitalization variants that look like typical DBI driver names.
	# If the given hint matches a driver name except for the capitalization, then these simple  
	# tries should work for about 75% of the DBI drivers that I know about.
	$driver_hint = uc( $driver_hint );
	eval { DBI->install_driver( $driver_hint ); }; unless( $@ ) { return( $driver_hint ); }
	$driver_hint = lc( $driver_hint );
	eval { DBI->install_driver( $driver_hint ); }; unless( $@ ) { return( $driver_hint ); }
	$driver_hint = ucfirst( lc( $driver_hint ) );
	eval { DBI->install_driver( $driver_hint ); }; unless( $@ ) { return( $driver_hint ); }

	# Now ask DBI for a list of installed drivers, and compare each to the driver hint, 
	# including some sub-string match attempts.
	my @available_drivers = DBI->available_drivers();
	unless( @available_drivers ) {
		$engine->_throw_error_message( 'ROS_G_NO_DBI_DRIVER_HINT_MATCH', 
			{ 'NAME' => $semi_original_driver_hint } );
	}
	my $matched_driver_name = undef;
	foreach my $driver_name (@available_drivers) {
		if( $driver_name =~ m|$driver_hint|i or $driver_hint =~ m|$driver_name|i ) {
			$matched_driver_name = $driver_name;
			last;
		}
	}
	if( $matched_driver_name ) {
		eval { DBI->install_driver( $matched_driver_name ); }; 
		unless( $@ ) { return( $matched_driver_name ); }
	}

	# If we get here then all attempts have failed, so give up.
	$engine->_throw_error_message( 'ROS_G_NO_DBI_DRIVER_HINT_MATCH', 
		{ 'NAME' => $semi_original_driver_hint } );
}

######################################################################

sub make_ssm_node {
	my ($engine, $node_type, $container) = @_;
	my $node = $container->new_node( $node_type );
	$node->set_node_id( $container->get_next_free_node_id( $node_type ) );
	$node->put_in_container( $container );
	$node->add_reciprocal_links();
	return( $node );
}

sub make_child_ssm_node {
	my ($engine, $node_type, $pp_node, $pp_attr) = @_;
	my $container = $pp_node->get_container();
	my $node = $pp_node->new_node( $node_type );
	$node->set_node_id( $container->get_next_free_node_id( $node_type ) );
	$node->put_in_container( $container );
	$node->add_reciprocal_links();
	$node->set_node_ref_attribute( $pp_attr, $pp_node );
	$node->set_parent_node_attribute_name( $pp_attr );
	return( $node );
}

######################################################################

sub prepare_cmd_db_list {
	my ($env_eng, $env_intf, $command_bp_node) = @_;

	my $container = $command_bp_node->get_container();
	my $app_inst_node = $env_intf->get_parent_interface()->get_parent_interface()->get_ssm_node();
	my $app_bp_node = $app_inst_node->get_node_ref_attribute( 'blueprint' );

	my $routine = sub {
		# This routine is a closure.
		my ($rtv_lit_prep_eng, $rtv_lit_prep_intf, $rtv_args) = @_;

		my @cat_link_inst_nodes = ();

		my $dlp_node = $env_intf->get_ssm_node(); # A 'data_link_product' Node (repr ourself).
		foreach my $dbi_driver (DBI->available_drivers()) {
			# Tested $dbi_driver values on my system are (space-delimited):
			# [DBM ExampleP File Proxy SQLite Sponge mysql]; they are ready to use as is.
			eval { DBI->install_driver( $dbi_driver ); }; $@ and next; # Skip bad driver.
			# If we get here, then the $dbi_driver will load without problems.
			my $dsp_node = $env_eng->make_ssm_node( 'data_storage_product', $container );
			$dsp_node->set_literal_attribute( 'product_code', $dbi_driver );
			foreach my $dbi_data_source (DBI->data_sources( $dbi_driver )) {
				#Examples of $dbi_data_source formats are: 
				#dbi:DriverName:database_name
				#dbi:DriverName:database_name@hostname:port
				#dbi:DriverName:database=database_name;host=hostname;port=port 
				my (undef, undef, $local_dsn) = split( ':', $dbi_data_source );
				my $cat_bp_node = $env_eng->make_ssm_node( 'catalog', $container );
				my $cat_link_bp_node = $env_eng->make_child_ssm_node( 
					'catalog_link', $app_bp_node, 'application' );
				$cat_link_bp_node->set_literal_attribute( 'name', $dbi_data_source );
				$cat_link_bp_node->set_node_ref_attribute( 'target', $cat_bp_node );
				my $cat_inst_node = $env_eng->make_ssm_node( 'catalog_instance', $container );
				$cat_inst_node->set_node_ref_attribute( 'product', $dsp_node );
				$cat_inst_node->set_node_ref_attribute( 'blueprint', $cat_bp_node );
				$cat_inst_node->set_literal_attribute( 'name', $dbi_data_source );
				my $cat_link_inst_node = $env_eng->make_child_ssm_node( 
					'catalog_link_instance', $app_inst_node, 'application' );
				$cat_link_inst_node->set_node_ref_attribute( 'product', $dlp_node );
				$cat_link_inst_node->set_node_ref_attribute( 'unrealized', $cat_link_bp_node );
				$cat_link_inst_node->set_node_ref_attribute( 'target', $cat_inst_node );
				$cat_link_inst_node->set_literal_attribute( 'local_dsn', $local_dsn );
				push( @cat_link_inst_nodes, $cat_link_inst_node );
			}
		}

		my $rtv_lit_eng = $rtv_lit_prep_eng->new();
		$rtv_lit_eng->{$PROP_PAYLOAD} = \@cat_link_inst_nodes;

		my $rtv_lit_intf = $rtv_lit_prep_intf->new( $INTFTP_LITERAL, undef, 
			$rtv_lit_prep_intf, $rtv_lit_eng );
		return( $rtv_lit_intf );
	};

	my $lit_prep_eng = $env_eng->new();

	my $lit_prep_intf = $env_intf->new( $INTFTP_PREPARATION, undef, 
		$env_intf, $lit_prep_eng, $command_bp_node, $routine );
	return( $lit_prep_intf );
}

######################################################################

sub prepare_cmd_db_info {
	my ($env_eng, $env_intf, $command_bp_node) = @_;


}

######################################################################

sub prepare_cmd_db_open {
	my ($env_eng, $env_intf, $command_bp_node) = @_;

	# This block gathers info from SSM which describes db connection we are to open.
	my $container = $command_bp_node->get_container();
	my $cat_link_bp_node = $command_bp_node->get_node_ref_attribute( 'command_arg_1' );
	my $app_inst_node = $env_intf->get_parent_interface()->get_parent_interface()->get_ssm_node();
	my $cat_link_inst_node = undef;
	foreach my $link (@{$app_inst_node->get_child_nodes( 'catalog_link_instance' )}) {
		if( $link->get_node_ref_attribute( 'unrealized' ) eq $cat_link_bp_node ) {
			$cat_link_inst_node = $link;
			last;
		}
	}
	my %cat_link_inst_opts = map { 
			( $_->get_literal_attribute( 'key' ) => $_->get_literal_attribute( 'value' ) ) 
		} @{$cat_link_inst_node->get_child_nodes( 'catalog_link_instance_opt' )};
	my $cat_inst_node = $cat_link_inst_node->get_node_ref_attribute( 'target' );
	my $local_dsn = $cat_link_inst_opts{'local_dsn'} || 
		$cat_link_inst_node->get_literal_attribute( 'local_dsn' );
	my $login_user = $cat_link_inst_opts{'login_user'} || 
		$cat_link_inst_node->get_literal_attribute( 'login_user' );
	my $login_pass = $cat_link_inst_opts{'login_pass'} || 
		$cat_link_inst_node->get_literal_attribute( 'login_pass' );
	my $dbi_driver = $env_eng->install_dbi_driver( $cat_link_inst_opts{'dbi_driver'}, $cat_inst_node );

	my $builder = $env_eng->{$PROP_SQL_BUILDER} = Rosetta::Utility::SQLBuilder->new();
	$cat_link_inst_opts{'delim_ident'} and 
		$builder->delimited_identifiers( $cat_link_inst_opts{'delim_ident'} );

	my $routine = sub {
		# This routine is a closure.
		my ($rtv_conn_prep_eng, $rtv_conn_prep_intf, $rtv_args) = @_;
		$rtv_args->{'local_user'} and $login_user = $rtv_args->{'local_user'};
		$rtv_args->{'local_pass'} and $login_pass = $rtv_args->{'local_pass'};

		my $dbi_dbh = $rtv_conn_prep_eng->open_dbi_connection( 
			$dbi_driver, $local_dsn, $login_user, $login_pass );

		my $rtv_conn_eng = $rtv_conn_prep_eng->new();
		$rtv_conn_eng->{$PROP_DBI_OBJ} = $dbi_dbh;

		my $rtv_conn_intf = $rtv_conn_prep_intf->new( $INTFTP_CONNECTION, undef, 
			$rtv_conn_prep_intf, $rtv_conn_eng );
		return( $rtv_conn_intf );
	};

	my $conn_prep_eng = $env_eng->new();

	my $conn_prep_intf = $env_intf->new( $INTFTP_PREPARATION, undef, 
		$env_intf, $conn_prep_eng, $command_bp_node, $routine );
	return( $conn_prep_intf );
}

######################################################################

sub prepare_cmd_db_close {
	my ($conn_eng, $conn_intf, $command_bp_node) = @_;

	my $routine = sub {
		# This routine is a closure.
		my ($rtv_tomb_prep_eng, $rtv_tomb_prep_intf, undef) = @_;

		if( @{$rtv_tomb_prep_intf->get_sibling_interfaces( 1 )} > 0 ) {
			$rtv_tomb_prep_eng->_throw_error_message( 'ROS_G_CMD_DB_CLOSE_CONN_IN_USE' );
		}

		$conn_eng->close_dbi_connection( $conn_eng->{$PROP_DBI_OBJ} );

		my $rtv_tomb_intf = $rtv_tomb_prep_intf->new( $INTFTP_TOMBSTONE );
		$rtv_tomb_prep_intf->destroy();
		$conn_intf->destroy(); # removes last ref to DBI dbh object
		return( $rtv_tomb_intf );
	};

	my $tomb_prep_eng = $conn_eng->new();

	my $tomb_prep_intf = $conn_intf->new( $INTFTP_PREPARATION, undef, 
		$conn_intf, $tomb_prep_eng, $command_bp_node, $routine );
	return( $tomb_prep_intf );
}

######################################################################

sub get_supported_features {
	my ($env_eng, $env_intf, $feature_name) = @_;
	my $rh_supported_features = $env_eng->get_static_const_supported_features();
	if( defined( $feature_name ) ) {
		return( $rh_supported_features->{$feature_name} );
	} else {
		return( {%{$rh_supported_features}} );
	}
}

######################################################################

sub payload {
	my ($lit_eng, $lit_intf) = @_;
	return( $lit_eng->{$PROP_PAYLOAD} );
}

######################################################################

sub finalize {
	my ($curs_eng, $curs_intf) = @_;
	# Now DO something.
}

######################################################################

sub has_more_rows {
	my ($curs_eng, $curs_intf) = @_;
	# Now DO something.
	return( 0 );
}

######################################################################

sub fetch_row {
	my ($curs_eng, $curs_intf) = @_;
	# Now DO something.
	return( {} );
}

######################################################################

sub fetch_all_rows {
	my ($curs_eng, $curs_intf) = @_;
	# Now DO something.
	my @rows = ();
	while( $curs_eng->has_more_rows() ) {
		push( @rows, $curs_eng->fetch_row() );
	}
	$curs_eng->finalize();
	return( \@rows );
}

######################################################################

sub get_static_const_use_auto_commit {
	# This function is separated so sub-classes can override it easily.
	return( 0 );
}

######################################################################

sub get_static_const_supported_features {
	# This function is separated so sub-classes can override it easily.
	return( \%SUPPORTED_FEATURES );
}

######################################################################
######################################################################

1;
__END__

=head1 SYNOPSIS

This is an example showing about the minimum amount of code required to go from
nothing to an open database connection using Rosetta::Engine::Generic, or any
standard Rosetta Engine module for that matter (about 40 lines of code).

	# First load our basic requirements, Rosetta and SQL::SyntaxModel.
	use Rosetta;

	# Now create a schema model we will use everywhere.
	my $model = SQL::SyntaxModel->new_container();

	# Now create the model root node representing the app running right now / that we are.
	my $app_bp = make_a_node( 'application', $model );

	# Define an application instance, for testers, which is the app running right now.
	my $test_app = make_a_node( 'application_instance', $model );
	$test_app->set_node_ref_attribute( 'blueprint', $app_bp );

	# Now create a new Rosetta Interface tree that will mediate db access for us.
	my $application = Rosetta->new_application( $test_app );

	# Now create the model root node that represents a database/catalog we will 
	# be using (may be several), and a node belonging to said app representing a 
	# yet-unrealized data connection from the app to the db.
	my $catalog_bp = make_a_node( 'catalog', $model );
	my $app_cl = make_a_child_node( 'catalog_link', $app_bp, 'application' );
	$app_cl->set_literal_attribute( 'name', 'big_data' );
	$app_cl->set_node_ref_attribute( 'target', $catalog_bp );

	# ... Next, probably (or you can do it later), make and stuff a whole bunch 
	# of nodes in the model that describe the database/catalog schema and any 
	# application-stored queries or routines that would run against the db.

	# As an example of the above, a command to 'connect' to the database.
	my $open_cmd = make_a_child_node( 'command', $app_bp, 'application' );
	$open_cmd->set_enumerated_attribute( 'command_type', 'DB_OPEN' );
	$open_cmd->set_node_ref_attribute( 'command_arg_1', $app_cl );

	# Now create the node that says we will use Rosetta::Engine::Generic as 
	# our data link product to talk to some database.
	my $dlp = make_a_node( 'data_link_product', $model );
	$dlp->set_literal_attribute( 'product_code', 'Rosetta::Engine::Generic' );

	# Now create the model node that says what data storage product we will 
	# use to implement/host the catalog/database, Oracle 9i in this case.
	# The string 'Oracle_9_i' is something that Rosetta::Engine::Generic 
	# and/or its supporting modules specifically recognize.
	my $dsp = make_a_node( 'data_storage_product', $model );
	$dsp->set_literal_attribute( 'product_code', 'Oracle_9_i' );
	$dsp->set_literal_attribute( 'is_network_svc', 1 );

	# Define a database catalog instance, in this case, an Oracle db for testers.
	my $test_db = make_a_node( 'catalog_instance', $model );
	$test_db->set_node_ref_attribute( 'product', $dsp );
	$test_db->set_node_ref_attribute( 'blueprint', $catalog_bp );

	# Now realize the data connection to run from the app instance to the catalog instance.
	# This is where we say that the app running right now (we) will use Rosetta::Engine::Generic.
	# Each application instance may only have 1 realization of the same unrealized link.
	my $test_app_cl = make_a_child_node( 'catalog_link_instance', $test_app, 'application' );
	$test_app_cl->set_node_ref_attribute( 'product', $dlp );
	$test_app_cl->set_node_ref_attribute( 'unrealized', $app_cl );
	$test_app_cl->set_node_ref_attribute( 'target', $test_db );
	$test_app_cl->set_literal_attribute( 'local_dsn', 'test' );

	# Now tell Rosetta to get ready to open a connection to the catalog/database.
	# This is when Perl will actually try to "require" Rosetta::Engine::Generic, 
	# compiling it and DBI, and load the DBD module if possible.
	my $prepared_open_cmd = $application->prepare( $open_cmd );

	# Now we will actually call DBI->connect() and related stuff; this will cause a new 
	# Rosetta Interface to be returned that fronts the resulting DBI database handle.
	# The bind vars 'login_user/pass' are specifically recognized by the DB_OPEN command.
	my $db_conn = $prepared_open_cmd->execute( { 'login_user' => 'jane', 'login_pass' => 'pawd' } );

	# ... Now we do whatever else we wanted to do with the database, such as running queries.

	sub make_a_node {
		my ($node_type, $model) = @_;
		my $node = $model->new_node( $node_type );
		$node->set_node_id( $model->get_next_free_node_id( $node_type ) );
		$node->put_in_container( $model );
		$node->add_reciprocal_links();
		return( $node );
	}

	sub make_a_child_node {
		my ($node_type, $pp_node, $pp_attr) = @_;
		my $container = $pp_node->get_container();
		my $node = $pp_node->new_node( $node_type );
		$node->set_node_id( $container->get_next_free_node_id( $node_type ) );
		$node->put_in_container( $container );
		$node->add_reciprocal_links();
		$node->set_node_ref_attribute( $pp_attr, $pp_node );
		$node->set_parent_node_attribute_name( $pp_attr );
		return( $node );
	}

If $model were dumped right now as an XML string, after running the actual code
above, it would look like this:

	<root>
		<elements />
		<blueprints>
			<application id="1">
				<catalog_link id="1" application="1" name="big_data" target="1" />
				<command id="1" application="1" command_type="DB_OPEN" command_arg_1="1" />
			</application>
			<catalog id="1" />
		</blueprints>
		<tools>
			<data_link_product id="1" product_code="Rosetta::Engine::Generic" />
			<data_storage_product id="1" product_code="Oracle_9_i" is_network_svc="1" />
		</tools>
		<sites>
			<application_instance id="1" blueprint="1">
				<catalog_link_instance id="1" product="1" application="1" unrealized="1" target="1" local_dsn="test" />
			</application_instance>
			<catalog_instance id="1" product="1" blueprint="1" />
		</sites>
		<circumventions />
	</root>

=head1 DESCRIPTION

This module is a reference implementation of fundamental Rosetta features.

The Rosetta::Engine::Generic Perl 5 module is a functional but quickly built
Rosetta Engine that interfaces with a wide variety of SQL databases.  Mainly
this is all databases that have a DBI driver module for them and that support
SQL natively; multi-database DBD modules like DBD::ODBC are supported on equal
terms as single-database ones like DBD::Oracle.  I created this module to be a
"first line of support" so that Rosetta works with a variety of databases as
soon as possible.  

While a better long term solution would probably be to make
a separate Engine for each database, I will leave this up to other people that
have the expertise and desire to make "better" support for each database;
likewise, I leave it up to others to make Engines that don't use a DBI module,
such as one built on Win32::ODBC, or Engines that talk to non-SQL databases
like dBase (?), FoxPro (?) or FileMaker.

Rosetta::Engine::Generic has an external dependency in several
Rosetta::Utility::* modules, which do most of the actual work in SQL generating
(usual task) or parsing; the latter is for some types of schema reverse
engineering.  However, reverse engineering from "information schemas" will
likely be done in Generic itself or a third module, as those are not SQL based.

As with all Rosetta::Engine::* modules, you are not supposed to instantiate
objects of Rosetta::Engine::Generic directly; rather, you use this module
indirectly through the Rosetta::Interface class.  Following this logic, there 
is no class function or method documentation here.

Note that Rosetta::Engine::Generic will always use transactions and require
explicit commits for database actions to be saved; it will also declare its
transaction support in its feature set.  Since transactions are too difficult
to emulate properly, Generic won't try; it simply won't work as expected with
databases that lack native support.  A second Engine named ::GenericAC exists
for non-transactional databases, and declares its lack of support for the
feature.  This Engine sub-classes Generic and should be identical except for
the fact that it conceptually auto-commits every database action; if used with
a transaction-supporting database, it will auto-commit, to emulate the
non-support behaviour.  See Rosetta::Engine::GenericAC for further details.

I<CAVEAT: THIS ENGINE IS "UNDER CONSTRUCTION" AND MANY FEATURES DESCRIBED BY 
SQL::SyntaxModel ARE NOT YET IMPLEMENTED.>

=head1 SUPPORTED ROSETTA FEATURES

Rosetta::Engine::Generic explicitly declares support for the following Rosetta
Native Interface features:

	CONN_BASIC 
	TRAN_BASIC 

	DOMAIN_LIST 
	DOMAIN_DEFN_BASIC

	TABLE_LIST
	TABLE_DEFN_BASIC
	TABLE_UKEY_BASIC TABLE_UKEY_MULTI

	QUERY_BASIC
	QUERY_RETURN_SPEC_COLS QUERY_RETURN_COL_EXPRS
	QUERY_WHERE
	QUERY_JOIN_BASIC QUERY_JOIN_OUTER_LEFT
	QUERY_WINDOW_ORDER

This Engine may contain code that supports additional features, but these have
not been tested at all and so are not yet declared.

=head1 ENGINE CONFIGURATION OPTIONS

The SQL::SyntaxModel objects that comprise Rosetta's inputs have special
compartments for passing configuration options that are only recognizable to
the chosen "data link product", which in Rosetta terms is an Engine.  When you
have a "catalog_link_instance" Node that associates an Engine with your
application's prospective data link, these options are in child
"catalog_link_instance_opt" Nodes of that one.

Rosetta::Engine::Generic recognizes these link instance options:

=over 4

=item

B<local_dsn> - cstr - This is the locally recognized "data source name" of the
database/catalog that you want to connect to.  I<Details or other related
Engine options to be worked out.>

=item

B<login_user> - cstr - This is a database natively recognized "authorization
identifier" or "user name" that your application wants to log-in to the
database as every time it connects.  You typically only set this if the
user-name is hidden from the application user such as if it is stored in a
application configuration file, and the user would not be prompted for a
different one if it fails to work.  If the database user name is provided by
the user, then you typically pass it as a bind variable value at execute() time
instead of storing it in the model.  If you do both, then the execute()
argument will be used instead of the model-provided value.  If you do not
provide this value either in the model or at execute() time, we will assume the
database doesn't require authentication, or we will try to log in anonymously.

=item

B<login_pass> - cstr - This is the database natively recognized "password" that
you provide along with the B<login_user>.  All parts of the above description
for the "user" apply to the "pass" also.

=item

B<dbi_driver> - cstr - Seeing as Rosetta::Engine::Generic is meant to sit on
top of DBI and any of its drivers, this option lets you explicitely pick which
one to use.  If this is not set, then Generic will make an educated guess for
which DBD module to use based on the "data_storage_product" you chose for the
database catalog you are connecting to, or it will fall back to DBD::ODBC.

=item

B<delim_ident> - bool - The SQL standard defines 2 main formats for naming
identifiers (eg table, column names) which we will call "delimited identifiers"
and "bareword identifiers".  Delimited identifiers are case-sensitive and can
contain any character (probably) like a literal string; however, all references
to them must be quoted/delimited, which looks unusual considering their
conceptual equivalence to variable or function or class names in programming
languages.  Bareword identifiers are case-insensitive (and practically fully
uppercase) and can contain only letters, underscores, and partly numbers; SQL
using this format tends to look much cleaner, and this format is also
considered the "normal" way of doing things in SQL, with most databases
supporting it only, if not both.  As delimited identifiers carry more
information (a full superset), that is what Rosetta and SQL::SyntaxModel
support internally.  Movement from a delimited format to a bareword one will
render all alpha characters uppercase and strip the non-allowed characters, and
both steps discard information; movement the other way will keep all
information and match as uppercase.  Rosetta::Engine::Generic will generate SQL
in either format, as determined either by a database product's abilities, or
according to this Engine configuration option.  True (the default) says to use
delimited identifiers, where false says to use bareword ones.  Identifiers are
usually delimited by double-quotes ('"', as distinct from string delimiting
single-quotes), or back-ticks ('`').  I<Feature details subject to change.>

=back

More options will be added, or some will be changed, over time.

=head1 BUGS

This module is currently in pre-alpha development status, meaning that some
parts of it will be changed in the near future, perhaps in incompatible ways.

I am only testing this module against the newest versions of DBI, so if you are 
using older versions then there may be un-anticipated problems.

=head1 SEE ALSO

perl(1), Rosetta, SQL::SyntaxModel, Locale::KeyedText,
Rosetta::Utility::SQLBuilder, DBI.

=cut
