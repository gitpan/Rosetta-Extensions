=head1 NAME

Rosetta::Engine::Generic - A catch-all Engine for any DBI-supported SQL database

=cut

######################################################################

package Rosetta::Engine::Generic;
use 5.006;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = '0.01';

use Locale::KeyedText 0.03;
use SQL::SyntaxModel 0.16;
use Rosetta 0.14;
use DBI;
use Rosetta::Utility::SQLBuilder 0.01;

use base qw( Rosetta::Engine );

######################################################################

=head1 DEPENDENCIES

Perl Version: 5.006

Standard Modules: I<none>

Nonstandard Modules: 

	Locale::KeyedText 0.03 (for error messages)
	SQL::SyntaxModel 0.16
	Rosetta 0.14
	DBI (v1.42 or higher recommended)
	Rosetta::Utility::SQLBuilder 0.01

I<I prefer to simply require that people have DBI v1.42 or later (newest
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
my $PROP_DBI_OBJ = 'dbi_obj'; # Typically either undefined or a 'dbh' or a 'sth' we are wrapping.
my $PROP_SQL_BUILDER = 'sql_builder'; # Assoc with a connection; the SQLBuilder object we use.

# Names of the allowed Interface types go here:
my $INTFTP_ERROR       = 'error'; # What is returned if an error happens, in place of another Intf type
my $INTFTP_TOMBSTONE   = 'tombstone'; # What is returned when execute() destroys an Interface
my $INTFTP_APPLICATION = 'application'; # What you get when you create an Interface out of any context
	# This type is the root of an Interface tree; when you create one, you provide an 
	# "application_instance" SQL::SyntaxModel Node; that provides the necessary context for 
	# subsequent "command" or "routine" Nodes you pass to any child Intf's "prepare" method.
my $INTFTP_PREPARATION = 'preparation'; # That which is returned by the 'prepare()' method
my $INTFTP_ENVIRONMENT = 'environment'; # Parent to all CONNECTION INTFs impl by same engine
my $INTFTP_CONNECTION  = 'connection'; # Result of executing a 'connect' command
my $INTFTP_TRANSACTION = 'transaction'; # Result of asking to start a new transaction
my $INTFTP_CURSOR      = 'cursor'; # Result of executing a query that would return rows to the caller
my $INTFTP_ROW         = 'row'; # Result of executing a query that returns one row
my $INTFTP_LITERAL     = 'literal'; # Result of execution that isn't one of the above, like an IUD

# Names of SQL::SyntaxModel-recognized enumerated values such as Node types go here:
my $SSMNTP_LINKPRD = 'data_link_product';
my $SSMNTP_COMMAND = 'command';
my $SSMNTP_ROUTINE = 'routine';

# These are SQL::SyntaxModel-recognized Command Types this Engine implements now:
my $SSM_CMDTP_DB_OPEN      = 'DB_OPEN'     ; # call on env only
my $SSM_CMDTP_DB_CLOSE     = 'DB_CLOSE'    ; # call on conn only
my $SSM_CMDTP_TRA_OPEN     = 'TRA_OPEN'    ; # call on conn only
my $SSM_CMDTP_TRA_CLOSE    = 'TRA_CLOSE'   ; # call on tra only
my $SSM_CMDTP_TABLE_CREATE = 'TABLE_CREATE'; # call on tra only
my $SSM_CMDTP_TABLE_DELETE = 'TABLE_DELETE'; # call on tra only

######################################################################

sub new {
	my ($class) = @_;
	my $engine = bless( {}, ref($class) || $class );
	$engine->{$PROP_DBI_OBJ} = undef;
	return( $engine );
}

######################################################################

sub destroy {
	my ($engine, $interface) = @_;
	# Assume Interface won't let us be called if child Interfaces (and Engines) exist
	%{$engine} = ();
}

######################################################################

sub prepare {
	my ($engine, $interface, $routine_defn) = @_;
	my $preparation = undef;
	my $intf_type = $interface->get_interface_type();
	my $node_type = $routine_defn->get_node_type();
	if( $node_type eq $SSMNTP_ROUTINE ) {
		unless( $intf_type eq $INTFTP_TRANSACTION ) {
			$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_GEN_RTN', 
				{ 'ITYPE' => $intf_type } );
		}
		$preparation = $engine->prepare_routine( $interface, $routine_defn );
	} elsif( $node_type eq $SSMNTP_COMMAND ) {
		my $cmd_type = $routine_defn->get_enumerated_attribute( 'command_type' );
		if( $intf_type eq $INTFTP_ENVIRONMENT ) {
			# Note that 'environment' is synonymous with 'application', 
			# but an Engine->prepare() should never be called with an 'application' Interface.
			if( $cmd_type eq $SSM_CMDTP_DB_OPEN ) {
				$preparation = $engine->prepare_cmd_db_open( $interface, $routine_defn );
			} else {
				$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_THIS_CMD', 
					{ 'ITYPE' => $intf_type, 'CTYPE' => $cmd_type } );
			}
		} elsif( $intf_type eq $INTFTP_CONNECTION ) {
			if( $cmd_type eq $SSM_CMDTP_DB_CLOSE ) {
				$preparation = $engine->prepare_cmd_db_close( $interface, $routine_defn );
			} elsif( $cmd_type eq $SSM_CMDTP_TRA_OPEN ) {
				$preparation = $engine->prepare_cmd_tra_open( $interface, $routine_defn );
			} else {
				$engine->_throw_error_message( 'ROS_G_PREPARE_INTF_NSUP_THIS_CMD', 
					{ 'ITYPE' => $intf_type, 'CTYPE' => $cmd_type } );
			}
		} elsif( $intf_type eq $INTFTP_TRANSACTION ) {
			if( $cmd_type eq $SSM_CMDTP_TRA_CLOSE ) {
				$preparation = $engine->prepare_cmd_tra_close( $interface, $routine_defn );
			} elsif( $cmd_type eq $SSM_CMDTP_TABLE_CREATE ) {
				$preparation = $engine->prepare_cmd_table_create( $interface, $routine_defn );
			} elsif( $cmd_type eq $SSM_CMDTP_TABLE_DELETE ) {
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

sub clean_up_dbi_driver_string {
	# This code is partly derived from part of DBI->install_driver().
	my ($engine, $driver_name) = @_;
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
		my $dsp_node = $cat_inst_node->get_node_ref_attribute( 'data_storage_product' );
		my $driver_hint = $dsp_node->get_literal_attribute( 'product_code' );
	}

	# This trims the hint to essentials if it is formatted like specific DBI driver strings.
	$driver_hint = $engine->clean_up_dbi_driver_string( $driver_hint );

	# If driver hint is empty because it just contained junk characters before, then use a default.
	$driver_hint = 'ODBC'; # This is the fall-back we use, as stated in module documentation.

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

sub dbi_driver_req_autocommit {
	my ($engine, $dbi_driver, $cat_inst_node) = @_;
	my $auto_commit = 0; # As is the case with most drivers, probably.

#	my $dsp_node = $cat_inst_node->get_node_ref_attribute( 'data_storage_product' );
#	my $storage_product_node = $dsp_node->get_literal_attribute( 'product_code' );

	$dbi_driver =~ m|MySQL|i and $auto_commit = 1; # A haxie; make smart later.

	return( $auto_commit );
}

######################################################################

sub prepare_cmd_db_open {
	my ($env_eng, $env_intf, $command_bp_node) = @_;

	# This block gathers info from SSM which describes db connection we are to open.
	my $container = $command_bp_node->get_container();
	my $cat_link_bp_id = $command_bp_node->get_literal_attribute( 'command_arg' );
	my $cat_link_bp_node = $container->get_node( 'catalog_link', $cat_link_bp_id );
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
	my $auto_commit = $env_eng->dbi_driver_req_autocommit( $dbi_driver, $cat_inst_node ); # usually 0

	my $builder = $env_eng->{$PROP_SQL_BUILDER} = Rosetta::Utility::SQLBuilder->new();
	$cat_link_inst_opts{'insen_ident'} and 
		$builder->insensitive_identifiers( $cat_link_inst_opts{'insen_ident'} );

	my $conn_prep_eng = $env_eng->new();

	my $routine = sub {
		# This routine is a closure.
		my ($rtv_prep_eng, $rtv_prep_intf, $rtv_args) = @_;
		$rtv_args->{'local_user'} and $login_user = $rtv_args->{'local_user'};
		$rtv_args->{'local_pass'} and $login_pass = $rtv_args->{'local_pass'};

		my $dbi_dbh = DBI->connect( 
			"DBI:".$dbi_driver.":".$local_dsn,
			$login_user,
			$login_pass,
			{ RaiseError => 1, AutoCommit => $auto_commit },
		);

		my $conn_eng = $rtv_prep_eng->new();
		$conn_eng->{$PROP_DBI_OBJ} = $dbi_dbh;

		my $conn_intf = $rtv_prep_intf->new( $INTFTP_CONNECTION, undef, 
			$rtv_prep_intf, $conn_eng, $command_bp_node );
		return( $conn_intf );
	};

	my $conn_prep_intf = $env_intf->new( $INTFTP_PREPARATION, undef, 
		$env_intf, $conn_prep_eng, $command_bp_node, $routine );
	return( $conn_prep_intf );
}

######################################################################

sub prepare_cmd_db_close {
	my ($conn_eng, $conn_intf, $routine_defn) = @_;

	my $routine = sub {
		# This routine is a closure.
		my ($rtv_prep_eng, $rtv_prep_intf, undef) = @_;
		my $rtv_siblings = $conn_intf->get_child_interfaces();
		if( @{$rtv_siblings} > 1 or $rtv_siblings->[0] ne $conn_eng ) {
			$rtv_prep_eng->_throw_error_message( 'ROS_G_CMD_DB_CLOSE_CONN_IN_USE' );
		}
		$conn_eng->{$PROP_DBI_OBJ}->disconnect();
		my $tombstone = $rtv_prep_intf->new( $INTFTP_TOMBSTONE );
		$rtv_prep_intf->destroy();
		$conn_intf->destroy(); # removes last ref to DBI dbh object
		return( $conn_intf );
	};

	my $prep_eng = $conn_eng->new();
	my $prep_intf = $conn_intf->new( $INTFTP_PREPARATION, undef, 
		$conn_intf, $prep_eng, $routine_defn, $routine );
	return( $prep_intf );
}

######################################################################

sub finalize {
	my ($engine, $interface) = @_;
	# Now DO something.
}

######################################################################

sub has_more_rows {
	my ($engine, $interface) = @_;
	# Now DO something.
	return( 0 );
}

######################################################################

sub fetch_row {
	my ($engine, $interface) = @_;
	# Now DO something.
	return( {} );
}

######################################################################

sub fetch_all_rows {
	my ($engine, $interface) = @_;
	# Now DO something.
	my @rows = ();
	while( $engine->has_more_rows() ) {
		push( @rows, $engine->fetch_row() );
	}
	$engine->finalize();
	return( \@rows );
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
	my $app_bp = make_a_node( 'application', 1, $model );

	# Define an application instance, for testers, which is the app running right now.
	my $test_app = make_a_node( 'application_instance', 1, $model );
	$test_app->set_node_ref_attribute( 'blueprint', $app_bp );

	# Now create a new Rosetta Interface tree that will mediate db access for us.
	my $application = Rosetta->new_application( $test_app );

	# Now create the model root node that represents a database/catalog we will 
	# be using (may be several), and a node belonging to said app representing a 
	# yet-unrealized data connection from the app to the db.
	my $catalog_bp = make_a_node( 'catalog', 1, $model );
	my $app_cl = make_a_child_node( 'catalog_link', 1, $app_bp, 'application' );
	$app_cl->set_literal_attribute( 'name', 'big_data' );
	$app_cl->set_node_ref_attribute( 'target', $catalog_bp );

	# ... Next, probably (or you can do it later), make and stuff a whole bunch 
	# of nodes in the model that describe the database/catalog schema and any 
	# application-stored queries or routines that would run against the db.

	# As an example of the above, a command to 'connect' to the database.
	my $open_cmd = make_a_child_node( 'command', 1, $app_bp, 'application' );
	$open_cmd->set_enumerated_attribute( 'command_type', 'DB_OPEN' );
	$open_cmd->set_literal_attribute( 'command_arg', 1 ); # id num of $app_cl

	# Now create the node that says we will use Rosetta::Engine::Generic as 
	# our data link product to talk to some database.
	my $dlp = make_a_node( 'data_link_product', 1, $model );
	$dlp->set_literal_attribute( 'product_code', 'Rosetta::Engine::Generic' );

	# Now create the model node that says what data storage product we will 
	# use to implement/host the catalog/database, Oracle 9i in this case.
	# The string 'Oracle_9_i' is something that Rosetta::Engine::Generic 
	# and/or its supporting modules specifically recognize.
	my $dsp = make_a_node( 'data_storage_product', 1, $model );
	$dsp->set_literal_attribute( 'product_code', 'Oracle_9_i' );
	$dsp->set_literal_attribute( 'is_network_svc', 1 );

	# Define a database catalog instance, in this case, an Oracle db for testers.
	my $test_db = make_a_node( 'catalog_instance', 1, $model );
	$test_db->set_node_ref_attribute( 'product', $dsp );
	$test_db->set_node_ref_attribute( 'blueprint', $catalog_bp );

	# Now realize the data connection to run from the app instance to the catalog instance.
	# This is where we say that the app running right now (we) will use Rosetta::Engine::Generic.
	# Each application instance may only have 1 realization of the same unrealized link.
	my $test_app_cl = make_a_child_node( 'catalog_link_instance', 1, $test_app, 'application' );
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
		my ($node_type, $node_id, $model) = @_;
		my $node = $model->new_node( $node_type );
		$node->set_node_id( $node_id );
		$node->put_in_container( $model );
		$node->add_reciprocal_links();
		return( $node );
	}

	sub make_a_child_node {
		my ($node_type, $node_id, $pp_node, $pp_attr) = @_;
		my $node = $pp_node->new_node( $node_type );
		$node->set_node_id( $node_id );
		$node->put_in_container( $pp_node->get_container() );
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
				<command id="1" application="1" command_type="DB_OPEN" command_arg="1" />
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

I<CAVEAT: THIS ENGINE IS "UNDER CONSTRUCTION" AND MANY FEATURES DESCRIBED BY 
SQL::SyntaxModel ARE NOT YET IMPLEMENTED.>

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

B<insen_ident> - bool - The SQL standard says that the language keywords and
identifiers (eg table, column names) are case-insensitive, the latter also
containing only letters, underscores, and partly numbers; such identifiers
usually appear in SQL as bare-words.  Whether standard or not, some databases
do allow you to have case-sensitive identifiers and/or identifiers with unusual
characters, if all SQL references to them are quoted in SQL such as with
double-quotes ('"', as distinct from string delimiting single-quotes), or
back-ticks ('`').  As the latter carries more information, that is what Rosetta
and SQL::SyntaxModel support by default; however, if this flag is set to true,
then all identifiers will be coerced to uppercase and get used as bare-words,
which discards information.  Movement from a case-insensitive environment to a
sensitive one will render bareword identifiers fully uppercased, as seems the
standard.  I<The details and name of this feature still need to be worked out.>

=back

More options will be added, or some will be changed, over time.

=head1 BUGS

This module is currently in pre-alpha development status, meaning that some
parts of it will be changed in the near future, perhaps in incompatible ways.

I am only testing this module against the newest versions of DBI, so if you are 
using older versions then there may be un-anticipated problems.

=head1 SEE ALSO

perl(1), Rosetta, SQL::SyntaxModel, DBI.

=cut
