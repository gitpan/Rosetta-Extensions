#!perl

use 5.008001; use utf8; use strict; use warnings;

package Rosetta::Engine::Generic;
our $VERSION = '0.11';

use Rosetta '0.40';
use DBI '1.46';
use Rosetta::Utility::SQLBuilder '0.14';

use base qw( Rosetta::Engine );

######################################################################

=encoding utf8

=head1 NAME

Rosetta::Engine::Generic - A catch-all Engine for any DBI-supported SQL database

=head1 DEPENDENCIES

Perl Version: 5.008001

Standard Modules: I<none>

Nonstandard Modules: 

	Rosetta 0.40
	DBI 1.46 (highest version recommended)
	Rosetta::Utility::SQLBuilder 0.14

=head1 COPYRIGHT AND LICENSE

This file is part of the Rosetta::Extensions collection of Rosetta feature
reference implementations.

Rosetta::Extensions is Copyright (c) 1999-2004, Darren R. Duncan.  All rights
reserved.  Address comments, suggestions, and bug reports to
B<perl@DarrenDuncan.net>, or visit "http://www.DarrenDuncan.net" for more
information.

Rosetta::Extensions is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License (GPL) version 2 as published
by the Free Software Foundation (http://www.fsf.org/).  You should have
received a copy of the GPL as part of the Rosetta::Extensions distribution, in
the file named "LICENSE"; if not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

Linking Rosetta::Extensions statically or dynamically with other modules is
making a combined work based on Rosetta::Extensions.  Thus, the terms and
conditions of the GPL cover the whole combination.  As a special exception, the
copyright holders of Rosetta::Extensions give you permission to link
Rosetta::Extensions with independent modules, regardless of the license terms
of these independent modules, and to copy and distribute the resulting combined
work under terms of your choice, provided that every copy of the combined work
is accompanied by a complete copy of the source code of Rosetta::Extensions
(the version of Rosetta::Extensions used to produce the combined work), being
distributed under the terms of the GPL plus this exception.  An independent
module is a module which is not derived from or based on Rosetta::Extensions,
and which is fully useable when not linked to Rosetta::Extensions in any form.

Any versions of Rosetta::Extensions that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits.  Rosetta::Extensions is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

While it is by no means required, the copyright holders of Rosetta::Extensions
would appreciate being informed any time you create a modified version of
Rosetta::Extensions that you are willing to distribute, because that is a
practical way of suggesting improvements to the standard version.

=cut

######################################################################
######################################################################

# Names of properties for objects of the Rosetta::Engine::Generic class are declared here:
my $PROP_IN_PROGRESS_PREP_ENG = 'in_progress_prep_eng'; # Generic object - 
	# This stores a brand new $prep_eng while it is being constructed (rather than 
	# passing it internally as an argument).  Our internal routines can set properties 
	# in it while the $routine is being constructed, so that the compiled $routine 
	# can later reference them.  This is set and cleared by prepare(); 
	# it must always be undefined when prepare() of the current object isn't executing.
my $PROP_ENV_PERL_RTNS = 'env_perl_rtns'; # hash (str,code) - For Environment Intfs - 
	# This stores all of the SRT routines that were compiled into Perl code.
	# Hash keys are unique generated ids that incorporate the name-space hierarchy of a routine.
	# Hash values are Perl CODE refs, the result of "$r = sub { ... };"
	# One purpose this property serves is to be a cache so that if the same SRT routine is 
	# invoked multiple times, each additional call doesn't have the compile overhead.
	# TODO: Make sure this works properly with closed and re-opened connections, 
	# as old DBI $sth's created during the compile phase probably wouldn't be valid anymore.
	# Another purpose this property serves is to make it easy for multiple routines to 
	# invoke each other, including recursively.  
	# Each 'routine' SRT Node becomes a single element value in this hash-list property.
my $PROP_ENV_PERL_RTN_STRS = 'env_perl_rtn_strs'; # hash (str,code) - For Environment Intfs - 
	# The Perl source code strs that compiled to the above CODE refs; this prop is for 
	# debugging use; for now, it only has source that compiled successfully; 
	# source for unsuccessful calls is included with the Message object thrown on that failure.
my $PROP_CONN_PREP_ECO = 'conn_prep_eco'; # hash (str,lit) - For Connection Prep Intfs - 
	# These are the Engine Config Opts that apply to all Connection Intfs made from this 
	# Conn Prep; they will override any Conn's execute(ROUTINE_ARGS) values.
	# They are copied here from the SRT Nodes partly for speed and partly to 
	# prevent tampering during an open Connection due to modifying the original SRT.
	# Note: At this moment there is no $PROP_ENV*ECO, as that would seem to be 
	# counter-productive; eg, conflicting Env|Conn.features() values.
my $PROP_CONN_IS_OPEN = 'conn_is_open'; # boolean - 
	# Whether this is set to 1 or not determines whether the Conn state is open or closed.
my $PROP_CONN_ECO = 'conn_eco'; # hash (str,lit) - For Connection Intfs - 
	# This is like the previous property, but it incorporates any given ROUTINE_ARGS 
	# when making this specific Connection.
	# This property is set when a Connection context is set to an open state; 
	# this property is cleared when a Connection context is set to a closed state.
	# Note: One of these opts says whether this Connection will auto-commit or not; 
	# it is the definitive source for whether Connection.features() will 
	# declare support for the TRAN_BASIC feature or not (decl only at conn resolution, not env).
my $PROP_CONN_DBH_OBJ = 'conn_dbh_obj'; # DBI $dbh object - For Connection Intfs - 
	# This is the DBI-implemented "database handle" that we are using behind the scenes.
	# It is setup using up to 5 of the conn_eco values (dsn, user, pass, driver, auto).
my $PROP_CONN_SQL_BUILDER = 'conn_sql_builder'; # SQLBuilder object - For Connection Intfs - 
	# This is used to generate all the SQL that will be sent through the Connection.
my $PROP_LIT_PREP_STH_OBJ = 'lit_prep_sth_obj'; # DBI $sth object - For Literal Prep Intfs - 
	# This is the DBI-implemented "prepared statement handle" that we are using behind the scenes.
my $PROP_LIT_PAYLOAD = 'lit_payload'; # lit|ref|obj - For Literal Intfs - 
	# This is the payload that the Literal Intf represents.
my $PROP_CURS_PREP_STH_OBJ = 'curs_prep_sth_obj'; # DBI $sth object - For Cursor Prep Intfs - 
	# This is the DBI-implemented "prepared statement handle" that we are using behind the scenes.

# Names of Rosetta::Engine::Generic Engine Configuration Options go here:
my $ECO_LOCAL_DSN   = 'local_dsn';
my $ECO_LOGIN_NAME  = 'login_name';
my $ECO_LOGIN_PASS  = 'login_pass';
my $ECO_DBI_DRIVER  = 'dbi_driver';
my $ECO_AUTO_COMMIT = 'auto_commit';
my $ECO_DELIM_IDENT = 'delim_ident';

# Declarations of feature support at Rosetta::Engine::Generic Environment level:
my %FEATURES_SUPP_BY_ENV = (
	'CATALOG_LIST' => 1,
	'CATALOG_INFO' => 0,
	'CONN_BASIC' => 1,
	'CONN_MULTI_SAME' => 1,
	'CONN_MULTI_DIFF' => 1,
	'CONN_PING' => 0,
	'TRAN_MULTI_SIB' => 0,
	'TRAN_MULTI_CHILD' => 0,
	'USER_LIST' => 0,
	'USER_INFO' => 0,
	'SCHEMA_LIST' => 0,
	'SCHEMA_INFO' => 0,
	'DOMAIN_LIST' => 0,
	'DOMAIN_INFO' => 0,
	'DOMAIN_DEFN_VERIFY' => 0,
	'DOMAIN_DEFN_BASIC' => 0,
	'TABLE_LIST' => 0,
	'TABLE_INFO' => 0,
	'TABLE_DEFN_VERIFY' => 0,
	'TABLE_DEFN_BASIC' => 0,
	'TABLE_UKEY_BASIC' => 0,
	'TABLE_UKEY_MULTI' => 0,
	'TABLE_FKEY_BASIC' => 0,
	'TABLE_FKEY_MULTI' => 0,
	'QUERY_BASIC' => 0,
	'QUERY_SCHEMA_VIEW' => 0,
	'QUERY_RETURN_SPEC_COLS' => 0,
	'QUERY_RETURN_COL_EXPRS' => 0,
	'QUERY_WHERE' => 0,
	'QUERY_COMPARE_PRED' => 0,
	'QUERY_BOOLEAN_EXPR' => 0,
	'QUERY_NUMERIC_EXPR' => 0,
	'QUERY_STRING_EXPR' => 0,
	'QUERY_LIKE_PRED' => 0,
	'QUERY_JOIN_BASIC' => 0,
	'QUERY_JOIN_OUTER_LEFT' => 0,
	'QUERY_JOIN_ALL' => 0,
	'QUERY_GROUP_BY_NONE' => 0,
	'QUERY_GROUP_BY_SOME' => 0,
	'QUERY_AGG_CONCAT' => 0,
	'QUERY_AGG_EXIST' => 0,
	'QUERY_OLAP' => 0,
	'QUERY_HAVING' => 0,
	'QUERY_WINDOW_ORDER' => 0,
	'QUERY_WINDOW_LIMIT' => 0,
	'QUERY_COMPOUND' => 0,
	'QUERY_SUBQUERY' => 0,
);
# These features are conditionally supported at the Connection level:
	# TRAN_BASIC
	# TRAN_ROLLBACK_ON_DEATH
	# See the features() method for the conditions.

######################################################################

sub _throw_error_message {
	# This overrides the same-named method of 'Rosetta'.
	my ($engine, $error_code, $args) = @_;
	ref($args) eq 'HASH' or $args = {};
	if( my $routine_node = $args->{'RNAME'} ) {
		$args->{'RNAME'} = $engine->build_perl_identifier_rtn( $routine_node, 1 );
	}
	$engine->SUPER::_throw_error_message( $error_code, $args );
}

######################################################################

sub new {
	my ($class) = @_;
	my $engine = bless( {}, ref($class) || $class );
	$engine->{$PROP_IN_PROGRESS_PREP_ENG} = undef;
	$engine->{$PROP_ENV_PERL_RTNS} = undef;
	$engine->{$PROP_ENV_PERL_RTN_STRS} = undef;
	$engine->{$PROP_CONN_PREP_ECO} = undef;
	$engine->{$PROP_CONN_IS_OPEN} = undef;
	$engine->{$PROP_CONN_ECO} = undef;
	$engine->{$PROP_CONN_DBH_OBJ} = undef;
	$engine->{$PROP_CONN_SQL_BUILDER} = undef;
	$engine->{$PROP_LIT_PREP_STH_OBJ} = undef;
	$engine->{$PROP_LIT_PAYLOAD} = undef;
	$engine->{$PROP_CURS_PREP_STH_OBJ} = undef;
	return( $engine );
}

######################################################################

sub destroy {
	my ($engine, $interface) = @_;
	if( $engine->{$PROP_CONN_IS_OPEN} ) {
		$engine->close_dbi_connection( $engine->{$PROP_CONN_DBH_OBJ},
			$engine->{$PROP_CONN_ECO}->{$ECO_AUTO_COMMIT} );
	}
	# Assume Interface won't let us be called if child Interfaces (and Engines) exist
	%{$engine} = ();
}

sub DESTROY {
	my ($engine) = @_;
	if( $engine->{$PROP_CONN_IS_OPEN} ) {
		$engine->close_dbi_connection( $engine->{$PROP_CONN_DBH_OBJ},
			$engine->{$PROP_CONN_ECO}->{$ECO_AUTO_COMMIT} );
	}
	%{$engine} = ();
}

######################################################################

sub features {
	my ($engine, $interface, $feature_name) = @_;
	my %feature_list = %FEATURES_SUPP_BY_ENV;
	if( $interface->get_interface_type() eq $Rosetta::INTFTP_CONNECTION ) {
		if( $engine->{$PROP_CONN_ECO}->{$ECO_AUTO_COMMIT} ) {
			$feature_list{'TRAN_BASIC'} = 0;
		} # else TRAN_BASIC missing since don't know yet.
		if( $engine->{$PROP_CONN_IS_OPEN} ) {
			# If we get here, Conn is in open state; more info available.
			if( $engine->{$PROP_CONN_ECO}->{$ECO_AUTO_COMMIT} ) {
				$feature_list{'TRAN_BASIC'} = 0;
			} else {
				# Now query the db to know whether TRAN_BASIC is supported or not.
			}
			# Now query the db to know whether TRAN_ROLLBACK_ON_DEATH is supported or not.
		} else {} # Conn is in closed state; less info available.
	} else {} # Intf Type is Environment.
	return( defined($feature_name) ? $feature_list{$feature_name} : \%feature_list );
}

######################################################################

sub prepare {
	# Assume we only get called off of Environment and Connection and Cursor interfaces.
	my ($engine, $interface, $routine_node) = @_;
	if( $interface->get_interface_type() eq $Rosetta::INTFTP_ENVIRONMENT ) {
		$engine->{$PROP_ENV_PERL_RTNS} ||= {}; # This couldn't have been done at Env Eng creation time.
		$engine->{$PROP_ENV_PERL_RTN_STRS} ||= {}; # Ditto.
	}

	if( $routine_node->get_primary_parent_attribute()->get_node_type() eq 'routine' ) {
		# Only externally visible routines can be externally invoked; nested routines are not.
		$engine->_throw_error_message( 'ROS_G_NEST_RTN_NO_INVOK', { 'RNAME' => $routine_node } );
	}

	my $prep_eng = $engine->new();

	$engine->{$PROP_IN_PROGRESS_PREP_ENG} = $prep_eng;
	my $routine = eval {
		return( $engine->build_perl_routine( $interface, $routine_node ) );
	};
	$engine->{$PROP_IN_PROGRESS_PREP_ENG} = undef; # must be empty before we exit
	$@ and die $@;

	my $prep_intf = $interface->new( $Rosetta::INTFTP_PREPARATION, undef, 
		$interface, $prep_eng, $routine_node, $routine );
	return( $prep_intf );
}

######################################################################

sub payload {
	my ($lit_eng, $lit_intf) = @_;
	return( $lit_eng->{$PROP_LIT_PAYLOAD} );
}

######################################################################

sub routine_source_code {
	my ($env_eng, $env_intf, $routine_node) = @_;
	my $routine_name = $env_eng->build_perl_identifier_rtn( $routine_node );
	return( $env_eng->{$PROP_ENV_PERL_RTN_STRS}->{$routine_name} ); # undef if none
}

######################################################################

sub build_perl_routine {
	my ($engine, $interface, $routine_node) = @_;
	my ($env_eng, $env_intf) = $engine->get_env_cx_e_and_i( $interface );

	my $routine_name = $engine->build_perl_identifier_rtn( $routine_node );
	my $routine_name_debug = $engine->build_perl_identifier_rtn( $routine_node, 1 );

	if( my $routine = $env_eng->{$PROP_ENV_PERL_RTNS}->{$routine_name} ) {
		return( $routine ); # This routine was compiled previously; use that one.
	}

	my $routine_type = $routine_node->get_enumerated_attribute( 'routine_type' );
	unless( $routine_type eq 'FUNCTION' or $routine_type eq 'PROCEDURE' ) {
		# You can not directly invoke a trigger or other non-func/proc
		$engine->_throw_error_message( 'ROS_G_RTN_TP_NO_INVOK', 
			{ 'RNAME' => $routine_node, 'RTYPE' => $routine_type } );
	}

	my $routine_str = <<__EOL; # This routine is a closure.
return( sub {
	# routine: "$routine_name_debug".
	my (\$rtv_prep_eng, \$rtv_prep_intf, \$rtv_args) = \@_;
__EOL

	if( my $routine_cxt_node = $routine_node->get_child_nodes( 'routine_context' )->[0] ) {
		my $routine_cxt_name = $engine->build_perl_identifier_rtn_var( $routine_cxt_node );
		my $routine_cxt_name_eng = $routine_cxt_name.'_eng';
		my $routine_cxt_name_debug = $engine->build_perl_identifier_rtn_var( $routine_cxt_node, 1 );
		my $cont_type = $routine_cxt_node->get_enumerated_attribute( 'cont_type' );
		if( $cont_type eq 'CONN' ) {
			$routine_str .= <<__EOL;
	my ($routine_cxt_name_eng, $routine_cxt_name) = # routine_cxt: $routine_cxt_name_debug
		\$rtv_prep_eng->get_conn_cx_e_and_i( \$rtv_prep_intf );
__EOL
		} elsif( $cont_type eq 'CURSOR' ) {
			$routine_str .= <<__EOL;
	my (undef, $routine_cxt_name) = # routine_cxt: $routine_cxt_name_debug
		\$rtv_prep_eng->get_curs_cx_e_and_i( \$rtv_prep_intf );
__EOL
		} else {}
	}

	foreach my $routine_arg_node (@{$routine_node->get_child_nodes( 'routine_arg' )}) {
		my $routine_arg_name = $engine->build_perl_identifier_rtn_var( $routine_arg_node );
		my $routine_arg_name_debug = $engine->build_perl_identifier_rtn_var( $routine_arg_node, 1 );
		my $routine_arg_name_cstr = $engine->build_perl_literal_cstr_from_atvl( $routine_arg_node );
		$routine_str .= <<__EOL;
	my $routine_arg_name = \$rtv_args->{$routine_arg_name_cstr}; # routine_arg: $routine_arg_name_debug
__EOL
	}

	if( $routine_node->get_primary_parent_attribute()->get_node_type() eq 'schema' ) {
		# This new Perl routine is to CALL a database stored procedure/function.
#		$routine_str .= $engine->build_perl_call_to_db_rtn( $interface, $routine_node );
	} else {
		# This new Perl routine is an application-side routine, either externally visible or nested.
		$routine_str .= $engine->build_perl_routine_body( $interface, $routine_node );
	}

	if( $routine_type eq 'PROCEDURE' ) {
		# All procedures conceptually return nothing, actually return SUCCESS when ok.
		$routine_str .= <<__EOL;
	return( \$rtv_prep_intf->new( \$Rosetta::INTFTP_SUCCESS ) );
__EOL
	}

	$routine_str .= <<__EOL;
} );
__EOL

	if( my $trace_fh = $interface->get_trace_fh() ) {
		my $class = ref($engine);
		print $trace_fh "$class built a new routine whose code is:\n----------\n$routine_str\n----------\n";
	}

	my $routine = eval $routine_str;
	if( $@ ) {
		$engine->_throw_error_message( 'ROS_G_PERL_COMPILE_FAIL', 
			{ 'RNAME' => $routine_node, 'PERL_ERROR' => $@, 'PERL_CODE' => $routine_str } );
	}

	$env_eng->{$PROP_ENV_PERL_RTNS}->{$routine_name} = $routine;
	$env_eng->{$PROP_ENV_PERL_RTN_STRS}->{$routine_name} = $routine_str;
	return( $routine ); # This routine is now compiled for the first time.
}

######################################################################

sub build_perl_routine_body {
	my ($engine, $interface, $routine_node) = @_;
	my $routine_str = '';

	foreach my $routine_var_node (@{$routine_node->get_child_nodes( 'routine_var' )}) {
		my $routine_var_name = $engine->build_perl_identifier_rtn_var( $routine_var_node );
		my $routine_var_name_debug = $engine->build_perl_identifier_rtn_var( $routine_var_node, 1 );
		$routine_str .= <<__EOL;
	my $routine_var_name = undef; # routine_var: $routine_var_name_debug
__EOL
		my $cont_type = $routine_var_node->get_enumerated_attribute( 'cont_type' );
		if( $cont_type eq 'ERROR' ) {
		} elsif( $cont_type eq 'SCALAR' ) {
			my $init_val = $engine->build_perl_literal_cstr_from_atvl( $routine_var_node, 'init_lit_val' );
			$routine_str .= <<__EOL;
	$routine_var_name = $init_val;
__EOL
		} elsif( $cont_type eq 'ROW' ) {
		} elsif( $cont_type eq 'SC_ARY' ) {
		} elsif( $cont_type eq 'RW_ARY' ) {
		} elsif( $cont_type eq 'CONN' ) {
			$routine_str .= $engine->build_perl_declare_cx_conn( $interface, $routine_node, $routine_var_node );
		} elsif( $cont_type eq 'CURSOR' ) {
			$routine_str .= $engine->build_perl_declare_cx_cursor( $interface, $routine_node, $routine_var_node );
		} elsif( $cont_type eq 'LIST' ) {
		} elsif( $cont_type eq 'SRT_NODE' ) {
		} elsif( $cont_type eq 'SRT_NODE_LIST' ) {
		} else {}
	}

	foreach my $routine_stmt_node (@{$routine_node->get_child_nodes( 'routine_stmt' )}) {
		$routine_str .= $engine->build_perl_stmt( $interface, $routine_node, $routine_stmt_node );
	}

	return( $routine_str );
}

######################################################################

sub build_perl_stmt {
	my ($engine, $interface, $routine_node, $routine_stmt_node) = @_;
	if( my $compound_stmt_routine = $routine_stmt_node->get_node_ref_attribute( 'block_routine' ) ) {
		# Not implemented yet.
	} elsif( my $assign_dest_node = $routine_stmt_node->get_node_ref_attribute( 'assign_dest' ) || 
			$routine_stmt_node->get_node_ref_attribute( 'assign_dest' ) ) {
		my $assign_dest_name = $engine->build_perl_identifier_rtn_var( $assign_dest_node );
		my $expr_str = $engine->build_perl_expr( $interface, $routine_node, 
			$routine_stmt_node->get_child_nodes( 'routine_expr' )->[0] );
		return( <<__EOL );
	$assign_dest_name = $expr_str;
__EOL
	} elsif( $routine_stmt_node->get_enumerated_attribute( 'call_sroutine' ) ) {
		return( $engine->build_perl_stmt_srtn( $interface, $routine_node, $routine_stmt_node ) );
	} elsif( $routine_stmt_node->get_node_ref_attribute( 'call_uroutine' ) ) {
		return( $engine->build_perl_stmt_urtn( $interface, $routine_node, $routine_stmt_node ) );
	} else {}
}

######################################################################

sub build_perl_stmt_srtn {
	my ($engine, $interface, $routine_node, $routine_stmt_node) = @_;
	my $sroutine = $routine_stmt_node->get_enumerated_attribute( 'call_sroutine' );
	my %child_cxt_exprs = 
		map { ($_->get_enumerated_attribute( 'call_sroutine_cxt' ) => $_) } 
		grep { $_->get_enumerated_attribute( 'call_sroutine_cxt' ) }
		@{$routine_stmt_node->get_child_nodes()};
	my %child_arg_exprs = 
		map { ($_->get_enumerated_attribute( 'call_sroutine_arg' ) => $_) } 
		grep { $_->get_enumerated_attribute( 'call_sroutine_arg' ) }
		@{$routine_stmt_node->get_child_nodes()};
	if( $sroutine eq 'CATALOG_OPEN' ) {
		my $conn_cx = $engine->build_perl_expr( $interface, $routine_node, $child_cxt_exprs{'CONN_CX'} );
		my $login_name = $engine->build_perl_expr( $interface, $routine_node, $child_arg_exprs{'LOGIN_NAME'} );
		my $login_pass = $engine->build_perl_expr( $interface, $routine_node, $child_arg_exprs{'LOGIN_PASS'} );
		return( <<__EOL );
	\$rtv_prep_eng->srtn_catalog_open( \$rtv_prep_intf, { 'CONN_CX' => $conn_cx, 
		'LOGIN_NAME' => $login_name, 'LOGIN_PASS' => $login_pass } );
__EOL
	} elsif( $sroutine eq 'CATALOG_CLOSE' ) {
		my $conn_cx = $engine->build_perl_expr( $interface, $routine_node, $child_cxt_exprs{'CONN_CX'} );
		return( <<__EOL );
	\$rtv_prep_eng->srtn_catalog_close( \$rtv_prep_intf, { 'CONN_CX' => $conn_cx } );
__EOL
	} elsif( $sroutine eq 'RETURN' ) {
		my $return_value = $engine->build_perl_expr( $interface, $routine_node, $child_arg_exprs{'RETURN_VALUE'} );
		return( <<__EOL );
	return( $return_value );
__EOL
	} else {}
	$engine->_throw_error_message( 'ROS_G_STD_RTN_NO_IMPL', 
		{ 'RNAME' => $routine_node, 'SRNAME' => $sroutine } );
}

######################################################################

sub build_perl_stmt_urtn {
	my ($engine, $interface, $routine_node, $routine_stmt_node) = @_;
	return( '' );
}

######################################################################

sub build_perl_expr {
	my ($engine, $interface, $routine_node, $expr_node) = @_;
	unless( $expr_node ) {
		return( 'undef' );
	}
	my $cont_type = $expr_node->get_enumerated_attribute( 'cont_type' );
	if( $cont_type eq 'LIST' ) {
		return( '('.join( ', ', map { $engine->build_perl_expr( $interface, $routine_node, $_ ) } 
			@{$expr_node->get_child_nodes()} ).')' );
	} else {
		if( my $valf_literal = $expr_node->get_literal_attribute( 'valf_literal' ) ) {
			#my $domain_node = $expr_node->get_node_ref_attribute( 'scalar_data_type' );
			return( $engine->build_perl_literal_cstr( $valf_literal ) );
		} elsif( my $routine_item_node = $expr_node->get_node_ref_attribute( 'valf_p_routine_item' ) ) {
			return( $engine->build_perl_identifier_rtn_var( $routine_item_node ) );
		} elsif( $expr_node->get_enumerated_attribute( 'valf_call_sroutine' ) ) {
			return( $engine->build_perl_expr_srtn( $interface, $routine_node, $expr_node ) );
		} elsif( $expr_node->get_node_ref_attribute( 'valf_call_uroutine' ) ) {
			return( $engine->build_perl_expr_urtn( $interface, $routine_node, $expr_node ) );
		} else {}
	}
}

######################################################################

sub build_perl_expr_srtn {
	my ($engine, $interface, $routine_node, $routine_expr_node) = @_;
	my $sroutine = $routine_expr_node->get_enumerated_attribute( 'valf_call_sroutine' );
	my %child_cxt_exprs = 
		map { ($_->get_enumerated_attribute( 'call_sroutine_cxt' ) => $_) } 
		grep { $_->get_enumerated_attribute( 'call_sroutine_cxt' ) }
		@{$routine_expr_node->get_child_nodes()};
	my %child_arg_exprs = 
		map { ($_->get_enumerated_attribute( 'call_sroutine_arg' ) => $_) } 
		grep { $_->get_enumerated_attribute( 'call_sroutine_arg' ) }
		@{$routine_expr_node->get_child_nodes()};
	if( $sroutine eq 'CATALOG_LIST' ) {
		my $recursive = $engine->build_perl_expr( $interface, $routine_node, $child_arg_exprs{'RECURSIVE'} );
		return( "\$rtv_prep_eng->srtn_catalog_list( \$rtv_prep_intf, { 'RECURSIVE' => $recursive } )" );
	} else {}
	$engine->_throw_error_message( 'ROS_G_STD_RTN_NO_IMPL', 
		{ 'RNAME' => $routine_node, 'SRNAME' => $sroutine } );
}

######################################################################

sub build_perl_expr_urtn {
	my ($engine, $interface, $routine_node, $routine_stmt_node) = @_;
	return( '' );
}

######################################################################

sub get_env_cx_e_and_i {
	my ($engine, $interface) = @_;
	my $intf_type = $interface->get_interface_type();
	if( $intf_type eq $Rosetta::INTFTP_PREPARATION ) {
		my $p_intf = $interface->get_parent_interface();
		my $p_eng = $p_intf->get_engine();
		return( $p_eng->get_env_cx_e_and_i( $p_intf ) );
	}
	if( $intf_type eq $Rosetta::INTFTP_ENVIRONMENT ) {
		return( $engine, $interface );
	}
	if( $intf_type eq $Rosetta::INTFTP_CONNECTION ) {
		my $env_intf = $interface->get_parent_interface()->get_parent_interface();
		my $env_eng = $env_intf->get_engine();
		return( $env_eng, $env_intf );
	}
	if( $intf_type eq $Rosetta::INTFTP_CURSOR ) {
		my $conn_intf = $interface->get_parent_interface()->get_parent_interface();
		my $env_intf = $conn_intf->get_parent_interface()->get_parent_interface();
		my $env_eng = $env_intf->get_engine();
		return( $env_eng, $env_intf );
	}
	# We should never get here.
}

sub get_conn_cx_e_and_i {
	my ($engine, $interface) = @_;
	my $intf_type = $interface->get_interface_type();
	if( $intf_type eq $Rosetta::INTFTP_PREPARATION ) {
		my $p_intf = $interface->get_parent_interface();
		my $p_eng = $p_intf->get_engine();
		return( $p_eng->get_conn_cx_e_and_i( $p_intf ) );
	}
	if( $intf_type eq $Rosetta::INTFTP_CONNECTION ) {
		return( $engine, $interface );
	}
	if( $intf_type eq $Rosetta::INTFTP_CURSOR ) {
		my $conn_intf = $interface->get_parent_interface()->get_parent_interface();
		my $conn_eng = $conn_intf->get_engine();
		return( $conn_eng, $conn_intf );
	}
	return( undef, undef ); # $intf_type eq $INTFTP_ENVIRONMENT
}

sub get_curs_cx_e_and_i {
	my ($engine, $interface) = @_;
	my $intf_type = $interface->get_interface_type();
	if( $intf_type eq $Rosetta::INTFTP_PREPARATION ) {
		my $p_intf = $interface->get_parent_interface();
		my $p_eng = $p_intf->get_engine();
		return( $p_eng->get_curs_cx_e_and_i( $p_intf ) );
	}
	if( $intf_type eq $Rosetta::INTFTP_CURSOR ) {
		return( $engine, $interface );
	}
	return( undef, undef ); # $intf_type eq $INTFTP_ENVIRONMENT or $intf_type eq $INTFTP_CONNECTION
}

######################################################################

sub encode_perl_identifier {
	# This function allows users to put any values they want in names, and 
	# they will turn into valid Perl var names, that are guaranteed to be 
	# distinct between themselves and any variable names in Generic.pm.
	# If $debug is true, less encoding is done to make more human readable, for 
	# generated comments or errors; don't use that as identifier names in Perl code.
	my ($engine, $name, $debug) = @_;
	if( $debug ) {
		$name =~ s|[^a-zA-Z0-9]|*|g;
		return( $name );
	} else {
		return( join( '', map { unpack( 'H2', $_ ) } split( '', $name ) ) );
	}
}

sub build_perl_identifier_element {
	my ($engine, $object_node, $debug) = @_;
	return( $engine->encode_perl_identifier( $object_node->get_literal_attribute( 'si_name' ), $debug ) );
}

sub build_perl_identifier_rtn {
	my ($engine, $routine_node, $debug) = @_;
	my $routine_name = $engine->build_perl_identifier_element( $routine_node, $debug );
	my $routine_pp_node = $routine_node->get_primary_parent_attribute();
	my $routine_pp_node_type = $routine_pp_node->get_node_type();
	if( $routine_pp_node_type eq 'schema' ) {
		my $schema_name = $engine->build_perl_identifier_element( $routine_pp_node, $debug );
		my $cat_node = $routine_pp_node->get_primary_parent_attribute();
		my $cat_name = $engine->build_perl_identifier_element( $cat_node, $debug );
		return( 'cat_'.$cat_name.'_'.$schema_name.'_'.$routine_name )
	}
	if( $routine_pp_node_type eq 'application' ) {
		my $app_name = $engine->build_perl_identifier_element( $routine_pp_node, $debug );
		return( 'app_'.$app_name.'_'.$routine_name )
	}
	# $routine_pp_node_type eq 'routine'
	return( $engine->build_perl_identifier_rtn( $routine_pp_node, $debug ).'_'.$routine_name )
}

sub build_perl_identifier_rtn_var {
	my ($engine, $routine_var_node, $debug) = @_;
	return( "\$rtv_enc_".$engine->build_perl_identifier_element( $routine_var_node, $debug ) );
}

######################################################################

sub encode_perl_literal_cstr {
	my ($engine, $literal) = @_;
	if( defined( $literal ) ) {
		$literal =~ s|\\|\\\\|g;
		$literal =~ s|'|\'|g;
		return( "'".$literal."'" );
	} else {
		return( "undef" );
	}
}

sub build_perl_literal_cstr_from_atvl {
	my ($engine, $object_node, $attr_name) = @_;
	$attr_name ||= 'si_name';
	return( $engine->encode_perl_literal_cstr( $object_node->get_literal_attribute( $attr_name ) ) );
}

######################################################################

sub open_dbi_connection {
	my ($engine, $dbi_driver, $local_dsn, $login_name, $login_pass, $auto_commit) = @_;
	my $dbi_dbh = DBI->connect( 
		"DBI:".$dbi_driver.":".($local_dsn||''),
		$login_name,
		$login_pass,
		{ RaiseError => 1, AutoCommit => $auto_commit },
	); # throws exception on failure
	return( $dbi_dbh );
}

sub close_dbi_connection {
	my ($engine, $dbi_dbh, $auto_commit) = @_;
	unless( $auto_commit ) {
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
	my ($engine, $driver_hint) = @_;

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

sub make_srt_node {
	my ($engine, $node_type, $container) = @_;
	my $node = $container->new_node( $node_type );
	$node->set_node_id( $container->get_next_free_node_id() );
	$node->put_in_container( $container );
	return( $node );
}

sub make_child_srt_node {
	my ($engine, $node_type, $pp_node, $pp_attr) = @_;
	my $container = $pp_node->get_container();
	my $node = $pp_node->new_node( $node_type );
	$node->set_node_id( $container->get_next_free_node_id() );
	$node->put_in_container( $container );
	$node->set_primary_parent_attribute( $pp_node );
	return( $node );
}

######################################################################

sub build_perl_declare_cx_conn {
	my ($engine, $interface, $routine_node, $routine_var_node) = @_;
	my $prep_eng = $engine->{$PROP_IN_PROGRESS_PREP_ENG};

	my $app_intf = $interface->get_root_interface();
	my $cat_link_bp_node = $routine_var_node->get_node_ref_attribute( 'conn_link' );

	# Now figure out link target by cross-referencing app inst with cat link bp.
	my $app_inst_node = $app_intf->get_srt_node();
	my $cat_link_inst_node = undef;
	foreach my $link (@{$app_inst_node->get_child_nodes( 'catalog_link_instance' )}) {
		if( $link->get_node_ref_attribute( 'blueprint' ) eq $cat_link_bp_node ) {
			$cat_link_inst_node = $link;
			last;
		}
	}
	my $cat_inst_node = $cat_link_inst_node->get_node_ref_attribute( 'target' );
	my $dsp_node = $cat_inst_node->get_node_ref_attribute( 'product' );

	my %conn_prep_eco = (
		'product_code' => $dsp_node->get_literal_attribute( 'product_code' ),
		'is_memory_based' => $dsp_node->get_literal_attribute( 'is_memory_based' ),
		'is_file_based' => $dsp_node->get_literal_attribute( 'is_file_based' ),
		'is_local_proc' => $dsp_node->get_literal_attribute( 'is_local_proc' ),
		'is_network_svc' => $dsp_node->get_literal_attribute( 'is_network_svc' ),
		'file_path' => $cat_inst_node->get_literal_attribute( 'file_path' ),
		'local_dsn' => $cat_link_inst_node->get_literal_attribute( 'local_dsn' ),
		'login_name' => $cat_link_inst_node->get_literal_attribute( 'login_name' ),
		'login_pass' => $cat_link_inst_node->get_literal_attribute( 'login_pass' ),
	);
	foreach my $opt_node (@{$cat_inst_node->get_child_nodes( 'catalog_instance_opt' )}, 
			@{$cat_link_inst_node->get_child_nodes( 'catalog_link_instance_opt' )}) {
		my $key = $opt_node->get_literal_attribute( 'si_key' );
		my $value = $opt_node->get_literal_attribute( 'value' );
		defined( $value ) or next;
		if( !defined( $conn_prep_eco{$key} ) ) {
			$conn_prep_eco{$key} = $value;
		}
	}

	$conn_prep_eco{'dbi_driver'} = # May modifiy DBI driver string; dies if DBI driver won't load.
		$engine->install_dbi_driver( $conn_prep_eco{'dbi_driver'} || $conn_prep_eco{'product_code'} );
	$conn_prep_eco{'local_dsn'} = 
		$conn_prep_eco{'is_file_based'} ? $conn_prep_eco{'file_path'} : # file_path must be set if is_file_based
		$conn_prep_eco{'local_dsn'}; # used for non-file-based

	my $rtn_var_nm = $engine->build_perl_identifier_rtn_var( $routine_var_node );
	my $rtn_var_nm_eng = $rtn_var_nm.'_eng';

	$prep_eng->{$PROP_CONN_PREP_ECO} = \%conn_prep_eco;

	return( <<__EOL );
	my $rtn_var_nm_eng = \$rtv_prep_eng->new();
	$rtn_var_nm = \$rtv_prep_intf->new( \$Rosetta::INTFTP_CONNECTION, undef, 
		\$rtv_prep_intf, $rtn_var_nm_eng );
__EOL
}

######################################################################

sub srtn_catalog_list {
	my ($prep_eng, $prep_intf, $args) = @_;
	my ($env_eng, $env_intf) = $prep_eng->get_env_cx_e_and_i( $prep_intf );

	my $recursive = $args->{'RECURSIVE'}; # TODO: implement this

	# TODO: Each MySQL "database" is actually a "schema"; there is one "catalog" per server.
	# TODO: As for SQLite, we'll continue to see each database file as a 1-sch catalog for now.
	# For file-based dbs, a file is a catalog;
	# For client-server based dbs with one specific spot for data, all they manage is 1 catalog.
	# Note: the treatment for MySQL is confirmed as the official conception by their developers.

	my $app_inst_node = $prep_intf->get_root_interface()->get_srt_node();
	my $app_bp_node = $app_inst_node->get_node_ref_attribute( 'blueprint' );
	my $container = $app_inst_node->get_container();

	my @cat_link_bp_nodes = ();

	my $dlp_node = $env_intf->get_srt_node(); # A 'data_link_product' Node (repr ourself).
	foreach my $dbi_driver (DBI->available_drivers()) {
		# Tested $dbi_driver values on my system are (space-delimited):
		# [DBM ExampleP File Proxy SQLite Sponge mysql]; they are ready to use as is.
		eval { DBI->install_driver( $dbi_driver ); }; $@ and next; # Skip bad driver.
		# If we get here, then the $dbi_driver will load without problems.
		$dbi_driver eq 'ExampleP' and next; # Skip useless DBI-bundled driver.
		$dbi_driver eq 'File' and next; # Skip useless DBI-bundled driver.
		# If we get here, then the $dbi_driver is something "normal".
		my $dsp_node = $env_eng->make_srt_node( 'data_storage_product', $container );
		$dsp_node->set_literal_attribute( 'si_name', $dbi_driver );
		$dsp_node->set_literal_attribute( 'product_code', $dbi_driver );
		if( $dbi_driver eq 'Sponge' ) {
			$dsp_node->set_literal_attribute( 'is_memory_based', 1 ); # common setting for DBDs
		} elsif( $dbi_driver eq 'DBM' or $dbi_driver eq 'SQLite2' or $dbi_driver eq 'SQLite' ) {
			$dsp_node->set_literal_attribute( 'is_file_based', 1 ); # common setting for DBDs
		} elsif( $dbi_driver eq 'mysql' ) {
			$dsp_node->set_literal_attribute( 'is_local_proc', 1 ); # common setting for DBDs
		} elsif( 0 ) {
			$dsp_node->set_literal_attribute( 'is_network_svc', 1 ); # common setting for DBDs
		} else {
			$dsp_node->set_literal_attribute( 'is_local_proc', 1 ); # may not be correct
		}
		foreach my $dbi_data_source (DBI->data_sources( $dbi_driver )) {
			#Examples of $dbi_data_source formats are: 
			#dbi:DriverName:database_name
			#dbi:DriverName:database_name@hostname:port
			#dbi:DriverName:database=database_name;host=hostname;port=port 
			my (undef, undef, $local_dsn) = split( ':', $dbi_data_source );
			my $cat_bp_node = $env_eng->make_srt_node( 'catalog', $container );
			$cat_bp_node->set_literal_attribute( 'si_name', $dbi_data_source );
			my $cat_link_bp_node = $env_eng->make_child_srt_node( 
				'catalog_link', $app_bp_node );
			$cat_link_bp_node->set_literal_attribute( 'si_name', $dbi_data_source );
			$cat_link_bp_node->set_node_ref_attribute( 'target', $cat_bp_node );
			my $cat_inst_node = $env_eng->make_srt_node( 'catalog_instance', $container );
			$cat_inst_node->set_node_ref_attribute( 'product', $dsp_node );
			$cat_inst_node->set_node_ref_attribute( 'blueprint', $cat_bp_node );
			$cat_inst_node->set_literal_attribute( 'si_name', $dbi_data_source );
			if( $dbi_driver eq 'DBM' or $dbi_driver eq 'SQLite2' or $dbi_driver eq 'SQLite' ) {
				# is_file_based always uses file_path.
				my $file_path = $local_dsn;
				if( $dbi_driver eq 'DBM' ) {
					$file_path =~ s|f_dir=(.*)|$1|;
				}
				$cat_inst_node->set_literal_attribute( 'file_path', $file_path );
			}
			my $cat_link_inst_node = $env_eng->make_child_srt_node( 
				'catalog_link_instance', $app_inst_node );
			$cat_link_inst_node->set_node_ref_attribute( 'product', $dlp_node );
			$cat_link_inst_node->set_node_ref_attribute( 'blueprint', $cat_link_bp_node );
			$cat_link_inst_node->set_node_ref_attribute( 'target', $cat_inst_node );
			unless( $dbi_driver eq 'DBM' or $dbi_driver eq 'SQLite2' or $dbi_driver eq 'SQLite' ) {
				# non file-based currently uses local_dsn.
				$cat_link_inst_node->set_literal_attribute( 'local_dsn', $local_dsn );
			}
			push( @cat_link_bp_nodes, $cat_link_bp_node );
		}
	}

	my $lit_eng = $prep_eng->new();
	$lit_eng->{$PROP_LIT_PAYLOAD} = \@cat_link_bp_nodes;

	my $lit_intf = $prep_intf->new( $Rosetta::INTFTP_LITERAL, undef, 
		$prep_intf, $lit_eng );
	return( $lit_intf );
}

######################################################################

sub srtn_catalog_open {
	my ($prep_eng, $prep_intf, $args) = @_;
	my $conn_intf = $args->{'CONN_CX'};
	my $conn_eng = $conn_intf->get_engine();
	my $conn_prep_intf = $conn_intf->get_parent_interface();
	my $conn_prep_eng = $conn_prep_intf->get_engine();

	if( $conn_eng->{$PROP_CONN_IS_OPEN} ) {
		my $routine_node = $prep_intf->get_srt_node();
		$prep_eng->_throw_error_message( 'ROS_G_CATALOG_OPEN_CONN_STATE_OPEN', 
			{ 'RNAME' => $routine_node } );
	}

	my %conn_eco = %{$conn_prep_eng->{$PROP_CONN_PREP_ECO}};
	defined( $conn_eco{'login_name'} ) or $conn_eco{'login_name'} = $args->{'LOGIN_NAME'};
	defined( $conn_eco{'login_pass'} ) or $conn_eco{'login_pass'} = $args->{'LOGIN_PASS'};

	my $dbi_driver = $conn_eco{'dbi_driver'}; # product_code merged in by build_perl_declare_cx_conn()
	my $local_dsn = $conn_eco{'local_dsn'}; # file_path merged in by build_perl_declare_cx_conn()
	my $login_name = $conn_eco{'login_name'};
	my $login_pass = $conn_eco{'login_pass'};
	my $auto_commit = $conn_eco{'auto_commit'};

	my $dbi_dbh = $prep_eng->open_dbi_connection( 
		$dbi_driver, $local_dsn, $login_name, $login_pass, $auto_commit );

	my $builder = Rosetta::Utility::SQLBuilder->new();
	defined( $conn_eco{'delim_ident'} ) and 
		$builder->delimited_identifiers( $conn_eco{'delim_ident'} );

	$conn_eng->{$PROP_CONN_IS_OPEN} = 1;
	$conn_eng->{$PROP_CONN_ECO} = \%conn_eco;
	$conn_eng->{$PROP_CONN_DBH_OBJ} = $dbi_dbh;
	$conn_eng->{$PROP_CONN_SQL_BUILDER} = $builder;
}

######################################################################

sub srtn_catalog_close {
	my ($prep_eng, $prep_intf, $args) = @_;
	my $conn_intf = $args->{'CONN_CX'};
	my $conn_eng = $conn_intf->get_engine();

	unless( $conn_eng->{$PROP_CONN_IS_OPEN} ) {
		my $routine_node = $prep_intf->get_srt_node();
		$prep_eng->_throw_error_message( 'ROS_G_CATALOG_CLOSE_CONN_STATE_CLOSED', 
			{ 'RNAME' => $routine_node } );
	}

	$conn_eng->close_dbi_connection( $conn_eng->{$PROP_CONN_DBH_OBJ},
		$conn_eng->{$PROP_CONN_ECO}->{$ECO_AUTO_COMMIT} );

	$conn_eng->{$PROP_CONN_IS_OPEN} = undef;
	$conn_eng->{$PROP_CONN_ECO} = undef;
	$conn_eng->{$PROP_CONN_DBH_OBJ} = undef;
	$conn_eng->{$PROP_CONN_SQL_BUILDER} = undef;
}

######################################################################
######################################################################

1;
__END__

=head1 SYNOPSIS

I<The previous SYNOPSIS was removed; a new one will be written later.>

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

I<CAVEAT: THIS ENGINE IS "UNDER CONSTRUCTION" AND MANY FEATURES DESCRIBED BY 
SQL::Routine::Language AND Rosetta::Features ARE NOT YET IMPLEMENTED.>

=head1 ROSETTA FEATURES SUPPORTED BY ENVIRONMENT

Rosetta::Engine::Generic explicitly declares the support levels for certain
Rosetta Native Interface features at the Environment level, listed below. 
Those with 'yes' are always available regardless of any Connection
circumstances; those with 'no' are never available.

	CATALOG_LIST
		yes
	CATALOG_INFO
		no
	CONN_BASIC
		yes
	CONN_MULTI_SAME
		yes
	CONN_MULTI_DIFF
		yes
	CONN_PING
		no
	TRAN_MULTI_SIB
		no
	TRAN_MULTI_CHILD
		no
	USER_LIST
		no
	USER_INFO
		no
	SCHEMA_LIST
		no
	SCHEMA_INFO
		no
	DOMAIN_LIST
		no
	DOMAIN_INFO
		no
	DOMAIN_DEFN_VERIFY
		no
	DOMAIN_DEFN_BASIC
		no
	TABLE_LIST
		no
	TABLE_INFO
		no
	TABLE_DEFN_VERIFY
		no
	TABLE_DEFN_BASIC
		no
	TABLE_UKEY_BASIC
		no
	TABLE_UKEY_MULTI
		no
	TABLE_FKEY_BASIC
		no
	TABLE_FKEY_MULTI
		no
	QUERY_BASIC
		no
	QUERY_SCHEMA_VIEW
		no
	QUERY_RETURN_SPEC_COLS
		no
	QUERY_RETURN_COL_EXPRS
		no
	QUERY_WHERE
		no
	QUERY_COMPARE_PRED
		no
	QUERY_BOOLEAN_EXPR
		no
	QUERY_NUMERIC_EXPR
		no
	QUERY_STRING_EXPR
		no
	QUERY_LIKE_PRED
		no
	QUERY_JOIN_BASIC
		no
	QUERY_JOIN_OUTER_LEFT
		no
	QUERY_JOIN_ALL
		no
	QUERY_GROUP_BY_NONE
		no
	QUERY_GROUP_BY_SOME
		no
	QUERY_AGG_CONCAT
		no
	QUERY_AGG_EXIST
		no
	QUERY_OLAP
		no
	QUERY_HAVING
		no
	QUERY_WINDOW_ORDER
		no
	QUERY_WINDOW_LIMIT
		no
	QUERY_COMPOUND
		no
	QUERY_SUBQUERY
		no

This Engine may contain code that supports additional features, but these have
not been tested at all and so are not yet declared.

=head1 ROSETTA FEATURES SUPPORTED PER CONNECTION

Rosetta::Engine::Generic explicitly declares the support levels for certain
Rosetta Native Interface features at the Connection level, listed below. 
Whether or not each is available depends on what Connection you have.  The
conditions for each feature are listed with them, below and indented.

	TRAN_BASIC
		- If "auto_commit" ECO is true then:
			no
		- Else
			don't know yet
	TRAN_ROLLBACK_ON_DEATH
		don't know yet

=head1 ENGINE CONFIGURATION OPTIONS

The SQL::Routine objects that comprise Rosetta's inputs have special
compartments for passing configuration options that are only recognizable to
the chosen "data link product", which in Rosetta terms is an Engine.  At the
moment, all Engine Configuration Options are conceptually passed in at "catalog
link realization time", which is usually when or before a Connection Interface
is about to be made (by a prepare(CATALOG_OPEN)/execute() combination), or it
can be when or before an analagous operation (such as a CATALOG_INFO).  When a
catalog link is realized, a short chain of related SRT Nodes is consulted for
their attributes or associated child *_opt Nodes, one each of:
catalog_link_instance, catalog_instance, data_link_product,
data_storage_product.  Option values declared later in this list are
increasingly global, and those declared earlier are increasingly local; any
time there are name collisions, the most global values have precedence.  The
SRT Nodes are read at prepare() time.  At execute() time, any ROUTINE_ARGS
values can fill in blanks, but they can not override any any SRT Node option
values.  Once a Connection is created, the configuration settings for it can
not be changed.

These options are explicitly defined by SQL::Routine and have their own
dedicated Node attributes; the options listed here have the same names
(lower-case) as the attribute names in question.  You can provide each of these
options either in the dedicated attribute or in a *_opt Node having a
same-named si_key; if both are set, the attribute takes precedence:

=over 4

=item

B<product_code> - cstr - Corresponds to "data_storage_product.product_code".

=item

B<is_memory_based> - cstr - Corresponds to "data_storage_product.is_memory_based".

=item

B<is_file_based> - cstr - Corresponds to "data_storage_product.is_file_based".

=item

B<is_local_proc> - cstr - Corresponds to "data_storage_product.is_local_proc".

=item

B<is_network_svc> - cstr - Corresponds to "data_storage_product.is_network_svc".

=item

B<file_path> - cstr - Corresponds to "catalog_instance.file_path".  When using
a data storage product that is file based, this config option is required; it
contains the file path for the data storage file.  TODO: server_ip,
server_domain, server_port.

=item

B<local_dsn> - cstr - Corresponds to "catalog_link_instance.local_dsn".  This
is the locally recognized "data source name" of the database/catalog that you
want to connect to.

=item

B<login_name> - cstr - Corresponds to "catalog_link_instance.login_name".  This
is a database natively recognized "authorization identifier" or "user name"
that your application wants to log-in to the database as every time it
connects.  You typically only set this if the user-name is hidden from the
application user such as if it is stored in a application configuration file,
and the user would not be prompted for a different one if it fails to work.  If
the database user name is provided by the user, then you typically pass it as a
host parameter value at execute() time instead of storing it in the model.  If
you do not provide this value either in the model or at execute() time, we will
assume the database doesn't require authentication, or we will try to log in
anonymously.

=item

B<login_pass> - cstr - Corresponds to "catalog_link_instance.login_pass".  This
is the database natively recognized "password" that you provide along with the
B<login_name>.  All parts of the above description for the "name" apply to the
"pass" also.

=back

Rosetta::Engine::Generic recognizes these options:

=over 4

=item

B<dbi_driver> - cstr - Seeing as Rosetta::Engine::Generic is meant to sit on
top of DBI and any of its drivers, this option lets you explicitely pick which
one to use.  If this is not set, then Generic will make an educated guess for
which DBD module to use based on the B<product_code> engine configuration
option, or it will fall back to DBD::ODBC if possible.

=item

B<auto_commit> - bool - If this option is false (the default),
Rosetta::Engine::Generic will always use transactions and require explicit
commits for database actions to be saved; if this option is true, then it will
instead auto-commit every database action, so separate commits are not
necessary.  When this option is true, then this module should behave as
expected with every kind of data storage product; automatic explicit commits
will be issued for transaction supporting databases, and this behaviour will
just happen anyway on non-supporting ones.  When this option is false, then you
must make sure to only use database products with it that have native support
for transactions; Generic won't even try to emulate transactions since that is
too difficult to do properly; this module simply won't work properly with
databases that lack native transaction support, even though it will incorrectly
declare support for said activity.

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
information (a full superset), that is what Rosetta and SQL::Routine
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

=head1 SEE ALSO

L<perl(1)>, L<Rosetta>, L<SQL::Routine>, L<Locale::KeyedText>,
L<Rosetta::Utility::SQLBuilder>, L<Rosetta::Utility::SQLParser>, L<DBI>.

=cut
