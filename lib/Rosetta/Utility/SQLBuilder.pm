=head1 NAME

Rosetta::Utility::SQLBuilder - Generate ANSI/ISO SQL-2003 and other SQL variants

=cut

######################################################################

package Rosetta::Utility::SQLBuilder;
use 5.006;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = '0.06';

use Locale::KeyedText 0.06;
use SQL::SyntaxModel 0.24;

######################################################################

=head1 DEPENDENCIES

Perl Version: 5.006

Standard Modules: I<none>

Nonstandard Modules: 

	Locale::KeyedText 0.06 (for error messages)
	SQL::SyntaxModel 0.24

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

# Names of properties for objects of the Rosetta::Utility::SQLBuilder class are declared here:
# This set of properties are generally set once at the start of a SQLBuilder object's life 
# and aren't changed later, since they are generally static configuration data.
my $PROP_POSIT_BVARS = 'posit_bvars'; # boolean; true if bind vars are positional; false if named
my $PROP_DELIM_IDENT = 'delim_ident'; # boolean; true if identifiers are delimited, case-sensitive
my $PROP_IDENT_QUOTC = 'ident_quotc'; # character; character used to delimit identifiers with
my $PROP_DATA_TYPES  = 'data_types' ; # hash ref
my $PROP_ORA_SEQ_USAGE = 'ora_seq_usage'; # boolean; true if sequence usage in Oracle style
my $PROP_ORA_ROUTINES = 'ora_routines'; # boolean; true to declare routines in Oracle style
my $PROP_INLINE_SUBQ = 'inline_subq'; # boolean; true to inline all subq, false if "with" supported
my $PROP_INLINE_DOMAIN = 'inline_domain'; # boolean; true inl data type, false named domains supp
my $PROP_SINGLE_SCHEMA = 'single_schema'; # boolean; true to emulate mult schemas in single schema
my $PROP_SS_JOIN_CHARS = 'ss_join_chars'; # char str; join schema name + sch obj name with this
my $PROP_EMUL_SUBQUERY = 'emul_subquery'; # boolean; true to emulate subqueries with temp tables
my $PROP_EMUL_COMPOUND = 'emul_compound'; # boolean; true to emulate unions etc with tt, joins
my $PROP_ET_JOIN_CHARS = 'et_join_chars'; # char str; join parts of temp table names for emulations

# Here are more Rosetta::Utility::SQLBuilder object properties:
# Each of these contains either very short term configuration options (meant to have 
# the life of about one external build* method call) that are only set externally as 
# usual, or some may also be set or changed by SQLBuilder code, and can be used effectively 
# as extra output from the build* method; they maintain state for a build* invocation.
my $PROP_MAKE_BVARS = 'make_bvars'; # boolean; true when routine vars are bind vars, false when not
my $PROP_PBV_MAP_ARY = 'pbv_map_ary'; # array ref; holds state for bind var map  of current sql code
my $PROP_UNWRAP_VIEWS = 'unwrap_views'; # boolean; true to use original src names, false for correl

# Names of specific data types, used as keys in $PROP_DATA_TYPES hash.
my $DT_NUM_INT_8  = 'NUM_INT_8' ; # what signed ints up to  8 bits are stored as
my $DT_NUM_INT_16 = 'NUM_INT_16'; # what signed ints up to 16 bits are stored as
my $DT_NUM_INT_24 = 'NUM_INT_24'; # what signed ints up to 24 bits are stored as
my $DT_NUM_INT_32 = 'NUM_INT_32'; # what signed ints up to 32 bits are stored as
my $DT_NUM_INT_64 = 'NUM_INT_64'; # what signed ints up to 64 bits are stored as
my $DT_NUM_INT_128 = 'NUM_INT_128'; # what signed ints up to 128 bits are stored as
my $DT_NUM_INT_LG = 'NUM_INT_LG'; # what signed ints larger than 128 bits are stored as
my $DT_NUM_EXA_WS = 'NUM_EXA_WS'; # an exact non-integer num; use when 'scale' is defined
my $DT_NUM_EXA_NS = 'NUM_EXA_NS'; # an exact non-integer num; use when 'scale' not defined
my $DT_NUM_APR_32 = 'NUM_APR_32'; # what floating-point nums up to 32 bits are stored as
my $DT_NUM_APR_64 = 'NUM_APR_64'; # what floating-point nums up to 64 bits are stored as
my $DT_NUM_APR_128 = 'NUM_APR_128'; # what floating-point nums up to 128 bits are stored as
my $DT_NUM_APR_LG = 'NUM_APR_LG'; # what floating-point nums larger than 128 bits are stored as
my $DT_NUM_UNS_SFX = 'NUM_UNS_SFX'; # suffix added to numeric type decls to make unsigned
my $DT_STR_BIT_255 = 'STR_BIT_255'; # storage for binary data up to 255 bytes, var-size
my $DT_STR_BIT_255F = 'STR_BIT_255F'; # storage for binary data up to 255 bytes, fixed-size
my $DT_STR_BIT_2K = 'STR_BIT_2K'; # storage for binary data up to 2000 bytes, var-size
my $DT_STR_BIT_2KF = 'STR_BIT_2KF'; # storage for binary data up to 2000 bytes, fixed-size
my $DT_STR_BIT_4K = 'STR_BIT_4K'; # storage for binary data up to 4000 bytes, var-size
my $DT_STR_BIT_4KF = 'STR_BIT_4KF'; # storage for binary data up to 4000 bytes, fixed-size
my $DT_STR_BIT_32K = 'STR_BIT_32K'; # storage for binary data up to 32767 bytes
my $DT_STR_BIT_65K = 'STR_BIT_65K'; # storage for binary data up to 65535 bytes
my $DT_STR_BIT_16M = 'STR_BIT_16M'; # storage for binary data up to 16777215 bytes
my $DT_STR_BIT_2G = 'STR_BIT_2G'; # storage for binary data up to 2147483647 bytes
my $DT_STR_BIT_4G = 'STR_BIT_4G'; # storage for binary data up to 4294967295 bytes
my $DT_STR_BIT_LG = 'STR_BIT_LG'; # storage for larger binary data (over 4GB)
my $DT_STR_CHAR_255 = 'STR_CHAR_255'; # storage for character data up to 255 chars, var-size
my $DT_STR_CHAR_255F = 'STR_CHAR_255F'; # storage for character data up to 255 chars, fixed-size
my $DT_STR_CHAR_2K = 'STR_CHAR_2K'; # storage for character data up to 2000 chars, var-size
my $DT_STR_CHAR_2KF = 'STR_CHAR_2KF'; # storage for character data up to 2000 chars, fixed-size
my $DT_STR_CHAR_4K = 'STR_CHAR_4K'; # storage for character data up to 4000 chars, var-size
my $DT_STR_CHAR_4KF = 'STR_CHAR_4KF'; # storage for character data up to 4000 chars, fixed-size
my $DT_STR_CHAR_32K = 'STR_CHAR_32K'; # storage for character data up to 32767 chars
my $DT_STR_CHAR_65K = 'STR_CHAR_65K'; # storage for character data up to 65535 chars
my $DT_STR_CHAR_16M = 'STR_CHAR_16M'; # storage for character data up to 16777215 chars
my $DT_STR_CHAR_2G = 'STR_CHAR_2G'; # storage for character data up to 2147483647 chars
my $DT_STR_CHAR_4G = 'STR_CHAR_4G'; # storage for character data up to 4294967295 chars
my $DT_STR_CHAR_LG = 'STR_CHAR_LG'; # storage for larger character data (over 4GB)
my $DT_BOOLEAN = 'BOOLEAN'; # type can only be TRUE,FALSE,UNKNOWN
my $DT_BOOL_USE_NUMS = 'BOOL_USE_NUMS'; # if true, give 1,0,undef for above rather than words
my $DT_DATM_FULL = 'DATM_FULL'; # storage for full datetime/timestamp
my $DT_DATM_DATE = 'DATM_DATE'; # storage for date only
my $DT_DATM_TIME = 'DATM_TIME'; # storage for time only
my $DT_INTRVL_YM = 'INTRVL_YM'; # storage for year-month interval
my $DT_INTRVL_DT = 'INTRVL_DT'; # storage for day-time (day-hour-min-sec) interval
my $DT_HAS_ENUM_TYPE = 'HAS_ENUM_TYPE'; # boolean; if true use ENUM, if false, use CHECK

# Miscellaneous constant values
my $INFINITY = 1_000_000_000_000_000_000; # A hack to mean 'unlimited size'

######################################################################

sub new {
	my ($class) = @_;
	my $builder = bless( {}, ref($class) || $class );

	$builder->{$PROP_POSIT_BVARS} = 0;
	$builder->{$PROP_DELIM_IDENT} = 0;
	$builder->{$PROP_IDENT_QUOTC} = '"'; # doublequote given in ANSI example
		# set to '"' for Oracle and FireBird, '`' for MySQL
	$builder->{$PROP_DATA_TYPES} = $builder->_get_default_data_type_customizations();
	$builder->{$PROP_ORA_SEQ_USAGE} = 0;
	$builder->{$PROP_ORA_ROUTINES} = 0;
	$builder->{$PROP_INLINE_SUBQ} = 0;
	$builder->{$PROP_INLINE_DOMAIN} = 0;
	$builder->{$PROP_SINGLE_SCHEMA} = 0;
	$builder->{$PROP_SS_JOIN_CHARS} = '__'; # double underscore should normally be unique
		# unique value is necessary to reliably reverse-engineer model from a database schema
	$builder->{$PROP_EMUL_SUBQUERY} = 0;
	$builder->{$PROP_EMUL_COMPOUND} = 0;
	$builder->{$PROP_ET_JOIN_CHARS} = '__';

	$builder->{$PROP_MAKE_BVARS} = 0;
	$builder->{$PROP_PBV_MAP_ARY} = [];
	$builder->{$PROP_UNWRAP_VIEWS} = 0;

	return( $builder );
}

sub _get_default_data_type_customizations {
	return( {
		$DT_NUM_INT_8  => 'SMALLINT', # standard; 'TINYINT' for MySQL; 'NUMBER' for Oracle
		$DT_NUM_INT_16 => 'SMALLINT', # for SQL89, MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_INT_24 => 'INTEGER' , # standard; 'MEDIUMINT' for MySQL; 'NUMBER' for Oracle
		$DT_NUM_INT_32 => 'INTEGER' , # for SQL92, MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_INT_64 => 'BIGINT'  , # for SQL-2003 (but not 99), MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_INT_128 => 'DECIMAL({np},0)', # standard, MySQL; 'NUMBER' for Oracle
		$DT_NUM_INT_LG => 'DECIMAL({np},0)' , # standard, MySQL; 'RAW' for Oracle
		$DT_NUM_EXA_WS => 'DECIMAL({np},{ns})', # for SQL99, MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_EXA_NS => 'DECIMAL({np})'     , # for SQL99, MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_APR_32 => 'FLOAT({np})' , # standard, MySQL; 'NUMBER' for Oracle
		$DT_NUM_APR_64 => 'FLOAT({np})' , # standard; 'DOUBLE' for MySQL; 'NUMBER' for Oracle
		$DT_NUM_APR_128 => 'FLOAT({np})', # 'DECIMAL' for MySQL?; 'NUMBER' for Oracle
		$DT_NUM_APR_LG => 'FLOAT({np})' , # 'DECIMAL' for MySQL?; 'RAW' for Oracle
		$DT_NUM_UNS_SFX => 'UNSIGNED', # for MySQL
			# Note: the SQL-2003 standard says that exact numerics can take precision and scale 
			# arguments (if NUMERIC or DECIMAL; precision is mandatory, scale is optional),
			# approximate ones take precision only (if FLOAT; REAL and DOUBLE do not take anything), 
			# integers (INTEGER, SMALLINT) can not take either.
		$DT_STR_BIT_255 => 'BIT VARYING({mo})', # standard (or 'VARBIT'?); 'RAW' for Oracle; 'TINYBLOB' for MySQL
		$DT_STR_BIT_255F => 'BIT({mo})', # standard; 'RAW' for Oracle; 'TINYBLOB' for MySQL
			# According to SQL-2003 Foundation, Annex E (p1173), there had been data types 
			# called 'BIT' and 'BIT VARYING' in SQL-1999, but they are removed in SQL-2003.
		$DT_STR_BIT_2K  => 'BLOB({mo})', # for MySQL; 'RAW' for Oracle
		$DT_STR_BIT_2KF => 'BLOB({mo})', # for MySQL; 'RAW' for Oracle
		$DT_STR_BIT_4K  => 'BLOB({mo})', # for MySQL, Oracle
		$DT_STR_BIT_4KF => 'BLOB({mo})', # for MySQL, Oracle
		$DT_STR_BIT_32K => 'BLOB({mo})', # for MySQL, Oracle
		$DT_STR_BIT_65K => 'BLOB({mo})', # for MySQL, Oracle
		$DT_STR_BIT_16M => 'BLOB({mo})', # standard, Oracle; 'MEDIUMBLOB' for MySQL
		$DT_STR_BIT_2G  => 'BLOB({mo})', # standard, Oracle; 'LONGBLOB' for MySQL
		$DT_STR_BIT_4G  => 'BLOB({mo})', # standard, Oracle; 'LONGBLOB' for MySQL
		$DT_STR_BIT_LG  => 'BLOB({mo})', # standard
		$DT_STR_CHAR_255 => 'VARCHAR({mc})', # for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_255F => 'CHAR({mc})'  , # for MySQL, Oracle
		$DT_STR_CHAR_2K  => 'VARCHAR({mc})', # 'TEXT' for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_2KF => 'CHAR({mc})'   , # 'TEXT' for MySQL; 'CHAR' for Oracle
		$DT_STR_CHAR_4K  => 'VARCHAR({mc})', # 'TEXT' for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_4KF => 'CHAR({mc})'   , # 'TEXT' for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_32K => 'VARCHAR({mc})', # 'VARCHAR2'/'CLOB' for Oracle; 'TEXT' for MySQL
		$DT_STR_CHAR_65K => 'VARCHAR({mc})', # standard, Oracle; 'TEXT' for MySQL
		$DT_STR_CHAR_16M => 'CLOB({mc})'   , # standard, Oracle; 'MEDIUMTEXT' for MySQL
		$DT_STR_CHAR_2G  => 'CLOB({mc})'   , # standard, Oracle; 'LONGTEXT' for MySQL
		$DT_STR_CHAR_4G  => 'CLOB({mc})'   , # standard, Oracle; 'LONGTEXT' for MySQL
		$DT_STR_CHAR_LG  => 'CLOB({mc})'   , # standard
		$DT_BOOLEAN => 'BOOLEAN', # standard; Oracle uses 'CHAR(1)'; MySQL 'TINYINT' or 'BIT'
		$DT_BOOL_USE_NUMS => 0, # SQL-2003; not sure what dbs require nums
		$DT_DATM_FULL => 'TIMESTAMP', # standard; 'DATETIME' for MySQL; Oracle uses 'DATE'
		$DT_DATM_DATE => 'DATE'     , # standard, Oracle
		$DT_DATM_TIME => 'TIME'     , # standard
		$DT_INTRVL_YM => 'INTERVAL', # still need to add '<interval qualifier>'
		$DT_INTRVL_DT => 'INTERVAL', # still need to add '<interval qualifier>'
		$DT_HAS_ENUM_TYPE => 0, # for standard, Oracle use CHECK; MySQL supports ENUM
	} );
}

######################################################################

sub positional_bind_vars {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_POSIT_BVARS} = $new_value;
	}
	return( $builder->{$PROP_POSIT_BVARS} );
}

######################################################################

sub delimited_identifiers {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_DELIM_IDENT} = $new_value;
	}
	return( $builder->{$PROP_DELIM_IDENT} );
}

sub identifier_delimiting_char {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_IDENT_QUOTC} = $new_value;
	}
	return( $builder->{$PROP_IDENT_QUOTC} );
}

######################################################################

sub get_data_type_customizations {
	my ($builder) = @_;
	return( {%{$builder->{$PROP_DATA_TYPES}}} );
}

sub set_data_type_customizations {
	my ($builder, $new_value) = @_;
	if( ref( $new_value ) eq 'HASH' ) {
		my $data_types = $builder->{$PROP_DATA_TYPES};
		while( my ($key, $value) = each %{$new_value} ) {
			$data_types->{$key} = $value;
		}
	}
}

sub reset_default_data_type_customizations {
	my ($builder) = @_;
	$builder->{$PROP_DATA_TYPES} = $builder->_get_default_data_type_customizations();
}

######################################################################

sub ora_style_seq_usage {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_ORA_SEQ_USAGE} = $new_value;
	}
	return( $builder->{$PROP_ORA_SEQ_USAGE} );
}

sub ora_style_routines {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_ORA_ROUTINES} = $new_value;
	}
	return( $builder->{$PROP_ORA_ROUTINES} );
}

######################################################################

sub inlined_subqueries {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_INLINE_SUBQ} = $new_value;
	}
	return( $builder->{$PROP_INLINE_SUBQ} );
}

######################################################################

sub inlined_domains {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_INLINE_DOMAIN} = $new_value;
	}
	return( $builder->{$PROP_INLINE_DOMAIN} );
}

######################################################################

sub flatten_to_single_schema {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_SINGLE_SCHEMA} = $new_value;
	}
	return( $builder->{$PROP_SINGLE_SCHEMA} );
}

sub single_schema_join_chars {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_SS_JOIN_CHARS} = $new_value;
	}
	return( $builder->{$PROP_SS_JOIN_CHARS} );
}

######################################################################

sub emulate_subqueries {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_EMUL_SUBQUERY} = $new_value;
	}
	return( $builder->{$PROP_EMUL_SUBQUERY} );
}

sub emulate_compound_queries {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_EMUL_COMPOUND} = $new_value;
	}
	return( $builder->{$PROP_EMUL_COMPOUND} );
}

sub emulated_query_temp_table_join_chars {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_ET_JOIN_CHARS} = $new_value;
	}
	return( $builder->{$PROP_ET_JOIN_CHARS} );
}

######################################################################

sub make_bind_vars {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_MAKE_BVARS} = $new_value;
	}
	return( $builder->{$PROP_MAKE_BVARS} );
}

######################################################################

sub get_positional_bind_var_map_array {
	my ($builder) = @_;
	return( [@{$builder->{$PROP_PBV_MAP_ARY}}] );
}

sub clear_positional_bind_var_map_array {
	my ($builder) = @_;
	@{$builder->{$PROP_PBV_MAP_ARY}} = ();
}

######################################################################

sub unwrap_views {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_UNWRAP_VIEWS} = $new_value;
	}
	return( $builder->{$PROP_UNWRAP_VIEWS} );
}

######################################################################

sub quote_literal {
	my ($builder, $literal, $base_type) = @_;
	return( $base_type eq 'NUM_INT' ? $builder->quote_integer_literal( $literal ) : 
		$base_type eq 'NUM_EXA' ? $builder->quote_numeric_literal( $literal ) : 
		$base_type eq 'NUM_APR' ? $builder->quote_numeric_literal( $literal ) : 
		$base_type eq 'STR_BIT' ? $builder->quote_hex_string_literal( $literal ) : 
		$base_type eq 'STR_CHAR' ? $builder->quote_char_string_literal( $literal ) : 
		$base_type eq 'BOOLEAN' ? $builder->quote_boolean_literal( $literal ) : 
		$builder->quote_char_string_literal( $literal ) ); # treat misc/date/interval as char
}

sub quote_char_string_literal {
	my ($builder, $literal) = @_;
	$literal =~ s|'|''|g;
	# MySQL also supports escaping of NULs and control characters, like with "\0"
	return( "'".$literal."'" );
	# Input of "Perl" becomes output of "'Perl'".
	# More work is needed.  See SQL-2003, 02-Foundation, 5.3 <literal> (pg 143).
	# We need to support both <character string literal> 
	# and <Unicode character string literal>.
}

sub quote_bin_string_literal {
	my ($builder, $literal) = @_;
	return( "B'".unpack( 'B32', $literal )."'" );
	# Input of "Perl" becomes output of "B'01010000011001010111001001101100'".
}

sub quote_hex_string_literal {
	my ($builder, $literal) = @_;
	return( "X'".uc(unpack( 'H8', $literal ))."'" );
	# Input of "Perl" becomes output of "X'5065726C'".
}

sub quote_integer_literal {
	my ($builder, $literal) = @_;
	return( "'".(int $literal)."'" ); # quotes make MySQL ENUMS work correctly
}

sub quote_numeric_literal {
	my ($builder, $literal) = @_;
	return( "'".(0 + $literal)."'" ); # quotes make MySQL ENUMS work correctly
}

sub quote_boolean_literal {
	my ($builder, $literal) = @_;
	if( $builder->{$PROP_DATA_TYPES}->{$DT_BOOL_USE_NUMS} ) {
		return( !defined( $literal ) ? 'NULL' : $literal ? 1 : 0 );
	} else {
		return( !defined( $literal ) ? 'UNKNOWN' : $literal ? 'TRUE' : 'FALSE' );
	}
}

######################################################################

sub quote_identifier {
	my ($builder, $name) = @_;
	if( $builder->{$PROP_DELIM_IDENT} ) {
		my $quotc = $builder->{$PROP_IDENT_QUOTC};
		$name =~ s|$quotc|$quotc$quotc|g;
		$name = $quotc.$name.$quotc;
	} else {
		$name = uc( $name );
		$name =~ s|[^A-Z0-9_]||g;
	}
	return( $name );
	# More work is needed.  See SQL-2003, 02-Foundation, 5.4 Names and identifiers (pg 151).
	# We need to support <regular identifier> and <delimited identifier> 
	# and <Unicode delimited identifier>; only first two are done now.
}

sub build_identifier_element {
	# This function is for getting the unqualified name of a non-schema object, 
	# such as a local variable.
	my ($builder, $object_node) = @_;
	return( $builder->quote_identifier( $object_node->get_literal_attribute( 'name' ) ) );
}

sub build_identifier_schema_obj { # SQL-2003, 6.6 "<identifier chain>" (p183)
	# fd=0; This function is for getting the name of an existing schema object that is not 
	# being created or dropped, such as most of the times it is referred to.
	# fd=1; This function is for getting the name of a schema object to be 
	# created or dropped, which may require you to be logged into the 
	# schema being created in, and schema object names may have to be unqualified.
	my ($builder, $object_node, $for_defn) = @_;
	my $object_name = $object_node->get_literal_attribute( 'name' );
	my $schema_node = $object_node->get_node_ref_attribute( 'schema' );
	my $schema_name = $schema_node->get_literal_attribute( 'name' );
	# TODO: support for referencing into other catalogs
	if( $builder->{$PROP_SINGLE_SCHEMA} ) {
		return( $builder->quote_identifier( 
			$schema_name.$builder->{$PROP_SS_JOIN_CHARS}.$object_name ) );
	} else {
		if( $for_defn ) {
			return( $builder->quote_identifier( $object_name ) );
			# Oracle lets you opt prefix schema name when defining; don't know if standard does.
		} else {
			return( $builder->quote_identifier( $schema_name ).'.'.
				$builder->quote_identifier( $object_name ) );
		}
	}
}

sub build_identifier_view_src_col {
	my ($builder, $view_src_col) = @_;
	my $view_src_node = $view_src_col->get_node_ref_attribute( 'src' );
	my $match_col_node = ($view_src_col->get_node_ref_attribute( 'match_table_col' ) || 
		$view_src_col->get_node_ref_attribute( 'match_view_col' ));
	return( $builder->build_identifier_fake_view_src_col( $view_src_node, $match_col_node ) );
}

sub build_identifier_fake_view_src_col {
	my ($builder, $view_src_node, $match_col_node) = @_;
	if( $builder->{$PROP_UNWRAP_VIEWS} ) {
		# We are probably in the WHERE/etc clause of an INSERT|UPDATE|DELETE statement.
		# Assume IUD statement is against one source for now, so unqualified src col names are fine.
		return( $builder->quote_identifier( $match_col_node->get_literal_attribute( 'name' ) ) );
	} else {
		# We are in a normal SELECT statement or view.
		# As usual, have fully qualified name to support multiple sources.
		return( $builder->quote_identifier( $view_src_node->get_literal_attribute( 'name' ) ).'.'.
			$builder->quote_identifier( $match_col_node->get_literal_attribute( 'name' ) ) );
	}
}

sub build_identifier_temp_table_for_emul {
	# This function is for getting the name of a temporary table that will be 
	# used by this module when emulating sub-queries or compound queries, to 
	# hold intermediate values.
	my ($builder, $inner_view_node) = @_;
	my @tt_name_parts = ();
	my $curr_node = $inner_view_node;
	push( @tt_name_parts, $curr_node->get_literal_attribute( 'name' ) );
	while( $curr_node->get_node_ref_attribute( 'p_view' ) ) {
		$curr_node = $curr_node->get_node_ref_attribute( 'p_view' );
		push( @tt_name_parts, $curr_node->get_literal_attribute( 'name' ) );
	}
	while( $curr_node->get_node_ref_attribute( 'routine' ) ) {
		$curr_node = $curr_node->get_node_ref_attribute( 'routine' );
		push( @tt_name_parts, $curr_node->get_literal_attribute( 'name' ) );
	}
	$curr_node = $curr_node->get_node_ref_attribute( 'schema' ) || 
		$curr_node->get_node_ref_attribute( 'application' );
	push( @tt_name_parts, $curr_node->get_literal_attribute( 'name' ) );
	return( $builder->quote_identifier( 
		join( $builder->{$PROP_ET_JOIN_CHARS}, @tt_name_parts ) ) );
}

######################################################################

sub build_expr {
	my ($builder, $expr_node) = @_;
	my $expr_type = $expr_node->get_enumerated_attribute( 'expr_type' );
	if( $expr_type eq 'LIT' ) {
		my $domain_node = $expr_node->get_node_ref_attribute( 'domain' );
		my $lit_val = $expr_node->get_literal_attribute( 'lit_val' );
		return( $builder->quote_literal( $lit_val, 
			$domain_node->get_enumerated_attribute( 'base_type' ) ) );
	} elsif( $expr_type eq 'CAST' ) {
		return( $builder->build_expr_cast_spec( $expr_node ) );
	} elsif( $expr_type eq 'COL' ) {
		return( $builder->build_identifier_view_src_col( 
			$expr_node->get_node_ref_attribute( 'src_col' ) ) );
	} elsif( $expr_type eq 'MCOL' ) {
		return( $builder->build_identifier_element( 
			$expr_node->get_node_ref_attribute( 'match_col' ) ) );
	} elsif( $expr_type eq 'VARG' ) {
		return( $builder->build_identifier_element( 
			$expr_node->get_node_ref_attribute( 'view_arg' ) ) );
	} elsif( $expr_type eq 'ARG' ) {
		my $routine_arg_node = $expr_node->get_node_ref_attribute( 'routine_arg' );
		if( $builder->{$PROP_MAKE_BVARS} ) {
			# We are currently within an 'ANONYMOUS' routine, so arg is an app bind var.
			if( $builder->{$PROP_POSIT_BVARS} ) {
				# Insert positional bind variable/placeholder.
				push( @{$builder->{$PROP_PBV_MAP_ARY}}, 
					$routine_arg_node->get_literal_attribute( 'name' ) );
				return( '?' ); # DBI-style positional place-holders.
			} else {
				# Insert named bind variable/placeholder.
				return( ':'.$builder->build_identifier_element( $routine_arg_node ) );
				# This named style is in the SQL-1999 standard, apparently.  Oracle also uses it.
			}
		} else {
			# We are *not* within an 'ANONYMOUS' routine, so arg is a compiled routine var.
			return( $builder->build_identifier_element( $routine_arg_node ) );
		}
	} elsif( $expr_type eq 'VAR' ) {
		return( $builder->build_identifier_element( 
			$expr_node->get_node_ref_attribute( 'routine_var' ) ) );
	} elsif( $expr_type eq 'SEQN' ) {
		return( $builder->build_expr_seq_next( 
			$expr_node->get_node_ref_attribute( 'sequence' ) ) );
	} elsif( $expr_type eq 'CVIEW' ) {
		return( $builder->build_query_subquery( $expr_node ) );
	} elsif( $expr_type eq 'SFUNC' ) {
		return( $builder->build_expr_call_sfunc( $expr_node ) );
	} elsif( $expr_type eq 'UFUNC' ) {
		return( $builder->build_expr_call_ufunc( $expr_node ) );
	} elsif( $expr_type eq 'LIST' ) {
		return( '('.join( ', ', map { $builder->build_expr( $_ ) } 
			@{$expr_node->get_child_nodes()} ).')' );
	} else {}
}

######################################################################

sub build_expr_predef_data_type { # SQL-2003, 6.1 "<data type>" (p161)
	my ($builder, $domain_node) = @_;

	my $base_type = $domain_node->get_enumerated_attribute( 'base_type' );
	my $num_precision = $domain_node->get_literal_attribute( 'num_precision' );
	my $num_scale = $domain_node->get_literal_attribute( 'num_scale' );
	my $num_octets = $domain_node->get_literal_attribute( 'num_octets' );
	my $num_unsigned = $domain_node->get_literal_attribute( 'num_unsigned' );
	my $max_octets = $domain_node->get_literal_attribute( 'max_octets' );
	my $max_chars = $domain_node->get_literal_attribute( 'max_chars' );
	my $store_fixed = $domain_node->get_literal_attribute( 'store_fixed' );
	my $char_enc = $domain_node->get_enumerated_attribute( 'char_enc' );
	my $trim_white = $domain_node->get_literal_attribute( 'trim_white' );
	my $uc_latin = $domain_node->get_literal_attribute( 'uc_latin' );
	my $lc_latin = $domain_node->get_literal_attribute( 'lc_latin' );
	my $pad_char = $domain_node->get_literal_attribute( 'pad_char' );
	my $trim_pad = $domain_node->get_literal_attribute( 'trim_pad' );
	my $calendar = $domain_node->get_enumerated_attribute( 'calendar' );
	my $range_min = $domain_node->get_literal_attribute( 'range_min' );
	my $range_max = $domain_node->get_literal_attribute( 'range_max' );
	my @allowed_values = map { $_->get_literal_attribute( 'value' ) } 
		@{$domain_node->get_child_nodes( 'domain_opt' )};
		# Note: SSM guarantees that domain_opt attrs have a defined value, though could be ''

	my $type_conv = $builder->{$PROP_DATA_TYPES};

	my $sql = '';

	if( $base_type eq 'NUM_INT' ) {
		$num_precision <= 0 and $num_precision = $INFINITY;
		$num_octets <= 0 and $num_octets = $INFINITY;
		if( $num_precision <= 2 or $num_octets <= 1 ) {
			$sql = $type_conv->{$DT_NUM_INT_8};
		} elsif( $num_precision <= 4 or $num_octets <= 2 ) {
			$sql = $type_conv->{$DT_NUM_INT_16};
		} elsif( $num_precision <= 6 or $num_octets <= 3 ) {
			$sql = $type_conv->{$DT_NUM_INT_24};
		} elsif( $num_precision <= 9 or $num_octets <= 4 ) {
			$sql = $type_conv->{$DT_NUM_INT_32};
		} elsif( $num_precision <= 18 or $num_octets <= 8 ) {
			$sql = $type_conv->{$DT_NUM_INT_64};
		} elsif( $num_precision <= 38 or $num_octets <= 16 ) {
			$sql = $type_conv->{$DT_NUM_INT_128};
		} else {
			$sql = $type_conv->{$DT_NUM_INT_LG};
		}
		if( $num_precision < $INFINITY ) {
			$sql = $builder->substitute_macros( $sql, { 'np' => $num_precision } );
		}
		if( $num_unsigned ) {
			$sql .= ' '.$type_conv->{$DT_NUM_UNS_SFX};
		}
	}

	if( $base_type eq 'NUM_EXA' ) {
		if( defined( $num_scale ) ) {
			$sql = $type_conv->{$DT_NUM_EXA_WS};
		} else {
			$sql = $type_conv->{$DT_NUM_EXA_NS};
		}
		$sql = $builder->substitute_macros( $sql, { 'np' => $num_precision, 'ns' => $num_scale } );
		if( $num_unsigned ) {
			$sql .= ' '.$type_conv->{$DT_NUM_UNS_SFX};
		}
	}

	if( $base_type eq 'NUM_APR' ) {
		$num_precision <= 0 and $num_precision = $INFINITY;
		$num_octets <= 0 and $num_octets = $INFINITY;
		if( $num_precision <= 9 or $num_octets <= 4 ) {
			$sql = $type_conv->{$DT_NUM_APR_32};
		} elsif( $num_precision <= 18 or $num_octets <= 8 ) {
			$sql = $type_conv->{$DT_NUM_APR_64};
		} elsif( $num_precision <= 38 or $num_octets <= 16 ) {
			$sql = $type_conv->{$DT_NUM_APR_128};
		} else {
			$sql = $type_conv->{$DT_NUM_APR_LG};
		}
		if( $num_precision < $INFINITY ) {
			$sql = $builder->substitute_macros( $sql, { 'np' => $num_precision, 'ns' => $num_scale } );
		}
		if( $num_unsigned ) {
			$sql .= ' '.$type_conv->{$DT_NUM_UNS_SFX};
		}
	}

	if( $base_type eq 'STR_BIT' ) {
		$max_octets <= 0 and $max_octets = $INFINITY;
		if( $max_octets <= 255 ) {
			$sql = $store_fixed ? $type_conv->{$DT_STR_BIT_255F} : $type_conv->{$DT_STR_BIT_255};
		} elsif( $max_octets <= 2000 ) {
			$sql = $store_fixed ? $type_conv->{$DT_STR_BIT_2KF} : $type_conv->{$DT_STR_BIT_2K};
		} elsif( $max_octets <= 4000 ) {
			$sql = $store_fixed ? $type_conv->{$DT_STR_BIT_4KF} : $type_conv->{$DT_STR_BIT_4K};
		} elsif( $max_octets <= (2**15-1) ) {
			$sql = $type_conv->{$DT_STR_BIT_32K};
		} elsif( $max_octets <= (2**16-1) ) {
			$sql = $type_conv->{$DT_STR_BIT_65K};
		} elsif( $max_octets <= (2**24-1) ) {
			$sql = $type_conv->{$DT_STR_BIT_16M};
		} elsif( $max_octets <= (2**31-1) ) {
			$sql = $type_conv->{$DT_STR_BIT_2G};
		} elsif( $max_octets <= (2**32-1) ) {
			$sql = $type_conv->{$DT_STR_BIT_4G};
		} else {
			$sql = $type_conv->{$DT_STR_BIT_LG};
		}
		if( $max_octets < $INFINITY ) {
			$sql = $builder->substitute_macros( $sql, { 'mo' => $max_octets } );
		}
	}

	if( $base_type eq 'STR_CHAR' ) {
		$max_chars <= 0 and $max_chars = $INFINITY;
		if( $max_chars <= 255 ) {
			$sql = $store_fixed ? $type_conv->{$DT_STR_CHAR_255F} : $type_conv->{$DT_STR_CHAR_255};
		} elsif( $max_chars <= 2000 ) {
			$sql = $store_fixed ? $type_conv->{$DT_STR_CHAR_2KF} : $type_conv->{$DT_STR_CHAR_2K};
		} elsif( $max_chars <= 4000 ) {
			$sql = $store_fixed ? $type_conv->{$DT_STR_CHAR_4KF} : $type_conv->{$DT_STR_CHAR_4K};
		} elsif( $max_chars <= (2**15-1) ) {
			$sql = $type_conv->{$DT_STR_CHAR_32K};
		} elsif( $max_chars <= (2**16-1) ) {
			$sql = $type_conv->{$DT_STR_CHAR_65K};
		} elsif( $max_chars <= (2**24-1) ) {
			$sql = $type_conv->{$DT_STR_CHAR_16M};
		} elsif( $max_chars <= (2**31-1) ) {
			$sql = $type_conv->{$DT_STR_CHAR_2G};
		} elsif( $max_chars <= (2**32-1) ) {
			$sql = $type_conv->{$DT_STR_CHAR_4G};
		} else {
			$sql = $type_conv->{$DT_STR_CHAR_LG};
		}
		if( $max_chars < $INFINITY ) {
			$sql = $builder->substitute_macros( $sql, { 'mc' => $max_chars } );
		}
		if( $char_enc ) {
			$sql .= ' CHARACTER SET '.$char_enc; # content of char_enc needs transforming
		}
	}

	if( $base_type eq 'BOOLEAN' ) {
		$sql = $type_conv->{$DT_BOOLEAN};
	}

	if( $base_type eq 'DATM_FULL' ) {
		$sql = $type_conv->{$DT_DATM_FULL};
	}
	if( $base_type eq 'DATM_DATE' ) {
		$sql = $type_conv->{$DT_DATM_DATE};
	}
	if( $base_type eq 'DATM_TIME' ) {
		$sql = $type_conv->{$DT_DATM_TIME};
	}

	if( $base_type eq 'INTRVL_YM' ) {
		$sql = $type_conv->{$DT_INTRVL_YM};
	}
	if( $base_type eq 'INTRVL_DT' ) {
		$sql = $type_conv->{$DT_INTRVL_DT};
	}

	if( @allowed_values ) {
		if( $type_conv->{$DT_HAS_ENUM_TYPE} ) {
			# ENUM type declaration replaces existing SQL type declaration.
			my @quoted = map { $builder->quote_literal( $_, 'STR_CHAR' ) } @allowed_values;
			$sql = 'ENUM('.join( ', ', @quoted ).')'; # MySQL syntax
			# All literals are quoted as strings since MySQL treats integer values 
			# as list indexes rather than list values.
		} else {
			# Append CHECK CONSTRAINT to existing SQL type declaration.
			my @quoted = map { $builder->quote_literal( $_, $base_type ) } @allowed_values;
			$sql .= ' CHECK VALUE IN ('.join( ', ', @quoted ).')'; # may be wrong syntax
		}
	}

	return( $sql );
}

sub build_expr_data_type_or_domain_name { # SQL-2003, 11.4 "<column definition>" (p536)
	my ($builder, $domain_node) = @_;
	if( $builder->{$PROP_INLINE_DOMAIN} ) {
		return( $builder->build_expr_predef_data_type( $domain_node ) ); # <data type>
	} else {
		return( $builder->build_identifier_schema_obj( $domain_node ) ); # <domain name>
	}
}

######################################################################

sub build_expr_cast_spec { # SQL-2003, 6.12 "<cast specification>" (p201)
	my ($builder, $expr_node) = @_;
	# We are assuming that enumerated attribute 'expr_type' is 'CAST'.
	my $domain_node = $expr_node->get_node_ref_attribute( 'domain' );
	my $child_expr_nodes = $expr_node->get_child_nodes();
	my $cast_operand = $builder->build_expr( $child_expr_nodes->[0] );
	if( 0 ) {
		# Expand this later to support non-standard operators like TO_STR, TO_NUM, TO_DATE, etc.
	} else {
		my $cast_target = $builder->build_expr_data_type_or_domain_name( $domain_node );
		return( 'CAST ('.$cast_operand.' AS '.$cast_target.')' );
	}
}

######################################################################

sub build_expr_seq_next { # SQL-2003, 6.13 "<next value expression>" (p217)
	my ($builder, $sequence_node) = @_;
	my $sequence_name = $builder->build_identifier_schema_obj( $sequence_node );
	if( $builder->{$PROP_ORA_SEQ_USAGE} ) {
		return( $sequence_name.".NEXTVAL" );
	} else {
		return( 'NEXT VALUE FOR '.$sequence_name );
	}
}

######################################################################

sub build_expr_call_sfunc {
	# Corresponds to these sections:
	# 6.11 "<case expression>" (p197)
	# 6.26 "<numeric value expression>" (p241)
	# 6.27 "<numeric value function>" (p243)
	# 6.28 "<string value expression>" (p252)
	# 6.29 "<string value function>" (p256)
	# 6.34 "<boolean value expression>" (p278)
	# 6.30 "<datetime value expression>" (p267)
	# 6.31 "<datetime value function>" (p270)
	# 6.32 "<interval value expression>" (p272)
	# 6.33 "<interval value function>" (p277)
	# 8.2 "<comparison predicate>" (p375)
	# 8.5 "<like predicate>" (p385)
	# 8.7 "<null predicate>" (p397)
	# 8.9 "<exists predicate>" (p401)
	# 10.9 "<aggregate function>" (p505)
	my ($builder, $expr_node) = @_;
	my $sfunc = $expr_node->get_enumerated_attribute( 'call_sfunc' );
	my $child_exprs = $expr_node->get_child_nodes();
	if( $sfunc eq 'NOT' ) { #     - a logical 'not', true iif lone arg is false
		return( '(NOT '.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'AND' ) { #     - a logical 'and', true iif every arg is true
		return( '('.join( ' AND ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'OR' ) { #      - a logical 'or', true iif at least one arg is true
		return( '('.join( ' OR ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'XOR' ) { #     - a logical 'xor', true iif 1+ arg true and 1+ arg false
		# Not implemented yet.
	} elsif( $sfunc eq 'EQ' ) { #      - true if both args are equal (both args cast same tp)
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' = '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'NE' ) { #      - true if both args are unequal (when same data type)
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' <> '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'LT' ) { #      - true if first arg is less than second
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' < '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'GT' ) { #      - true if first arg is greater than second
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' > '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'LE' ) { #      - true if first arg is less than or equal to second
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' <= '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'GE' ) { #      - true if first arg is greater than or equal to second
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' >= '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'IS_NULL' ) { # - true if only arg is not a null value
		return( '('.$builder->build_expr( $child_exprs->[0] ).' IS NULL)' );
	} elsif( $sfunc eq 'NOT_NULL' ) { # - true if only arg is a null value
		return( '('.$builder->build_expr( $child_exprs->[0] ).' IS NOT NULL)' );
	} elsif( $sfunc eq 'COALESCE' ) { # - returns first arg which is not null (like Oracle 'NVL')
		return( 'COALESCE ('.join( ', ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
		# Oracle calls this NVL(...).
	} elsif( $sfunc eq 'SWITCH' ) { #  - a logical switch-case expr (like Oracle \'decode\')
		# Not implemented yet.  But the CASE/ELSE described at 6.11 (p197) would be used.
	} elsif( $sfunc eq 'LIKE' ) { #    - true if first arg contains second; args 3,4 are flags
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' LIKE '.$builder->build_expr( $child_exprs->[1] ).')' );
		# Flags (args 3,4 in SSM) not implemented yet.
	} elsif( $sfunc eq 'ADD' ) { #     - result of adding all args as numbers
		return( '('.join( ' + ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'SUB' ) { #     - result of subtracting all subsequent args from first
		return( '('.join( ' - ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'MUL' ) { #     - result of multiplying all arguments
		return( '('.join( ' * ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'DIV' ) { #     - result of dividing first argument by second
		return( '('.$builder->build_expr( $child_exprs->[0] ).
			' / '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'DIVI' ) { #    - integral division of first arg by second
		# Not implemented yet.
	} elsif( $sfunc eq 'MOD' ) { #     - modulus of integral division of first arg by second
		return( 'MOD ('.$builder->build_expr( $child_exprs->[0] ).
			', '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'ROUND' ) { #   - rounds first arg to N dec places; N is second arg or 0
		# Not implemented yet.
	} elsif( $sfunc eq 'ABS' ) { #     - absolute value of the operand (distance from zero)
		return( 'ABS ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'POWER' ) { #   - raises first arg to the power of the second
		return( 'POWER ('.$builder->build_expr( $child_exprs->[0] ).
			', '.$builder->build_expr( $child_exprs->[1] ).')' );
	} elsif( $sfunc eq 'LOG' ) { #     - logarithm of the first arg on the base of second
		# Note that SQL-2003 only defines LN(x), the natural logarithm, which is LOG-base-e-power-x;
		# we will implement LOG ourselves in terms of LN and DIV.
		my $operand = $builder->build_expr( $child_exprs->[0] );
		my $base = $builder->build_expr( $child_exprs->[1] );
		return( '(LN('.$operand.') / LN('.$base.'))' );
	} elsif( $sfunc eq 'SCONCAT' ) { # - L.cstr concat of all arguments
		return( '('.join( ' || ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'SLENGTH' ) { # - length of input string in characters
		return( 'CHAR_LENGTH ('.$builder->build_expr( $child_exprs->[0] ).')' );
		# OCTET_LENGTH for binary strings not implemented yet.
	} elsif( $sfunc eq 'SINDEX' ) { #  - pos in arg 1 of arg 2 if present, start at arg 3
		my $look_in = $builder->build_expr( $child_exprs->[0] );
		my $look_for = $builder->build_expr( $child_exprs->[1] );
		my $start_pos = $builder->build_expr( $child_exprs->[2] );
		return( 'POSITION ('.$look_for.' IN '.$look_in.')' ); # Arg 3 not implemented yet.
	} elsif( $sfunc eq 'SUBSTR' ) { #  - substr in arg 1 starting pos arg 2 of length arg 3
		my $look_in = $builder->build_expr( $child_exprs->[0] );
		my $start_pos = $builder->build_expr( $child_exprs->[1] );
		my $str_len = $builder->build_expr( $child_exprs->[2] );
		return( 'SUBSTRING ('.$look_in.' FROM '.$start_pos.($str_len?' FOR '.$str_len:'').')' );
		# Version using SIMILAR to look for regular expressions not implemented yet.
	} elsif( $sfunc eq 'SREPEAT' ) { # - L.cstr concat arg 1 to self repeated by arg 2 instances
		# Not implemented yet.
	} elsif( $sfunc eq 'STRIM' ) { #   - trims leading and trailing whitespace from arg 1
		my $trim_source = $builder->build_expr( $child_exprs->[0] );
		return( 'TRIM ('.$trim_source.')' );
		# SQL-2003, p259, says that "TRIM (<src>)" is implicitly equivalent 
		# to "TRIM (BOTH ' ' FROM <src>)", which behaviour we want.
		# Other similar functions, such as just trimming left or right, or something other 
		# than whitespace, isn't implemented yet.
	} elsif( $sfunc eq 'SPAD' ) { #    - lengthens arg 1 to length of arg 2 using arg 3 or space
		# Not implemented yet.  Perhaps OVERLAY defined in SQL-2003, 6.29 is what does this.
	} elsif( $sfunc eq 'SPADL' ) { #   - like spad but add filler on left rather than right
		# Not implemented yet.  Perhaps OVERLAY defined in SQL-2003, 6.29 is what does this.
	} elsif( $sfunc eq 'LC' ) { #      - lowercases latin chars in a string (SQL-2003 says this is a type of "folding")
		return( 'LOWER ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'UC' ) { #      - uppercases latin chars in a string (SQL-2003 says this is a type of "folding")
		return( 'UPPER ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'COUNT' ) { #  - aggregate - count of rows a view/cursor can see
		return( 'COUNT(*)' ); # specified in 10.9
	} elsif( $sfunc eq 'MIN' ) { #    - aggregate - minimum of values in all records in one view col
		return( 'MIN ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'MAX' ) { #    - aggregate - maximum of values in all records in one view col
		return( 'MAX ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'SUM' ) { #    - aggregate - sum of values in all records in one view col
		return( 'SUM ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'AVG' ) { #    - aggregate - average of values in all records in one view col
		return( 'AVG ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'CONCAT' ) { # - aggregate - L.cstr concat of values in all records in one view col
		# Not implemented yet.
	} elsif( $sfunc eq 'EVERY' ) { #  - aggregate - is true when all rec values in one col are true
		return( 'EVERY ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'ANY' ) { #    - aggregate - is true when at least one rec value in one col is true
		return( 'ANY ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'SOME' ) { #   - aggregate - is true when some rec values are true
		return( 'SOME ('.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'EXISTS' ) { # - aggregate - is true when if there are > 0 rows
		return( '(EXISTS '.$builder->build_expr( $child_exprs->[0] ).')' );
	} elsif( $sfunc eq 'GB_SETS' ) { # - olap, use in group-by - produces GROUPING SETS ( sub-exprs )
		return( 'GROUPING SETS ('.join( ', ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'GB_RLUP' ) { # - olap, use in group-by - produces ROLLUP ( sub-exprs )
		return( 'ROLLUP ('.join( ', ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} elsif( $sfunc eq 'GB_CUBE' ) { # - olap, use in group-by - produces CUBE ( sub-exprs )
		return( 'CUBE ('.join( ', ', map { $builder->build_expr( $_ ) } @{$child_exprs} ).')' );
	} else {}
}

######################################################################

sub build_expr_call_ufunc {
	my ($builder, $expr_node) = @_;
	my $ufunc = $expr_node->get_enumerated_attribute( 'call_ufunc' );
	my $ufunc_name = $builder->build_identifier_schema_obj( $ufunc );
	my %ufunc_arg_exprs = 
		map { ($_->get_node_ref_attribute( 'call_ufunc_arg' ) => $_) } 
		@{$expr_node->get_child_nodes()}; # gets child [view/routine]_expr Nodes
	# Note: The build_expr() calls are done below to ensure the arg values are 
	# defined in the same order they are output; this lets optional insertion 
	# of positionally determined bind vars (and their mapping) to work right.
	my $arg_val_list = join( ', ', 
		map { $ufunc_arg_exprs{$_} ? $builder->build_expr( $ufunc_arg_exprs{$_} ) : 'NULL' } 
		@{$ufunc->get_child_nodes( 'routine_arg' )} );
	return( $ufunc_name.($arg_val_list ? '('.$arg_val_list.')' : '') );
}

######################################################################

sub build_query_table_expr { # SQL-2003, 7.4 "<table expression>" (p300)
	my ($builder, $view_node) = @_;
	return( $builder->build_query_from_clause( $view_node ).
		' '.$builder->build_query_where_clause( $view_node ).
		' '.$builder->build_query_group_clause( $view_node ).
		' '.$builder->build_query_having_clause( $view_node ).
		' '.$builder->build_query_window_clause( $view_node ) );
}

######################################################################

sub build_query_from_clause { 
	# SQL-2003, 7.5 "<from clause>" (p301)
	# SQL-2003, 7.6 "<table reference>" (p303)
	# SQL-2003, 7.7 "<joined table>" (p312)
	# Method assumes that $view_node.view_type ne 'COMPOUND'; 
	# method should never be invoked for those kinds of views.
	# Function returns empty string if view has no 'from' clause (a rarity).
	my ($builder, $view_node) = @_;
	my $view_type = $view_node->get_enumerated_attribute( 'view_type' );
	my @view_src_nodes = $view_node->get_child_nodes( 'view_src' );
	my @view_join_nodes = $view_node->get_child_nodes( 'view_join' );
	if( @view_src_nodes == 0 ) {
		# There are no sources, and hence, no 'from' clause.
		return( '' );
	} elsif( $view_type eq 'MATCH' or $view_type eq 'SINGLE' or @view_src_nodes == 1 ) {
		# Trivial case: There is exactly one source, aka a single "<table factor>"; 
		# it can be either a table or named view or subquery.
		return( 'FROM '.$builder->build_query_table_factor( $view_src_nodes[0] ) );
	} else {
		# Complex case: There are 2 or more sources that are being joined.
		# The first step is to determine the join order, and only afterwards are 
		# each <table factor> rendered into SQL; each <table factor> must be 
		# generated in appearance order, so positional bind var mapping works right.
		# Note: This code isn't very smart and assumes that all the view_join Nodes 
		# are declared in the same order they should be output, even if their is 
		# other evidence to the contrary.  This code can be smartened later.
		# This code also assumes that the SSM is correct, such that all defined 
		# view_src Nodes are involved in a single common view_join; there should be 
		# exactly one fewer view_join Node than there are view_src Nodes.
		# TODO: Support the Oracle-8 way of putting join conditions in WHERE.
		my @sql_fragment_list = ();
		push( @sql_fragment_list, $builder->build_query_table_factor( 
			$view_join_nodes[0]->get_node_ref_attribute( 'lhs_src' ) ) );
		foreach my $view_join_node (@view_join_nodes) {
			my $join_type = $view_join_node->get_enumerated_attribute( 'join_type' );
			push( @sql_fragment_list, $join_type eq 'CROSS' ? 'CROSS JOIN' : 
				$join_type eq 'INNER' ? 'INNER JOIN' : 
				$join_type eq 'LEFT' ? 'LEFT OUTER JOIN' : 
				$join_type eq 'RIGHT' ? 'RIGHT OUTER JOIN' : 
				'FULL OUTER JOIN' ); # $join_type eq 'FULL'
			push( @sql_fragment_list, $builder->build_query_table_factor( 
				$view_join_node->get_node_ref_attribute( 'rhs_src' ) ) );
			my @join_on_sql = ();
			foreach my $view_join_col_node (@{$view_join_node->get_child_nodes( 'view_join_col' )}) {
				my $lhs_src_col_name = $builder->build_identifier_view_src_col( 
					$view_join_node->get_node_ref_attribute( 'lhs_src_col' ) );
				my $rhs_src_col_name = $builder->build_identifier_view_src_col( 
					$view_join_node->get_node_ref_attribute( 'rhs_src_col' ) );
				push( @join_on_sql, $rhs_src_col_name.' = '.$lhs_src_col_name );
			}
			push( @sql_fragment_list, 'ON '.join( ' AND ', @join_on_sql ) );
		}
		return( 'FROM '.join( ' ', @sql_fragment_list ) );
	}
}

sub build_query_table_factor { # SQL-2003, 7.6 "<table reference>" (p303)
	my ($builder, $view_src_node) = @_;
	my $match_node = ($view_src_node->get_node_ref_attribute( 'match_table' ) || 
		$view_src_node->get_node_ref_attribute( 'match_view' ));
	my $match_type = $match_node->get_node_type();
	# Maybe TODO: <derived column list>
	my $correlation_name = $builder->build_identifier_element( $view_src_node );
	if( $match_type eq 'table' ) {
		my $table_name = $builder->build_identifier_schema_obj( $match_node );
		return( $table_name.' AS '.$correlation_name );
	} else { # $match_type eq 'view'
		if( $match_node->get_node_ref_attribute( 'p_view' ) ) {
			# The view we are matching is a subquery.
			if( $builder->{$PROP_INLINE_SUBQ} ) {
				# Embed an anonymous subquery; argument passing is not yet supported.
				my $query_expression = $builder->build_query_query_expr( $match_node );
				return( '('.$query_expression.') AS '.$correlation_name );
			} else {
				# Call a named subquery; argument passing is supported.
				my %src_args_to_view_exprs = 
					map { ($_->get_node_ref_attribute( 'view_src_arg' ) => $_) } 
					grep { $_->get_enumerated_attribute( 'view_part' ) eq 'FROM' } 
					@{$view_src_node->get_node_ref_attribute( 'view' )->get_child_nodes( 'view_expr' )};
				my %view_args_to_src_args = 
					map { ($_->get_node_ref_attribute( 'match_view_arg' ) => $_) } 
					@{$view_src_node->get_child_nodes( 'view_src_arg' )};
				# Note: The build_expr() calls are done below to ensure the arg values are 
				# defined in the same order they are output; this lets optional insertion 
				# of positionally determined bind vars (and their mapping) to work right.
				my $arg_list = join( ', ', 
					map { ($_ ? $builder->build_expr( $_ ) : 'NULL') } 
					map { $src_args_to_view_exprs{$view_args_to_src_args{$_}} } 
					@{$match_node->get_child_nodes( 'view_arg' )} );
				my $view_name = $builder->build_identifier_element( $match_node );
				return( $view_name.($arg_list?'('.$arg_list.')':'').' AS '.$correlation_name );
			}
		} else { 
			# The view we are matching is a schema object.
			my $view_name = $builder->build_identifier_schema_obj( $match_node );
			return( $view_name.' AS '.$correlation_name );
		}
	}
}

######################################################################

sub build_query_where_clause { # SQL-2003, 7.8 "<where clause>" (p319)
	# Function returns empty string if view has no where clause.
	my ($builder, $view_node) = @_;
	my @expr_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'WHERE' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	return( @expr_list ? 'WHERE '.$expr_list[0] : '' );
}

######################################################################

sub build_query_group_clause { # SQL-2003, 7.9 "<group by clause>" (p320)
	# Function returns empty string if view has no group by clause.
	my ($builder, $view_node) = @_;
	my @expr_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'GROUP' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	return( @expr_list ? 'GROUP BY '.join( ', ', @expr_list ) : '' );
	# Notes: Within build_expr(): 
	# <grouping column reference> implemented by COL basic_expr_type,
	# <ordinary grouping set> and <empty grouping set> implemented by LIST basic_expr_type,
	# <grouping sets specification> impl by SFUNC named GB_SETS,
	# <rollup list> impl by SFUNC named GB_RLUP,
	# <cube list> impl by SFUNC named GB_CUBE.
	# Note: <group by clause> has opt <set quantifier>, looks redundant w SEL DIST|ALL; not impl.
}

######################################################################

sub build_query_having_clause { # SQL-2003, 7.10 "<having clause>" (p329)
	# Function returns empty string if view has no having clause.
	my ($builder, $view_node) = @_;
	my @expr_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'HAVING' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	return( @expr_list ? 'HAVING '.$expr_list[0] : '' );
}

######################################################################

sub build_query_window_clause { # SQL-2003, 7.11 "<window clause>" (p331)
	# Function returns empty string if view has no window clause.
	my ($builder, $view_node) = @_;
	# TODO: I need to first update SQL::SyntaxModel a bit re the various 
	# parts of a <window clause>, then fix here.  Meanwhile, I dump what I got.
	# Also see SQL-2003, 10.10 "<sort specification list>" (p517) for future reference.
	my @order_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'ORDER' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	my @maxr_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'MAXR' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	my @skipr_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'SKIPR' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	return( (@order_list ? 'ORDER BY '.join( ', ', @order_list ) : '').' '.
		(@maxr_list ? 'LIMIT '.$maxr_list[0] : '').' '.
		(@skipr_list ? 'OFFSET '.$skipr_list[0] : '') );
}

######################################################################

sub build_query_query_spec { 
	# SQL-2003, 7.12 "<query specification>" (p341)
	# SQL-2003, 14.5 "<select statement: single row>" (p824)
	my ($builder, $view_node, $make_into_clause) = @_;
	# Method assumes that $view_node.view_type ne 'COMPOUND'; 
	# method should never be invoked for those kinds of views.
	my $set_quantifier = $view_node->get_literal_attribute( 'distinct_rows' ) ? 'DISTINCT' : 'ALL';
	my $select_list = $builder->build_query_select_list( $view_node );
	my $into_clause = $make_into_clause ? $builder->build_query_into_clause( $view_node ) : '';
	my $table_expression = $builder->build_query_table_expr( $view_node );
	return( 'SELECT '.$set_quantifier.' '.$select_list.' '.$into_clause.' '.$table_expression );
}

######################################################################

sub build_query_select_list { # SQL-2003, 7.12 "<query specification>" (p341)
	# Method returns comma-delimited list expression where each list item is a 
	# "<derived column> ::= <value expression> AS <column name>".
	my ($builder, $view_node) = @_;
	if( $view_node->get_enumerated_attribute( 'view_type' ) eq 'MATCH' ) {
		# Each result column must match a source column exactly.
		if( $view_node->get_literal_attribute( 'match_all_cols' ) ) {
			# Every source table/view result column is output, with the same name.
			my $view_src_node = $view_node->get_child_nodes( 'view_src' )->[0];
			my $match_node = ($view_src_node->get_node_ref_attribute( 'match_table' ) || 
				$view_src_node->get_node_ref_attribute( 'match_view' ));
			return( join( ', ',
				map { $builder->build_identifier_fake_view_src_col( $view_src_node, $_ ).
					' AS '.$builder->build_identifier_element( $_ ) } 
				@{$match_node->get_child_nodes( $match_node->get_node_type().'_col' )} ) );
		} else {
			# Only some source table/view result columns are output; they may have different names.
			return( join( ', ',
				map { $builder->build_identifier_view_src_col( $_->get_node_ref_attribute( 'src_col' ) ).
					' AS '.$builder->build_identifier_element( $_ ) } 
				@{$view_node->get_child_nodes( 'view_col' )} ) );
		}
	} else { # view_type ne 'MATCH'
		# Each result column may come from an arbitrarily complex expression.
		# We have two statements below instead of one because we want the result cols shown  
		# in order of the 'view_col' Nodes, not the order of the 'view_part' if different.
		my %select_list_exprs = 
			map { ($_->get_node_ref_attribute( 'view_col' ) => $_) } 
			grep { $_->get_enumerated_attribute( 'view_part' ) eq 'RESULT' } 
			@{$view_node->get_child_nodes( 'view_expr' )};
		# Note: The build_expr() calls are done below to ensure the arg values are 
		# defined in the same order they are output; this lets optional insertion 
		# of positionally determined bind vars (and their mapping) to work right.
		return( join( ', ',
			map { ($_->get_node_ref_attribute( 'src_col' ) ? 
					$builder->build_identifier_view_src_col( $_->get_node_ref_attribute( 'src_col' ) ) : 
				$select_list_exprs{$_} ? $builder->build_expr( $select_list_exprs{$_} ) : 'NULL').
				' AS '.$builder->build_identifier_element( $_ ) } 
			@{$view_node->get_child_nodes( 'view_col' )} ) );
			# Note that the "||'NULL'" thing deals with view_col that don't have any view_expr.
			# TODO: Note that the 'view_col' Nodes we actually need may be in a parent view 
			# of the current view; right now we only are looking in the current view.
	}
}

######################################################################

sub build_query_into_clause { 
	# SQL-2003, 14.3 "<fetch statement>" (p817)
	# SQL-2003, 14.5 "<select statement: single row>" (p824)
	# Function assumes that there is an 'INTO' view_expr Node for every 
	# 'RESULT' view_expr Node, and that view.match_all_cols is false.
	my ($builder, $view_node) = @_;
	my %col_nodes_to_into_nodes = 
		map { ($_->get_node_ref_attribute( 'view_col' ) => $_) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'INTO' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	my @target_list = ();
	foreach my $view_col_node (@{$view_node->get_child_nodes( 'view_col' )}) {
		my $into_expr_node = $col_nodes_to_into_nodes{$view_col_node};
		push( @target_list, $builder->build_identifier_element( 
		$into_expr_node->get_node_ref_attribute( 'routine_arg' ) || 
		$into_expr_node->get_node_ref_attribute( 'routine_var' ) ) );
	}
	return( 'INTO '.join( ', ', @target_list ) );
}

######################################################################

sub build_query_query_expr { # SQL-2003, 7.13 "<query expression>" (p351)
	my ($builder, $view_node) = @_;
	my $view_type = $view_node->get_enumerated_attribute( 'view_type' );
	my $with_clause = '';
	if( $view_type eq 'SUBQUERY' and !$builder->{$PROP_INLINE_SUBQ} ) {
		my @with_list = ();
		my $recursive = 0;
		foreach my $child_view_node (@{$view_node->get_child_nodes( 'view' )}) {
			if( $child_view_node->get_enumerated_attribute( 'view_type' ) eq 'RECURSIVE' ) {
				$recursive = 1;
			}
			my $with_item = $builder->build_identifier_element( $child_view_node );
			if( my @child_arg_nodes = @{$child_view_node->get_child_nodes( 'view_arg' )} ) {
				$with_item .= '('.join( ', ', 
					map { $builder->build_identifier_element( $_ ) } 
					@child_arg_nodes ).')';
			}
			$with_item .= ' AS ('.$builder->build_query_query_expr( $child_view_node ).')';
			push( @with_list, $with_item );
		}
		if( @with_list ) {
			$with_clause = 'WITH '.($recursive ? 'RECURSIVE ' : '').join( ', ', @with_list );
		}
	}
	my $query_expression_body = $builder->build_query_query_expr_body( $view_node );
	return( $with_clause.$query_expression_body );
	# TODO: SQL-2003, 7.14 "<search or cycle clause>" (p365).
}

######################################################################

sub build_query_query_expr_body { # SQL-2003, 7.13 "<query expression>" (p351)
	my ($builder, $view_node) = @_;
	my $view_type = $view_node->get_enumerated_attribute( 'view_type' );
	if( $view_type eq 'COMPOUND' ) {
		# Result is multiple "SELECT ..." connected by one or more compound operators.
		my $compound_op = $view_node->get_enumerated_attribute( 'compound_op' );
		my $set_quantifier = $view_node->get_literal_attribute( 'distinct_rows' ) ? 'DISTINCT' : 'ALL';
		my @operand_list = ();
		foreach my $child_view_node (@{$view_node->get_child_nodes( 'view' )}) {
			push( @operand_list, $builder->build_query_query_expr_body( $child_view_node ) );
		}
		my $sql_operator = $compound_op eq 'UNION' ? 'UNION' : 
			$compound_op eq 'DIFFERENCE' ? 'EXCEPT' : 
			$compound_op eq 'INTERSECTION' ? 'INTERSECT' : 
			'EXCLUSION'; # $compound_op eq 'EXCLUSION'; this 4th option not in SQL-2003.
			# TODO: try to emulate 'EXCLUSION' somewhere.
		return( '('.join( $sql_operator.'_'.$set_quantifier, @operand_list ).')' );
		# TODO: deal with engines that don't like "()" bounding compound operations.
		# TODO: possibly implement <corresponding spec>.
	} else { # $view_type ne 'COMPOUND'
		# Result is a single "SELECT ...", also known as a single "<query specification>".
		return( $builder->build_query_query_spec( $view_node ) );
	}
}

######################################################################

sub build_query_subquery { # SQL-2003, 7.15 "<subquery>" (p370)
	my ($builder, $expr_node) = @_;
	my $cview = $expr_node->get_enumerated_attribute( 'call_view' );
	if( $builder->{$PROP_INLINE_SUBQ} ) {
		# Embed an anonymous subquery; argument passing is not yet supported.
		my $query_expression = $builder->build_query_query_expr( $cview );
		return( '('.$query_expression.')' );
	} else {
		# Call a named subquery; argument passing is supported.
		my $cview_name = $builder->build_identifier_schema_obj( $cview );
		my %cview_arg_exprs = 
			map { ($_->get_node_ref_attribute( 'call_view_arg' ) => $_) } 
			@{$expr_node->get_child_nodes()}; # gets child view_expr Nodes
		# Note: The build_expr() calls are done below to ensure the arg values are 
		# defined in the same order they are output; this lets optional insertion 
		# of positionally determined bind vars (and their mapping) to work right.
		my $arg_val_list = join( ', ', 
			map { $cview_arg_exprs{$_} ? $builder->build_expr( $cview_arg_exprs{$_} ) : 'NULL' } 
			@{$cview->get_child_nodes( 'view_arg' )} );
		return( $cview_name.($arg_val_list ? '('.$arg_val_list.')' : '') );
	}
	# Note: Direct calls to schema object tables or views is not supported outside of 'FROM'.
}

######################################################################

sub build_schema_schema_create { # SQL-2003, 11.1 "<schema definition>" (p519)
	my ($builder, $schema_node) = @_;
	my $schema_name = $builder->build_identifier_element( $schema_node );
	my $authorization = ''; # TODO: AUTHORIZATION <authorization identifier>
	# Some other features in 11.1, such as default character set.
	return( 'CREATE SCHEMA '.$schema_name.' '.$authorization.';' );
}

sub build_schema_schema_delete { # SQL-2003, 11.2 "<drop schema statement>" (p522)
	my ($builder, $schema_node) = @_;
	my $schema_name = $builder->build_identifier_element( $schema_node );
	return( 'DROP SCHEMA '.$schema_name.';' );
}

######################################################################

sub build_schema_domain_create { # SQL-2003, 11.24 "<domain definition>" (p603)
	my ($builder, $domain_node) = @_;
	my $domain_name = $builder->build_identifier_schema_obj( $domain_node, 1 );
	my $predefined_type = $builder->build_expr_predef_data_type( $domain_node );
	# TODO: default clause, domain constraint, collate clause.
	return( 'CREATE DOMAIN '.$domain_name.' AS '.$predefined_type.';' );
}

sub build_schema_domain_delete { # SQL-2003, 11.30 "<drop domain statement>" (p610)
	my ($builder, $domain_node) = @_;
	my $domain_name = $builder->build_identifier_schema_obj( $domain_node, 1 );
	return( 'DROP DOMAIN '.$domain_name.';' );
}

######################################################################

sub build_schema_sequence_create { # SQL-2003, 11.62 "<sequence generator definition>" (p726)
	my ($builder, $sequence_node) = @_;
	# SQL-2003 allows multiple data types for this, but we stick to integers for now.
	my $sequence_name = $builder->build_identifier_schema_obj( $sequence_node, 1 );
	my $increment = $sequence_node->get_literal_attribute( 'increment' );
	my $min_val = $sequence_node->get_literal_attribute( 'min_val' );
	my $max_val = $sequence_node->get_literal_attribute( 'max_val' );
	my $start_val = $sequence_node->get_literal_attribute( 'start_val' );
	my $cycle = $sequence_node->get_literal_attribute( 'cycle' );
	my $order = $sequence_node->get_literal_attribute( 'order' );
	# Note that SQL::SyntaxModel guarantees all integer attributes are already valid integers.
	return( 'CREATE SEQUENCE '.$sequence_name.
		(defined( $increment ) ? ' INCREMENT BY '.$increment : '').
		(defined( $start_val ) ? ' START WITH '.$start_val : '').
		(defined( $min_val ) ? ' MINVALUE '.$min_val : ' NO MINVALUE').
		(defined( $max_val ) ? ' MAXVALUE '.$max_val : ' NO MAXVALUE').
		($cycle ? ' CYCLE' : ' NO CYCLE').
		($order ? ' ORDER' : ' NO ORDER'). # standard doesn't mention this one
		';' );
}

sub build_schema_sequence_delete { # SQL-2003, 11.64 "<drop sequence generator statement>" (p729)
	my ($builder, $sequence_node) = @_;
	my $sequence_name = $builder->build_identifier_schema_obj( $sequence_node, 1 );
	return( 'DROP SEQUENCE '.$sequence_name.';' );
}

######################################################################

sub build_schema_table_create { 
	# SQL-2003, 6.2 "<field definition>" (p173)
	# SQL-2003, 11.3 "<table definition>" (p525)
	# SQL-2003, 11.4 "<column definition>" (p536)
	# SQL-2003, 11.5 "<default clause>" (p541)
	# SQL-2003, 11.6 "<table constraint definition>" (p545)
	# SQL-2003, 11.7 "<unique constraint definition>" (p547)
	# SQL-2003, 11.8 "<referential constraint definition>" (p549)
	# TODO: SQL-2003, 11.9 "<check constraint definition>" (p569)
	my ($builder, $table_node, $is_temp) = @_;
	my $table_name = $builder->build_identifier_schema_obj( $table_node, 1 );
	my @table_col_sql = ();
	my %col_name_cache = (); # used when making ind defs
	my %mandatory_col_cache = (); # used when making ind defs
	foreach my $table_col_node (@{$table_node->get_child_nodes( 'table_col' )}) {
		my $table_col_name = $builder->build_identifier_element( $table_col_node );
		unless( exists( $col_name_cache{$table_col_node} ) ) {
			$col_name_cache{$table_col_node} = $table_col_name;
		}
		my $domain_node = $table_col_node->get_node_ref_attribute( 'domain' );
		my $domain_sql = $builder->build_expr_data_type_or_domain_name( $domain_node );
		my $mandatory = $table_col_node->get_literal_attribute( 'mandatory' );
		$mandatory and $mandatory_col_cache{$table_col_node} = 1;
		my $default_val = $table_col_node->get_literal_attribute( 'default_val' );
		my $auto_inc = $table_col_node->get_literal_attribute( 'auto_inc' );
		my $default_seq_node = $table_col_node->get_node_ref_attribute( 'default_seq' );
		push( @table_col_sql, 
			$table_col_name.' '.$domain_sql.
			($mandatory ? ' NOT NULL' : ' NULL').
			(defined( $default_val ) ? ' DEFAULT '.$builder->quote_literal( 
				$default_val, $domain_node->get_enumerated_attribute( 'base_type' ) ) : '').
			($auto_inc ? ' AUTO_INCREMENT' : '').
			($default_seq_node ? ' DEFAULT '.
				$builder->build_expr_seq_next( $default_seq_node ) : '')
		);
	}
	my @table_ind_sql = ();
	my $pk_is_made = 0;
	foreach my $table_ind_node (@{$table_node->get_child_nodes( 'table_ind' )}) {
		my $table_ind_name = $builder->build_identifier_element( $table_ind_node );
		my $ind_type = $table_ind_node->get_enumerated_attribute( 'ind_type' );
		my @table_ind_col_nodes = @{$table_ind_node->get_child_nodes( 'table_ind_col' )};
		my $local_col_names_sql = join( ', ', map { 
				$col_name_cache{$_->get_node_ref_attribute( 'table_col' )} 
			} @table_ind_col_nodes );
		if( $ind_type eq 'ATOMIC' ) {
			push( @table_ind_sql, 'INDEX '.$table_ind_name.' ('.$local_col_names_sql.')' );
		}
		if( $ind_type eq 'FULLTEXT' ) {
			push( @table_ind_sql, 'FULLTEXT INDEX '.$table_ind_name.' ('.$local_col_names_sql.')' );
		}
		if( $ind_type eq 'UNIQUE' or $ind_type eq 'UFOREIGN' ) {
			my $make_a_pk_now = 0;
			unless( $pk_is_made ) {
				# All component columns of a primary key must be mandatory; check for it.
				$make_a_pk_now = 1;
				foreach my $table_ind_col_node (@table_ind_col_nodes) {
					my $table_col_node = $table_ind_col_node->get_node_ref_attribute( 'table_col' );
					unless( $mandatory_col_cache{$table_col_node} ) {
						$make_a_pk_now = 0;
						last;
					}
				}
			}
			if( $make_a_pk_now ) {
				push( @table_ind_sql, 'CONSTRAINT PRIMARY KEY ('.$local_col_names_sql.')' );
			} else {
				push( @table_ind_sql, 'CONSTRAINT '.$table_ind_name.' UNIQUE '.
					' ('.$local_col_names_sql.')' ); # standard does not say INDEX after UNIQUE
			}
		}
		if( $ind_type eq 'FOREIGN' or $ind_type eq 'UFOREIGN' ) {
			my $foreign_table_name = $builder->build_identifier_schema_obj( 
				$table_ind_node->get_node_ref_attribute( 'f_table' ) );
			my $foreign_col_names_sql = join( ', ', map { 
					$builder->build_identifier_element( $_->get_node_ref_attribute( 'f_table_col' ) )
				} @table_ind_col_nodes );
			push( @table_ind_sql, 'CONSTRAINT '.$table_ind_name.' FOREIGN KEY '.
				' ('.$local_col_names_sql.') REFERENCES '.$foreign_table_name.
				' ('.$foreign_col_names_sql.')' );
		}
	}
	return( 'CREATE'.($is_temp ? ' TEMPORARY' : '').' TABLE '.$table_name.
		' ('.join(', ', @table_col_sql, @table_ind_sql).');' );
}

sub build_schema_table_delete { # SQL-2003, 11.21 "<drop table statement>" (p587)
	my ($builder, $table_node) = @_;
	my $table_name = $builder->build_identifier_schema_obj( $table_node, 1 );
	return( 'DROP TABLE '.$table_name.';' );
}

######################################################################

sub build_schema_view_create { # SQL-2003, 11.22 "<view definition>" (p590)
	my ($builder, $view_node) = @_;
	my $view_name = $builder->build_identifier_schema_obj( $view_node, 1 );
	my $query_expression = $builder->build_query_query_expr( $view_node );
	return( 'CREATE VIEW '.$view_name.' AS '.$query_expression.';' );
	# Note: Several interesting looking features are not implemented yet.
}

sub build_schema_view_delete { # SQL-2003, 11.23 "<drop view statement>" (p600)
	my ($builder, $view_node) = @_;
	my $view_name = $builder->build_identifier_schema_obj( $view_node, 1 );
	return( 'DROP VIEW '.$view_name.';' );
}

######################################################################

sub build_schema_routine_create { 
	# SQL-2003, 11.39 "<trigger definition>" (p629)
	# SQL-2003, 11.50 "<SQL-invoked routine>" (p675)
	my ($builder, $routine_node) = @_;
	my $routine_type = $routine_node->get_enumerated_attribute( 'routine_type' );
	my $routine_name = $builder->build_identifier_schema_obj( $routine_node, 1 );
	if( $routine_type eq 'PACKAGE' ) {
		# Not implemented yet.
	} elsif( $routine_type eq 'TRIGGER' ) {
		my $table_or_view_name = $builder->build_identifier_schema_obj( 
			$routine_node->get_node_ref_attribute( 'table' ) || 
			$routine_node->get_node_ref_attribute( 'view' ) );
		my $trigger_event = $routine_node->get_enumerated_attribute( 'trigger_event' );
		my $trigger_event_sql = $trigger_event eq 'BEFR_INS' ? 'BEFORE INSERT' : 
			$trigger_event eq 'AFTR_INS' ? 'AFTER INSERT' : 
			$trigger_event eq 'INST_INS' ? 'INSTEAD OF INSERT' : 
			$trigger_event eq 'BEFR_UPD' ? 'BEFORE UPDATE' : 
			$trigger_event eq 'AFTR_UPD' ? 'AFTER UPDATE' : 
			$trigger_event eq 'INST_UPD' ? 'INSTEAD OF UPDATE' : 
			$trigger_event eq 'BEFR_DEL' ? 'BEFORE DELETE' : 
			$trigger_event eq 'AFTR_DEL' ? 'AFTER DELETE' : 
			'INSTEAD OF DELETE'; # $trigger_event eq 'INST_DEL'
			# Note: INSTEAD OF is not standard SQL, but supported by SQLServer 2000, maybe Oracle, ?.
		my $for_each_stmt = $routine_node->get_literal_attribute( 'trigger_per_stmt' );
		# TODO: Implement optional OF <trigger column list>.
		my @transition_var_names = (); # TODO: NEW/OLD ROW AS <... variable name>
		my $triggered_sql_statement = $builder->build_dmanip_routine_body( $routine_node, 1 );
		return( 'CREATE TRIGGER '.$routine_name.' '.
			$trigger_event_sql.' ON '.$table_or_view_name.
			(@transition_var_names ? ' REFERENCING '.join( ' ', @transition_var_names ) : '').
			($for_each_stmt ? ' FOR EACH STATEMENT' : ' FOR EACH ROW').
			# TODO: WHEN ( <search condition> )
			($builder->{$PROP_ORA_ROUTINES} ? 'AS ' : '').
			$triggered_sql_statement.
			';' );
	} elsif( $routine_type eq 'PROCEDURE' ) {
		my @param_decl_list = ();
		# TODO: <routine characteristics> where appropriate.
		my $routine_body = $builder->build_dmanip_routine_body( $routine_node );
		return( 'CREATE PROCEDURE '.$routine_name.
			(@param_decl_list ? '('.join( ', ', @param_decl_list ).')' : '').
			($builder->{$PROP_ORA_ROUTINES} ? 'AS ' : '').
			' '.$routine_body.';' );
	} elsif( $routine_type eq 'FUNCTION' ) {
		my @param_decl_list = ();
		# TODO: <routine characteristics> where appropriate.
		my $routine_body = $builder->build_dmanip_routine_body( $routine_node );
		my $return_data_type = $builder->build_expr_data_type_or_domain_name( 
			$routine_node->get_node_ref_attribute( 'domain' ) );
		return( 'CREATE FUNCTION '.$routine_name.
			(@param_decl_list ? '('.join( ', ', @param_decl_list ).')' : '').
			' RETURNS '.$return_data_type.
			($builder->{$PROP_ORA_ROUTINES} ? 'AS ' : '').
			' '.$routine_body.';' );
	} else {} # $routine_type eq 'BLOCK' or 'ANONYMOUS'
		# 'BLOCK': no-op; you should call build_dmanip_routine_body() directly instead
		# 'ANONYMOUS': no-op; it isn't a schema object   
}

sub build_schema_routine_delete { 
	# SQL-2003, 11.40 "<drop trigger statement>" (p633)
	# SQL-2003, 11.52 "<drop routine statement>" (p703)
	my ($builder, $routine_node) = @_;
	my $routine_type = $routine_node->get_enumerated_attribute( 'routine_type' );
	my $routine_name = $builder->build_identifier_schema_obj( $routine_node, 1 );
	# Note: 10.6 "<specific routine designator>" (p499) may be useful later.
	if( $routine_type eq 'PACKAGE' ) {
		# Not implemented yet.
	} elsif( $routine_type eq 'TRIGGER' ) {
		return( 'DROP TRIGGER '.$routine_name.';' );
	} elsif( $routine_type eq 'PROCEDURE' ) {
		return( 'DROP PROCEDURE '.$routine_name.';' );
	} elsif( $routine_type eq 'FUNCTION' ) {
		return( 'DROP FUNCTION '.$routine_name.';' );
	} else {} # $routine_type eq 'BLOCK' or 'ANONYMOUS'; no-op
}

######################################################################

sub build_access_role_create { # SQL-2003, 12.4 "<role definition>" (p743)
	my ($builder, $role_node) = @_;
	my $role_name = $builder->build_identifier_element( $role_node );
	return( 'CREATE ROLE '.$role_name.';' );
}

sub build_access_role_delete { # SQL-2003, 12.6 "<drop role statement>" (p746)
	my ($builder, $role_node) = @_;
	my $role_name = $builder->build_identifier_element( $role_node );
	return( 'DROP ROLE '.$role_name.';' );
}

######################################################################

sub build_access_grant { 
	# Function returns empty string if given grantee has no privileges.
	# SQL-2003, 12.1 "<grant statement>" (p731)
	# SQL-2003, 12.2 "<grant privilege statement>" (p736)
	# SQL-2003, 12.3 "<privileges>" (p739)
	# SQL-2003, 12.5 "<grant role statement>" (p744)
	my ($builder, $grantee_node) = @_;
	my $node_type = $grantee_node->get_node_type();
	my $grantee_name = $builder->build_identifier_schema_obj( $grantee_node );
	if( $node_type eq 'role' ) {
		my @grant_stmts = ();
		foreach my $priv_on_node (@{$grantee_node->get_child_nodes( 'privilege_on' )}) {
			my $attrs = $priv_on_node->get_node_ref_attributes();
			my $object_node = $attrs->{'schema'} || $attrs->{'domain'} || $attrs->{'sequence'} || 
				$attrs->{'table'} || $attrs->{'routine'}; # assumes exactly one is set
			my $object_name = $builder->build_identifier_schema_obj( $object_node );
			my @priv_types = map { $_->get_enumerated_attribute( 'priv_type' ) } 
				@{$priv_on_node->get_child_nodes( 'privilege_for' )};
			my @object_privs = ();
			if( grep { $_ eq 'ALL' } @priv_types ) {
				push( @object_privs, 'ALL PRIVILEGES' );
			} else {
				foreach my $priv_type (@priv_types) {
					push( @object_privs, scalar(
						$priv_type eq 'SELECT' ? 'SELECT' : # TODO: allow only specific columns
						$priv_type eq 'DELETE' ? 'DELETE' : 
						$priv_type eq 'INSERT' ? 'INSERT' : # TODO: allow only specific columns
						$priv_type eq 'UPDATE' ? 'UPDATE' : # TODO: allow only specific columns
						'' # TODO: REFERENCES, USAGE, TRIGGER, UNDER, EXECUTE; what do they mean?
					) );
				}
			}
			push( @grant_stmts, 'GRANT '.join( ', ', @object_privs ).
				' ON '.$object_name.' TO '.$grantee_name.';' );
		}
		return( join( '', @grant_stmts ) );
	} elsif( $node_type eq 'user' ) {
		my @role_names = map { $builder->build_identifier_schema_obj( $_ ) } 
			$grantee_node->get_child_nodes( 'user_role' );
		return( @role_names ? 'GRANT '.join( ', ', @role_names ).' TO '.$grantee_name.';' : '' );
	} else {}
}

sub build_access_revoke { 
	# SQL-2003, 12.7 "<revoke statement>" (p747)
	# SQL-2003, 12.3 "<privileges>" (p739)
	my ($builder, $grantee_node) = @_;
	my $node_type = $grantee_node->get_node_type();
	my $grantee_name = $builder->build_identifier_schema_obj( $grantee_node );
	if( $node_type eq 'role' ) {
		my @revoke_stmts = ();
		foreach my $priv_on_node (@{$grantee_node->get_child_nodes( 'privilege_on' )}) {
			my $attrs = $priv_on_node->get_node_ref_attributes();
			my $object_node = $attrs->{'schema'} || $attrs->{'domain'} || $attrs->{'sequence'} || 
				$attrs->{'table'} || $attrs->{'routine'}; # assumes exactly one is set
			my $object_name = $builder->build_identifier_schema_obj( $object_node );
			push( @revoke_stmts, 'REVOKE ALL PRIVILEGES'.
				' ON '.$object_name.' FROM '.$grantee_name.';' );
		}
		return( join( '', @revoke_stmts ) );
	} elsif( $node_type eq 'user' ) {
		my @role_names = map { $builder->build_identifier_schema_obj( $_ ) } 
			$grantee_node->get_child_nodes( 'user_role' );
		return( @role_names ? 'REVOKE '.join( ', ', @role_names ).' FROM '.$grantee_name.';' : '' );
	} else {}
}

######################################################################

sub build_dmanip_routine_body { 
	# Corresponds to these sections:
	# ?
	# SQL-2003, 13.5 "<SQL procedure statement>" (p790)
	# SQL-2003, 14.1 "<declare cursor>" (p809)
	my ($builder, $routine_node, $is_atomic) = @_;
	my $is_ora_routines = $builder->{$PROP_ORA_ROUTINES};
	my @rtn_var_declare_sql = ();
	foreach my $rtn_var_node (@{$routine_node->get_child_nodes( 'routine_var' )}) {
		my $var_name = $builder->build_identifier_element( $rtn_var_node );
		my $var_type = $rtn_var_node->get_enumerated_attribute( 'var_type' );
		if( $var_type eq 'SCALAR' ) {
			my $domain_node = $rtn_var_node->get_node_ref_attribute( 'domain' );
			my $domain_sql = $builder->build_expr_data_type_or_domain_name( $domain_node );
			my $init_lit_val = $rtn_var_node->get_literal_attribute( 'init_lit_val' );
			my $is_constant = $rtn_var_node->get_literal_attribute( 'is_constant' );
			push( @rtn_var_declare_sql, 
				($is_ora_routines ? '' : 'DECLARE ').
				$var_name.' '.$domain_sql.
				# TODO: use $is_constant
				(defined( $init_lit_val ) ? ' DEFAULT '.$builder->quote_literal( 
					$init_lit_val, $domain_node->get_enumerated_attribute( 'base_type' ) ) : '').
				';'
			);
		} elsif( $var_type eq 'RECORD' ) {
			# Not implemented yet.
		} elsif( $var_type eq 'ARRAY' ) {
			# Not implemented yet.
		} elsif( $var_type eq 'CURSOR' ) {
			my $view_node = $rtn_var_node->get_node_ref_attribute( 'curs_view' );
			my $query_expr = $builder->build_query_query_expr( $view_node );
			my $order_by_clause = $builder->build_query_window_clause( $view_node );
			my $updatability_clause = 
				$rtn_var_node->get_literal_attribute( 'curs_for_update' ) ? 
				'FOR UPDATE' : 'FOR READ ONLY'; # TODO: [ OF <column name list> ]
			# TODO: sensitivity, scrollability, holdability, returnability
			my $cursor_spec = $query_expr.' '.$order_by_clause.' '.$updatability_clause;
			push( @rtn_var_declare_sql, 'DECLARE '.$var_name.' CURSOR FOR '.$cursor_spec.';' );
		} else {}
	}
	my @rtn_stmt_sql = ();
	foreach my $rtn_stmt_node (@{$routine_node->get_child_nodes( 'routine_stmt' )}) {
		push( @rtn_stmt_sql, $builder->build_dmanip_routine_stmt( $rtn_stmt_node ) );
	}
	my $atomic_clause = $is_atomic ? 'ATOMIC ' : '';
	return( join( ' ', 
		# TODO: proper handling of vars declared within 'BLOCK' routines, when in Oracle syntax.
		($is_ora_routines ? 'VAR ' : 'BEGIN '.$atomic_clause),
		@rtn_var_declare_sql,
		($is_ora_routines ? 'BEGIN '.$atomic_clause : ''),
		@rtn_stmt_sql,
		'END;'
	) );
}

######################################################################

sub build_dmanip_routine_stmt { 
	# SQL-2003, 13.5 "<SQL procedure statement>" (p790)
	# SQL-2003, 15.2 "<return statement>" (p886)
	my ($builder, $rtn_stmt_node) = @_;
	my $stmt_type = $rtn_stmt_node->get_enumerated_attribute( 'stmt_type' );
	if( $stmt_type eq 'BLOCK' ) {
		my $compound_stmt_routine = $rtn_stmt_node->get_node_ref_attribute( 'block_routine' );
		return( $builder->build_dmanip_routine_body( $compound_stmt_routine ) );
	} elsif( $stmt_type eq 'ASSIGN' ) {
		my $dest = $builder->build_identifier_element( 
			$rtn_stmt_node->get_node_ref_attribute( 'dest_arg' ) || 
			$rtn_stmt_node->get_node_ref_attribute( 'dest_var' ) );
		my $src = $builder->build_expr( 
			$rtn_stmt_node->get_child_nodes( 'routine_expr' )->[0] );
		if( $builder->{$PROP_ORA_ROUTINES} ) {
			return( $dest.' := '.$src.';' );
		} else {
			return( 'SET '.$dest.' = '.$src.';' );
		}
	} elsif( $stmt_type eq 'RETURN' ) {
		my $child_exprs = $rtn_stmt_node->get_child_nodes();
		return( 'RETURN '.$builder->build_expr( $child_exprs->[0] ).';' ); # no parens in standard
	} elsif( $stmt_type eq 'SPROC' ) {
		return( $builder->build_dmanip_call_sproc( $rtn_stmt_node ) );
	} elsif( $stmt_type eq 'UPROC' ) {
		return( $builder->build_dmanip_call_uproc( $rtn_stmt_node ) );
	} else {}
}

######################################################################

sub build_dmanip_call_sproc {
	# Corresponds to these sections:
	# SQL-2003, 14.2 "<open statement>" (p815)
	# SQL-2003, 14.3 "<fetch statement>" (p817)
	# SQL-2003, 14.4 "<close statement>" (p822)
	my ($builder, $rtn_stmt_node) = @_;
	my $sproc = $rtn_stmt_node->get_enumerated_attribute( 'call_sproc' );
	if( $sproc eq 'CURSOR_OPEN' ) { # opens a select cursor for reading from (or performs a select if in right context)
		my $cursor_name = $builder->build_identifier_element( 
			$rtn_stmt_node->get_node_ref_attribute( 'curs_arg' ) || 
			$rtn_stmt_node->get_node_ref_attribute( 'curs_var' ) );
		return( 'OPEN '.$cursor_name.';' );
	} elsif( $sproc eq 'CURSOR_CLOSE' ) { # closes a select cursor when you're done with it
		my $cursor_name = $builder->build_identifier_element( 
			$rtn_stmt_node->get_node_ref_attribute( 'curs_arg' ) || 
			$rtn_stmt_node->get_node_ref_attribute( 'curs_var' ) );
		return( 'CLOSE '.$cursor_name.';' );
	} elsif( $sproc eq 'CURSOR_FETCH' ) { # reads a row from an opened cursor and puts it in a row/list variable
		my $cursor_name = $builder->build_identifier_element( 
			$rtn_stmt_node->get_node_ref_attribute( 'curs_arg' ) || 
			$rtn_stmt_node->get_node_ref_attribute( 'curs_var' ) );
		my $fetch_orient = ''; # TODO: the explicit <fetch orientation> options; NEXT is default
		my $view_node = ($rtn_stmt_node->get_node_ref_attribute( 'curs_arg' ) || 
			$rtn_stmt_node->get_node_ref_attribute( 'curs_var' )
			)->get_node_ref_attribute( 'curs_view' );
		my $into_clause = $builder->build_query_into_clause( $view_node );
		return( 'FETCH '.$fetch_orient.' FROM '.$cursor_name.' '.$into_clause.';' );
	} elsif( $sproc eq 'SELECT_INTO' ) { # fetches one row from a table/view and puts it in a row/list variable
		my $view_node = $rtn_stmt_node->get_node_ref_attribute( 'view_for_dml' );
		return( $builder->build_query_query_spec( $view_node, 1 ).';' );
	} elsif( $sproc eq 'INSERT' ) { # inserts a row into a table/view
		my $view_node = $rtn_stmt_node->get_node_ref_attribute( 'view_for_dml' );
		return( $builder->build_dmanip_insert_stmt( $view_node ) );
	} elsif( $sproc eq 'UPDATE' ) { # updates a row in a table/view
		my $view_node = $rtn_stmt_node->get_node_ref_attribute( 'view_for_dml' );
		return( $builder->build_dmanip_update_stmt( $view_node ) );
	} elsif( $sproc eq 'DELETE' ) { # deletes a row in a table/view
		my $view_node = $rtn_stmt_node->get_node_ref_attribute( 'view_for_dml' );
		return( $builder->build_dmanip_delete_stmt( $view_node ) );
	} elsif( $sproc eq 'COMMIT' ) { # commits the current transaction, then starts a new one
		return( 'COMMIT;' );
	} elsif( $sproc eq 'ROLLBACK' ) { # rolls back the current transaction, then starts a new one
		return( 'ROLLBACK;' ); # TODO: rollback to a named save point only
	} else {} # There are a bunch more that aren't implemented yet.
}

######################################################################

sub build_dmanip_src_schema_object_name {
	my ($builder, $view_node) = @_;
	my $view_type = $view_node->get_enumerated_attribute( 'view_type' );
	my @view_src_nodes = $view_node->get_child_nodes( 'view_src' );
	if( @view_src_nodes == 0 ) {
		return( undef ); # No source at all.
	} elsif( $view_type eq 'MATCH' or $view_type eq 'SINGLE' or @view_src_nodes == 1 ) {
		my $object_node = $view_src_nodes[0]->get_node_ref_attribute( 'match_table' ) || 
			$view_src_nodes[0]->get_node_ref_attribute( 'match_view' );
		if( $object_node->get_node_ref_attribute( 'schema' ) ) {
			# The only source is a schema object, table or named view; use it directly.
			return( $builder->build_identifier_schema_obj( $view_node ) );
		} else {
			# The only source seems to be a sub-query in "from".
			return( undef ); # Adding recursion in all necessary places too complicated for now.
		}
	} else { # @view_src_nodes >= 2
		return( undef ); # Manual updates against multiple sources too complicated for now.
	}
}

######################################################################

sub build_dmanip_insert_stmt { 
	# SQL-2003, 7.3 "<table value constructor>" (p298)
	# SQL-2003, 14.8 "<insert statement>" (p834)
	my ($builder, $view_node) = @_;
	$builder->{$PROP_UNWRAP_VIEWS} = 1;
	my $object_name = $builder->build_dmanip_src_schema_object_name( $view_node );
	my @set_expr_nodes = 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'SET' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	my @set_cols_list = ();
	my @set_values_list = ();
	foreach my $expr_node (@set_expr_nodes) {
		push( @set_cols_list, $builder->build_identifier_element( 
			$expr_node->get_node_ref_attribute( 'set_view_col' ) ) );
		push( @set_values_list, $builder->build_expr( $expr_node ) );
	}
	my $insert_cols_and_src = '('.join( ', ', @set_cols_list ).') '.
		'VALUES ('.join( ', ', @set_values_list ).')';
	$builder->{$PROP_UNWRAP_VIEWS} = 0;
	return( 'INSERT INTO '.$object_name.' '.$insert_cols_and_src.';' );
}

######################################################################

sub build_dmanip_update_stmt { 
	# SQL-2003, 14.11 "<update statement: searched>" (p849)
	# SQL-2003, 14.12 "<set clause list>" (p853)
	my ($builder, $view_node) = @_;
	$builder->{$PROP_UNWRAP_VIEWS} = 1;
	my $object_name = $builder->build_dmanip_src_schema_object_name( $view_node );
	my @set_expr_nodes = 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'SET' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	my @set_clause_list = ();
	foreach my $expr_node (@set_expr_nodes) {
		my $set_target = $builder->build_identifier_element( 
			$expr_node->get_node_ref_attribute( 'set_view_col' ) );
		my $update_source = $builder->build_expr( $expr_node );
		push( @set_clause_list, $set_target.' = '.$update_source );
	}
	my $set_clause = 'SET '.join( ', ', @set_clause_list );
	my $where_clause = $builder->build_query_where_clause( $view_node );
	$builder->{$PROP_UNWRAP_VIEWS} = 0;
	return( 'UPDATE '.$object_name.' '.$set_clause.' '.$where_clause.';' );
}

######################################################################

sub build_dmanip_delete_stmt { # SQL-2003, 14.7 "<delete statement: searched>" (p831)
	my ($builder, $view_node) = @_;
	$builder->{$PROP_UNWRAP_VIEWS} = 1;
	my $object_name = $builder->build_dmanip_src_schema_object_name( $view_node );
	my $where_clause = $builder->build_query_where_clause( $view_node );
	$builder->{$PROP_UNWRAP_VIEWS} = 0;
	return( 'DELETE FROM '.$object_name.' '.$where_clause.';' );
}

######################################################################

sub build_dmanip_call_uproc { 
	# SQL-2003 ... <routine invocation> ...
	# SQL-2003, 15.1 "<call statement>" (p885)
	my ($builder, $rtn_stmt_node) = @_;
	my $uproc = $rtn_stmt_node->get_enumerated_attribute( 'call_uproc' );
	my $uproc_name = $builder->build_identifier_schema_obj( $uproc );
	my %uproc_arg_exprs = 
		map { ($_->get_node_ref_attribute( 'call_ufunc_arg' ) => $_) } # is called 'ufunc'
		@{$rtn_stmt_node->get_child_nodes()}; # gets child 'routine_expr' Nodes
	# Note: The build_expr() calls are done below to ensure the arg values are 
	# defined in the same order they are output; this lets optional insertion 
	# of positionally determined bind vars (and their mapping) to work right.
	my $arg_val_list = join( ', ', 
		map { $uproc_arg_exprs{$_} ? $builder->build_expr( $uproc_arg_exprs{$_} ) : 'NULL' } 
		@{$uproc->get_child_nodes( 'routine_arg' )} );
	# <call statement> ::= CALL <routine invocation>
	return( 'CALL '.$uproc_name.($arg_val_list ? '('.$arg_val_list.')' : '').';' );
}

######################################################################

sub substitute_macros {
	my ($builder, $str, $subs) = @_;
	while( my ($key,$value) = each %{$subs} ) {
		$str =~ s|\{$key\}|$value|;
	}
	return( $str );
}

######################################################################
######################################################################

1;
__END__

=head1 SYNOPSIS

	use Rosetta::Utility::SQLBuilder; # also loads SQL::SyntaxModel
	use DBI;

	my $model = SQL::SyntaxModel->new_container();

	# ... Probably around this time you would stuff $model full of nodes that 
	# describe the schema or action concepts you want to derive SQL from.
	# In this case, define a table, and a command to create it, and a routine 
	# to select from it; the command and routine nodes each have an id of 1.

	my $builder = Rosetta::Utility::SQLBuilder->new();

	my $dbh = DBI->connect( 'driver:db', 'user', 'pass' );

	my $cr_tbl_cmd_node = $model->get_node( 'command', 1 ); # TABLE_CREATE cmd def earlier
	my $create_sql = $builder->build_sql_routine( $cr_tbl_cmd_node ); # OUT OF DATE ?

	my $sth1 = $dbh->prepare( $create_sql );
	my $rv1 = $sth1->execute(); # creates a table in the database

	my %named_arg_values = ( 'foo' => 'abc', 'bar' => 7 ); # to use in select where clause

	my $select_from_tbl_rtn_node = $model->get_node( 'routine', 1 );
	my ($select_sql, $arg_map) = $builder->build_sql_routine( $select_from_tbl_rtn_node );
	my @ordered_arg_values = map { $named_arg_values{$_} } @{$arg_map};

	my $sth2 = $dbh->prepare( $select_sql );
	my $rv2 = $sth2->execute( @ordered_arg_values ); # opens a select cursor/query
	my $rowset = $sth2->fetchall_arrayref({});  # get array of hashes

	$dbh->close();

=head1 DESCRIPTION

This module is a reference implementation of fundamental Rosetta features.

The Rosetta::Utility::SQLBuilder Perl 5 module is a functional but quickly
built Rosetta utility class that converts a set of related SQL::SyntaxModel
Nodes into one or more SQL strings that are ready to give as input to a
particular SQL relational database management system.  This class will by
default produce SQL that is compliant with the ANSI/ISO SQL-2003 (or 1999 or
1992) standard, which should be useable as-is with most database products.  In
addition, this class takes arguments that let you vary the SQL output to an
alternate SQL dialect that particular database products either require or
prefer for use.  

Rosetta::Utility::SQLBuilder is designed to implement common functionality for
multiple Rosetta Engine classes (such as Rosetta::Engine::Generic) allowing
them to focus more on the non-SQL specific aspects of their work.  A Rosetta
Engine would typically invoke this class within its prepare() method.  This
class can also be used by code on the application-side of a Rosetta::Interface
tree; for example, a module that emulates an older database interface which
wants to return schema dumps as SQL strings ('create' statements usually) can
use this module to generate those.  (For your reference, see also the
Rosetta::Utility::SQLParser module, which implements the inverse functionality
to SQLBuilder, and is used in both of the same places.)

Rosetta::Utility::SQLBuilder has no dependence on any database link products or
libraries.  You would, for example, use it in exactly the same way (probably)
when generating SQL for an Oracle database regardless of whether the Engine is
employing ODBC or SQL*Net as the pipe over which the SQL is sent.  That said,
it does have specific support for the DBI module's standard way of indicating
run-time SQL bind variables (using a '?' for each instance); since DBI's
arguments are positional and SQL::SyntaxModel's are named, this class will also
return a map for the SQL that says what order to give the named values to DBI.

I<CAVEAT: SIGNIFICANT PORTIONS OF THIS MODULE ARE NOT WRITTEN YET.>

=head1 CONSTRUCTOR FUNCTIONS AND METHODS

This function/method is stateless and can be invoked off of either this
module's name or an existing module object, with the same result.

=head2 new()

	my $builder = Rosetta::Utility::SQLBuilder->new();
	my $builder2 = $builder->new();

This "getter" function/method will create and return a single
Rosetta::Utility::SQLBuilder (or subclass) object.  All of this object's
properties are set to default values that should cause the object to generate
SQL in a SQL-2003 standard conforming manner.

=head1 STATIC CONFIGURATION PROPERTY ACCESSOR METHODS

These methods are stateful and can only be invoked from this module's objects. 
This set of properties are generally set once at the start of a SQLBuilder
object's life and aren't changed later, since they are generally static
configuration data.

=head2 positional_bind_vars([ NEW_VALUE ])

	my $old_val = $builder->positional_bind_vars();
	$builder->positional_bind_vars( 1 );

This getter/setter method returns this object's "positional bind vars" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is false (the default), then any SQL this
object makes will include bind variable declarations in named format; eg:
":FOO" and ":BAR".  If this property is true, then bind variables are declared
in positional format; they will all be "?" (as the DBI module specifies), and
the SQL-making method will also return an array ref with maps bind variable
names to the positional "?" in the new SQL.  This property simply indicates the
database engine's capability; it does not say "act now".  The "positional bind
var map array" property can be set regardless of the engine's capability, but
SQLBuilder code will only do something with it if "positional bind vars" is true.

=head2 delimited_identifiers([ NEW_VALUE ])

	my $old_val = $builder->identifier_delimiting_char();
	$builder->identifier_delimiting_char( 1 );

This getter/setter method returns this object's "delimited identifiers" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is false (the default), then this object
will generate SQL identifiers (such as table or column or schema names) that
are non-delimited, case-insensitive (and uppercase), and contain only a limited
range of characters such as: letters, underscore, numbers (non-leading).  If
this property is true, then generated SQL identifiers will be delimited,
case-sensitive, and able to contain any characters (including whitespace). 
Note that both formats are supported by the SQL standard, and many database
products, though the non-delimited version is considered more "normal".

=head2 identifier_delimiting_char([ NEW_VALUE ])

	my $old_val = $builder->identifier_delimiting_char();
	$builder->identifier_delimiting_char( '`' );

This getter/setter method returns this object's "identifier delimiting char"
scalar property; if the optional NEW_VALUE argument is defined, this property
is first set to that value.  When the "delimited identifiers" property is true,
then "identifier delimiting char" defines what character to delimit identifiers
with.  The double-quote (") is used by default, as it is given by example in
the SQL standard and many databases such as Oracle support it; however, a
commonly used alternative is the back-tick (`), such as MySQL supports.  You
may use any delimiter you want by setting this property to it.  Note that any
occurance of your chosen delimiter in the actual identifier name will be
escaped in generated SQL by way of a double occurance (eg: '"' becomes '""').

=head2 get_data_type_customizations()

	my $rh_old_values = $builder->get_data_type_customizations();

This "getter" method returns this object's "data type customizations" family of
properties in a new hash ref.  The family has 46 members with more likely to be
added later; see the source code for a list.  Most of the members are used to
map SQL::SyntaxModel qualified data types or domains to RDBMS native data
types.  As data types is one of the places that RDBMS products are more likely
to differ from each other, the customization related to them is fine grained in
SQLBuilder.  The current values either match the 2003 SQL standard or are as
close to it as possible; often, many members can be kept the same for use with
particular database products, but often many members will also have to be
changed for each product.  The next 2 methods are for changing these members.

=head2 set_data_type_customizations( NEW_VALUES )

	$builder->set_data_type_customizations( { 'NUM_INT_8' => 'TINYINT' } );

This "setter" method lets you change one or more member of this object's "data
type customizations" family of properties; you provide replacements in the
NEW_VALUES hash ref argument, where the keys match the member name and the
values are the new values.  Invalid keys will also be added to the member list, 
but the SQL generating code will ignore them.

=head2 reset_default_data_type_customizations()

	$builder->reset_default_data_type_customizations();

This "setter" method lets you reset all of this object's "data
type customizations" family of properties to their default values, such as they 
were when the SQLBuilder object was first created.

=head2 ora_style_seq_usage([ NEW_VALUE ])

	my $old_val = $builder->ora_style_seq_usage();
	$builder->ora_style_seq_usage( 1 );

This getter/setter method returns this object's "ora style seq usage" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is false (the default), then sequence
next-value expressions will have the format 'NEXT VALUE FOR seq-name'; if this
property is true, they will be 'seq-name.NEXTVAL' instead, as Oracle likes.

=head2 ora_style_routines([ NEW_VALUE ])

	my $old_val = $builder->ora_style_routines();
	$builder->ora_style_routines( 1 );

This getter/setter method returns this object's "ora style routines" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is false (the default), then generated
routine layouts will follow the SQL-2003 standard, meaning that local variables
are declared inside BEGIN/END blocks using 'DECLARE var-name ...', and value
assignments take the form 'SET dest-var = src-expr'.  If this property is true,
then routine layouts will follow the Oracle 8 style where variable declarations
are above BEGIN/END blocks and assignments are of the form 'dest-var :=
src-expr'.

=head2 inlined_subqueries([ NEW_VALUE ])

	my $old_val = $builder->inlined_subqueries();
	$builder->inlined_subqueries( 1 );

This getter/setter method returns this object's "inlined subqueries" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is false (the default), then query
expressions will be generated having a "with" clause when any sub-queries have
names; if this property is true then all sub-queries will be in-lined whether
they have names or not (since the database engine doesn't support "with").

=head2 inlined_domains([ NEW_VALUE ])

	my $old_val = $builder->inlined_domains();
	$builder->inlined_domains( 1 );

This getter/setter method returns this object's "inlined domains" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is false (the default), then any data
container declarations like table columns will refer to named domain schema
objects as their data type; if this property is true then data type definitions
will always be inlined such as for table column declarations (since the
database engine doesn't support named domains).

=head2 flatten_to_single_schema([ NEW_VALUE ])

	my $old_val = $builder->flatten_to_single_schema();
	$builder->flatten_to_single_schema( 1 );

This getter/setter method returns this object's "flatten to single schema"
boolean property; if the optional NEW_VALUE argument is defined, this property
is first set to that value.  If this property is false (the default), then we
assume that the database engine in use supports multiple named schemas in a
single catalog, and we are using them; if this property is true then we assume
the database only supports a single schema per catalog, or the current user is
only allowed to use a single schema (effectively the same problem), so we will
flatten all of our schema objects into a single namespace where each object
name includes the schema name prefixed to it, then emulating multiple schemas.

=head2 single_schema_join_chars([ NEW_VALUE ])

	my $old_val = $builder->single_schema_join_chars();
	$builder->single_schema_join_chars( '___' );

This getter/setter method returns this object's "single schema join chars"
scalar property; if the optional NEW_VALUE argument is defined, this property
is first set to that value.  When the "flatten to single schema" property is
true, then "single schema join chars" defines what short character string to
concatenate the SSM schema name and schema object name with when flattening
them from two levels to one.  The default value of this property is '__' (a
double underscore).  It should have a value that is guaranteed to never appear
in either the schema names or schema object names being joined, so that
reverse-engineering such a flattened schema into a SSM can be trivial.

=head2 emulate_subqueries([ NEW_VALUE ])

	my $old_val = $builder->emulate_subqueries();
	$builder->emulate_subqueries( 1 );

This getter/setter method returns this object's "emulate subqueries" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is false (the default), then we assume
that the database engine in use supports sub-queries of some kind, either named
or inlined, and we are using them; if this property is true then we assume the
database does not support subqueries at all, and so we will need to emulate
them using multiple simple or joining queries as well as temporary tables to
hold intermediate values.  I<Note that the emulator will only support static
sub-queries for the near future, meaning those that don't take any view_args,
and that are evaluated exactly once prior to the invoking query.  For that
matter, some database engines only have native support for static sub-queries
also, such as SQLite 2.8.13.>

=head2 emulate_compound_queries([ NEW_VALUE ])

	my $old_val = $builder->emulate_compound_queries();
	$builder->emulate_compound_queries( 1 );

This getter/setter method returns this object's "emulate compound queries"
boolean property; if the optional NEW_VALUE argument is defined, this property
is first set to that value.  If this property is false (the default), then we
assume that the database engine in use supports the various compound queries
(union, intersect, except), and we are using them; if this property is true
then we assume the database does not support compound queries at all, and so we
will need to emulate them using multiple simple or joining queries as well as
temporary tables to hold intermediate values.

=head2 emulated_query_temp_table_join_chars([ NEW_VALUE ])

	my $old_val = $builder->emulated_query_temp_table_join_chars();
	$builder->emulated_query_temp_table_join_chars( '___' );

This getter/setter method returns this object's "emulated query temp table join
chars" scalar property; if the optional NEW_VALUE argument is defined, this
property is first set to that value.  When either or both of the "emulate
subqueries" or "emulate compound queries" properties are true, then "emulated
query temp table join chars" defines what short character string to concatenate
the parts of the names of each temporary table used by the emulation to hold
intermediate values.  Given that "inner views" used in subquery or compound
query definitions are declared inside the main view and/or each other, each
temporary table name is the concatenation of the names of the inner view being
invoked and each of its parent views, parent-most on the left.  The default
value of this property is '__' (a double underscore).  So, for example, a
parent view named 'foo' having an inner view named 'bar' will produce a
temporary table named 'foo__bar' when an emulation happens.

=head1 DYNAMIC STATE MAINTENANCE PROPERTY ACCESSOR METHODS

These methods are stateful and can only be invoked from this module's objects.
Each of these contains either very short term configuration options (meant to
have the life of about one external build* method call) that are only set
externally as usual, or some may also be set or changed by SQLBuilder code, and
can be used effectively as extra output from the build* method; they maintain
state for a build* invocation.

=head2 make_bind_vars([ NEW_VALUE ])

	my $old_val = $builder->make_bind_vars();
	$builder->make_bind_vars( 1 );

This getter/setter method returns this object's "make bind vars" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  This property helps manage the fact that routine_arg SSM
Nodes can have dual purposes when being converted to SQL.  With an ordinary
stored routine, they turn into normal argument declarations and are used by SQL
routine code by name as usual.  With an ANONYMOUS routine, or BLOCKs inside
those, they instead represent application bind variables, which are formatted
differently when put in SQL.  This property stores the current state as to
whether any referenced routine_arg should be turned into bind vars or not. 
This property should be set false (the default) when we are in an ordinary
routine, and it should be set true when we are in an ANONYMOUS routine.

=head2 get_positional_bind_var_map_array()

This "getter" method returns a new array ref having a copy of this object's
"positional bind var map array" array property.  This property is explicitely
emptied by external code, by invoking the clear_positional_bind_var_map_array()
method, prior to that code requesting that we build SQL which contains
positional bind variables.  When we build said SQL, each time we are to insert
a bind variable, we simply put a "?" in the SQL, and we add to this array the
name of the routine_arg whose value is supposed to substitute at exec time. As
soon as said SQL is made and returned, external code reads this array's values
using the get_positional_bind_var_map_array() method.

=head2 clear_positional_bind_var_map_array()

This "setter" method empties this object's "positional bind var map array"
array property.  See the previous method's documentation for when to use it.

=head2 unwrap_views([ NEW_VALUE ])

	my $old_val = $builder->unwrap_views();
	$builder->unwrap_views( 1 );

This getter/setter method returns this object's "unwrap views" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  This property helps manage the fact that when we are making
INSERT or UPDATE or DELETE statements, these operate on a single table or named
view by its original (not correlation) name.  The outer view that stores the
details of the I|U|D is "unwrapped".  This property stores the current state as
to whether any view_src_col should be referenced by their original names or
not; false (the default) means to use the correlation name, and true means to
use the original.  External code which is working with I|U|D statements, or
several functions in this module, would set this true before calling this
module's generic SQL making functions, and then set it false just afterwards.  

=head1 SQL LEXICAL ELEMENT CONSTRUCTION METHODS

These "getter" methods each do trivial SQL construction; each one returns what
amounts to a single 'token', such as a formatted identifier name or a quoted
literal value.  Typically these are only called by other SQL making functions.
See the subsections of SQL-2003 Foundation section 5 "Lexical elements" (p131).

=head2 quote_literal( LITERAL, BASE_TYPE )

	my $quoted = $builder->quote_literal( "can't you come?", 'STR_CHAR' );
	# Function returns "'can''t you come?'".

This method takes a literal scalar value in the argument LITERAL and returns a
quoted and/or escaped version of it, according to the rules of the SSM simple
data type specified in BASE_TYPE.  This method is a wrapper for the other
quote_*_literal( LITERAL ) methods, with BASE_TYPE determining which to call.

=head2 quote_char_string_literal( LITERAL )

	my $quoted = $builder->quote_char_string_literal( "Perl" );
	# Function returns "'Perl'".

This method takes a literal scalar value in the argument LITERAL and returns a 
quoted and/or escaped version of it, as a character string.

=head2 quote_bin_string_literal( LITERAL )

	my $quoted = $builder->quote_char_string_literal( "Perl" );
	# Function returns "B'01010000011001010111001001101100'".

This method takes a literal scalar value in the argument LITERAL and returns a
quoted and/or escaped version of it, as a binary-digit string.  Note that
quote_literal() never calls this for binary literals, but rather 'hex'.

=head2 quote_hex_string_literal( LITERAL )

	my $quoted = $builder->quote_char_string_literal( "Perl" );
	# Function returns "X'5065726C'".

This method takes a literal scalar value in the argument LITERAL and returns a 
quoted and/or escaped version of it, as a hex-digit (or hexit) string.

=head2 quote_integer_literal( LITERAL )

	my $quoted = $builder->quote_integer_literal( 54 );

This method takes a literal scalar value in the argument LITERAL and returns a 
quoted and/or escaped version of it, as an integer.

=head2 quote_numeric_literal( LITERAL )

	my $quoted = $builder->quote_numeric_literal( 7.53 );

This method takes a literal scalar value in the argument LITERAL and returns a 
quoted and/or escaped version of it, as a numeric of arbitrary scale.

=head2 quote_boolean_literal( LITERAL )

	my $true = $builder->quote_boolean_literal( 1 );
	my $false = $builder->quote_boolean_literal( 0 );
	my $unknown = $builder->quote_boolean_literal( undef );

This method takes a literal scalar value in the argument LITERAL and returns a
quoted and/or escaped version of it, as a boolean value.  By default the
returned values are bare-words of either [TRUE, FALSE, UNKNOWN] in accordance
with the SQL-2003 standard; however, if the "data type customizations" element
called 'BOOL_USE_NUMS' is set to true, then [1, 0, NULL] are returned instead.

=head2 quote_identifier( NAME )

	my $quoted = $builder->quote_identifier( 'my_data' );
	my $quoted2 = $builder->quote_identifier( 'My Data' );

This method takes a raw SQL identifier (such as a table or column name) in NAME
and returns an appropriately formatted version, taking into account the current
object's "delimited identifiers" and "identifier delimiting char" properties.  
This function only works on un-qualified identifiers; to quote a qualified 
identifier, pass each piece here separately, and join with "." afterwards.

=head2 build_identifier_element( OBJECT_NODE )

	my $sql = $builder->build_identifier_element( $object_node );

This method takes a SQL::SyntaxModel::Node object in OBJECT_NODE, extracts its
'name' attribute, and returns that after passing it to quote_identifier().  The
result is an "unqualified identifier".  This is the most widely used, at least
internally, build_identifier_*() method; for example, it is used for table/view
column names in table/view definitions, and for all declaration or use of
routine variables, or for routine/view arguments.  Note that SQL::SyntaxModel
will throw an exception if the Node argument is of the wrong type.

=head2 build_identifier_schema_obj( OBJECT_NODE[, FOR_DEFN] )

	my $sql = $builder->build_identifier_schema_obj( $object_node );
	my $sql = $builder->build_identifier_schema_obj( $object_node, 1 );

This method is like build_identifier_element() except that it will generate a
"qualified identifier", by following parent Nodes of the given Node, and that
it is specifically for schema object (domain, sequence generator, table, view,
routine) names.  For example, passing a 'table' node will usually return
'<schema_name>.<table_name>', or '<schema_name>__<table_name>' if the "flatten
to single schema" property is true.  This method is used mostly when
referencing an existing schema object, such as in a query's FROM clause, or a
table's foreign key constraint definition, or a trigger's ON declaration. See
SQL-2003 6.6 "<identifier chain>".  If the optional boolean argument FOR_DEFN
is true, then it will create a (possibly identical) variant of the object name
that is to be used in statements that create or drop the same object; this is
in case there is a requirement for names to be unqualified on definition (and
you are logged in to the schema where they live).

=head2 build_identifier_view_src_col( VIEW_SRC_COL_NODE )

	my $sql = $builder->build_identifier_view_src_col( $view_src_col_node );

This method makes an identifier chain that is used within a query/view
expression to refer to a source table/view column that we are taking the value
of.  Assuming that all view sources have local correlation names, the
identifier chain will usually look like '<correlation name>.<original
table/view column name>'.  (As an exception, this method will output just the
unqualified column name when this object's "unwrap views" property is true, as
that format works best in WHERE/etc clauses of INSERT|UPDATE|DELETE.)

=head2 build_identifier_fake_view_src_col( VIEW_SRC_NODE, MATCH_COL_NODE )

	my $sql = $builder->build_identifier_fake_view_src_col( $view_src_node, $match_col_node );

This method is like build_identifier_view_src_col() except that it does not
require you to provide an actual "view_src_col" Node as input.  This method is
used to implement the "match all source columns" feature in "view" Nodes, a
short-hand that saves developers from having to specify all of a view's columns
individually.  You instead provide the "view_src" and "table_col"/"view_col"
Nodes that the "view_src_col" would have specified internally.

=head2 build_identifier_temp_table_for_emul( INNER_VIEW_NODE )

	my $sql = $builder->build_identifier_temp_table_for_emul( $inner_view_node );

This method makes an identifier for a temporary table that is intended to be
used by code that is emulating sub-queries and/or compound queries, such that
the table would hold intermediate values.  See the
emulated_query_temp_table_join_chars() method documentation for more details on
what the new identifier name looks like.

=head1 SCALAR EXPRESSION AND PREDICATE SQL CONSTRUCTION METHODS

These "getter" methods build SQL expressions and correspond to the subsections 
of SQL-2003 Foundation section 6 "Scalar expressions" (p161) and 
section 8 "Predicates" (p373).

=head2 build_expr( EXPR_NODE )

	my $sql = $builder->build_expr( $expr_node );

This method takes any kind of "expression" Node ("view_expr" or "routine_expr")
and builds the corresponding SQL fragment.  Sometimes this method is simply a
wrapper for other build_expr_*() methods, which are called for specific
'expr_type' values, but other times this method does the work by itself.

=head2 build_expr_predef_data_type( DOMAIN_NODE )

	my $sql = $builder->build_expr_predef_data_type( $domain_node );

This method takes a 'domain' SSM Node and builds a corresponding SQL fragment
such as would be used in the "data type" reference of a table column
definition.  Example return values are "VARCHAR(40)", "DECIMAL(7,2)", "BOOLEAN"
"INTEGER UNSIGNED".  Most of the "data type customizations" property elements
are used to customize this method.  See SQL-2003 6.1 "<data type>" (p161).

=head2 build_expr_data_type_or_domain_name( DOMAIN_NODE )

	my $sql = $builder->build_expr_data_type_or_domain_name( $domain_node );

This method takes a 'domain' SSM Node and returns one of two kinds of SQL
fragments, depending on whether or not the current database engine supports
"domain" schema objects (and we are using them).  If it does then this method
returns the identifier of the domain schema object to refer to; if it does
not, then this method instead returns the data type definition to use.  See 
SQL-2003, 11.4 "<column definition>" (p536).

=head2 build_expr_cast_spec( EXPR_NODE )

	my $sql = $builder->build_expr_cast_spec( $expr_node );

This method takes an "*_expr" Node whose 'expr_type' is 'CAST' and generates
the corresponding "CAST( <operand> AS <target> )" SQL fragment, if the database
engine supports the syntax; it generates alternative casting expressions
otherwise, such as Oracle 8's "TO_*(...)" functions.  See SQL-2003, 6.12 "<cast
specification>" (p201).

=head2 build_expr_seq_next( SEQUENCE_NODE )

	my $sql = $builder->build_expr_seq_next( $sequence_node );

This method takes a "sequence" Node and generates the appropriate SQL
expression for reading the next value from the corresponding schema object.
See SQL-2003, 6.13 "<next value expression>" (p217).

=head2 build_expr_call_sfunc( EXPR_NODE )

	my $sql = $builder->build_expr_call_sfunc( $expr_node );

This method takes an "*_expr" Node whose 'expr_type' is 'SFUNC' and generates
the corresponding "built-in function" call, which includes basic predicates
(comparison or assertion), math operations, string operations, logic gates and
switches, aggregate functions, and olap functions.  Child "*_expr" Nodes
provide the argument values to give said "built-in function), if there are any.

=head2 build_expr_call_ufunc( EXPR_NODE )

	my $sql = $builder->build_expr_call_ufunc( $expr_node );

This method takes an "*_expr" Node whose 'expr_type' is 'UFUNC' and generates a
call to a named FUNCTION routine schema object.  Child "*_expr" Nodes provide
the argument values to give said FUNCTION, if there are any.

=head1 QUERY EXPRESSION SQL CONSTRUCTION METHODS

These "getter" methods build SQL query expressions and correspond to the
subsections of SQL-2003 Foundation section 7 "Query expressions" (p293).

=head2 build_query_table_expr( VIEW_NODE )

	my $sql = $builder->build_query_table_expr( $view_node );

This method takes a "view" Node and generates the main body of a query, namely
the concatenation of these 5 clauses in order: FROM, WHERE, GROUP BY, HAVING,
and the window clause.  All of these clauses are optional in a query (except
FROM in most cases); whether or not they are generated is determined by whether
the given VIEW_NODE comes with definitions for them.  This method is a shim
that calls 5 separate build_query_*_clause() methods which do the actual work,
and concatenates the results.  See SQL-2003, 7.4 "<table expression>" (p300).

=head2 build_query_from_clause( VIEW_NODE )

	my $sql = $builder->build_query_from_clause( $view_node );

This method takes a "view" Node and generates the FROM clause in the
corresponding query, if the query has a FROM clause (most do); it returns the
empty string otherwise.  The query has a FROM clause if it has at least one
child "view_src" Node; if it has multiple "view_src", then they are all joined
together based on this view's child "view_join" Nodes, to form a single "joined
table".  Each "view_src" can be either a table or view schema object, or a call
to a named subquery (with optional arguments), or an embedded anonymous
subquery; it is rendered into SQL, by the build_query_table_factor() method,
with a unique "correlation name" (expression AS name) that every other part of
this view's query references it with.  Note that this method should never be
invoked on "COMPOUND" views.  See SQL-2003, 7.5 "<from clause>" (p301) and
SQL-2003, 7.6 "<table reference>" (p303) and SQL-2003, 7.7 "<joined table>"
(p312).

=head2 build_query_table_factor( VIEW_SRC_NODE )

	my $sql = $builder->build_query_table_factor( $view_src_node );

This method is invoked by build_query_from_clause() for each "table factor" 
that needs to be generated in a FROM clause; see that method for context 
information.  See SQL-2003, 7.6 "<table reference>" (p303).

=head2 build_query_where_clause( VIEW_NODE )

	my $sql = $builder->build_query_where_clause( $view_node );

This method takes a "view" Node and generates the WHERE clause in the
corresponding query, if the query has a WHERE clause; it returns the
empty string otherwise.  See SQL-2003, 7.8 "<where clause>" (p319).

=head2 build_query_group_clause( VIEW_NODE )

	my $sql = $builder->build_query_group_clause( $view_node );

This method takes a "view" Node and generates the GROUP BY clause in the
corresponding query, if the query has a GROUP BY clause; it returns the
empty string otherwise.  See SQL-2003, 7.9 "<group by clause>" (p320).

=head2 build_query_having_clause( VIEW_NODE )

	my $sql = $builder->build_query_having_clause( $view_node );

This method takes a "view" Node and generates the HAVING clause in the
corresponding query, if the query has a HAVING clause; it returns the
empty string otherwise.  See SQL-2003, 7.10 "<having clause>" (p329).

=head2 build_query_window_clause( VIEW_NODE )

	my $sql = $builder->build_query_window_clause( $view_node );

This method takes a "view" Node and generates the window clause in the
corresponding query, if the query has a window clause; it returns the empty
string otherwise.  The window clause includes things like "ORDER BY", "LIMIT",
"OFFSET".  See SQL-2003, 7.11 "<window clause>" (p331).

=head2 build_query_query_spec( VIEW_NODE[, MAKE_INTO_CLAUSE] )

	my $sql = $builder->build_query_query_spec( $view_node );
	my $sql2 = $builder->build_query_query_spec( $view_node, 1 );

This method takes a "view" Node and generates the main body of a query plus a
SELECT list.  The output looks like "SELECT <set quantifier> <select list>
<into clause> <table expression>", where "<set quantifier>" is [DISTINCT|ALL],
and the other three parts are built respectively by: build_query_select_list(),
build_query_into_clause(), build_query_table_expr().  This method should not be
called for a COMPOUND view.  The <into clause> is only made if the boolean
argument MAKE_INTO_CLAUSE is true; that is only intended to happen for root
SELECT statements inside routines.  See SQL-2003, 7.12 "<query specification>"
(p341) and SQL-2003, 14.5 "<select statement: single row>" (p824).

=head2 build_query_select_list( VIEW_NODE )

	my $sql = $builder->build_query_select_list( $view_node );

This method takes a "view" Node and generates the "select list" portion of the
corresponding query, which defines the output columns of the query.  This
method returns a comma-delimited list expression where each list item is a
"<derived column> ::= <value expression> AS <column name>".  See SQL-2003, 7.12
"<query specification>" (p341).

=head2 build_query_into_clause( VIEW_NODE )

	my $sql = $builder->build_query_into_clause( $view_node );

This method takes a "view" Node and generates the INTO clause in a
corresponding "SELECT ... INTO ..." or "FETCH ... INTO ..." statement.  See
SQL-2003, 14.3 "<fetch statement>" (p817) and SQL-2003, 14.5 "<select
statement: single row>" (p824).

=head2 build_query_query_expr( VIEW_NODE )

	my $sql = $builder->build_query_query_expr( $view_node );

This method takes a "view" Node and outputs the definitions of the named
subqueries for the current query, if the database engine supports named
subqueries (and we are using them), plus the definition of the main query
itself, which may be a compound query.  The output may look like "WITH ...
SELECT ...".  See SQL-2003, 7.13 "<query expression>" (p351).

=head2 build_query_query_expr_body( VIEW_NODE )

	my $sql = $builder->build_query_query_expr_body( $view_node );

This method takes a "view" Node and produces the SQL for a compound query where
this view's child views are the queries being compounded together, if the
current view is a COMPOUND view.  If the current view is not a COMPOUND view,
then this method is simply a shim for build_query_query_spec().  See SQL-2003,
7.13 "<query expression>" (p351).

=head2 build_query_subquery( EXPR_NODE )

	my $sql = $builder->build_query_subquery( $expr_node );

This method takes a "view_expr" Node whose 'expr_type' is 'CVIEW' and generates
either a call to a named subquery, or inlines the definition of an anonymous
subquery, depending on what the database engine supports (and we are using).
View schema or table objects are not invoked directly here, but can be
indirectly via a subquery.  Child "*_expr" Nodes provide the argument values to
give said subquery, if it takes arguments.  See SQL-2003, 7.15 "<subquery>" (p370).

=head1 SCHEMA DEFINITION SQL CONSTRUCTION METHODS

These "getter" methods build SQL strings or fragments thereof that are used
mainly when declaring or defining (or removing) database schema constructs.
They correspond to the subsections of SQL-2003 Foundation section 11 "Schema
definition and manipulation" (p519).

=head2 build_schema_schema_create( SCHEMA_NODE )

	my $sql = $builder->build_schema_schema_create( $schema_node );

This method takes a 'schema' SSM Node and builds a corresponding "CREATE
SCHEMA" DDL SQL statement, which it returns.  See SQL-2003, 11.1 "<schema
definition>" (p519).

=head2 build_schema_schema_delete( SCHEMA_NODE )

	my $sql = $builder->build_schema_schema_delete( $schema_node );

This method takes a 'schema' SSM Node and builds a corresponding "DROP SCHEMA"
DDL SQL statement, which it returns.  See SQL-2003, 11.2 "<drop schema
statement>" (p522).

=head2 build_schema_domain_create( DOMAIN_NODE )

	my $sql = $builder->build_schema_domain_create( $domain_node );

This method takes a 'domain' SSM Node and builds a corresponding "CREATE
DOMAIN" DDL SQL statement, which it returns.  See SQL-2003, 11.24 "<domain
definition>" (p603).

=head2 build_schema_domain_delete( DOMAIN_NODE )

	my $sql = $builder->build_schema_domain_delete( $domain_node );

This method takes a 'domain' SSM Node and builds a corresponding "DROP DOMAIN"
DDL SQL statement, which it returns.  See SQL-2003, 11.30 "<drop domain
statement>" (p610).

=head2 build_schema_sequence_create( SEQUENCE_NODE )

	my $sql = $builder->build_schema_sequence_create( $sequence_node );

This method takes a 'sequence' SSM Node and builds a corresponding "CREATE
SEQUENCE" DDL SQL statement, which it returns.  See SQL-2003, 11.62 "<sequence
generator definition>" (p726).

=head2 build_schema_sequence_delete( SEQUENCE_NODE )

	my $sql = $builder->build_schema_sequence_delete( $sequence_node );

This method takes a 'sequence' SSM Node and builds a corresponding "DROP
SEQUENCE" DDL SQL statement, which it returns.  See SQL-2003, 11.64 "<drop
sequence generator statement>" (p729).

=head2 build_schema_table_create( TABLE_NODE )

	my $sql = $builder->build_schema_table_create( $table_node );

This method takes a 'table' SSM Node and builds a corresponding "CREATE TABLE"
DDL SQL statement, which it returns.  See SQL-2003, 6.2 "<field definition>"
(p173) and SQL-2003, 11.3 "<table definition>" (p525) and SQL-2003, 11.4
"<column definition>" (p536) and SQL-2003, 11.5 "<default clause>" (p541) and
SQL-2003, 11.6 "<table constraint definition>" (p545) and SQL-2003, 11.7
"<unique constraint definition>" (p547) and SQL-2003, 11.8 "<referential
constraint definition>" (p549) and SQL-2003, 11.9 "<check constraint
definition>" (p569).

=head2 build_schema_table_delete( TABLE_NODE )

	my $sql = $builder->build_schema_table_delete( $table_node );

This method takes a 'table' SSM Node and builds a corresponding "DROP TABLE"
DDL SQL statement, which it returns.  See SQL-2003, 11.21 "<drop table
statement>" (p587).

=head2 build_schema_view_create( VIEW_NODE )

	my $sql = $builder->build_schema_view_create( $view_node );

This method takes a 'view' SSM Node and builds a corresponding "CREATE VIEW"
DDL SQL statement, which it returns.  See SQL-2003, 11.22 "<view definition>"
(p590).

=head2 build_schema_view_delete( VIEW_NODE )

	my $sql = $builder->build_schema_view_delete( $view_node );

This method takes a 'view' SSM Node and builds a corresponding "DROP VIEW" DDL
SQL statement, which it returns.  See SQL-2003, 11.23 "<drop view statement>"
(p600).

=head2 build_schema_routine_create( ROUTINE_NODE )

	my $sql = $builder->build_schema_routine_create( $routine_node );

This method takes a 'routine' SSM Node and builds a corresponding "CREATE
ROUTINE/PROCEDURE/FUNCTION" DDL SQL statement, which it returns.  See SQL-2003,
11.39 "<trigger definition>" (p629) and SQL-2003, 11.50 "<SQL-invoked routine>"
(p675).

=head2 build_schema_routine_delete( ROUTINE_NODE )

	my $sql = $builder->build_schema_routine_delete( $routine_node );

This method takes a 'routine' SSM Node and builds a corresponding "DROP
ROUTINE/PROCEDURE/FUNCTION" DDL SQL statement, which it returns.  See SQL-2003,
11.40 "<drop trigger statement>" (p633) and SQL-2003, 11.52 "<drop routine
statement>" (p703).

=head1 ACCESS CONTROL SQL CONSTRUCTION METHODS

These "getter" methods build SQL statements that are used mainly when declaring
users or roles and their permissions.  They correspond to the subsections of
SQL-2003 Foundation section 12 "Access control" (p731).  Note that
SQL::SyntaxModel assigns privileges to roles, and roles to users; privileges
are not assigned to users directly.

=head2 build_access_role_create( ROLE_NODE )

	my $sql = $builder->build_access_role_create( $role_node );

This method takes a 'role' SSM Node and builds a corresponding "CREATE ROLE"
SQL statement, which it returns.  See SQL-2003, 12.4 "<role definition>"
(p743).

=head2 build_access_role_delete( ROLE_NODE )

	my $sql = $builder->build_access_role_delete( $role_node );

This method takes a 'role' SSM Node and builds a corresponding "DROP ROLE" SQL
statement, which it returns.  See SQL-2003, 12.6 "<drop role statement>"
(p746).

=head2 build_access_grant( GRANTEE_NODE )

	my $sql = $builder->build_access_grant( $grantee_node );

This method takes a "grantee" ("role" or "user") Node and builds a list of
"GRANT ... TO ..." SQL statements, which it returns as a string.  If the
grantee is a 'role', then grant statements for all of the privileges assigned
to that role are created.  If the grantee is a 'user', then grant statements
for all the roles assigned to that user are created.  This method returns an
empty string if the grantee has no privileges or roles.  See SQL-2003, 12.1
"<grant statement>" (p731) and SQL-2003, 12.2 "<grant privilege statement>"
(p736) and SQL-2003, 12.3 "<privileges>" (p739) and SQL-2003, 12.5 "<grant role
statement>" (p744).

=head2 build_access_revoke( GRANTEE_NODE )

	my $sql = $builder->build_access_revoke( $grantee_node );

This method takes a "grantee" ("role" or "user") Node and builds a list of
"REVOKE ... FROM ..." SQL statements, which it returns as a string.  If the
grantee is a 'role', then revoke statements for all of the privileges assigned
to that role are created.  If the grantee is a 'user', then revoke statements
for all the roles assigned to that user are created.  This method returns an
empty string if the grantee has no privileges or roles.  See SQL-2003, 12.7
"<revoke statement>" (p747) and SQL-2003, 12.3 "<privileges>" (p739).

=head1 DATA MANIPULATION SQL CONSTRUCTION METHODS

These "getter" methods build SQL statements that are used mainly with cursors
or routine statements, or when manipulating data, such as insert/update/delete
commands. They correspond to the subsections of SQL-2003 Foundation sections:
14 "Data manipulation" (p809), 15 "Control statements", plus part of
"SQL-client modules" (p765).

=head2 build_dmanip_routine_body( ROUTINE_NODE[, IS_ATOMIC] )

	my $sql = $builder->build_dmanip_routine_body( $routine_node, 1 );

This method takes a 'routine' SSM Node and constructs the main body SQL of that
routine, which is the BEGIN...END compound statement, all the contained routine
statements, and the variable declarations; these are all returned as a string.
This method does not construct a method name or argument list.  It is suitable
for both named/stored routines and anonymous/application routines.  If the
optional boolean argument IS_ATOMIC is true, then "BEGIN ATOMIC" is generated
instead of "BEGIN"; it is used for trigger bodies.  See SQL-2003, 14.1
"<declare cursor>" (p809) plus other relevant sections of SQL-2003.

=head2 build_dmanip_routine_stmt( STMT_NODE )

	my $sql = $builder->build_dmanip_routine_stmt( $stmt_node );

This method takes a "routine_stmt" Node and builds the corresponding SQL
statement.  Sometimes this method is simply a wrapper for other
build_dmanip_*() methods, which are called for specific 'stmt_type' values, but
other times this method does the work by itself.  See SQL-2003, 13.5 "<SQL
procedure statement>" (p790) and SQL-2003, 15.2 "<return statement>" (p886).

=head2 build_dmanip_call_sproc( STMT_NODE )

	my $sql = $builder->build_dmanip_call_sproc( $stmt_node );

This method takes a "routine_stmt" Node whose 'stmt_type' is 'SPROC' and
generates the corresponding "built-in procedure" call, which includes creation
and use of cursors, selects, inserts, updates, deletes, commit, rollback, etc. 
Child "*_expr" Nodes provide the argument values to give said "built-in
function), if there are any.  See SQL-2003, 14.2 "<open statement>" (p815) and
SQL-2003, 14.3 "<fetch statement>" (p817) and SQL-2003, 14.4 "<close
statement>" (p822).

=head2 build_dmanip_src_schema_object_name( VIEW_NODE )

	my $sql = $builder->build_dmanip_src_schema_object_name( $view_node );

This method takes a "view" Node and returns the schema-qualified name of its
single source, if it has a single source and that is a schema object (table or
a named view); it returns the undefined value otherwise.  This function is used
by the methods which generate INSERT|UPDATE|DELETE statements.

=head2 build_dmanip_insert_stmt( VIEW_NODE )

	my $sql = $builder->build_dmanip_insert_stmt( $view_node );

This method takes a "view" Node and returns the corresponding INSERT SQL
statement, assuming the view has details for one.  See SQL-2003, 7.3 "<table
value constructor>" (p298) and SQL-2003, 14.8 "<insert statement>" (p834).

=head2 build_dmanip_update_stmt( VIEW_NODE )

	my $sql = $builder->build_dmanip_update_stmt( $view_node );

This method takes a "view" Node and returns the corresponding UPDATE SQL
statement, assuming the view has details for one.  See SQL-2003, 14.11 "<update
statement: searched>" (p849) and SQL-2003, 14.12 "<set clause list>" (p853).

=head2 build_dmanip_delete_stmt( VIEW_NODE )

	my $sql = $builder->build_dmanip_delete_stmt( $view_node );

This method takes a "view" Node and returns the corresponding DELETE SQL
statement, assuming the view has details for one.  See SQL-2003, 14.7 "<delete
statement: searched>" (p831).

=head2 build_dmanip_call_uproc( STMT_NODE )

	my $sql = $builder->build_dmanip_call_uproc( $stmt_node );

This method takes a "routine_stmt" Node whose 'stmt_type' is 'UPROC' and
generates a call to a named PROCEDURE routine schema object.  Child "*_expr"
Nodes provide the argument values to give said PROCEDURE, if there are any. 
See SQL-2003, 15.1 "<call statement>" (p885).

=head1 UTILITY METHODS

=head2 substitute_macros( STR, SUBS )

	my $result = $builder->substitute_macros( 'NUMBER({p},{s})', { 'p' => 7, 's' => 2 } )

This method takes a string in STR which contains brace-delimited tokens and
returns a version of that string having the tokens replaced by corresponding
values provided in the hash ref SUBS.  This method is used mainly by
build_expr_predef_data_type() at the moment.

=head1 BUGS

This module is currently in pre-alpha development status, meaning that some
parts of it will be changed in the near future, perhaps in incompatible ways.

=head1 SEE ALSO

perl(1), Rosetta, SQL::SyntaxModel, Rosetta::Engine::Generic,
Rosetta::Utility::SQLParser.

=cut
