=head1 NAME

Rosetta::Utility::SQLBuilder - Generate ANSI/ISO SQL-2003 and other SQL variants

=cut

######################################################################

package Rosetta::Utility::SQLBuilder;
use 5.006;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = '0.02';

use Locale::KeyedText 0.03;
use SQL::SyntaxModel 0.17;

######################################################################

=head1 DEPENDENCIES

Perl Version: 5.006

Standard Modules: I<none>

Nonstandard Modules: 

	Locale::KeyedText 0.03 (for error messages)
	SQL::SyntaxModel 0.17

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
my $PROP_POSIT_BVARS = 'posit_bvars'; # boolean; true if bind vars are positional; false if named
my $PROP_DELIM_IDENT = 'delim_ident'; # boolean; true if identifiers are delimited, case-sensitive
my $PROP_IDENT_QUOTC = 'ident_quotc'; # character; character used to delimit identifiers with
my $PROP_DATA_TYPES  = 'data_types' ; # hash ref
my $PROP_ORA_SEQ_USAGE = 'ora_seq_usage'; # boolean; true if sequence usage in Oracle style
my $PROP_NAMED_SUBQ = 'named_subq'; # boolean; true if "with" supported; false to inline all subq

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
my $DT_HAS_ENUM_TYPE = 'has_eNUM_type'; # boolean; if true use ENUM, if false, use CHECK

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
	$builder->{$PROP_NAMED_SUBQ} = 1;
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

######################################################################

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

######################################################################

sub named_subqueries {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_NAMED_SUBQ} = $new_value;
	}
	return( $builder->{$PROP_NAMED_SUBQ} );
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

######################################################################

sub build_expr {
	my ($builder, $expr_node) = @_;
	my $expr_type = $expr_node->get_enumerated_attribute( 'expr_type' );
	if( $expr_type eq 'LIT' ) {
		# For now we treat all literals as character strings; in the future, 
		# make this smarter so that things like non-char data are quoted right.
		# This may require the expr Node 'domain' attribute to be set optionally with 'LIT' exprs.
		return( $builder->quote_literal( 
			$expr_node->get_literal_attribute( 'lit_val' ), 'STR_CHAR' ) );
	} elsif( $expr_type eq 'COL' ) {
		return( $builder->build_expr_identifier_chain( 
			$expr_node->get_node_ref_attribute( 'src_col' ) ) );
	} elsif( $expr_type eq 'MCOL' ) {
		return( $builder->build_expr_identifier_element( 
			$expr_node->get_node_ref_attribute( 'match_col' ) ) );
	} elsif( $expr_type eq 'VARG' ) {
		return( $builder->build_expr_identifier_element( 
			$expr_node->get_node_ref_attribute( 'view_arg' ) ) );
	} elsif( $expr_type eq 'ARG' ) {
		return( $builder->build_expr_identifier_element( 
			$expr_node->get_node_ref_attribute( 'routine_arg' ) ) );
	} elsif( $expr_type eq 'VAR' ) {
		return( $builder->build_expr_identifier_element( 
			$expr_node->get_node_ref_attribute( 'routine_var' ) ) );
	} elsif( $expr_type eq 'CAST' ) {
		return( $builder->build_expr_cast_spec( $expr_node ) );
	} elsif( $expr_type eq 'SEQN' ) {
		return( $builder->build_expr_seq_next( 
			$expr_node->get_node_ref_attribute( 'sequence' ) ) );
	} elsif( $expr_type eq 'CVIEW' ) {
		return( $builder->build_expr_call_cview( $expr_node ) );
	} elsif( $expr_type eq 'SFUNC' ) {
		return( $builder->build_expr_call_sfunc( $expr_node ) );
	} else { # $expr_type eq 'UFUNC'
		return( $builder->build_expr_call_ufunc( $expr_node ) );
	}
}

######################################################################

sub build_expr_data_type_spec { # function corres to SQL-2003, 6.1 <data type>
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
		my @quoted = map { $builder->quote_literal( $_, $base_type ) } @allowed_values;
		if( $type_conv->{$DT_HAS_ENUM_TYPE} ) {
			# ENUM type declaration replaces existing SQL type declaration.
			$sql = 'ENUM('.join( ', ', @quoted ).')'; # MySQL syntax
		} else {
			# Append CHECK CONSTRAINT to existing SQL type declaration.
			$sql .= ' CHECK VALUE IN ('.join( ', ', @quoted ).')'; # may be wrong syntax
		}
	}

	return( $sql );
}

######################################################################

sub build_expr_identifier_element {
	my ($builder, $object_node) = @_;
	return( $builder->quote_identifier( $object_node->get_literal_attribute( 'name' ) ) );
}

sub build_expr_identifier_chain { # function corres to SQL-2003, 6.6 <identifier chain>
	my ($builder, $object_node) = @_;
	my $node_type = $object_node->get_node_type();
	my $unqualified_name = $object_node->get_literal_attribute( 'name' );
	my $parent_name = undef;
	if( $object_node->valid_node_type_node_ref_attributes( $node_type, 'schema' ) ) {
		if( my $schema_node = $object_node->get_node_ref_attribute( 'schema' ) ) {
			$parent_name = $builder->build_expr_identifier_chain( $schema_node );
		} else {}
	} else {}
	return( ($parent_name ? $parent_name.'.' : '').
		$builder->quote_identifier( $unqualified_name ) );
}

######################################################################

sub build_expr_cast_spec { # function corres to SQL-2003, 6.12 <cast specification>
	my ($builder, $expr_node) = @_;
	# We are assuming that enumerated attribute 'expr_type' is 'CAST'.
	my $domain_node = $expr_node->get_node_ref_attribute( 'domain' );
	my $child_expr_nodes = $expr_node->get_child_nodes();
	my $cast_operand = $builder->build_expr( $child_expr_nodes->[0] );
	if( 0 ) {
		# Expand this later to support non-standard operators like TO_STR, TO_NUM, TO_DATE, etc.
	} else {
		my $cast_target = $builder->build_expr_data_type_spec( $domain_node );
		# Should expand to take either schema domain object names or data type expr.
		return( 'CAST ('.$cast_operand.' AS '.$cast_target.')' );
	}
}

######################################################################

sub build_expr_seq_next { # function corres to SQL-2003, 6.13 <next value expression>
	my ($builder, $sequence_node) = @_;
	my $sequence_name = $builder->build_expr_identifier_chain( $sequence_node );
	if( $builder->{$PROP_ORA_SEQ_USAGE} ) {
		return( $sequence_name.".NEXTVAL" );
	} else {
		return( 'NEXT VALUE FOR '.$sequence_name );
	}
}

######################################################################

sub build_expr_call_cview {
	my ($builder, $expr_node) = @_;
	my $view = $expr_node->get_enumerated_attribute( 'call_view' );
	my $child_exprs = $expr_node->get_child_nodes();

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
	} else {}
}

######################################################################

sub build_expr_call_ufunc {
	my ($builder, $expr_node) = @_;
	my $ufunc = $expr_node->get_enumerated_attribute( 'call_ufunc' );
	my $child_exprs = $expr_node->get_child_nodes();

}

######################################################################

sub build_query_row_value_constr { # SQL-2003, 7.1 "<row value constructor>" (p293)
	my ($builder, $expr_node) = @_;
	# Not implemented yet.
}

sub build_query_row_value_expr { # SQL-2003, 7.2 "<row value expression>" (p296)
	my ($builder, $expr_node) = @_;
	# Not implemented yet.
}

sub build_query_table_value_constr { # SQL-2003, 7.3 "<table value constructor>" (p298)
	my ($builder, $expr_node) = @_;
	# Not implemented yet.

	return( 'VALUES '.join( ', ', () ) );
}

sub build_query_table_expr { # SQL-2003, 7.4 "<table expression>" (p300)
	my ($builder, $view_node) = @_;
	return( $builder->build_query_from_clause( $view_node ).
		' '.$builder->build_query_where_clause( $view_node ).
		' '.$builder->build_query_group_clause( $view_node ).
		' '.$builder->build_query_having_clause( $view_node ).
		' '.$builder->build_query_window_clause( $view_node ) );
}

sub build_query_from_clause { # SQL-2003, 7.5 "<from clause>" (p301)
	# Also 
	my ($builder, $expr_node) = @_;


# FROM <table reference> [ { <comma> <table reference> }... ]
}

sub build_query_table_ref { # SQL-2003, 7.6 "<table reference>" (p303)
	my ($builder, $expr_node) = @_;

}

sub build_query_joined_table { # SQL-2003, 7.7 "<joined table>" (p312)
	my ($builder, $expr_node) = @_;

}

sub build_query_where_clause { # SQL-2003, 7.8 "<where clause>" (p319)
	# Function returns empty string if view has no where clause.
	my ($builder, $view_node) = @_;
	my @expr_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'WHERE' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	return( @expr_list ? 'WHERE '.$expr_list[0] : '' );
}

sub build_query_group_clause { # SQL-2003, 7.9 "<group by clause>" (p320)
	# Function returns empty string if view has no group by clause.
	my ($builder, $view_node) = @_;
	my $set_quantifier = ''; # what is this thing (which is optional)?
	my @expr_list = 
		map { $builder->build_query_grouping_element( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'GROUP' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	return( @expr_list ? 'GROUP BY '.$set_quantifier.' '.join( ', ', @expr_list ) : '' );
}

sub build_query_grouping_element { # SQL-2003, 7.9 "<group by clause>" (p320)
	my ($builder, $expr_node) = @_;
	return( $builder->build_expr( $expr_node ) ); # returns "<grouping column reference>"
	# TODO: Most group-by related features like ROLLUP, CUBE, GROUPING SETS, etc.
}

sub build_query_having_clause { # SQL-2003, 7.10 "<having clause>" (p329)
	# Function returns empty string if view has no having clause.
	my ($builder, $view_node) = @_;
	my @expr_list = 
		map { $builder->build_expr( $_ ) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'HAVING' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	return( @expr_list ? 'HAVING '.$expr_list[0] : '' );
}

sub build_query_window_clause { # SQL-2003, 7.11 "<window clause>" (p331)
	# Function returns empty string if view has no window clause.
	my ($builder, $view_node) = @_;
	# TODO: I need to first update SQL::SyntaxModel a bit re the various 
	# parts of a <window clause>, then fix here.  Meanwhile, I dump what I got.
	# Also see SQL-2003, 10.10 "<sort specification list>" for future reference.
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

sub build_query_query_spec { # SQL-2003, 7.12 "<query specification>" (p341)
	my ($builder, $view_node) = @_;
	my $set_quantifier = ''; # what is this thing (which is optional)?
	# We have two statements below instead of one because we want the result cols shown  
	# in order of the 'view_col' Nodes, not the order of the 'view_part' if different.
	my %select_list_exprs = 
		map { ($_->get_node_ref_attribute( 'view_col' ) => $builder->build_expr( $_ )) } 
		grep { $_->get_enumerated_attribute( 'view_part' ) eq 'RESULT' } 
		@{$view_node->get_child_nodes( 'view_expr' )};
	my $select_list = join( ', ',
		map { ($select_list_exprs{$_}||'NULL').' AS '.$builder->build_expr_identifier_element( $_ ) } 
		@{$view_node->get_child_nodes( 'view_col' )} );
		# Note that the "||'NULL'" thing deals with view_col that don't have any view_expr.
		# TODO: Note that the 'view_col' Nodes we actually need may be in a parent view 
		# of the current view; right now we only are looking in the current view.
	my $table_expression = $builder->build_query_table_expr( $view_node );
	return( 'SELECT '.$set_quantifier.' '.$select_list.' '.$table_expression );
}

sub build_query_query_expr { # SQL-2003, 7.13 "<query expression>" (p351)
	my ($builder, $view_node) = @_;
	my $view_type = $view_node->get_enumerated_attribute( 'view_type' );
	my $with_clause = '';
	if( $view_type eq 'SUBQUERY' and $builder->{$PROP_NAMED_SUBQ} ) {
		my @with_list = ();
		my $recursive = 0;
		foreach my $child_view_node (@{$view_node->get_child_nodes( 'view' )}) {
			unless( $child_view_node->get_enumerated_attribute( 'name' ) ) {
				next;
			}
			if( $child_view_node->get_enumerated_attribute( 'view_type' ) eq 'RECURSIVE' ) {
				$recursive = 1;
			}
			my $with_item = $builder->build_expr_identifier_element( $child_view_node );
			if( my @child_arg_nodes = @{$child_view_node->get_child_nodes( 'view_arg' )} ) {
				$with_item .= '('.join( ', ', 
					map { $builder->build_expr_identifier_element( $_ ) } 
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
}

sub build_query_query_expr_body { # SQL-2003, 7.13 "<query expression>" (p351)
	my ($builder, $view_node) = @_;
	my $view_type = $view_node->get_enumerated_attribute( 'view_type' );
	if( $view_type eq 'COMPOUND' ) {
		my $compound_op = $view_node->get_enumerated_attribute( 'c_merge_type' );

	}


	return(  );
}

sub build_query_search_or_cycle { # SQL-2003, 7.14 "<search or cycle clause>" (p365)
	my ($builder, $expr_node) = @_;
	# Not implemented yet.
}

sub build_query_subquery { # SQL-2003, 7.15 "<subquery>" (p370)
	my ($builder, $view_node) = @_;
	return( '('.$builder->build_query_query_expr( $view_node ).')' );
}

######################################################################

sub build_schema_schema_create { # SQL-2003, 11.1 "<schema definition>" (p519)
	my ($builder, $schema_node) = @_;
	my $schema_name = $builder->build_expr_identifier_chain( $schema_node );
	my $authorization = ''; # AUTHORIZATION <authorization identifier>
	# Some other features in 11.1, such as default character set.
	return( 'CREATE SCHEMA '.$schema_name.' '.$authorization.';' );
}

sub build_schema_schema_delete { # SQL-2003, 11.2 "<drop schema statement>" (p522)
	my ($builder, $schema_node) = @_;
	my $schema_name = $builder->build_expr_identifier_chain( $schema_node );
	return( 'DROP SCHEMA '.$schema_name.';' );
}

######################################################################

sub build_schema_domain_create { # SQL-2003, 11.24 "<domain definition>" (p603)
	my ($builder, $domain_node) = @_;
	# Not currently implemented, as domain nodes do not belong to schema 
	# Nodes in the SQL::SyntaxModel.  Definitions are currently embedded 
	# where data types are used, such as in table column specs.
}

sub build_schema_domain_delete { # SQL-2003, 11.30 "<drop domain statement>" (p610)
	my ($builder, $domain_node) = @_;
	# Not currently implemented, as domain nodes do not belong to schema 
	# Nodes in the SQL::SyntaxModel.  Definitions are currently embedded 
	# where data types are used, such as in table column specs.
}

######################################################################

sub build_schema_sequence_create { # SQL-2003, 11.62 "<sequence generator definition>" (p726)
	my ($builder, $sequence_node) = @_;
	# SQL-2003 allows multiple data types for this, but we stick to integers for now.
	my $sequence_name = $builder->build_expr_identifier_chain( $sequence_node );
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
	my $sequence_name = $builder->build_expr_identifier_chain( $sequence_node );
	return( 'DROP SEQUENCE '.$sequence_name.';' );
}

######################################################################

sub build_schema_table_create { 
	# SQL-2003, 11.3 "<table definition>" (p525)
	# SQL-2003, 11.4 "<column definition>" (p536)
	# SQL-2003, 11.5 "<default clause>" (p541)
	# SQL-2003, 11.6 "<table constraint definition>" (p545)
	# SQL-2003, 11.7 "<unique constraint definition>" (p547)
	# SQL-2003, 11.8 "<referential constraint definition>" (p549)
	# TODO: SQL-2003, 11.9 "<check constraint definition>" (p569)
	my ($builder, $table_node, $is_temp) = @_;
	my $table_name = $builder->build_expr_identifier_chain( $table_node );
	my @table_col_sql = ();
	my %domain_sql_cache = (); # used when making col defs
	my %col_name_cache = (); # used when making ind defs
	my %mandatory_col_cache = (); # used when making ind defs
	foreach my $table_col_node (@{$table_node->get_child_nodes( 'table_col' )}) {
		my $table_col_name = $builder->build_expr_identifier_element( $table_col_node );
		unless( exists( $col_name_cache{$table_col_node} ) ) {
			$col_name_cache{$table_col_node} = $table_col_name;
		}
		my $domain_node = $table_col_node->get_node_ref_attribute( 'domain' );
		my $domain_name = $domain_node->get_literal_attribute( 'name' );
		unless( exists( $domain_sql_cache{$domain_name} ) ) {
			$domain_sql_cache{$domain_name} = $builder->build_expr_data_type_spec( $domain_node );
		}
		my $domain_sql = $domain_sql_cache{$domain_name};
		# TODO: Allow use of named domain schema objects rather than inline definitions.
		my $mandatory = $table_col_node->get_literal_attribute( 'mandatory' );
		$mandatory and $mandatory_col_cache{$table_col_node} = 1;
		my $default_val = $table_col_node->get_literal_attribute( 'default_val' );
		my $auto_inc = $table_col_node->get_literal_attribute( 'auto_inc' );
		my $default_seq_node = $table_col_node->get_node_ref_attribute( 'default_seq' );
		push( @table_col_sql, 
			$table_col_name.' '.$domain_sql. # line corres to SQL-2003, 6.2 <field definition>
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
		my $table_ind_name = $builder->build_expr_identifier_element( $table_ind_node );
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
			my $foreign_table_name = $builder->build_expr_identifier_element( 
				$table_ind_node->get_node_ref_attribute( 'f_table' ) );
			my $foreign_col_names_sql = join( ', ', map { 
					$builder->build_expr_identifier_element( $_->get_node_ref_attribute( 'f_table_col' ) )
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
	my $table_name = $builder->build_expr_identifier_chain( $table_node );
	return( 'DROP TABLE '.$table_name.';' );
}

######################################################################

sub build_schema_view_create { # SQL-2003, 11.22 "<view definition>" (p590)
	my ($builder, $view_node) = @_;
	my $view_name = $builder->build_expr_identifier_chain( $view_node );
	my $query_expression = $builder->build_query_query_expr( $view_node );
	return( 'CREATE VIEW '.$view_name.' AS '.$query_expression.';' );
	# Note: Several interesting looking features are not implemented yet.
}

sub build_schema_view_delete { # SQL-2003, 11.23 "<drop view statement>" (p600)
	my ($builder, $view_node) = @_;
	my $view_name = $builder->build_expr_identifier_chain( $view_node );
	return( 'DROP VIEW '.$view_name.';' );
}

######################################################################

sub build_schema_trigger_create { # SQL-2003, 11.39 "<trigger definition>" (p629)
	my ($builder, $trigger_node) = @_;
	my $routine_node = $trigger_node->get_child_nodes( 'routine' )->[0];
	my $trigger_name = $builder->build_expr_identifier_element( $routine_node );
	my $table_node = $trigger_node->get_node_ref_attribute( 'table' );
	my $table_name = $builder->build_expr_identifier_element( $table_node );
	my $run_before = $trigger_node->get_literal_attribute( 'run_before' );
	my $run_after = $trigger_node->get_literal_attribute( 'run_after' );
	my $run_instead = $trigger_node->get_literal_attribute( 'run_instead' ); # TODO: use this
	my $on_insert = $trigger_node->get_literal_attribute( 'on_insert' );
	my $on_update = $trigger_node->get_literal_attribute( 'on_update' );
	my $on_delete = $trigger_node->get_literal_attribute( 'on_delete' );
	my $for_each_row = $trigger_node->get_literal_attribute( 'for_each_row' );
	my $trigger_action_time = $run_before ? 'BEFORE' : 'AFTER'; # SQL-2003: exactly one required
	my $trigger_event = $on_insert ? 'INSERT' : $on_update ? 'UPDATE' : 'DELETE'; # one required
	# TODO: Implement optional OF <trigger column list>.
	my @transition_var_names = (); # TODO: NEW/OLD ROW AS <... variable name>
	my $triggered_sql_statement = 'BEGIN ATOMIC '.join( '', 
		(map { $builder->build_schema_routine_stmt( $_ ) } 
			@{$routine_node->get_child_nodes( 'routine_var' )}),
		(map { $builder->build_schema_routine_stmt( $_ ) } 
			@{$routine_node->get_child_nodes( 'routine_stmt' )}),
		).'; END';
	return( 'CREATE TRIGGER '.$trigger_name.' '.
		$trigger_action_time.' '.$trigger_event.' ON '.$table_name.
		(@transition_var_names ? ' REFERENCING '.join( ' ', @transition_var_names ) : '').
		($for_each_row ? ' FOR EACH ROW' : ' FOR EACH STATEMENT').
		# TODO: WHEN ( <search condition> )
		$triggered_sql_statement.
		';' );
}

sub build_schema_trigger_delete { # SQL-2003, 11.40 "<drop trigger statement>" (p633)
	my ($builder, $trigger_node) = @_;
	my $routine_node = $trigger_node->get_child_nodes( 'routine' )->[0];
	my $trigger_name = $builder->build_expr_identifier_chain( $routine_node );
	return( 'DROP TRIGGER '.$trigger_name.';' );
}

######################################################################

sub build_schema_routine_create { # SQL-2003, 11.50 "<SQL-invoked routine>" (p675)
	my ($builder, $routine_node) = @_;
	my $routine_type = $routine_node->get_enumerated_attribute( 'routine_type' );
	my $routine_name = $builder->build_expr_identifier_chain( $routine_node );
	my @param_decl_list = ();
	# TODO: <routine characteristics> where appropriate.
	my $routine_body = 'BEGIN '.join( '', 
		(map { $builder->build_schema_routine_stmt( $_ ) } 
			@{$routine_node->get_child_nodes( 'routine_var' )}),
		(map { $builder->build_schema_routine_stmt( $_ ) } 
			@{$routine_node->get_child_nodes( 'routine_stmt' )}),
		).'; END';
	if( $routine_type eq 'PROCEDURE' ) {
		return( 'CREATE PROCEDURE '.$routine_name.
			(@param_decl_list ? '('.join( ', ', @param_decl_list ).')' : '').
			' '.$routine_body.';' );
	} elsif( $routine_type eq 'FUNCTION' ) {
		my $return_data_type = $builder->build_expr_data_type_spec( 
			$routine_node->get_node_ref_attribute( 'domain' ) );
		return( 'CREATE FUNCTION '.$routine_name.
			(@param_decl_list ? '('.join( ', ', @param_decl_list ).')' : '').
			' RETURNS '.$return_data_type.
			' '.$routine_body.';' );
	} else {}
}

sub build_schema_routine_delete { # SQL-2003, 11.52 "<drop routine statement>" (p703)
	my ($builder, $routine_node) = @_;
	my $routine_type = $routine_node->get_enumerated_attribute( 'routine_type' );
	my $routine_name = $builder->build_expr_identifier_chain( $routine_node );
	# Note: 10.6 "<specific routine designator>" (p499) may be useful later.
	if( $routine_type eq 'PROCEDURE' ) {
		return( 'DROP PROCEDURE '.$routine_name.';' );
	} elsif( $routine_type eq 'FUNCTION' ) {
		return( 'DROP FUNCTION '.$routine_name.';' );
	} else {}
}

######################################################################

sub build_select_rows {
	my ($builder, $view_node) = @_;

	# Note that 7 Query expressions starts on page 293 (317).


	return( 'SELECT ' );
}

######################################################################

sub build_insert_rows {
	my ($builder, $view_node) = @_;


	return( 'INSERT ' );
}

######################################################################

sub build_update_rows {
	my ($builder, $view_node) = @_;


	return( 'UPDATE ' );
}

######################################################################

sub build_delete_rows {
	my ($builder, $view_node) = @_;


	return( 'DELETE ' );
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
	my $create_sql = $builder->build_sql_routine( $cr_tbl_cmd_node );

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

I<CAVEAT: THIS MODULE IS "UNDER CONSTRUCTION" AND MANY FEATURES DESCRIBED BY 
SQL::SyntaxModel ARE NOT YET IMPLEMENTED.>

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

=head1 PROPERTY ACCESSOR METHODS

These methods are stateful and can only be invoked from this module's objects.

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
names to the positional "?" in the new SQL.

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

=head2 named_subqueries([ NEW_VALUE ])

	my $old_val = $builder->named_subqueries();
	$builder->named_subqueries( 0 );

This getter/setter method returns this object's "named subqueries" boolean
property; if the optional NEW_VALUE argument is defined, this property is first
set to that value.  If this property is true (the default), then query
expressions will be generated having a "with" clause when any sub-queries have
names; if this property is false then all sub-queries will be in-lined whether
they have names or not (since the database engine doesn't support "with").

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

=head1 SCALAR EXPRESSION SQL CONSTRUCTION METHODS

These "getter" methods build SQL expressions and correspond to the subsections 
of SQL-2003 Foundation section 6 "Scalar expressions" (p161).

=head2 build_expr_data_type_spec( DOMAIN_NODE )

	my $sql = $builder->build_expr_data_type_spec( $domain_node );

This method takes a 'domain' SSM Node and builds a corresponding SQL fragment
such as would be used in the "data type" reference of a table column
definition.  Example return values are "VARCHAR(40)", "DECIMAL(7,2)", "BOOLEAN"
"INTEGER UNSIGNED".  Most of the "data type customizations" property elements
are used to customize this method.  See SQL-2003 6.1 "<data type>".

=head2 build_expr_identifier_element( OBJECT_NODE )

	my $quoted = $builder->build_expr_identifier_element( $object_node );

This method takes a SQL::SyntaxModel::Node object in OBJECT_NODE, extracts its
'name' attribute, and returns that after passing it to quote_identifier().  The
result is an "unqualified identifier".  Note that SQL::SyntaxModel will throw
an exception if the Node is of the wrong type.

=head2 build_expr_identifier_chain( OBJECT_NODE )

	my $quoted = $builder->build_expr_identifier_chain( $object_node );

This method is like build_expr_identifier_element() except that it will also
trace all of the relevant parent Nodes of the given OBJECT_NODE, extracting and
quoting their 'name' also.  Then all of the quoted 'name' are stitched together
with ".", as with the SQL standard, with the combined string returned.  The
result is a "qualified identifier".  For example, passing a 'table' node will
usually return 'schema_name.table_name'.  See SQL-2003 6.6 "<identifier
chain>".

=head1 QUERY EXPRESSION SQL CONSTRUCTION METHODS

These "getter" methods build SQL expressions and correspond to the subsections 
of SQL-2003 Foundation section 7 "Query expressions" (p293).

=head1 SCHEMA DEFINITION SQL CONSTRUCTION METHODS

These "getter" methods build SQL strings or fragments thereof that are used
mainly when declaring or defining (or removing) database schema constructs.

=head2 build_schema_sequence_create( SEQUENCE_NODE )

	my $sql = $builder->build_schema_sequence_create( $sequence_node );

This method takes a 'sequence' SSM Node and builds a corresponding "CREATE
SEQUENCE" DDL SQL statement, which it returns.

=head2 build_schema_sequence_delete( SEQUENCE_NODE )

	my $sql = $builder->build_schema_sequence_delete( $sequence_node );

This method takes a 'sequence' SSM Node and builds a corresponding "DROP
SEQUENCE" DDL SQL statement, which it returns.

=head2 build_schema_table_create( TABLE_NODE )

	my $sql = $builder->build_schema_table_create( $table_node );

This method takes a 'table' SSM Node and builds a corresponding "CREATE
TABLE" DDL SQL statement, which it returns.

Incorporates 6.2 "<field definition>".

=head2 build_schema_table_delete( TABLE_NODE )

	my $sql = $builder->build_schema_sequence_delete( $table_node );

This method takes a 'table' SSM Node and builds a corresponding "DROP
TABLE" DDL SQL statement, which it returns.

=head2 build_schema_view_create( VIEW_NODE )

	my $sql = $builder->build_schema_view_create( $view_node );

This method takes a 'view' SSM Node and builds a corresponding "CREATE
VIEW" DDL SQL statement, which it returns.

=head2 build_schema_view_delete( VIEW_NODE )

	my $sql = $builder->build_schema_view_delete( $view_node );

This method takes a 'view' SSM Node and builds a corresponding "DROP
VIEW" DDL SQL statement, which it returns.

=head2 build_schema_routine_create( ROUTINE_NODE )

	my $sql = $builder->build_schema_routine_create( $routine_node );

This method takes a 'routine' SSM Node and builds a corresponding "CREATE
ROUTINE/PROCEDURE/FUNCTION" DDL SQL statement, which it returns.

=head2 build_schema_routine_delete( ROUTINE_NODE )

	my $sql = $builder->build_schema_routine_delete( $routine_node );

This method takes a 'routine' SSM Node and builds a corresponding "DROP
ROUTINE/PROCEDURE/FUNCTION" DDL SQL statement, which it returns.

=head1 SQL CONSTRUCTION METHODS

I<This documentation isn't written yet.  Meanwhile, look at the source code for
this module.  The property getters/setters appear first, and the SQL building
methods appear below them.  Also see source of Rosetta::Engine::Generic, which
uses this module.>

=head1 UTILITY METHODS

=head2 substitute_macros( STR, SUBS )

	my $result = $builder->substitute_macros( 'NUMBER({p},{s})', { 'p' => 7, 's' => 2 } )

This method takes a string in STR which contains brace-delimited tokens and
returns a version of that string having the tokens replaced by corresponding
values provided in the hash ref SUBS.  This method is used mainly by
build_expr_data_type_spec() at the moment.

=head1 BUGS

This module is currently in pre-alpha development status, meaning that some
parts of it will be changed in the near future, perhaps in incompatible ways.

=head1 SEE ALSO

perl(1), Rosetta, SQL::SyntaxModel, Rosetta::Engine::Generic, Rosetta::Utility::SQLParser.

=cut
