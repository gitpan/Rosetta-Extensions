=head1 NAME

Rosetta::Utility::SQLBuilder - Generate ANSI/ISO SQL-1999 and other SQL variants

=cut

######################################################################

package Rosetta::Utility::SQLBuilder;
use 5.006;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = '0.01';

use Locale::KeyedText 0.03;
use SQL::SyntaxModel 0.16;

######################################################################

=head1 DEPENDENCIES

Perl Version: 5.006

Standard Modules: I<none>

Nonstandard Modules: 

	Locale::KeyedText 0.03 (for error messages)
	SQL::SyntaxModel 0.16

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
my $PROP_POSIT_BVARS = 'posit_bvars'; # boolean
my $PROP_INSEN_IDENT = 'insen_ident'; # boolean
my $PROP_IDENT_QUOTC = 'ident_quotc'; # character
my $PROP_DATA_TYPES  = 'data_types' ; # hash ref

# Names of specific data types, used as keys in $PROP_DATA_TYPES hash.
my $DT_NUM_INT_8  = 'num_int_8' ; # what signed ints up to  8 bits are stored as
my $DT_NUM_INT_16 = 'num_int_16'; # what signed ints up to 16 bits are stored as
my $DT_NUM_INT_24 = 'num_int_24'; # what signed ints up to 24 bits are stored as
my $DT_NUM_INT_32 = 'num_int_32'; # what signed ints up to 32 bits are stored as
my $DT_NUM_INT_64 = 'num_int_64'; # what signed ints up to 64 bits are stored as
my $DT_NUM_INT_128 = 'num_int_128'; # what signed ints up to 128 bits are stored as
my $DT_NUM_INT_LG = 'num_int_lg'; # what signed ints larger than 128 bits are stored as
my $DT_NUM_EXA  = 'num_exa'; # an exact non-integer number of arbitrary length (stored as chars)
my $DT_NUM_APR_32 = 'num_apr_32'; # what floating-point nums up to 32 bits are stored as
my $DT_NUM_APR_64 = 'num_apr_64'; # what floating-point nums up to 64 bits are stored as
my $DT_NUM_APR_128 = 'num_apr_128'; # what floating-point nums up to 128 bits are stored as
my $DT_NUM_APR_LG = 'num_apr_lg'; # what floating-point nums larger than 128 bits are stored as
my $DT_NUM_UNS_SFX = 'num_uns_sfx'; # suffix added to numeric type decls to make unsigned
my $DT_STR_BIT_255 = 'str_bit_255'; # storage for binary data up to 255 bytes, var-size
my $DT_STR_BIT_255F = 'str_bit_255f'; # storage for binary data up to 255 bytes, fixed-size
my $DT_STR_BIT_2K = 'str_bit_2k'; # storage for binary data up to 2000 bytes, var-size
my $DT_STR_BIT_2KF = 'str_bit_2kf'; # storage for binary data up to 2000 bytes, fixed-size
my $DT_STR_BIT_4K = 'str_bit_4k'; # storage for binary data up to 4000 bytes, var-size
my $DT_STR_BIT_4KF = 'str_bit_4kf'; # storage for binary data up to 4000 bytes, fixed-size
my $DT_STR_BIT_32K = 'str_bit_32k'; # storage for binary data up to 32767 bytes
my $DT_STR_BIT_65K = 'str_bit_65k'; # storage for binary data up to 65535 bytes
my $DT_STR_BIT_16M = 'str_bit_16m'; # storage for binary data up to 16777215 bytes
my $DT_STR_BIT_2G = 'str_bit_2g'; # storage for binary data up to 2147483647 bytes
my $DT_STR_BIT_4G = 'str_bit_4g'; # storage for binary data up to 4294967295 bytes
my $DT_STR_BIT_LG = 'str_bit_lg'; # storage for larger binary data (over 4GB)
my $DT_STR_CHAR_255 = 'str_char_255'; # storage for character data up to 255 chars, var-size
my $DT_STR_CHAR_255F = 'str_char_255f'; # storage for character data up to 255 chars, fixed-size
my $DT_STR_CHAR_2K = 'str_char_2k'; # storage for character data up to 2000 chars, var-size
my $DT_STR_CHAR_2KF = 'str_char_2kf'; # storage for character data up to 2000 chars, fixed-size
my $DT_STR_CHAR_4K = 'str_char_4k'; # storage for character data up to 4000 chars, var-size
my $DT_STR_CHAR_4KF = 'str_char_4kf'; # storage for character data up to 4000 chars, fixed-size
my $DT_STR_CHAR_32K = 'str_char_32k'; # storage for character data up to 32767 chars
my $DT_STR_CHAR_65K = 'str_char_65k'; # storage for character data up to 65535 chars
my $DT_STR_CHAR_16M = 'str_char_16m'; # storage for character data up to 16777215 chars
my $DT_STR_CHAR_2G = 'str_char_2g'; # storage for character data up to 2147483647 chars
my $DT_STR_CHAR_4G = 'str_char_4g'; # storage for character data up to 4294967295 chars
my $DT_STR_CHAR_LG = 'str_char_lg'; # storage for larger character data (over 4GB)
my $DT_BOOLEAN  = 'boolean';
my $DT_DATETIME = 'datetime';
my $DT_INTERVAL = 'interval';
my $DT_HAS_ENUM_TYPE = 'has_enum_type'; # boolean; if true use ENUM, if false, use CHECK

# Miscellaneous constant values
my $INFINITY = 1_000_000_000_000_000_000; # A hack to mean 'unlimited size'

######################################################################

sub new {
	my ($class) = @_;
	my $builder = bless( {}, ref($class) || $class );
	$builder->{$PROP_POSIT_BVARS} = 0;
	$builder->{$PROP_INSEN_IDENT} = 0;
	$builder->{$PROP_IDENT_QUOTC} = '"'; # doublequote given in ANSI example
		# set to '"' for Oracle, '`' for MySQL
	$builder->{$PROP_DATA_TYPES} = {
		$DT_NUM_INT_8  => 'TINYINT'  , # for MySQL; 'NUMBER' for Oracle
		$DT_NUM_INT_16 => 'SMALLINT' , # for SQL89, MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_INT_24 => 'MEDIUMINT', # for MySQL; 'NUMBER' for Oracle
		$DT_NUM_INT_32 => 'INTEGER'  , # for SQL92, MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_INT_64 => 'BIGINT'   , # for MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_INT_128 => 'DECIMAL' , # for MySQL; 'NUMBER' for Oracle
		$DT_NUM_INT_LG => 'DECIMAL'  , # for MySQL; 'RAW' for Oracle
		$DT_NUM_EXA => 'DECIMAL', # for SQL99, MySQL, Pg; 'NUMBER' for Oracle
		$DT_NUM_APR_32 => 'FLOAT'  , # for MySQL; 'NUMBER' for Oracle
		$DT_NUM_APR_64 => 'DOUBLE'   , # for MySQL; 'NUMBER' for Oracle
		$DT_NUM_APR_128 => 'DECIMAL' , # for MySQL; 'NUMBER' for Oracle
		$DT_NUM_APR_LG => 'DECIMAL'  , # for MySQL; 'RAW' for Oracle
		$DT_NUM_UNS_SFX => 'UNSIGNED', # for MySQL
		$DT_STR_BIT_255 => 'VARBIT', # standard; 'RAW' for Oracle; 'TINYBLOB' for MySQL
		$DT_STR_BIT_255F => 'BIT'  , # standard; 'RAW' for Oracle; 'TINYBLOB' for MySQL
		$DT_STR_BIT_2K  => 'BLOB'  , # for MySQL; 'RAW' for Oracle
		$DT_STR_BIT_2KF => 'BLOB'  , # for MySQL; 'RAW' for Oracle
		$DT_STR_BIT_4K  => 'BLOB'  , # for MySQL, Oracle
		$DT_STR_BIT_4KF => 'BLOB'  , # for MySQL, Oracle
		$DT_STR_BIT_32K => 'BLOB'  , # for MySQL, Oracle
		$DT_STR_BIT_65K => 'BLOB'  , # for MySQL, Oracle
		$DT_STR_BIT_16M => 'BLOB'  , # standard, Oracle; 'MEDIUMBLOB' for MySQL
		$DT_STR_BIT_2G  => 'BLOB'  , # standard, Oracle; 'LONGBLOB' for MySQL
		$DT_STR_BIT_4G  => 'BLOB'  , # standard, Oracle; 'LONGBLOB' for MySQL
		$DT_STR_BIT_LG  => 'BLOB'  , # standard
		$DT_STR_CHAR_255 => 'VARCHAR', # for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_255F => 'CHAR'  , # for MySQL, Oracle
		$DT_STR_CHAR_2K  => 'TEXT'   , # for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_2KF => 'TEXT'   , # for MySQL; 'CHAR' for Oracle
		$DT_STR_CHAR_4K  => 'TEXT'   , # for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_4KF => 'TEXT'   , # for MySQL; 'VARCHAR2' for Oracle
		$DT_STR_CHAR_32K => 'CLOB'   , # 'VARCHAR2'/'CLOB' for Oracle; 'TEXT' for MySQL
		$DT_STR_CHAR_65K => 'CLOB'   , # standard, Oracle; 'TEXT' for MySQL
		$DT_STR_CHAR_16M => 'CLOB'   , # standard, Oracle; 'MEDIUMTEXT' for MySQL
		$DT_STR_CHAR_2G  => 'CLOB'   , # standard, Oracle; 'LONGTEXT' for MySQL
		$DT_STR_CHAR_4G  => 'CLOB'   , # standard, Oracle; 'LONGTEXT' for MySQL
		$DT_STR_CHAR_LG  => 'CLOB'   , # standard
		$DT_BOOLEAN  => 'BOOLEAN' , # standard; Oracle uses 'CHAR(1)'; MySQL 'TINYINT'
		$DT_DATETIME => 'DATETIME', # for MySQL; Oracle uses 'DATE'
		$DT_INTERVAL => 'INTERVAL',
		$DT_HAS_ENUM_TYPE => 0, # for standard, Oracle use CHECK; MySQL supports ENUM
	};
	return( $builder );
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

sub insensitive_identifiers {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_INSEN_IDENT} = $new_value;
	}
	return( $builder->{$PROP_INSEN_IDENT} );
}

######################################################################

sub identifier_quote_char {
	my ($builder, $new_value) = @_;
	if( defined( $new_value ) ) {
		$builder->{$PROP_IDENT_QUOTC} = $new_value;
	}
	return( $builder->{$PROP_IDENT_QUOTC} );
}

######################################################################

sub quote_entity_name {
	my ($builder, $name) = @_;
	if( $builder->{$PROP_INSEN_IDENT} ) {
		$name = uc( $name );
		$name =~ s|[^A-Z0-9_]||g;
	} else {
		my $quotc = $builder->{$PROP_IDENT_QUOTC};
		$name =~ s|$quotc||g;
		$name = $quotc.$name.$quotc;
	}
	return( $name );
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
	return( "'".(int $literal)."'" );
}

sub quote_numeric_literal {
	my ($builder, $literal) = @_;
	return( "'".(0 + $literal)."'" );
}

sub quote_boolean_literal {
	my ($builder, $literal) = @_;
	return( "'".($literal ? 1 : 0)."'" );
}

######################################################################

sub get_unqualified_object_name {
	my ($builder, $object_node) = @_;
	return( $builder->quote_entity_name( $object_node->get_literal_attribute( 'name' ) ) );
}

sub get_qualified_object_name {
	my ($builder, $object_node) = @_;
	my $node_type = $object_node->get_node_type();
	my $unqualified_name = $object_node->get_literal_attribute( 'name' );
	my $parent_name = undef;
	if( $object_node->valid_node_type_node_ref_attributes( $node_type, 'schema' ) ) {
		if( my $schema_node = $object_node->get_node_ref_attribute( 'schema' ) ) {
			$parent_name = $builder->get_qualified_object_name( $schema_node );
		} else {}
	} else {}
	return( ($parent_name ? $parent_name.'.' : '').
		$builder->quote_entity_name( $unqualified_name ) );
}

######################################################################

sub build_domain_defn {
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
			$sql .= '('.$num_precision.')';
		}
		if( $num_unsigned ) {
			$sql .= ' '.$type_conv->{$DT_NUM_UNS_SFX};
		}
	}

	if( $base_type eq 'NUM_EXA' ) {
		$sql = $type_conv->{$DT_NUM_EXA}.'('.$num_precision.','.($num_scale||0).')';
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
			$sql .= '('.$num_precision.','.($num_scale||0).')';
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
			$sql .= '('.$max_octets.')';
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
			$sql .= '('.$max_chars.')';
		}
	}

	if( $base_type eq 'BOOLEAN' ) {
		$sql = $type_conv->{$DT_BOOLEAN};
	}

	if( $base_type eq 'DATETIME' ) {
		$sql = $type_conv->{$DT_DATETIME};
	}

	if( $base_type eq 'INTERVAL' ) {
		$sql = $type_conv->{$DT_INTERVAL};
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

sub build_create_sequence {
	my ($builder, $sequence_node) = @_;
	my $sequence_name = $builder->get_qualified_object_name( $sequence_node );
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
		(defined( $min_val ) ? ' MINVALUE '.$min_val : ' NOMINVALUE').
		(defined( $max_val ) ? ' MAXVALUE '.$max_val : ' NOMAXVALUE').
		($cycle ? ' CYCLE' : ' NOCYCLE').
		($order ? ' ORDER' : ' NOORDER').
		';' );
}

sub build_delete_sequence {
	my ($builder, $sequence_node) = @_;
	my $sequence_name = $builder->get_qualified_object_name( $sequence_node );
	return( 'DROP SEQUENCE '.$sequence_name.';' );
}

######################################################################

sub build_create_table {
	my ($builder, $table_node, $is_temp) = @_;
	my $table_name = $builder->get_qualified_object_name( $table_node );
	my @table_col_sql = ();
	my %domain_sql_cache = (); # used when making col defs
	my %col_name_cache = (); # used when making ind defs
	my %mandatory_col_cache = (); # used when making ind defs
	foreach my $table_col_node (@{$table_node->get_child_nodes( 'table_col' )}) {
		my $table_col_name = $builder->get_unqualified_object_name( $table_col_node );
		unless( exists( $col_name_cache{$table_col_node} ) ) {
			$col_name_cache{$table_col_node} = $table_col_name;
		}
		my $domain_node = $table_col_node->get_node_ref_attribute( 'domain' );
		my $domain_name = $domain_node->get_literal_attribute( 'name' );
		unless( exists( $domain_sql_cache{$domain_name} ) ) {
			$domain_sql_cache{$domain_name} = $builder->build_domain_defn( $domain_node );
		}
		my $domain_sql = $domain_sql_cache{$domain_name};
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
				$builder->get_qualified_object_name( $default_seq_node ) : '')
		);
	}
	my @table_ind_sql = ();
	my $pk_is_made = 0;
	foreach my $table_ind_node (@{$table_node->get_child_nodes( 'table_ind' )}) {
		my $table_ind_name = $builder->get_unqualified_object_name( $table_ind_node );
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
				push( @table_ind_sql, 'CONSTRAINT UNIQUE INDEX '.$table_ind_name.
					' ('.$local_col_names_sql.')' );
			}
		}
		if( $ind_type eq 'FOREIGN' or $ind_type eq 'UFOREIGN' ) {
			my $foreign_table_name = $builder->get_unqualified_object_name( 
				$table_ind_node->get_node_ref_attribute( 'f_table' ) );
			my $foreign_col_names_sql = join( ', ', map { 
					$builder->get_unqualified_object_name( $_->get_node_ref_attribute( 'f_table_col' ) )
				} @table_ind_col_nodes );
			push( @table_ind_sql, 'CONSTRAINT FOREIGN KEY '.$table_ind_name.
				' ('.$local_col_names_sql.') REFERENCES '.$foreign_table_name.
				' ('.$foreign_col_names_sql.')' );
		}
	}
	return( 'CREATE'.($is_temp ? ' TEMPORARY' : '').' TABLE '.$table_name.' ('.
		join(', ', @table_col_sql).
		(@table_ind_sql ? ', '.join(', ', @table_ind_sql) : '').
		');' );
}

sub build_delete_table {
	my ($builder, $table_node) = @_;
	my $table_name = $builder->get_qualified_object_name( $table_node );
	return( 'DROP TABLE '.$table_name.';' );
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

The Rosetta::Utility::SQLBuilder Perl 5 module is a functional but quickly
built Rosetta utility class that converts a set of related SQL::SyntaxModel
Nodes into one or more SQL strings that are ready to give as input to a
particular SQL relational database management system.  This class will by
default produce SQL that is compliant with the ANSI/ISO SQL-1999 (or 1992 or
2003) standard, which should be useable as-is with most database products.  In
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

=head1 BUGS

This module is currently in pre-alpha development status, meaning that some
parts of it will be changed in the near future, perhaps in incompatible ways.

=head1 SEE ALSO

perl(1), Rosetta, SQL::SyntaxModel, Rosetta::Engine::Generic, Rosetta::Utility::SQLParser.

=cut
