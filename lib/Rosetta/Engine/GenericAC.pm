=head1 NAME

Rosetta::Engine::GenericAC - A catch-all Engine for any DBI-supported SQL database that auto-commits everything

=cut

######################################################################

package Rosetta::Engine::GenericAC;
use 5.006;
use strict;
use warnings;
use vars qw($VERSION);
$VERSION = '0.01';

use Rosetta::Engine::Generic 0.03;

use base qw( Rosetta::Engine::Generic );

######################################################################

=head1 DEPENDENCIES

Perl Version: 5.006

Standard Modules: I<none>

Nonstandard Modules: 

	Rosetta::Engine::Generic 0.03 (parent class)

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

# Names of features that Rosetta::Engine::GenericAC's parent class 
# claims to support, but GenericAC itself claims the opposite:
my @SUBTRACT_SUPPORTED_FEATURES = qw(
	TRAN_BASIC 
);

######################################################################

sub get_static_const_use_auto_commit {
	# This function is separated so sub-classes can override it easily.
	return( 1 );
}

######################################################################

sub get_static_const_supported_features {
	# This function is separated so sub-classes can override it easily.
	my ($env_eng) = @_;
	my %supported_features = %{$env_eng->SUPER::get_static_const_supported_features()};
	foreach my $feature_name (@SUBTRACT_SUPPORTED_FEATURES) {
		delete( $supported_features{$feature_name} );
	}
	return( \%supported_features );
}

######################################################################
######################################################################

1;
__END__

=head1 SYNOPSIS

See the SYNOPSIS of Rosetta::Engine::Generic, which this module sub-classes; 
substitute "Generic" for "GenericAC" as appropriate.

=head1 DESCRIPTION

This module is a reference implementation of fundamental Rosetta features.

The Rosetta::Engine::GenericAC Perl 5 module is a sub-class of
Rosetta::Engine::Generic and implements a few behaviour changes, mainly to
support database products that lack native transaction support.  For all
intents and purposes, this module is an optional drop-in replacement for the
parent.  While the parent will always use transactions and require explicit
commits for database actions to be saved, GenericAC will auto-commit every
database action (explicitly on transaction-supporting databases, and that just
happens anyway on non-supporting ones), so separate commits are not necessary.

See Rosetta::Engine::Generic for more information.

=head1 SUPPORTED ROSETTA FEATURES

Rosetta::Engine::GenericAC declares support for all of the same Rosetta Native
Interface features that its parent supports, but with the explicit exception of
the following, which are *not* supported:

	TRAN_BASIC 

=head1 BUGS

See the BUGS main documentation section of Rosetta::Engine::Generic since
everything said there applies to this module also.

=head1 SEE ALSO

Rosetta::Engine::Generic and the various other modules mentioned in its SEE
ALSO.

=cut
