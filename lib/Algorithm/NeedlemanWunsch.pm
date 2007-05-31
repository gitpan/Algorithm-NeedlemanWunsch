package Algorithm::NeedlemanWunsch;

use warnings;
use strict;

use List::Util qw(max);

sub new {
    my $class = shift;
    my $score_sub = shift;

    my $self = { score_sub => $score_sub };
    if (@_) {
        $self->{gap_penalty} = $_[0];
    }

    return bless $self, $class;
}

sub align {
    my $self = shift;

    my $a = shift;
    my $b = shift;

    my $cb;
    if (@_) {
        $cb = $_[0];
    } else {
        $cb = { };
    }

    if (!exists($self->{gap_penalty})) {
        $self->{gap_penalty} = &{$self->{score_sub}}();
    }

    if (exists($cb->{select_align})) {
        my @cn = qw(align shift_a shift_b);
	foreach (@cn) {
	    if (!exists($cb->{$_})) {
	        $cb->{$_} = $self->_synth_cb($cb->{select_align}, $_);
	    }
	}
    }

    $self->{callbacks} = $cb;

    my $D = [ [ 0 ] ];
    my $m = scalar(@$b);
    my $n = scalar(@$a);

    my $score_diag = sub {
        my ($i, $j) = @_;

	$D->[$i - 1]->[$j - 1] +
	    &{$self->{score_sub}}($a->[$j - 1], $b->[$i - 1]);
    };

    my $score_up = sub {
        my ($i, $j) = @_;

	$D->[$i - 1]->[$j] + $self->{gap_penalty};
    };

    my $score_left = sub {
        my ($i, $j) = @_;

	$D->[$i]->[$j - 1] + $self->{gap_penalty};
    };

    my @subproblems = ( $score_diag, $score_up, $score_left );

    my $j = 1;
    while ($j <= $n) {
        $D->[0]->[$j] = $j * $self->{gap_penalty};
	++$j;
    }

    my $i = 1;
    while ($i <= $m) {
        $D->[$i]->[0] = $i * $self->{gap_penalty};
	++$i;
    }

    $i = 1;
    while ($i <= $m) {
        $j = 1;
	while ($j <= $n) {
	    my @scores = map { &$_($i, $j); } @subproblems;
	    $D->[$i]->[$j] = max(@scores);

	    # my $x = join ', ', @scores;
	    # warn "$i, $j: $x -> ", $D->[$i]->[$j], "\n";
	    ++$j;
	}

	++$i;
    }

    $i = $m;
    $j = $n;
    while (($i > 0) || ($j > 0)) {
	my @alt;
	if (($i > 0) && ($j > 0)) {
	    push @alt, [ &$score_diag($i, $j), $i - 1, $j - 1];
	}

	if ($i > 0) {
	    push @alt, [ &$score_up($i, $j), $i - 1, $j ];
	}

	if ($j > 0) {
	    push @alt, [ &$score_left($i, $j), $i, $j - 1];
	}

        my $t = $D->[$i]->[$j];
        my @opt_alt = grep { $_->[0] == $t; } @alt;
	if (!@opt_alt) {
	    die "internal error";
	}

	my @sources = map { [ $_->[1], $_->[2] ]; } @opt_alt;

	my $cur = [ $i, $j ];
	my $move;
	if (@sources == 1) {
	    $move = $self->_simple_trace_back($cur, $sources[0], $cb);
	} else {
	    $move = $self->_trace_back($cur, \@sources);
	}

	if ($move eq 'align') {
	    --$i;
	    --$j;
	} elsif ($move eq 'shift_a') {
	    --$j;
	} elsif ($move eq 'shift_b') {
	    --$i;
	} else {
	    die "internal error";
	}
    }

    return $D->[$m]->[$n];
}

sub _trace_back {
    my ($self, $cur, $sources) = @_;

    my $arg = { };
    foreach my $next (@$sources) {
        my $m = $self->_simple_trace_back($cur, $next, { });
	if ($m eq 'align') {
	    $arg->{align} = [ $cur->[1] - 1, $cur->[0] - 1 ];
	} elsif ($m eq 'shift_a') {
	    $arg->{shift_a} = $cur->[1] - 1;
	} elsif ($m eq 'shift_b') {
	    $arg->{shift_b} = $cur->[0] - 1;
	} else {
	    die "internal error";
	}
    }

    my $move;
    my $cb = $self->{callbacks};
    if (exists($cb->{select_align})) {
        $move = &{$cb->{select_align}}($arg);
	if (!exists($arg->{$move})) {
	    die "select_align callback returned invalid selection $move.";
	}
    } else {
        my @cn = qw(align shift_a shift_b);
	foreach my $m (@cn) {
	    if (exists($arg->{$m})) {
	        $move = $m;
		last;
	    }
	}

	if (!$move) {
	    die "internal error";
	}

	if (exists($cb->{$move})) {
	    if ($move eq 'align') {
	        &{$cb->{align}}(@{$arg->{align}});
	    } else {
	        &{$cb->{$move}}($arg->{$move});
	    }
	}
    }

    return $move;
}

sub _simple_trace_back {
    my ($self, $cur, $next, $cb) = @_;

    if ($next->[0] == $cur->[0] - 1) {
        if ($next->[1] == $cur->[1] - 1) {
	    if (exists($cb->{align})) {
	        &{$cb->{align}}($next->[1], $next->[0]);
	    }

	    return 'align';
	} else {
	    if ($next->[1] != $cur->[1]) {
	        die "internal error";
	    }

	    if (exists($cb->{shift_b})) {
	        &{$cb->{shift_b}}($cur->[0] - 1);
	    }

	    return 'shift_b';
	}
    } else {
        if ($next->[0] != $cur->[0]) {
	    die "internal error";
	}

	if ($next->[1] != $cur->[1] - 1) {
	    die "internal error";
	}

	if (exists($cb->{shift_a})) {
	    &{$cb->{shift_a}}($cur->[1] - 1);
	}

	return 'shift_a';
    }
}

sub _synth_cb {
    my ($dummy, $univ_cb, $spec_name) = @_;

    my $cb;
    if ($spec_name eq 'align') {
        $cb = sub {
	    my $arg = { align => [ @_ ] };
	    my $rv = &$univ_cb($arg);
	    die "select_align callback returned invalid selection $rv."
	        unless $rv eq 'align';
	};
    } else {
        $cb = sub {
	    my $arg = { $spec_name => $_[0] };
	    my $rv = &$univ_cb($arg);
	    die "select_align callback returned invalid selection $rv."
	        unless $rv eq $spec_name;
	};
    }

    return $cb;
}

1;

__END__

=head1 NAME

Algorithm::NeedlemanWunsch - global sequence alignment with configurable scoring

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

    use Algorithm::NeedlemanWunsch;

    sub score_sub {
        if (!@_) {
	    return -2; # gap penalty
        }

	return ($_[0] eq $_[1]) ? 1 : -1;
    }

    my $matcher = Algorithm::NeedlemanWunsch->new(\&score_sub);
    my $score = $matcher->align(
               \@a,
               \@b,
               {   align     => \&on_align,
                   shift_a => \&on_shift_a,
                   shift_b => \&on_shift_b,
		   select_align => \&on_select_align
               });

=head1 DESCRIPTION

Sequence alignment is a way to find commonalities in two (or more)
similar sequences or strings of some items or characters. Standard
motivating example is the comparison of DNA sequences and their
functional and evolutionary similarities and differences, but the
problem has much wider applicability - for example finding the longest
common subsequence (that is, C<diff>) is a special case of sequence
alignment.

Conceptually, sequence alignment works by scoring all possible
alignments and choosing the alignment with maximal score. For example,
sequences C<a t c t> and C<t g a t> may be aligned

  sequence A: a t c - t
                | |   |
  sequence B: - t g a t

or

  sequence A: - - a t c t
                  | |
  sequence B: t g a t - -

(and exponentially many other ways, of course). Note that we're
considering I<global> alignments, over the entire length of both
sequences; each item is either aligned with an item of the other
sequence, or corresponds to a I<gap> (which is always aligned with an
item - aligning two gaps wouldn't help anything). This approach is
especially suitable for comparing sequences of comparable length and
somewhat similar along their whole lengths (that is, without long
stretches that have nothing to do with each other).

In the example above, the second alignment has more gaps than the
first, but perhaps your a's are structurally important and you like
them lined up so much that you'd still prefer the second
alignment. Conversely, if c is "almost the same" as g, it might be the
first alignment that matches better. Needleman-Wunsch formalizes such
considerations into a I<similarity matrix>, assigning payoffs to each
(ordered, but the matrix is normally symmetrical so that the order
doesn't matter) pair of possible sequence items, plus a I<gap
penalty>, quantifying the desirability of a gap in a sequence. A
preference of pairings over gaps is expressed by a low (relative to
the similarity matrix values, normally negative) gap penalty.

The alignment score is then defined as the sum, over the positions
where at least one sequence has an item, of the similarity matrix
values indexed by the first and second item (when both are defined)
and gap penalties (for items aligned with a gap). For example, if C<S>
is the similarity matrix and C<g> denotes the gap penalty, the
alignment

  sequence A: a a t t c c

  sequence B: a - - - t c

has score C<S[a, a] + 3 * g + S[c, t] + S[c, c]>.

When the gap penalty is 0 and the similarity an identity matrix, i.e.
assigning 1 to every match and 0 to every mismatch, Needleman-Wunsch
reduces to finding the longest common subsequence.

The algorithm for maximizing the score is a standard application of
dynamic programming, computing the optimal alignment score of empty
and 1-item sequences and building it up until the whole input
sequences are taken into consideration. Once the optimal score is
known, the algorithm traces back to find the gap positions. Note that
while the maximal score is obviously unique, the alignment having it
in general isn't; this module's interface allows the calling
application to choose between different optimal alignments.

=head1 METHODS

=head2 new(\&score_sub [, $gap_penalty ])

The constructor. Takes one mandatory argument, which is a coderef to a
sub implementing the similarity matrix, plus an optional gap penalty
argument. If the gap penalty isn't specified as a constructor
argument, the C<Algorithm::NeedlemanWunsch> object gets it by calling
the scoring sub without arguments; apart from that case, the sub is
called with 2 arguments, which are items from the first and second
sequence, respectively, passed to
C<Algorithm::NeedlemanWunsch::align>. Note that the sub must be pure,
i.e. always return the same value when called with the same arguments.

=head2 align(\@a, \@b [, \%callbacks ])

The core of the algorithm. Creates a bottom-up dynamic programming
matrix, fills it with alignment scores and then traces back to find an
optimal alignment, informing the application about its items by
invoking the callbacks passed to the method.

The first 2 arguments of C<align> are array references to the aligned
sequences, the third a hash reference with user-supplied
callbacks. The callbacks are identified by the hash keys, which are as
follows:

=over

=item align

Aligns two sequence items. The callback is called with 2 arguments,
which are the positions of the paired items in C<\@a> and C<\@b>,
respectively.

=item shift_a

Aligns an item of the first sequence with a gap in the second
sequence. The callback is called with 1 argument, which is the
position of the item in C<\@a>.

=item shift_b

Aligns a gap in the first sequence with an item of the second
sequence. The callback is called with 1 argument, which is the
position of the item in C<\@b>.

=item select_align

Called when there's more than one way to construct the optimal
alignment, with 1 argument which is a hashref enumerating the
possibilities. The hash may contain the following keys:

=over

=item align

If this key exists, the optimal alignment may align two sequence
items. The key's value is an arrayref with the positions of the paired
items in C<\@a> and C<\@b>, respectively.

=item shift_a

If this key exists, the optimal alignment may align an item of the
first sequence with a gap in the second sequence. The key's value is
the position of the item in C<\@a>.

=item shift_b

If this key exists, the optimal alignment may align a gap in the first
sequence with an item of the second sequence. The key's value is
the position of the item in C<\@b>.

=back

All keys are optional, but the hash will always have at least one. The
callback must select one of the possibilities by returning one of the
keys.

=back

All callbacks are optional. When there is just one way to make the
optimal alignment, the C<Algorithm::NeedlemanWunsch> object prefers
calling the specific callbacks, but will call C<select_align> if it's
defined and the specific callback isn't.

Note that the passed positions move backwards, from the sequence ends
to zero - if you're building the alignment in your callbacks, add
items to the front.

=head1 SEE ALSO

L<Algorithm::Diff>

=head1 AUTHOR

Vaclav Barta, C<< <vbar@comp.cz> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-algorithm-needlemanwunsch at rt.cpan.org>, or through the web
interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Algorithm-NeedlemanWunsch>.
I will be notified, and then you'll automatically be notified of
progress on your bug as I make changes.

=head1 COPYRIGHT & LICENSE

Copyright 2007 Vaclav Barta, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 CREDITS

The algorithm is defined by Saul Needleman and Christian Wunsch in "A
general method applicable to the search for similarities in the amino
acid sequence of two proteins", J Mol Biol. 48(3):443-53.

This implementation is based mostly on
L<http://www.ludwig.edu.au/course/lectures2005/Likic.pdf> .

=cut
