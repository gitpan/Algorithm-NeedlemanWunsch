#!perl -T

use Algorithm::NeedlemanWunsch;
use Test::More tests => 5;

sub kronecker {
    my ($a, $b) = @_;

    return ($a eq $b) ? 1 : 0;
}

sub nowarn_kronecker {
    no strict;
    no warnings;

    my ($a, $b) = @_;

    return ($a eq $b) ? 1 : 0;
}

my $matcher = Algorithm::NeedlemanWunsch->new(\&kronecker, 0);

my $score = $matcher->align([ ], [ ]);
is($score, 0);

$score = $matcher->align([ 'a', 'b', 'c' ], [ 'd', 'e' ]);
is($score, 0);

my $float_gap = -3.14;
my $eps = 0.0001;
$matcher = Algorithm::NeedlemanWunsch->new(\&kronecker, $float_gap);

$score = $matcher->align([ 1 ], [ ]);
my $delta = abs($score - $float_gap);
ok($delta < $eps);

$score = $matcher->align([ ], [ 2.5 ]);
$delta = abs($score - $float_gap);
ok($delta < $eps);

$matcher = Algorithm::NeedlemanWunsch->new(\&nowarn_kronecker, -1);
$score = $matcher->align([ '', undef, '' ], [ '', '', '', '' ]);
is($score, 2);
