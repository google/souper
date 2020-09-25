#!/usr/bin/perl -w

use strict;

my @nohups = glob "work*/nohup.out";
my @lines = ();

foreach my $n (@nohups) {
    open my $INF, "tail -n 1 $n |" or die;
  again:
    my $line = <$INF>;
    next unless $line;
    goto again if $line =~ /Deep recursion/;
    chomp $line;
    close $INF;
    push @lines, $n . " : " . $line;
}

sub bybest() {
    if (!($a =~ /best = ([0-9]+)$/)) {
	die "oops '$a'";
    }
    my $aa = $1;
    if (!($b =~ /best = ([0-9]+)$/)) {
	die "oops '$b'";
    }
    my $bb = $1;
    return $bb <=> $aa;
}

foreach my $l (sort bybest @lines) {
    print "$l\n";
}

print "total running = ".scalar(@lines)."\n";

