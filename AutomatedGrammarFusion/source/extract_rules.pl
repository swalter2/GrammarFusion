#!/usr/bin/perl

use strict;
use warnings;
use IPC::System::Simple qw(system capture);

# Mapping concepts from the first set to the second (and then replacing in the first)

# Paths
my $root_dir = "";

# Files
my @sets = ($ARGV[0], $ARGV[1]);
my $mtr = $ARGV[2];
my $map = $ARGV[3]; # 0 for concept mapping, 1 for rule mapping
my $sch = "lexsem";
my $thres = 1;
my $frg_lim = 0;

my %grammar_concepts = ();
my %grammar_rules = ();
# Extract concept fragments into list
foreach my $set (@sets) {
	#print $set."\n========";
	open(SET,$set) || die "Cannot open $set\n";
	while(<SET>) {
		chomp $_;
		my @blocks = split(",",$_);
		my $high_rule = 0;
		for(my $i=1;$i<$#blocks+1;$i++) {
			# Check if it is concept rule. If so, it should not contain other rules in its fragments.
			if ($blocks[$i] =~ /\<|\>/g) {	
				$high_rule = 1;
				if (($#blocks) >= $frg_lim) {
					#print "$blocks[0] is a high-level rule with $#blocks fragments\n";
					#if ($blocks[0] =~ /FROMTIME/) { print $_."\n"; }
					$grammar_rules{$set}{$blocks[0]} = $_;
				}
				last;
			}
		} # do so for each fragment
		if ($high_rule == 0) {
			if (($#blocks) >= $frg_lim) {
				#print "$blocks[0] is a low-level rule with $#blocks fragments\n";
				$grammar_concepts{$set}{$blocks[0]} = $_;
			}
		}
	} # do so for each grammar rule
	close(SET);
} # do so for each grammar set

# Create files 
foreach my $set (@sets) {
	if ($map == 0) {
		open(OUTC,">".$root_dir."concepts_".$set) || die "Cannot write to ".$root_dir."concepts_".$set."\n";
		foreach my $concept (keys %{$grammar_concepts{$set}}) {
			print OUTC $grammar_concepts{$set}{$concept}."\n";
		}
		close(OUTC);
	} elsif ($map == 1) {
		open(OUTR,">".$root_dir."rules_".$set) || die "Cannot write to ".$root_dir."rules_".$set."\n";
		foreach my $rule (keys %{$grammar_rules{$set}}) {
			print OUTR $grammar_rules{$set}{$rule}."\n";
		}
		close(OUTR);
	}
}

# Now we map using an external script
my @arguments = ();
my @results = ();
my %maps_to = ();
my $i = 0;
if ($map == 0) {
	# Concept mapping
	@arguments = ("concepts_".$sets[0], "concepts_".$sets[1], $sch, $mtr);
	#system("perl map_v2.pl concepts_".$sets[0]." concepts_".$sets[1]." $sch $mtr");
	@results = capture($^X, "map_v2.pl", @arguments);
	foreach (@results) {
		$i++;
		next if $i < 3;
		chomp $_;
		my @blocks = split(" ",$_);
		if ($blocks[2] <= $thres) {
			$maps_to{$blocks[0]} = $blocks[1];
		}
	}
} elsif ($map == 1) {
	# Rule mapping
	@arguments = ("rules_".$sets[0], "rules_".$sets[1], $sch, $mtr);
	#system("perl map_v2.pl rules_".$sets[0]." rules_".$sets[1]." $sch $mtr");
	@results = capture($^X, "map_v2.pl", @arguments);
	%maps_to = ();
	foreach (@results) {
		$i++;
		next if $i < 3;
		chomp $_;
		my @blocks = split(" ",$_);
		if ($blocks[2] <= $thres) {
			$maps_to{$blocks[0]} = $blocks[1];
		}
	}
}

# and pass the results to rename the first set concepts
my @replaces = ();
open(RENAME,$root_dir.$sets[0]) || die "Cannot open ".$sets[0]."\n";
my @lines = <RENAME>;
close(RENAME);
open(WRITE,">".$root_dir.$sets[0]) || die "Cannot write to ".$sets[0]."\n";
foreach my $line (@lines) {
	my $moved = 0;
	foreach my $concept (keys %maps_to) {
		my $replacement = $maps_to{$concept};
		# if we find the concept/rule, we remove it and append it of the other grammar (since they are mapped)
		if ($line =~ m/^$concept/) {
			$line =~ s/$concept/$replacement/g;
			push(@replaces, $line);
			$moved = 1;
		} else {
			$line =~ s/$concept/$replacement/g;
		}
	}
	if ($moved == 0) { print WRITE $line; }
}
close(WRITE);

# Now append the replaced to the other grammar
open(AP,$root_dir.$sets[1]) || die "Cannot open ".$sets[1]."\n";
my @otherlines = <AP>;
close(AP);
open(WRITE,">".$root_dir.$sets[1]) || die "Cannot write to ".$sets[1]."\n";
foreach my $otherline (@otherlines) {
	chomp $otherline;
	my ($othertag) = split(",",$otherline);
	foreach my $replace (@replaces) {
		chomp $replace;
		my ($tag) = split(",",$replace);
		if ($tag eq $othertag) {
			$replace =~ s/^$tag//;
			$otherline .= $replace;
		}
	}
	print WRITE $otherline."\n";
}
exit;
close(WRITE);
exit;
