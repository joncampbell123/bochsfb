#!/usr/bin/perl

$project = "tvbox-v2.2-bochsfb";
if ($project eq "") {
	$i = rindex($url,'/');
	if ($i < 0) {$i = 0;}
	else {$i++;}
	$project = substr($url,$i);
}

if (!open(S,"svn info --non-interactive |")) { exit 1; }
my $lcrev = "x";
my $i,$lcdate = "unknown";
foreach my $line (<S>) {
	chomp $line;
	my @nv = split(/: +/,$line);
	my $value = $nv[1];
	my $name = $nv[0];

	if ($name =~ m/^Last Changed Rev$/i && $lcrev eq "x") {
		$lcrev = int($value);
	}
	elsif ($name =~ m/^Revision$/i && $lcrev eq "x") {
		$lcrev = int($value);
	}
	elsif ($name =~ m/^Last Changed Date$/i) {
		my @b = split(/ +/,$value);
		my $date = $b[0];
		my $time = $b[1];

		$date =~ s/\-//g;
		$time =~ s/\://g;
		$lcdate = $date."-".$time;
	}
}
close(S);

my $pwd = `pwd`; chomp $pwd;
my $what = '';

my $filename = "../".$project."-rev-".sprintf("%08u",$lcrev)."-src.tar";
die unless -f "$filename.xz";
$what .= "$filename.xz ";

$x = system("scp -p -P 52222 $what root\@192.168.250.1:/mnt/main/jmc-storage/docs/Projects/bochsfb/");

