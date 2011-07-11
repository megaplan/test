#/usr/bin/perl
# reads rc_server log, extracts sequences, prints N longest ones.
use strict;
use Data::Dumper;
use Getopt::Long;
use FileHandle;

use vars qw($debug @dat $prev $start $sn $len $limit);

exit runall();
#-----------------------------------------------------
sub runall {
$debug = 0;
$limit = 4;
my $infile = '-';
GetOptions (
	"n|limit=i" => \$limit,
	"i|infile=s" => \$infile,
	"d|debug=i"  => \$debug
);

my $fd = new FileHandle $infile;
unless($fd){
	print STDERR "can't open infile '$infile': $!\n";
	return 1;
}
while(my $str = $fd->getline()){
	if($str =~ /rc_server::\d+ put\s+(\d+)/i){
		add_num($1);
	}
}
add_num(-1);
print_result();
}
#-----------------------------------------------------
sub add_num {
my($n) = @_;
return unless $n;
print STDERR "add_num n=$n, prev=$prev, start=$start, sn=$sn, len=$len\n" if $debug > 1;
if($prev && $n == $prev + 1){
	$prev = $n;
	$len++;
}elsif($prev){
	push @dat, {start=>$start, len=>$len, sn=>$sn};
	$start = $n;
	$prev = $n;
	$len = 1;
	$sn++;
}else{
	print STDERR "init, n=$n, prev=$prev, start=$start, sn=$sn, len=$len\n" if $debug;
	$start = $n;
	$prev = $n;
	$len = 1;
	$sn = 1;
}
}
#-----------------------------------------------------
sub print_result {
print STDERR "dat:\n".Dumper(\@dat)."\n" if $debug > 3;
my @t1 = sort {
	$a->{len} <=> $b->{len}
	||
	$b->{sn} <=> $a->{sn}
} @dat;
print STDERR "sorted dat:\n".Dumper(\@t1)."\n" if $debug > 3;
my @t2 = @t1[-$limit..-1];
print STDERR "result:\n".Dumper(\@t2)."\n" if $debug > 4;
for my $item (@t2){
	print "$item->{len} - $item->{start}\n";
}
}
#-----------------------------------------------------
