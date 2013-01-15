#! /usr/bin/perl
# Author: Tyler D Muth - http://tylermuth.wordpress.com
# Version: 2.2

use warnings;
use strict;
use Getopt::Long;
use Cwd;
my ($working_dir,@infiles, $headers, $header_file);
usage() if ( @ARGV < 1 or
          ! GetOptions('working_dir=s' => \$working_dir, 'infiles=s@' => \@infiles, 'headers=s' => \$headers, 'header_file=s' => \$header_file));

$headers = defined($headers) ? $headers : 'AUTO'; $headers=uc($headers);
$working_dir = defined($working_dir) ? $working_dir : '.';

my $original_dir=getcwd;

if (defined $ENV{MY_WORKING_DIR}) {
	$working_dir=$ENV{MY_WORKING_DIR};
}

print "\nAbout to process files in: $working_dir\n\n";
chdir $working_dir;
my $out_dir="$working_dir/CSVs";


file_loop();
my $work_dir = getcwd . "/CSVs";
$work_dir=~ s{\\}{/}g;
print "\n\n*** Use the following string in the setwd line near the top of the .R files: ***\n";
print "$work_dir\n\n";









		  
sub usage
{
  print "Unknown option: @_\n" if ( @_ );
  print qq/usage: perl sqlplus_spool_to_csv.pl [--working_dir "c:\\temp" | "\/tmp"] [--infiles "file.txt" | "*.out"] \n[--headers [AUTO] | FILE | NONE] [--header_file header.txt] [--help|-?]\n/;
  print qq/working_dir is the directory that contains the files you want to process \n/;
  print qq/Note that you can also set the OS environment variale MY_WORKING_DIR which will override the working_dir parameter. \n/;
  exit;
}

sub file_loop{
	mkdir $out_dir;
	for my $in_file ( map {glob($_)} @infiles ) { 
		print $in_file, "\n";
				
		my $unpack_layout =''; my $header_titles=''; my $headers_mapped='';my $dashes_line='';
		my $header_line_new='';my $extra_vals='';
		
		my @file_name_parts = split(/-/, $in_file);
		my $out_file_prefix = "$file_name_parts[3]-$file_name_parts[2]";
		#print "$out_file_prefix\n";
		open ( INFILE, "<$in_file" ) or die "$!";
		
		my @os_info = get_section(*INFILE,'~~BEGIN-OS-INFORMATION~~','~~END-OS-INFORMATION~~');		
		section_to_csv($out_file_prefix,"os",\@os_info);
		
		my @memory = get_section(*INFILE,'~~BEGIN-MEMORY~~','~~END-MEMORY~~');		
		section_to_csv($out_file_prefix,"memory",\@memory);
		
		my @size_on_disk = get_section(*INFILE,'~~BEGIN-SIZE-ON-DISK~~','~~END-SIZE-ON-DISK~~');		
		section_to_csv($out_file_prefix,"space",\@size_on_disk);
				
		my @main_stats = get_section(*INFILE,'~~BEGIN-MAIN-METRICS~~','~~END-MAIN-METRICS~~');		
		section_to_csv($out_file_prefix,"main",\@main_stats);
		
		my @aas = get_section(*INFILE,'~~BEGIN-AVERAGE-ACTIVE-SESSIONS~~','~~END-AVERAGE-ACTIVE-SESSIONS~~');		
		section_to_csv($out_file_prefix,"aas",\@aas);
		
		my @io_histogram = get_section(*INFILE,'~~BEGIN-IO-WAIT-HISTOGRAM~~','~~END-IO-WAIT-HISTOGRAM~~');		
		section_to_csv($out_file_prefix,"io-histogram",\@io_histogram);
		
		my @io_obj_type = get_section(*INFILE,'~~BEGIN-IO-OBJECT-TYPE~~','~~END-IO-OBJECT-TYPE~~');		
		section_to_csv($out_file_prefix,"io-obj-type",\@io_obj_type);
		
		my @sqlSummary = get_section(*INFILE,'~~BEGIN-TOP-SQL-SUMMARY~~','~~END-TOP-SQL-SUMMARY~~');		
		section_to_csv($out_file_prefix,"sql-summary",\@sqlSummary);
		
		my @sqlBySnap = get_section(*INFILE,'~~BEGIN-TOP-SQL-BY-SNAPID~~','~~END-TOP-SQL-BY-SNAPID~~');		
		section_to_csv($out_file_prefix,"sql-by-snap",\@sqlBySnap);
		
		close INFILE;
	};
}

sub section_to_csv{
	my $outfile_name=$_[0]."-$_[1].csv";
	my $in_array=$_[2]; 
	
	my @inArr=@{$in_array};
	
	my $unpack_layout =''; my $header_titles=''; my $headers_mapped='';my $dashes_line='';
	my $header_line_new='';my $extra_vals='';
	open ( OUTFILE, ">$out_dir/$outfile_name" ) or die "$!";
	get_unpack_layout(\@inArr,$unpack_layout,$dashes_line,$header_line_new);
	
	$headers_mapped=text_line_to_csv($header_line_new,$unpack_layout);
	
	print OUTFILE "$headers_mapped\n";
	my $found_dashes=0;
	foreach my $line (@$in_array) {
		last if ($found_dashes==1 && length($line) < 3);
		if($found_dashes==1){
			print OUTFILE text_line_to_csv($line ,$unpack_layout)."\n";
		}
	
		$found_dashes=1 if $line =~ /-{3,}.*/;
	}
	
	close ( OUTFILE );
}

sub get_section{
	my $infile_handle=$_[0];my $begin_string=$_[1];my $end_string=$_[2];
	my @lines=();
	seek($infile_handle, 0, 0);
	while (<$infile_handle>) {
	  if (/$begin_string/../$end_string/) {
		next if /$begin_string/ || /$end_string/;
		chomp;
		push(@lines,$_);
		#print;
	  }
	}
	return @lines;
		
}

sub get_unpack_layout{
	my $in_array=$_[0]; 
	my $unpack_layout_int='';

	
	my $header_line='';

	foreach my $line (@$in_array) {
		
		if ($line =~ /-{3,}.*/) { #if the line starts with 3 or more dashes (may cause problems for < 3 character leading columns
			chomp $line;
			$_[3]=$line; #return the line of dashes so we can skip them if repeated
			my @columns = split(' ', $line);
			foreach my $col (@columns) {
				my $col_length = length($col)+1;
				$unpack_layout_int .= "A$col_length ";
			}
			#print "$unpack_layout_int\n";
			#print "$header_line\n";
			$_[1]=$unpack_layout_int;
			$_[3]=$header_line;
			last;
		}
		$header_line=$line;
	}
}

#sub get_unpack_layout{
#	my $infile_handle=$_[0];my $outfile_handle=$_[1];my $unpack_layout_int='';
#	my $header_line='';
#	while( my $line = <$infile_handle> ){
#		if ($line =~ /-{3,}.*/) { #if the line starts with 3 or more dashes (may cause problems for < 3 character leading columns
#			chomp $line;
#			$_[3]=$line; #return the line of dashes so we can skip them if repeated
#			my @columns = split(' ', $line);
#			foreach my $col (@columns) {
#				my $col_length = length($col)+1;
#				$unpack_layout_int .= "A$col_length ";
#			}
#			$_[2]=$unpack_layout_int;
#			$_[4]=$header_line;
#			last;
#		}
#		$header_line=$line;
#	}
#}

#----------------------------------------------------------
sub get_header_titles{
	my $header_titles_in=$_[0]; my $header_titles_return='';my $dummy_line='';
	$header_titles_return=$header_titles_in if $headers eq 'AUTO';
	if($headers eq 'FILE'){
		open ( HEADERS_FILE, "<$original_dir/$header_file" ) or die "$!";
		$header_titles_return = <HEADERS_FILE>; 
		close ( HEADERS_FILE);
	}
	chomp $header_titles_return; $_[1]=$header_titles_return;
}
#----------------------------------------------------------
sub text_line_to_csv{
	my $in_line=$_[0]; my $in_unpack_layout=$_[1];
	
	my @f = unpack("$in_unpack_layout", $in_line);
	my $out_text = join ',', map { 
		s/^\s+//;  		 # strip leading spaces
			  s/\s+$//;  # strip trailing spaces
			  s/#+/ /g;  # replace any pound signs (#) in the line
		qq/"$_"/ } @f;
	
	#print "$out_text \n" if $out_text =~ m/\#+/g;
	return $out_text;
}


sub get_extra_data{
	my $infile_handle=$_[0];
	my $dummy_line='';
	my @values={};
	my %vals=();
	my $return_string = '';
	
	my $build_return_string = sub{
		my $in_var=$_[0];
		my $temp_val='';
		if (exists  $vals{$in_var}) {
			$temp_val = $vals{$in_var};
			#print "test1 $in_var: $temp_val\t";
			if ($in_var =~ /pga_aggregate_target|sga_target|sga_max_size|memory_target|memory_max_target/) {
				$temp_val = convert_mem_to_gb($temp_val);
			}
		}
		#print "test2 $in_var: $temp_val\n";
		$temp_val = '"'.$temp_val.'",';
		$return_string .= $temp_val;
	};
	
	while( my $line = <$infile_handle> ){
		if ($line =~ /SIZE_GB/) {
			$dummy_line=<$infile_handle>;
			$line=<$infile_handle>;
			chomp $line;
			$line = strip_spaces($line);
			$vals{ 'size_gb' } = $line;
		}
		
		if ($line =~ /pga_aggregate_target|sga_target|sga_max_size|compatible|memory_target|memory_max_target/) {
			chomp $line;
			@values = split(/\s+/, $line);
			$vals{ $values[0] } = $values[1];
		}
		
		if ($line =~ /~~CPU Information~~/) {
			$dummy_line=<$infile_handle>;$dummy_line=<$infile_handle>;$dummy_line=<$infile_handle>; #move forward 3 lines
			$line=<$infile_handle>;
			chomp $line;
			$line = strip_spaces($line);
			@values = split(/\s+/, $line);	
			#print "$line\n";
			$vals{ 'cpus'} = $values[0];
			$vals{ 'cores' } = $values[1];
			$vals{ 'sockets' } = $values[2];
		}
		
		if ($line =~ /~~Platform Information~~/) {
			$dummy_line=<$infile_handle>;$dummy_line=<$infile_handle>;$dummy_line=<$infile_handle>; #move forward 3 lines
			$line=<$infile_handle>;
			chomp $line;	
			$line = strip_spaces($line);
			@values = split(/\s+/, $line);
			
			$vals{ 'platform'} = $values[0];
			$vals{ 'version' } = $values[1];
			$vals{ 'db_name' } = $values[2];
		}
		
		
		
	}
	
	&$build_return_string('cpus');
	&$build_return_string('cores');
	&$build_return_string('sockets');
	
	&$build_return_string('size_gb');
	
	&$build_return_string('memory_target');
	&$build_return_string('memory_max_target');
	&$build_return_string('sga_target');
	&$build_return_string('sga_max_size');
	&$build_return_string('pga_aggregate_target');
	
	&$build_return_string('platform');
	&$build_return_string('version');
	&$build_return_string('db_name');
	$return_string=~s/,+$//;
	$return_string=",".$return_string;

	seek $infile_handle,0,0;
	$_[1]=$return_string;
		
}

sub strip_spaces{
	my $in_var=$_[0];
	$in_var=~s/^\s+//;
	$in_var=~s/\s+$//;
	return $in_var;
}

sub convert_mem_to_gb{
	my $in_val = $_[0];
	$in_val =~s/,//g;
	my $out_var='';
	
	if ($in_val =~ '.*K$'){
		my $val_kb=$in_val; $val_kb=~s/\D//;
		$out_var=int($val_kb)/1024/1024;
	}
	elsif ($in_val =~ '.*M$'){
		my $val_mb=$in_val; $val_mb=~s/\D//;
		$out_var=int($val_mb)/1024;
	}
	elsif($in_val =~ '.*G$') {
		my $val_gb=$in_val; $val_gb=~s/\D//;
		$out_var=int($val_gb);
	}
	else{
		my $val_bytes=$in_val; $val_bytes=~s/\D//;
		$out_var=int($val_bytes)/1024/1024/1024;
	}
	$out_var = sprintf("%.1f", $out_var); 
	return $out_var;
	
}

